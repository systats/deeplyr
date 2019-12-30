#' learner
#' @export
learner <- R6::R6Class("learner",
                       
  #inherit = backends, 
  private = list(
    init_backend = function(){
      
      if(self$backend == "keras"){
        self$fit <- fit_keras
        self$predict <- predict_keras
        #self$imp <- feature_imp_keras
        self$save_model <- save_keras
      }
      if(self$backend == "xgboost"){
        self$fit <- fit_xgboost
        self$predict <- predict_xgboost
        self$imp <- feature_imp_xgboost
        self$save_model <- save_xgboost
      }
      if(self$backend == "lightgbm"){
        self$fit <- fit_lightgbm
        self$predict <- predict_lightgbm
      }
      if(self$backend == "catboost"){
        self$fit <- fit_catboost
        self$predict <- predict_catboost
        self$imp <- h2o_feature_imp
      }
      # if(self$backend == "rgf"){
      #   self$fit <- fit_rgf
      #   self$predict <- predict_rgf
      # }
      if(str_detect(self$backend, "h2o_")){
        
        self$predict <- predict_h2o
        self$imp <- feature_imp_h2o
        self$save_model <- save_h2o
        
        if(self$backend == "h2o_glm") self$fit <- fit_h2o_glm
        if(self$backend == "h2o_rf") self$fit <- fit_h2o_rf
        if(self$backend == "h2o_nb")  self$fit <- fit_h2o_nb
        if(self$backend == "h2o_svm") self$fit <- fit_h2o_svm
        if(self$backend == "h2o_gbm") self$fit <- fit_h2o_gbm
        if(self$backend == "h2o_xgboost") self$fit <- fit_h2o_xgboost
        if(self$backend == "h2o_dnn") self$fit <- fit_h2o_dnn
        if(self$backend == "h2o_cox") self$fit <- fit_h2o_cox
      }
      if(str_detect(self$backend, "sk_")){
        
        self$predict <- predict_sk
        
        if(self$backend == "sk_glm") self$fit <- fit_sk_glm
        if(self$backend == "sk_tree") self$fit <- fit_sk_tree
      }
      if(self$backend == "ranger"){
        self$fit <- fit_ranger
        self$predict <- predict_ranger
      }
      if(self$backend == "randomForest"){
        self$fit <- fit_randomForest
        self$predict <- predict_randomForest
      }
      if(self$backend == "rpart"){
        self$fit <- fit_rpart
        self$predict <- predict_rpart
      }
    }
  ),
  public = list(
    
    ### initalize variables
    ### methods 
    fit = NULL,
    predict = NULL,
    imp = NULL,
    save_model = NULL,
    ### outputs
    meta = NULL,
    task = NULL,
    backend = NULL,
    data = NULL,
    params = NULL,
    model = NULL,
    preds = NULL,
    imps = NULL,
    metrics = NULL,
    
    ### init fun
    initialize = function(task, backend) {
      
      self$meta$task <- task
      self$meta$backend <- backend
      self$meta$start <- Sys.time()
      
      self$task <- task
      self$backend <- backend
    
    },
    feed = function(params, data){
      self$params <- c(self$params, params)
      self$data <- data
      
      self$meta$n_train <- nrow(data[[1]]$x)
      self$meta$n_test <- nrow(data[[2]]$x)
    },
    train = function(){
      
      private$init_backend()
    
      tictoc::tic()
      
      self$model <- self$fit(self)
      
      time <- tictoc::toc(log = T)
      
      suppressMessages(
        self$meta$runtime <- as.numeric(time$toc - time$tic) %>% round(1)
      )
      
    },
    test = function(dev = F){
      
      ### perform main prediction
      preds <- self$predict(self)
      
      ### dev gate
      if(dev) return(preds)
      
      ### bind target
      if(is.null(ncol(self$data$test$y))) preds <- preds %>% mutate(target = self$data$test$target)
      
      ### bind meta
      if(!is.null(self$data$test$meta)){
        self$preds <- cbind(preds, self$data$test$meta)
      } else {
        self$preds <- preds
      }
      
      ### compute eval metrics 
      self$metrics <- eval_model(self)
      
      ### compute feature importance
      if(!is.null(self$imp)){
        self$imps <- self$imp(self)
      }
      
      self$meta$end <- Sys.time()
    },
    add_metrics = function(cv){
      self$metrics <- cbind(self$metrics, cv)
    },
    save = function(path = NULL){
      
      if(is.null(path)) path <- "."
      
      ### model
      self$save_model <- purrr::possibly(self$save_model, NULL)
      self$save_model(self$model, "model", path)
      
      ### meta
      if(!is.null(self$meta)) save_json_pos(self$meta, "meta", path)
      
      ### params
      params <- self$params %>% keep(~is.numeric(.x)|is.character(.x))
      if(!is.null(params)) save_json_pos(params, "params", path)
      
      ### metrics
      if(!is.null(self$metrics)) save_json_pos(self$metrics, "metrics", path)
      
      ### preds
      if(!is.null(self$preds)) save_rds_pos(self$preds, "preds", path)
      
      ### imps
      if(!is.null(self$imps)) save_rds_pos(self$imps, "imps", path)
    }
  )
)

#' fit_learner
#' 
#' @export
fit_learner <- function(params, data, task, backend, path = NULL, dev = F){
  f <- learner$new(task, backend)
  f$feed(params, data)
  f$train()
  f$test()
  if(!is.null(path)) f$save(path)
  return(f)
}

#' split_cv
#' 
#' @export
split_cv <- function(data, fold){
  df <- list()
  ### training split
  df$train$x <- data[-fold] %>% map("x") %>% rlist::list.rbind()
  df$train$y <- data[-fold] %>% map("y") %>% unlist
  df$train$target <- data[-fold] %>% map("target") %>% unlist
  df$train$meta <- data[-fold] %>% map("meta") %>% rlist::list.rbind()
  
  ### testing split
  df$test$x <- data[fold] %>% map("x") %>% rlist::list.rbind()
  df$test$y <- data[fold] %>% map("y") %>% unlist
  df$test$target <- data[fold] %>% map("target") %>% unlist
  df$test$meta <- data[fold] %>% map("meta") %>% rlist::list.rbind()
  
  return(df)
}

#' fit_cv
#' 
#' @export
fit_cv <- function(params, data, task, backend, path = NULL, dev = F){
  
  ### calculate a few times the same model with rotating data
  models <- 1:length(data) %>%
    purrr::map(~{
      df <- split_cv(data, .x)
      fit_learner(params, data = df, task, backend)
    })
  
  ### extract metrics form cv models
  folds <- models %>% 
    purrr::map_dfr("metrics") %>% 
    dplyr::mutate(fold = 1:n())
  
  ### generate avergae fold stats
  cv <- folds %>%
    dplyr::select(-fold) %>%
    dplyr::summarise_all(mean) %>%
    dplyr::rename_all(~paste0("cv_", .x)) %>%
    dplyr::mutate(folds = list(folds))
  
  ### retrain best fit
  # splits <- split_cv(data, best)
  ### train custom model object
  # f <- learner$new(task, backend)
  # f$feed(params, splits)
  # f$train()
  # f$test()
  
  ### determine best model/fold to be extracted
  if(task == "linear") best <- folds %>% dplyr::arrange(rmse) %>% head(1) %>% dplyr::pull(fold)
  if(task %in% c("binary", "multi")) best <- folds %>% dplyr::arrange(dplyr::desc(accuracy)) %>% head(1) %>% dplyr::pull(fold)
  
  f <- models[[best]]
  f$add_metrics(cv)
  if(!is.null(path)) f$save(path)
  
  return(f)
}
