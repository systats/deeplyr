#' learner
#' @export
learner <- R6::R6Class("learner",
  #inherit = backends, 
  private = list(
    backend = NULL,
    objective = NULL,
    metrics = NULL,
    new_path = NULL,
    set_backend = function(backend){
      
      input <- backend %>%
        str_split(":") %>% unlist()
      
      private$backend <- input[1]
      private$objective <- input[2]
      
      if(private$backend == "keras") self$trainer <- trainer_keras$new(private$objective)
      if(private$backend == "xgboost") self$trainer <- trainer_xgboost$new(private$objective)
      if(private$backend == "ranger") self$trainer <- trainer_ranger$new(private$objective)
      if(private$backend == "catboost") self$trainer <- trainer_catboost$new(private$objective)
      if(private$backend == "rf") self$trainer <- trainer_rf$new(private$objective)
      if(private$backend == "rpart") self$trainer <- trainer_rpart$new(private$objective)
      
      self$eval <- evaluator$new(private$objective)
    }
  ),
  public = list(
    ### Initalize variables
    splits = NULL,
    data = NULL,
    param = NULL,
    trainer = NULL,
    eval = NULL, 
    results = NULL,
    meta = NULL, 
    ### Main Function
    initialize = function(backend, folder = "") {
      
      private$set_backend(backend)
      
    },
    get_entry = function(name){
      return(list(private = private[[name]], self = self[[name]]) %>% compact %>% .[[1]])
    },
    set_splits = function(splits){
      self$splits <- splits
    },
    set_param = function(list){
      self$param <- list
    },
    get_param = function(){
      return(self$param)
    },
    train = function(){
    
      tictoc::tic()
      
      self$trainer$set(param = self$param, data = self$splits)
      self$trainer$fit()
      
      time <- tictoc::toc(log = T)
      
      suppressMessages(
        self$param$duration <- as.numeric(time$toc - time$tic) %>% round(1)
      )
      
      self$param <- self$trainer$param
    },
    test = function(dev = F){
      
      preds <- self$trainer$predict()
      
      if(dev) return(preds)
      
      self$eval$eval(target = self$splits$test$y, pred = preds, meta = self$splits$test$meta)
      
      self$results <- self$trainer$param %>%
        keep(~is.atomic(.x) & length(.x) == 1) %>%
        bind_cols %>%
        mutate(metrics = list(self$eval$perform)) %>%
        mutate(timestamp = as.character(Sys.time()))
    }
    # report = function(){
    #   ### save model container
    #   private$save_model_container()
    #   
    #   ### save plots
    #   ggsave_pos <- possibly(ggsave, NULL)
    #   self$eval$plots %>% 
    #     iwalk(~ggsave_pos(.x, file = glue::glue("{private$new_path}/{.y}.png")))
    # }
  )
)

#' fit_learner
#' @export
fit_learner <- function(param, splits, backend = "xgboost:binary", dev = F){
  model <- deeplyr::learner$new(backend)
  model$set_param(param)
  model$set_splits(splits)
  model$train()
  model$test(dev = dev)
  return(model)
}