#' save_xgboost
#' @export
save_xgboost <- function(file, name, path) xgboost::xgb.save(file, fname = glue::glue("{path}/{name}"))


#' feature_imp_xgboost
feature_imp_xgboost <- function(self){

   xgboost::xgb.importance(feature_names = colnames(self$data$train$x), model = self$model) %>%
      as_tibble() %>%
      janitor::clean_names() 
}

#' fit_xgboost
#' @export
fit_xgboost <- function(self){

   ### define objective
   if(self$task == "linear") self$params$objective <- "reg:squarederror"
   if(self$task == "binary") self$params$objective <- "binary:logistic"
   if(self$task == "multi") self$params$objective <- "multi:softprob"
   
   ## set number of classes for object=multi
   if(is.null(self$params$num_class)){
     if(self$task == "multi"){
       if(length(unique(self$data$train$y)) > 2){
         self$params$num_class <- length(unique(self$data$train$y))
       }
     }
   }
      
   ### set training and evaluation data
   train <- xgboost::xgb.DMatrix(data = as.matrix(self$data$train$x), label = self$data$train$y)
   
   if(!is.null(self$data$val$x)){
      watchlist <- xgboost::xgb.DMatrix(data = as.matrix(self$data$val$x), label = self$data$val$y) %>% 
         list(train = train, eval = .)
   } else {
      watchlist <- xgboost::xgb.DMatrix(data = as.matrix(self$data$test$x), label = self$data$test$y) %>% 
         list(train = train, eval = .)
   }
   
   ### main call
   model <- xgboost::xgb.train(
      params = self$params,
      data = train,
      nround = 50, 
      nthread = 4, 
      missing = NA, 
      early_stopping_rounds = 3,
      verbose = T,
      watchlist
   )
   
   return(model)
}

#' predict_xgboost
#' @export
predict_xgboost <- function(self, x_test = NULL){
   
   if(is.null(x_test)) x_test <- self$data$test$x
   
   x_test <- xgboost::xgb.DMatrix(data = as.matrix(x_test))
   
   if(self$task == "linear"){
      pred <- round(predict(self$model, newdata = x_test), 3)
      tibble(pred)
   } else if(self$task == "binary"){
      prob <- round(predict(self$model, newdata = x_test), 3)
      pred <- ifelse(prob > .5, 1, 0)
      tibble(pred, prob0 = 1 - prob, prob1 = prob)
   } else if(self$task == "multi"){
      prob_col <- length(unique(self$data$train$y))
      probs <- round(predict(self$model, newdata = x_test, reshape = T), 3) %>% as_tibble %>% set_names(paste0("prob", 1:prob_col))
      pred <- probs %>% split(1:nrow(.)) %>% map_int(which.max)
      tibble(pred) %>% bind_cols(probs)
   }

}









