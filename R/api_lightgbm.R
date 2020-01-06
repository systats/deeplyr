#' fit_lightgbm
#' @export
fit_lightgbm <- function(self){
   
   ### set loss function
   if(self$task == "linear") {
      self$params$objective <- "regression" #regression_l1
      self$params$metric <- "l2"
   } else if(self$task == "multi") {
      self$params$objective <- "multiclass"
   } else if(self$task == "binary") {
      self$params$objective <- "binary"
   }
  
  ## set number of classes for object=multi
  if(is.null(self$params$num_class)){
    if(self$task == "multi"){
      if(length(unique(self$data$train$y)) > 2){
        self$params$num_class <- length(unique(self$data$train$y))
      }
    }
  }
   
   ### prepare data
   train <- lightgbm::lgb.Dataset(data = as.matrix(self$data$train$x), label = self$data$train$y)
   
   if(!is.null(self$data$val$x)){
      val <- lightgbm::lgb.Dataset(data = as.matrix(self$data$val$x), label = self$data$val$y)
   } else {
      val <- lightgbm::lgb.Dataset(data = as.matrix(self$data$test$x), label = self$data$test$y)
      #lightgbm::lgb.Dataset.create.valid(
   }
   
   ### main model
   model <- lightgbm::lgb.train(params = self$params, data = train, valids = list(val = val), nrounds = 100, early_stopping_rounds = 5L)

   return(model)
}


#' predict_lightgbm
#' @export
predict_lightgbm <- function(self, x_test = NULL){
   
   if(is.null(x_test)) x_test <- self$data$test$x
   
   #test <- lightgbm::lgb.Dataset(data = x_test)
   
   if(self$task == "linear"){
      pred <- round(predict(self$model, as.matrix(x_test)), 3)
      tibble(pred)
   } else if(self$task == "binary"){
      prob <- round(predict(self$model, as.matrix(x_test)), 3)
      pred <- ifelse(prob > .5, 1, 0)
      tibble(pred, prob)
   } else if(self$task == "multi"){
      pred <- round(predict(self$model, as.matrix(x_test), reshape = T), 3)
      probs <- as_tibble(pred) %>% set_names(paste0("prob", 1:ncol(pred)))
      pred <- pred %>% split(1:nrow(.)) %>% map_int(which.max)
      tibble(pred) %>% bind_cols(probs)
   }
}




