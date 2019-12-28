#' fit_randomForest
#' @export
fit_rpart <- function(self){
   
   self$data$train$x$y <- self$data$train$y
   
   if(self$task == "linear"){
      model <- rpart::rpart(
         formula = y ~ .,
         data = as_tibble(self$data$train$x),
         parms = self$params
      )
   } else {
      #self$params$method <- "class"
      model <- rpart::rpart(
         formula = y ~ .,
         data = as_tibble(self$data$train$x),
         parms = self$params
      )
   }
   

   

   
   return(model)
}

#' predict_rpart
#' @export
predict_rpart <- function(self, x_test = NULL){
   
   if(is.null(x_test)) x_test <- self$data$test$x
   
   pred <- round(predict(self$model, newdata = as_tibble(x_test)), 3)
   
   if(self$task == "linear"){
      tibble(pred)
   } else if(self$task == "binary"){
      prob <- pred
      pred <- ifelse(prob > .5, 1, 0)
      tibble(pred, prob)
   } else if(self$task == "multi"){
      pred
   }
}


