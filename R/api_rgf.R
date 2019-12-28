#' fit_rgf
#' @export
fit_rgf <- function(self){
   
   ### set loss function
   if(self$params$task == "linear") {

      model <- RGF:::RGF_Regressor$new()
      
   } else if(self$params$task == "multi") {

      model <- RGF::RGF_Classifier$new()
      
   } else if(self$params$task == "binary") {

      model <- RGF::RGF_Classifier$new()
   }

   model$fit(x = as.matrix(self$data$train$x), y = self$data$train$y)
   
   return(model)
}


#' predict_rgf
#' @export
predict_rgf <- function(self, x_test = NULL){
   
   if(is.null(x_test)) x_test <- self$data$test$x
   
   #test <- lightgbm::lgb.Dataset(data = x_test)
   preds <- self$model$predict(as.matrix(x_test))
   
   return(preds)
   # if(self$params$task == "linear"){
   #    pred <- round(predict(self$model, as.matrix(x_test)), 3)
   #    tibble(pred)
   # } else if(self$params$task == "binary"){
   #    prob <- round(predict(self$model, as.matrix(x_test)), 3)
   #    pred <- ifelse(prob > .5, 1, 0)
   #    tibble(pred, prob0 = 1 - prob, prob1 = prob)
   # } else if(self$params$task == "multi"){
   #    pred <- round(predict(self$model, as.matrix(x_test), reshape = T), 3)
   #    probs <- as_tibble(pred) %>% set_names(paste0("prob", 1:ncol(pred)))
   #    pred <- pred %>% split(1:nrow(.)) %>% map_int(which.max)
   #    tibble(pred) %>% bind_cols(probs)
   # }
}




