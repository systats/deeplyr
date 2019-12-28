#' fit_catboost
#' @export
fit_catboost <- function(self){
   
   ### set loss function
   if(self$task == "linear") {
      self$params$loss_function <- "RMSE"
   } else if(self$task == "multi") {
      self$params$loss_function <- "MultiClassOneVsAll"#"MultiClass"
   } else if(self$task == "binary") {
      self$params$loss_function <- "CrossEntropy"
   }
   
   ### prepare data
   train <- catboost::catboost.load_pool(data = as.matrix(self$data$train$x), label = self$data$train$y)
   
   if(!is.null(self$data$val$x)){
      val <- catboost::catboost.load_pool(data = as.matrix(self$data$val$x), label = self$data$val$y)
   } else {
      val <- catboost::catboost.load_pool(data = as.matrix(self$data$test$x), label = self$data$test$y)
   }
   
   ### main model
   self$params$random_seed <- 42
   model <- catboost::catboost.train(learn_pool = train, test_pool = val, params = self$params)
   
   return(model)
}


#' predict_catboost
#' @export
predict_catboost <- function(self, x_test = NULL){
   
   if(is.null(x_test)) x_test <- self$data$test$x
   
   if(self$task == "linear"){
      type <- "RawFormulaVal"
   } else {
      type <- "Probability"
   }
   
   test <- catboost::catboost.load_pool(data = as.matrix(x_test))
   pred <- round(catboost::catboost.predict(self$model, pool = test, prediction_type = type), 3)
   
   if(self$task == "linear"){
      tibble(pred)
   } else if(self$params$task == "binary"){
      prob <- pred
      pred <- ifelse(prob > .5, 1, 0)
      tibble(pred, prob0 = 1 - prob, prob1 = prob)
   } else if(self$task == "multi"){
      probs <- as_tibble(pred) %>% set_names(paste0("prob", 1:ncol(pred)))
      pred <- pred %>% split(1:nrow(.)) %>% map_int(which.max)
      tibble(pred) %>% bind_cols(probs)
   }
}




