#' trainer_catboost
#'  @export
trainer_catboost <- R6::R6Class("trainer_catboost",
   public = list(
     model = NULL,
     param = NULL,
     data = NULL,
     initialize = function(objective) {
       
       if (objective == "linear") {
          
          self$param$loss_function <- "RMSE"

       } else if (objective == "categorical") {

          self$param$loss_function <- "MultiClass"

       } else if (objective == "binary") {

         self$param$loss_function <- "CrossEntropy"

       }
       
     },
     set = function(param, data){
       
       self$data <- data
       self$param <- c(self$param, param)
       
     },
     fit = function(){
       
       train <- catboost::catboost.load_pool(data = as.matrix(self$data$train$x), label = self$data$train$y)
 
       if(!is.null(self$data$val$x)){
         val <- catboost::catboost.load_pool(data = as.matrix(self$data$val$x), label = self$data$val$y)
       } else {
         val <- catboost::catboost.load_pool(data = as.matrix(self$data$test$x), label = self$data$test$y)
       }

       if(is.null(self$param$random_seed)) self$param$random_seed <- 42
       
       self$model <- catboost::catboost.train(learn_pool = train, test_pool = val, params = self$param)
       
     },
     predict = function(x_test = NULL){
       
       if(is.null(x_test)) x_test <- self$data$test$x
       
       test <- catboost::catboost.load_pool(data = as.matrix(x_test))
       pred <- catboost::catboost.predict(self$model, pool = test)

       return(pred)
     }
   )
)
