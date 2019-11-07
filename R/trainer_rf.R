#' trainer_rf
#'  @export
trainer_rf <- R6::R6Class("trainer_rf",
   public = list(
     model = NULL,
     param = NULL,
     data = NULL,
     initialize = function(objective) {
       
     },
     set = function(param, data){
       
       self$data <- data
       self$param <- c(self$param, param)
       
     },
     fit = function(){
      
       if(!is.null(self$data$val$x)){
          xval <- as.matrix(self$data$val$x)
          yval <- self$data$val$y
       } else {
          xval <- as.matrix(self$data$test$x)
          yval <- self$data$test$y
       }
       
       self$model <- randomForest::randomForest(
          x = as.matrix(self$data$train$x),
          y = self$data$train$y,
          xtest = xval,
          ytest = yval,
          type = "classification",
          #ntree = 30,
          keep.forest = T
       )
       
     },
     predict = function(x_test = NULL){
       
       if(is.null(x_test)) x_test <- self$data$test$x
       
       pred <- predict(self$model, as.matrix(x_test))

       return(pred)
     }
   )
)
