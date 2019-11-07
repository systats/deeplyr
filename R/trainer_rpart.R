#' trainer_rpart
#'  @export
trainer_rpart <- R6::R6Class("trainer_rpart",
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
      
       # if(!is.null(self$data$val$x)){
       #   xval <- as.matrix(self$data$val$x)
       #   yval <- self$data$val$y
       # } else {
       #    xval <- as.matrix(self$data$test$x)
       #    yval <- self$data$test$y
       # }

        self$data$train$x$y <- self$data$train$y
        self$model <- rpart::rpart(
          formula = y ~ .,
          data = self$data$train$x,
          parms = self$param, 
          # xtest = xval,
          # ytest = yval,
          # type = "classification",
          #ntree = 30,
          #keep.forest = T
       )
       
     },
     predict = function(x_test = NULL){
       
       if(is.null(x_test)) x_test <- self$data$test$x
       
       pred <- predict(self$model, x_test)

       return(pred)
     }
   )
)
