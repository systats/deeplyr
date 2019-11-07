#' trainer_xgboost
#'  @export
trainer_xgboost <- R6::R6Class("trainer_xgboost",
  public = list(
   model = NULL,
   param = NULL,
   data = NULL,
   initialize = function(objective) {

      if (objective == "linear") {
         #self$param$objective <- "reg:squarederror"
         
      } else if (objective == "categorical") {
         
         self$param$objective <- "multi:softmax" # multi:softprob
         
      } else if (objective == "binary") {
         
         self$param$objective <- "binary:logistic"
         
      }
   
   },
   set = function(param, data){
     
     self$data <- data
     self$param <- c(self$param, param)
     
   },
   fit = function(){
     
     dt_train <- xgboost::xgb.DMatrix(data = as.matrix(self$data$train$x), label = self$data$train$y)
     
     if(!is.null(self$data$val$x)){
        dt_val <- xgboost::xgb.DMatrix(data = as.matrix(self$data$val$x), label = self$data$val$y)
        watchlist <- list(train = dt_train, eval = dt_val)
     } else {
        dt_test <- xgboost::xgb.DMatrix(data = as.matrix(self$data$test$x), label = self$data$test$y)
        watchlist <- list(train = dt_train, eval = dt_test)
     }
     
     self$model <- xgboost::xgb.train(
       self$param,
       dt_train,
       nround = 100,
       missing = NA,
       early_stopping_rounds = 5,
       verbose = T,
       watchlist
     )
     
     # model <- xgboost::xgb.cv(
     #   data = dt_train,
     #   params = self$param,
     #   nthread = 4,
     #   nfold = 5,
     #   nround = 100,
     #   missing = NA,
     #   early_stopping_rounds = 10,
     #   verbose = T,
     # )
  
   },
   predict = function(x_test = NULL){

     if(is.null(x_test)) x_test <- self$data$test$x
     
     data_test <- xgboost::xgb.DMatrix(data = as.matrix(x_test))
     pred <- predict(object = self$model, newdata = data_test)
     # pred <- predict(self$model, x_test)
     # 
     # if(ncol(pred) == 1) pred <- pred[,1, drop=T]
     # 
     return(pred)
   }
  )
)
