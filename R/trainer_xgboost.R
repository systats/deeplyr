#' trainer_xgboost
#'  @export
trainer_xgboost <- R6::R6Class("trainer_xgboost",
  private = list(
  ),
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
     
     dt_train <- xgboost::xgb.DMatrix(data = self$data$train$x, label = self$data$train$y)
     dt_test <- xgboost::xgb.DMatrix(data = self$data$test$x, label = self$data$test$y)
     watchlist <- list(train = dt_train, eval = dt_test)
     
     self$model <- xgboost::xgb.train(
       self$param,
       dt_train,
       nround = 100,
       missing = NA,
       early_stopping_rounds = 10,
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
     if(is.null(x_test)) return(message("No new data found"))
     
     dtest <- xgboost::xgb.DMatrix(data = as.matrix(x_test))
     pred <- predict(self$model, dtest) # take only second column for 1s
    
     # pred <- predict(self$model, x_test)
     # 
     # if(ncol(pred) == 1) pred <- pred[,1, drop=T]
     # 
     return(pred)
   }
  )
)
