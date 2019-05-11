#' @export
fit_xgboost <- function(self, private){
  
  keras_param <- list( 
    params = private$param,
    data = xgboost::xgb.DMatrix(data = private$x_train, label = private$y_train),  
    nthread = 4, 
    nround = 100, 
    missing = NA,
    early_stopping_rounds = 5,
    verbose = T
  ) 
  
  model <- do.call(xgboost::xgboost, compact(keras_param))
  
  return(model)
}

#' @export
predict_xgboost <- function(model, x_test){
  dtest <- xgboost::xgb.DMatrix(data = x_test)
  return(predict(model, dtest)) # take only second column for 1s
}