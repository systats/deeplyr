#' @export
fit_xgboost <- function(self, private, cv = F){
  
  dt_train <- xgboost::xgb.DMatrix(data = as.matrix(private$x_train), label = private$y_train)
  dt_test <- xgboost::xgb.DMatrix(data = as.matrix(private$x_test), label = private$y_test)
  watchlist <- list(train = dt_train, eval = dt_test)
  
  model <- xgboost::xgb.train(
    private$param,
    dt_train,
    nround = 100,
    missing = NA,
    early_stopping_rounds = 10,
    verbose = T,
    watchlist
  )
  # model <- xgboost::xgb.cv(
  #   data = dt_train,
  #   params = private$param,
  #   nthread = 4,
  #   nfold = 5,
  #   nround = 100,
  #   missing = NA,
  #   early_stopping_rounds = 10,
  #   verbose = T,
  # )
  
  return(model)
}

#' @export
predict_xgboost <- function(model, x_test){
  dtest <- xgboost::xgb.DMatrix(data = as.matrix(x_test))
  return(predict(model, dtest)) # take only second column for 1s
}
