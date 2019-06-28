#' @export
fit_xgboost <- function(self, private, cv = F){
  
  # print(nrow(self$splits$train$x))
  # print(length(self$splits$train$y))
  # print(nrow(self$splits$test$x))
  # print(length(self$splits$test$y))
  
  dt_train <- xgboost::xgb.DMatrix(data = self$splits$train$x, label = self$splits$train$y)
  dt_test <- xgboost::xgb.DMatrix(data = self$splits$test$x, label = self$splits$test$y)
  watchlist <- list(train = dt_train, eval = dt_test)
  
  model <- xgboost::xgb.train(
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
  
  return(model)
}

#' @export
predict_xgboost <- function(model, x_test){
  dtest <- xgboost::xgb.DMatrix(data = as.matrix(x_test))
  return(predict(model, dtest)) # take only second column for 1s
}
