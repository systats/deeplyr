#' save_h2o
#' @export
save_h2o <- function(file, name, path) {
  h2o::h2o.saveModel(file, path = path, force = T) #
  file.rename(file.path(path, file@model_id), glue::glue("{path}/model"))
}

#' feature_imp_h2o
#' @export
feature_imp_h2o <- function(self){
  self$model %>%
    h2o::h2o.varimp() %>%
    as.data.frame() %>%
    dplyr::rename(feature = variable)
}

#' predict_h2o
#' @export
predict_h2o <- function(self, x_test = NULL){
  
  if(is.null(x_test)) x_test <- self$data$test$x
  
  h2o_test <- h2o::as.h2o(x_test)
  
  preds <- as.data.frame(h2o::h2o.predict(self$model, h2o_test))
  
  if(self$task == "linear"){
    
    preds <- preds %>% 
      purrr::set_names("pred") %>% 
      dplyr::mutate_if(is.numeric, round, 3)
    
  } else if(self$task == "binary"){
    
    preds <- preds %>% 
      purrr::set_names(c("pred", paste0("prob", 0:(ncol(preds) - 2)))) %>% 
      dplyr::mutate_if(is.numeric, round, 3)
    
  } else if(self$task %in% c("ordinal", "multi")){
    
    preds <- preds %>% 
      purrr::set_names(c("pred", paste0("prob", 1:(ncol(preds) - 1)))) %>% 
      dplyr::mutate_if(is.numeric, round, 3)
    
  }
  
  return(preds)
}


#' fit_h2o_glm
#' @export
fit_h2o_glm <- function(self){
  
  ### define objective
  if(self$task == "linear") self$params$family <- "gaussian"
  if(self$task == "binary") self$params$family <- "binomial"
  if(self$task == "multi") self$params$family <- "multinomial"
  if(self$task == "ordinal") self$params$family <- "ordinal"
  
  # "gaussian", "binomial", "quasibinomial", "ordinal", "multinomial", 
  # "poisson", "gamma", "tweedie", "negativebinomial"
  
  h2o_train <- h2o::as.h2o(as_tibble(self$data$train$x))
  h2o_train$y <- h2o::as.h2o(self$data$train$y)
  
  if(self$task %in% c("binary", "ordinal", "multi")) h2o_train$y <- h2o::as.factor(h2o_train$y)
  
  if(!is.null(self$data$val$x)){
    h2o_val <- h2o::as.h2o(as_tibble(self$data$val$x))
    h2o_val$y <- h2o::as.h2o(self$data$val$y)
    if(self$task %in% c("binary", "ordinal", "multi")) h2o_val$y <- h2o::as.factor(h2o_val$y)
  } else {
    h2o_val <- NULL
  }
  
  x <- colnames(self$data$train$x)
  
  model_params <- list(
    #family = self$params$family,
    x = x, 
    y = "y", 
    training_frame = h2o_train, 
    validation_frame =  h2o_val,
    seed = 42
  ) %>%
  c(., self$params) %>%
  discard(is.null)
  
  model <- do.call(h2o::h2o.glm, model_params)
  
  return(model)
}


#' fit_h2o_rf
#' @export
fit_h2o_rf <- function(self){
  
  ### define objective
  # If the response is numeric, then a regression model will be trained, otherwise it will train a classification model.
  
  h2o_train <- h2o::as.h2o(as_tibble(self$data$train$x))
  h2o_train$y <- h2o::as.h2o(self$data$train$y)
  if(self$task %in% c("binary", "multi")) h2o_train$y <- h2o::as.factor(h2o_train$y)
  
  if(!is.null(self$data$val$x)){
    h2o_val <- h2o::as.h2o(as_tibble(self$data$val$x))
    h2o_val$y <- h2o::as.h2o(self$data$val$y)
    if(self$task %in% c("binary", "multi")) h2o_val$y <- h2o::as.factor(h2o_val$y)
  } else {
    h2o_val <- NULL
  }
  
  x <- colnames(self$data$train$x)
  
  model_params <- list(
    x = x, 
    y = "y", 
    training_frame = h2o_train, 
    validation_frame =  h2o_val,
    seed = 42
  ) %>%
    c(., self$params) %>%
    discard(is.null)
  
  
  model <- do.call(h2o::h2o.randomForest, model_params)
  
  return(model)
}


#' fit_h2o_nb
#' @export
fit_h2o_nb <- function(self){
  
  ### define objective
  # y must be categorical and must contain at least two unique categorical levels.
  
  h2o_train <- h2o::as.h2o(as_tibble(self$data$train$x))
  h2o_train$y <- h2o::as.h2o(self$data$train$y)
  if(self$task %in% c("binary", "multi")) h2o_train$y <- h2o::as.factor(h2o_train$y)
  
  if(!is.null(self$data$val$x)){
    h2o_val <- h2o::as.h2o(as_tibble(self$data$val$x))
    h2o_val$y <- h2o::as.h2o(self$data$val$y)
    if(self$task %in% c("binary", "multi")) h2o_val$y <- h2o::as.factor(h2o_val$y)
  } else {
    h2o_val <- NULL
  }
  
  x <- colnames(self$data$train$x)
  
  model_params <- list(
    #family = self$params$family,
    x = x, 
    y = "y", 
    training_frame = h2o_train, 
    validation_frame =  h2o_val
  ) %>%
    c(., self$params) %>%
    discard(is.null)
  
  
  model <- do.call(h2o::h2o.naiveBayes, model_params)
  
  return(model)
}



#' fit_h2o_svm
#' @export
fit_h2o_svm <- function(self){
  
  ### define objective
  # If the response is numeric, then a regression model will be trained, otherwise it will train a classification model.
  
  h2o_train <- h2o::as.h2o(as_tibble(self$data$train$x))
  h2o_train$y <- h2o::as.h2o(self$data$train$y)
  if(self$task %in% c("binary", "multi")) h2o_train$y <- h2o::as.factor(h2o_train$y)
  
  if(!is.null(self$data$val$x)){
    h2o_val <- h2o::as.h2o(as_tibble(self$data$val$x))
    h2o_val$y <- h2o::as.h2o(self$data$val$y)
    if(self$task %in% c("binary", "multi")) h2o_val$y <- h2o::as.factor(h2o_val$y)
  } else {
    h2o_val <- NULL
  }
  
  x <- colnames(self$data$train$x)
  
  model_params <- list(
    #family = self$params$family,
    x = x, 
    y = "y", 
    training_frame = h2o_train, 
    validation_frame =  h2o_val
  ) %>%
    c(., self$params) %>%
    discard(is.null)
  
  
  model <- do.call(h2o::h2o.psvm, model_params)
  
  return(model)
}

#' fit_h2o_gbm
#' @export
fit_h2o_gbm <- function(self){
  
  ### define objective
  if(self$task == "linear") self$params$distribution <- "gaussian"
  if(self$task == "binary") self$params$distribution <- "bernoulli"
  if(self$task == "multi") self$params$distribution <- "multinomial"
  
  # Distribution function Must be one of: 
  # "AUTO", "bernoulli", "quasibinomial", "multinomial",
  # "gaussian", "poisson", "gamma", "tweedie", "laplace", 
  # "quantile", "huber", "custom". Defaults to AUTO.
  
  h2o_train <- h2o::as.h2o(as_tibble(self$data$train$x))
  h2o_train$y <- h2o::as.h2o(self$data$train$y)
  if(self$task %in% c("binary", "multi")) h2o_train$y <- h2o::as.factor(h2o_train$y)
  
  if(!is.null(self$data$val$x)){
    h2o_val <- h2o::as.h2o(as_tibble(self$data$val$x))
    h2o_val$y <- h2o::as.h2o(self$data$val$y)
    if(self$task %in% c("binary", "multi")) h2o_val$y <- h2o::as.factor(h2o_val$y)
  } else {
    h2o_val <- NULL
  }
  
  x <- colnames(self$data$train$x)
  
  model_params <- list(
    #family = self$params$family,
    x = x, 
    y = "y", 
    training_frame = h2o_train, 
    validation_frame =  h2o_val
  ) %>%
    c(., self$params) %>%
    discard(is.null)
  
  print(model_params$distribution)
  
  model <- do.call(h2o::h2o.gbm, model_params)
  
  return(model)
}



#' fit_h2o_xgboost
#' @export
fit_h2o_xgboost <- function(self){
  
  ### define objective
  if(self$task == "linear") self$params$distribution <- "gaussian"
  if(self$task == "binary") self$params$distribution <- "bernoulli"
  if(self$task == "multi") self$params$distribution <- "multinomial"
  
#   If the distribution is bernoulli, the the response column must be 2-class categorical
#   If the distribution is multinomial, the response column must be categorical.
#   If the distribution is poisson, the response column must be numeric.
#   If the distribution is tweedie, the response column must be numeric.
#   If the distribution is gaussian, the response column must be numeric.
#   If the distribution is gamma, the response column must be numeric.
  
  h2o_train <- h2o::as.h2o(as_tibble(self$data$train$x))
  h2o_train$y <- h2o::as.h2o(self$data$train$y)
  if(self$task %in% c("binary", "multi")) h2o_train$y <- h2o::as.factor(h2o_train$y)
  
  if(!is.null(self$data$val$x)){
    h2o_val <- h2o::as.h2o(as_tibble(self$data$val$x))
    h2o_val$y <- h2o::as.h2o(self$data$val$y)
    if(self$task %in% c("binary", "multi")) h2o_val$y <- h2o::as.factor(h2o_val$y)
  } else {
    h2o_val <- NULL
  }
  
  x <- colnames(self$data$train$x)
  
  model_params <- list(
    #family = self$params$family,
    x = x, 
    y = "y", 
    training_frame = h2o_train, 
    validation_frame =  h2o_val
  ) %>%
    c(., self$params) %>%
    discard(is.null)
  
  print(model_params$distribution)
  
  model <- do.call(h2o::h2o.xgboost, model_params)
  
  return(model)
}



#' fit_h2o_dnn
#' @export
fit_h2o_dnn <- function(self){

  ### define objective
  if(self$task == "linear") self$params$distribution <- "gaussian"
  if(self$task == "binary") self$params$distribution <- "bernoulli"
  if(self$task == "multi") self$params$distribution <- "multinomial"
  
  # "bernoulli", "multinomial", "gaussian", "poisson", "gamma", "tweedie", 
  # "laplace", "quantile", "huber". Defaults to AUTO.
  
  h2o_train <- h2o::as.h2o(as_tibble(self$data$train$x))
  h2o_train$y <- h2o::as.h2o(self$data$train$y)
  if(self$task %in% c("binary", "multi")) h2o_train$y <- h2o::as.factor(h2o_train$y)
  
  if(!is.null(self$data$val$x)){
    h2o_val <- h2o::as.h2o(as_tibble(self$data$val$x))
    h2o_val$y <- h2o::as.h2o(self$data$val$y)
    if(self$task %in% c("binary", "multi")) h2o_val$y <- h2o::as.factor(h2o_val$y)
  } else {
    h2o_val <- NULL
  }
  
  x <- colnames(self$data$train$x)
  
  model_params <- list(
    #family = self$params$family,
    x = x, 
    y = "y", 
    training_frame = h2o_train, 
    validation_frame =  h2o_val
  ) %>%
    c(., self$params) %>%
    discard(is.null)

  model <- do.call(h2o::h2o.deeplearning, model_params)
  
  return(model)
}


#' fit_h2o_cox
#' @export
fit_h2o_cox <- function(self){
  
  ### define objective
  # If the response is numeric, then a regression model will be trained, otherwise it will train a classification model.
  
  h2o_train <- h2o::as.h2o(as_tibble(self$data$train$x))
  h2o_train$y <- h2o::as.h2o(self$data$train$y)
  if(self$task %in% c("binary", "multi")) h2o_train$y <- h2o::as.factor(h2o_train$y)
  
  if(!is.null(self$data$val$x)){
    h2o_val <- h2o::as.h2o(as_tibble(self$data$val$x))
    h2o_val$y <- h2o::as.h2o(self$data$val$y)
    if(self$task %in% c("binary", "multi")) h2o_val$y <- h2o::as.factor(h2o_val$y)
  } else {
    h2o_val <- NULL
  }
  
  x <- colnames(self$data$train$x)
  
  model_params <- list(
    #family = self$params$family,
    x = x, 
    y = "y", 
    training_frame = h2o_train, 
    validation_frame =  h2o_val
  ) %>%
    c(., self$params) %>%
    discard(is.null)
  
  
  model <- do.call(h2o::h2o.coxph, model_params)
  
  return(model)
}
