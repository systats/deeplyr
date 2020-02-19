#' load_h2o
#' @export
load_h2o <- function(path){
  h2o::h2o.loadModel(path)
}

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
predict_h2o <- function(self, new_data){
  
  preds <- h2o::h2o.predict(self$model, h2o::as.h2o(self$process$stream(new_data))) %>%
    as.data.frame %>%
    dplyr::mutate_if(is.numeric, round, 3) 
    
  
  if(self$meta$task == "linear"){
    
    preds <- preds %>% dplyr::select(pred = predict)
    
  } else if(self$meta$task == "binary"){
    
    preds <- preds %>% dplyr::select(pred = predict, prob = p1)
    
  } else if(self$meta$task %in% c("ordinal", "multi")){
    
    preds <- preds %>% 
      purrr::set_names(c("pred", paste0("prob", 1:(ncol(preds) - 1))))
  }
  
  return(preds)
}


#' fit_h2o_glm
#' @export
fit_h2o_glm <- function(self){
  
  ### define objective
  if(self$meta$task == "linear"){
    if(is.null(self$params$family)) self$params$family <- "gaussian"
  } 
  if(self$meta$task == "binary"){
    if(is.null(self$params$family)) self$params$family <- "binomial"
  } 
  if(self$meta$task == "multi"){
    if(is.null(self$params$family)) self$params$family <- "multinomial"
  }
  
  # "gaussian", "binomial", "quasibinomial", "ordinal", "multinomial", 
  # "poisson", "gamma", "tweedie", "negativebinomial"
  
  h2o_data <- h2o::as.h2o(self$process$juice())
  
  ps <- self$params[names(self$params) %in% names(formals(h2o::h2o.glm))] %>% compact
  
  model_params <- list(
    x = self$process$ask_x(), 
    y = self$process$ask_y(), 
    training_frame = h2o_data, 
    seed = 42
  ) %>%
    c(., ps)
  
  model <- do.call(h2o::h2o.glm, model_params)
  
  return(model)
}


#' fit_h2o_rf
#' @export
fit_h2o_rf <- function(self){
  
  ### define objective
  # If the response is numeric, then a regression model will be trained, otherwise it will train a classification model.
  # if(self$meta$task %in% c("binary", "multi")) h2o_train$y <- h2o::as.factor(h2o_train$y)
  # if(!is.null(self$process$val$x)){
  #   h2o_val <- h2o::as.h2o(as_tibble(self$process$val$x))
  #   h2o_val$y <- h2o::as.h2o(self$process$val$y)
  #   if(self$meta$task %in% c("binary", "multi")) h2o_val$y <- h2o::as.factor(h2o_val$y)
  # } else {
  #   h2o_val <- NULL
  # }
  
  h2o_data <- h2o::as.h2o(self$process$juice())
  
  ps <- self$params[names(self$params) %in% names(formals(h2o::h2o.randomForest))] %>% compact

  model_params <- list(
    x = self$process$ask_x(), 
    y = self$process$ask_y(), 
    training_frame = h2o_data, 
    seed = 42
  ) %>%
    c(., ps)
  
  model <- do.call(h2o::h2o.randomForest, model_params)
  
  return(model)
}


#' fit_h2o_nb
#' @export
fit_h2o_nb <- function(self){
  
  ### define objective
  # y must be categorical and must contain at least two unique categorical levels.
  
  h2o_data <- h2o::as.h2o(self$process$juice())
  
  ps <- self$params[names(self$params) %in% names(formals(h2o::h2o.naiveBayes))] %>% compact
  
  model_params <- list(
    x = self$process$ask_x(), 
    y = self$process$ask_y(), 
    training_frame = h2o_data, 
    seed = 42
  ) %>%
    c(., ps)
  
  model <- do.call(h2o::h2o.naiveBayes, model_params)
  
  return(model)
}



#' fit_h2o_svm
#' @export
fit_h2o_svm <- function(self){
  
  ### define objective
  # If the response is numeric, then a regression model will be trained, otherwise it will train a classification model.
  
  h2o_data <- h2o::as.h2o(self$process$juice())
  
  ps <- self$params[names(self$params) %in% names(formals(h2o::h2o.psvm))] %>% compact
  
  model_params <- list(
    x = self$process$ask_x(), 
    y = self$process$ask_y(), 
    training_frame = h2o_data, 
    seed = 42
  ) %>%
    c(., ps)
  
  
  model <- do.call(h2o::h2o.psvm, model_params)
  
  return(model)
}

#' fit_h2o_gbm
#' @export
fit_h2o_gbm <- function(self){
  
  ### define objective
  if(self$meta$task == "linear") self$params$distribution <- "gaussian"
  if(self$meta$task == "binary") self$params$distribution <- "bernoulli"
  if(self$meta$task == "multi") self$params$distribution <- "multinomial"
  
  # Distribution function Must be one of: 
  # "AUTO", "bernoulli", "quasibinomial", "multinomial",
  # "gaussian", "poisson", "gamma", "tweedie", "laplace", 
  # "quantile", "huber", "custom". Defaults to AUTO.
  
  h2o_data <- h2o::as.h2o(self$process$juice())
  
  ps <- self$params[names(self$params) %in% names(formals(h2o::h2o.gbm))] %>% compact
  
  model_params <- list(
    x = self$process$ask_x(), 
    y = self$process$ask_y(), 
    training_frame = h2o_data, 
    seed = 42
  ) %>%
    c(., ps)
  
  model <- do.call(h2o::h2o.gbm, model_params)
  
  return(model)
}



#' fit_h2o_xgb
#' @export
fit_h2o_xgb <- function(self){
  
  ### define objective
  if(self$meta$task == "linear") self$params$distribution <- "gaussian"
  if(self$meta$task == "binary") self$params$distribution <- "bernoulli"
  if(self$meta$task == "multi") self$params$distribution <- "multinomial"
  
#   If the distribution is bernoulli, the the response column must be 2-class categorical
#   If the distribution is multinomial, the response column must be categorical.
#   If the distribution is poisson, the response column must be numeric.
#   If the distribution is tweedie, the response column must be numeric.
#   If the distribution is gaussian, the response column must be numeric.
#   If the distribution is gamma, the response column must be numeric.
  
  h2o_data <- h2o::as.h2o(self$process$juice())

  ps <- self$params
  ps <- ps[names(ps) %in% names(formals(h2o::h2o.xgboost))] %>% compact
  ps$backend <- "auto"
    
  model_params <- list(
    x = self$process$ask_x(), 
    y = self$process$ask_y(), 
    training_frame = h2o_data, 
    seed = 42
  ) %>%
    c(., ps)
  
  model <- do.call(h2o::h2o.xgboost, model_params)
  
  return(model)
}



#' fit_h2o_dnn
#' @export
fit_h2o_dnn <- function(self){

  ### define objective
  if(self$meta$task == "linear") self$params$distribution <- "gaussian"
  if(self$meta$task == "binary") self$params$distribution <- "bernoulli"
  if(self$meta$task == "multi") self$params$distribution <- "multinomial"
  
  # "bernoulli", "multinomial", "gaussian", "poisson", "gamma", "tweedie", 
  # "laplace", "quantile", "huber". Defaults to AUTO.
  
  h2o_data <- h2o::as.h2o(self$process$juice())
  
  ps <- self$params[names(self$params) %in% names(formals(h2o::h2o.deeplearning))] %>% compact
  
  model_params <- list(
    x = self$process$ask_x(), 
    y = self$process$ask_y(), 
    training_frame = h2o_data, 
    seed = 42
  ) %>%
    c(., ps)
  
  model <- do.call(h2o::h2o.deeplearning, model_params)
  
  return(model)
}