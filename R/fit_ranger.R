#' @export
fit_ranger_linear <- function(self, private, ...){
  
  x_train <- self$splits$train$x %>%
    as_tibble() %>%
    set_names(paste0("V",colnames(.))) %>%
    mutate(y = as.factor(self$splits$train$y))
  
  return(ranger::ranger(y ~ ., data = x_train, importance = "impurity_corrected"))
}

#' @export
fit_ranger_binary <- function(self, private, ...){
  
  x_train <- self$splits$train$x %>%
    as_tibble() %>%
    set_names(paste0("V",colnames(.))) %>%
    mutate(y = as.factor(self$splits$train$y))
  
  return(ranger::ranger(y ~ ., data = x_train, probability = T, importance = "impurity_corrected"))
}

#' @export
fit_ranger_categorical <- function(self, private, ...){
  
  x_train <- self$splits$train$x %>%
    as_tibble() %>%
    set_names(paste0("V",colnames(.))) %>%
    mutate(y = as.factor(self$splits$train$y))
  
  return(ranger::ranger(y ~ ., data = x_train, probability = T, importance = "impurity_corrected"))
}


#' @export
predict_ranger_linear <- function(model, x_test){
  
  x_test <- x_test %>%
    as_tibble() %>%
    set_names(paste0("V", colnames(.)))
  
  return(predict(model, x_test)$predictions) # take only second column for 1s
}


#' @export
predict_ranger_binary <- function(model, x_test){
  
  x_test <- x_test %>%
    as_tibble() %>%
    set_names(paste0("V", colnames(.)))
  
  return(predict(model, x_test)$predictions[,-1]) # take only second column for 1s
}


#' @export
predict_ranger_categorical <- function(model, x_test){
  
  x_test <- x_test %>%
    as_tibble() %>%
    set_names(paste0("V", colnames(.)))
  
  return(predict(model, x_test)$predictions) # take only second column for 1s
}
