#' load_glmnet
#' @export
load_glmnet <- function(path) readRDS(glue::glue("{path}/model"))

#' save_glmnet
#' @export
save_glmnet <- function(file, name, path) saveRDS(file, file = glue::glue("{path}/{name}"))


#' fit_glmnet
#' @export
fit_glmnet <- function(self){
  
  ### define objective
  if(self$meta$task == "linear") self$params$family <- "gaussian"
  if(self$meta$task == "binary") self$params$family <- "binomial"
  if(self$meta$task == "multi") self$params$family <- "multinomial"
  
  if(is.null(self$params$alpha)) self$params$alpha <- .5
  
  model <- glmnet::cv.glmnet(
     x = as.matrix(self$meta$x), y = self$meta$y, 
     family = self$params$family,
     alpha = self$params$alpha
     # gamma = self$params$gamma
     # L1 penalty
     # alpha = 1
     # interested in the area under ROC curve
     # type.measure = "auc",
     # 5-fold cross-validation
     # nfolds = NFOLDS,
     # high value is less accurate, but has faster training
     # thresh = 1e-3,
     # again lower number of iterations for faster training
     # maxit = 1e3
  )
  
  return(model)
}

#' predict_glmnet
#' @export
predict_glmnet <- function(self, new_data) {
  
  if (self$meta$task == "linear") {
    
    pred <- predict(self$model, as.matrix(new_data)) %>% round(3)
    return(dplyr::tibble(pred))
    
  } else if (self$meta$task == "binary") {
    
    prob <- predict(self$model, as.matrix(new_data)) %>% 
      round(3)
    
    pred <- ifelse(prob > .5, 1, 0) %>% 
      as.factor()
    
    return(dplyr::tibble(pred, prob))
    
  } else if (self$meta$task == "multi") {
    
    probs <- predict(self$model, as.matrix(new_data)) %>% 
      round(3) %>%
      dplyr::as_tibble() %>% 
      purrr::set_names(paste0("prob", 1:length(.)))
    
    pred <- probs %>% 
      split(1:nrow(.)) %>% 
      purrr::map_int(which.max) %>% 
      as.factor() 
    
    return(tibble(pred) %>% dplyr::bind_cols(probs))
    
  }
}
