#' save_xgboost
#' @export
load_xgboost <- function(path) xgboost::xgb.load(glue::glue("{path}/model"))

#' save_xgboost
#' @export
save_xgboost <- function(file, name, path) xgboost::xgb.save(file, fname = glue::glue("{path}/{name}"))


#' feature_imp_xgboost
#' @export
feature_imp_xgboost <- function(self){

   xgboost::xgb.importance(feature_names = self$meta$x_name, model = self$model) %>%
      dplyr::as_tibble() %>%
      janitor::clean_names() 
}


#' fit_xgboost
#' @export
fit_xgboost <- function(self){

   ### define objective
   if(self$meta$task == "linear") self$params$objective <- "reg:squarederror"
   if(self$meta$task == "binary") self$params$objective <- "binary:logistic"
   if(self$meta$task == "multi") self$params$objective <- "multi:softprob"
   
   ## set number of classes for object=multi
   if(self$meta$task == "multi") self$params$num_class <- length(unique(self$meta$y))
   if(is.null(self$params$nrounds)) self$params$nrounds <- 30 
   if(is.null(self$params$nthread)) self$params$nthread <- 10
   
   ### set training and evaluation data
   input <- xgboost::xgb.DMatrix(
     data = as.matrix(self$meta$x), 
     label = self$meta$y
   )
   
   ### main call
   mparams <- self$params %>% 
      purrr::imap(~{ if(.y %in% c("nrounds", "nthread")) return(NULL) else  return(.x) }) %>%
      purrr::compact()
   
   model <- xgboost::xgboost(
      params = mparams,
      data = input, 
      nrounds = self$params$nrounds, 
      nthread = self$params$nthread, 
      missing = NA, 
      # early_stopping_rounds = 3,
      verbose = 1
   )
   
   return(model)
}



#' predict_xgboost
#' @export
predict_xgboost <- function(self, new_data) {

  input <- xgboost::xgb.DMatrix(data = as.matrix(new_data))
  
  if (self$meta$task == "linear") {
    
    pred <- predict(self$model, newdata = input) %>% round(3)
    return(dplyr::tibble(pred))
    
  } else if (self$meta$task == "binary") {
    
    prob <- predict(self$model, newdata = input) %>% 
      round(3)
    
    pred <- ifelse(prob > .5, 1, 0) %>% 
      as.factor()
    
    return(dplyr::tibble(pred, prob))
    
  } else if (self$meta$task == "multi") {
    
    probs <- predict(self$model, newdata = input, reshape = T) %>% 
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
