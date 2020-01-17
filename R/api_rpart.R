#' fit_rpart
#' @export
fit_rpart <- function(self){
  
  if(self$meta$task %in% c("binary", "multi")) self$params$method <- "class"
  
   model_params <- list(
      formula = self$process$formula,
      data = self$process$juice()
      #parms = self$params
   ) %>%
   c(self$params) %>%
   compact
   
   model <- do.call(rpart::rpart, model_params)
   
   return(model)
}

#' predict_rpart
#' @export
predict_rpart <- function(self, new_data){
   
   pred <- rpart:::predict.rpart(self$model, newdata = self$process$stream(new_data)) %>%
      round(3)
   
   if(self$meta$task == "linear"){
     
      dplyr::tibble(pred)
     
   } else if(self$meta$task == "binary"){
     
      prob <- pred[,2]
      
      pred <- ifelse(prob > .5, 1, 0) %>% 
        as.factor()
      
      dplyr::tibble(pred, prob)
      
   } else if(self$meta$task == "multi"){
    
      probs <- pred %>%
        dplyr::as_tibble() %>% 
        purrr::set_names(paste0("prob", 1:length(.)))
      
      pred <- probs %>% 
        split(1:nrow(.)) %>% 
        purrr::map_int(which.max) %>% 
        as.factor()
      
      tibble(pred) %>% bind_cols(probs)
   }
}

#' load_rpart
#' @export
load_rpart <- function(path){
  readr::read_rds(glue::glue("{path}/model"))
}

#' save_rpart
#' @export
save_rpart <- function(file, name, path){
  readr::write_rds(file, glue::glue("{path}/model"))
}

#' feature_imp_rpart
#' @export
feature_imp_rpart <- function(self){
  self$model$variable.importance %>%
    dplyr::as_tibble()
    #dplyr::rename(feature = variable)
}