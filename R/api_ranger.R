#' fit_ranger
#' @export
fit_ranger <- function(self){
   
   if(is.null(self$params$num_trees)) self$params$num_trees <- 500

   x_train <- self$data$train$x %>% 
      dplyr::mutate(y = self$data$train$y)
   
   if(!is.null(self$data$val)){
      x_train <- self$data$val$x %>% 
         dplyr::mutate(y = self$data$val$y) %>%
         dplyr::bind_rows(x_train)
   }
   if(self$params$task %in% c("binary", "multi")){
      x_train <- x_train %>% mutate(y = as.factor(y))
   
      model <- ranger::ranger(
         formula = y ~ .,
         data = x_train, 
         importance = "impurity",
         num.trees = self$params$num_trees,
         mtry = self$params$mtry,
         min.node.size = self$params$min_node_size,
         probability = TRUE,
         verbose = T,
         seed = 42
      )
   } else {
      model <- ranger::ranger(
         formula = y ~ .,
         data = x_train, 
         importance = "impurity",
         num.trees = self$params$num_trees,
         mtry = self$params$mtry,
         min.node.size = self$params$min_node_size,
         verbose = T,
         seed = 42
      )
   }
   
   return(model)
}

#' predict_ranger
#' @export
predict_ranger <- function(self, x_test = NULL){
   
   if(is.null(x_test)) x_test <- self$data$test$x
   
   pred <- round(predict(self$model, x_test)$predictions, 3) # take only second column for 1s

   if(self$params$task == "linear"){
      tibble(pred)
   } else if(self$params$task == "binary"){
      prob <- pred
      pred <- ifelse(prob > .5, 1, 0)
      tibble(pred, prob)
   } else if(self$params$task == "multi"){
      probs <- as_tibble(pred) %>% set_names(paste0("prob", 1:ncol(pred)))
      pred <- pred %>% split(1:nrow(.)) %>% map_int(which.max)
      tibble(pred) %>% bind_cols(probs)
   }
   
}





