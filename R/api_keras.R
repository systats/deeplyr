#' class_weight
#' @export
class_weight = function(y){
   
   y %>%
      tibble(var = .) %>%
      count(var) %>%
      mutate(n = max(n)/n) %>%
      pull(n) %>%
      as.list %>%
      set_names(c("0", "1"))
   
}

#' class_weights
#' @export
class_weights = function(ymat){
   
   ymat %>%
      as_tibble() %>%
      set_names(1:length(.)) %>%
      imap(~{
         out <- .x  %>%
            tibble(var = .) %>%
            count(var) %>%
            mutate(n = max(n)/n) %>%
            filter(var == 1) %>%
            pull(n)
         return(out)
      }) %>%
      c(list(`0` = 1), .)
   
}
 
#' get_params
#' @export  
get_params = function(model, params){
   
   out <- formals(model) %>%
      imap(~{
         if(.y %in% names(params)){
            .x <-  params[[.y]]
         }
         return(.x)
      })
   return(out)
}

#' comp_params
#' @export  
comp_params = function(params1, params2){
   not_included <- params2[!(names(params2) %in% names(params1))]
   out <- params1 %>%
      imap(~{
         if(.y %in% names(params2)){
            .x <-  params2[[.y]]
         }
         return(.x)
      }) %>%
      c(not_included)
   return(out)
}


#' fit_keras
#' @export
fit_keras <- function(self){
   
   #keras::use_session_with_seed(seed = 42)
   
   ### define objective/loss
   if(self$task == "linear") {
      
      if(is.null(self$params$output_dim)) self$params$output_dim <- 1
      if(is.null(self$params$output_fun)) self$params$output_fun <- "linear"
      if(is.null(self$params$loss)) self$params$loss <- "mse"
      if(is.null(self$params$metrics)) self$params$metrics <- "mean_squared_error"
      if(is.null(self$params$optimizer)) self$params$optimizer <- "adam" #optimizer_rmsprop(),
      
   } else if(self$task == "binary") {
      
      if(is.null(self$params$output_dim)) self$params$output_dim <- 1
      if(is.null(self$params$output_fun)) self$params$output_fun <- "sigmoid"
      if(is.null(self$params$loss)) self$params$loss <- "binary_crossentropy"
      if(is.null(self$params$metrics)) self$params$metrics <- "accuracy"
      if(is.null(self$params$optimizer)) self$params$optimizer <- "adam"
      
   } else if(self$task == "multi") {
      
      if(is.null(self$params$output_dim)) self$params$output_dim <- length(unique(self$data$train$y))#1#ncol(self$data$train$y)
      if(is.null(self$params$output_fun)) self$params$output_fun <- "softmax"
      if(is.null(self$params$loss)) self$params$loss <- "sparse_categorical_crossentropy"
      if(is.null(self$params$metrics)) self$params$metrics <- "accuracy"
      if(is.null(self$params$optimizer)) self$params$optimizer <- "adam"
      
   }
   
   if(is.null(self$params$batch_size)) self$params$batch_size <- 30
   if(is.null(self$params$verbose)) self$params$verbose <- 1
   if(is.null(self$params$callbacks)) self$params$callbacks <- c(keras::callback_early_stopping(monitor = "val_loss", patience = 1, mode = "auto"))

   # define classweights by default
   if(self$task != "linear"){
      if(!is.null(self$params$class_weights)){
         if(length(colnames(self$data$train$y)) == 0){
            class_weights <- class_weight()
         } else {
            class_weights <- class_weights()
         }
      }
   }

   if(is.null(self$params$model)){
      
      message("new simple model")
      
      simple_model = function(output_dim = self$params$output_dim, output_fun = self$params$output_fun){
         keras::keras_model_sequential() %>%
            keras::layer_dense(units = 10, activation = "relu", input_shape = ncol(self$data$train$x)) %>%
            keras::layer_dense(units = output_dim, activation = output_fun) # input_shape = ncol(self$data$train$x))
      }

      self$params$model <- list(simple_model)
   }

   ### compile keras model
   model_params <- get_params(self$params$model[[1]], self$params)
   #self$params <- comp_params(self$params, model_param)

   model <- do.call(self$params$model[[1]], model_params) %>%
      keras::compile(
         loss = self$params$loss,
         metric = self$params$metric,
         optimizer = self$params$optimizer
      )

   # val data?
   if(!is.null(self$data$val$x)){
      val_data <- list(as.matrix(self$data$val$x), self$data$val$y)
      self$params$validation_split <- NULL
   } else {
      #self$params$validation_split <- .2
      val_data = NULL
   }

   keras_params <- list(
      object = model,
      x = as.matrix(self$data$train$x),
      y = self$data$train$y,
      batch_size = self$params$batch_size,
      class_weight = self$params$class_weights,
      epochs = self$params$epochs, # old: x$epochs %error%  in combination with early stoping: free lunch!
      callbacks = self$params$callbacks,
      validation_split = self$params$validation_split,
      validation_data = val_data,
      verbose = self$params$verbose
   )

   do.call(keras::fit, compact(keras_params))
   
   return(keras_params$object)
}

#' predict_keras
#' @export
predict_keras <- function(self, x_test = NULL){
   
   if(is.null(x_test)) x_test <- self$data$test$x
   
   pred <- round(predict(self$model, as.matrix(x_test)), 3)
   
   if(self$task == "linear"){
      tibble(pred)
   } else if(self$task == "binary"){
      prob <- pred[,1]
      pred <- ifelse(prob > .5, 1, 0)
      tibble(pred, prob0 = 1 - prob, prob1 = prob)
   } else if(self$task == "multi"){
      probs <- as_tibble(pred) %>% set_names(paste0("prob", 1:ncol(pred)))
      pred <- pred %>% split(1:nrow(.)) %>% map_int(which.max)
      tibble(pred) %>% bind_cols(probs)
   }
}

#' save_keras
#' @export
save_keras <- function(file, name, path){
   keras::save_model_hdf5(object = file, filepath = glue::glue("{path}/{name}"), overwrite = T)
}

#' predict_mixture_keras <- function(model, x_test, output_dim, mix_dim){
#'   
#'   mdn <- reticulate::import("mdn")
#'   np <- reticulate::import("numpy")
#'   
#'   y_pred <- predict(model, x_test)
#'   y_samples = np$apply_along_axis(mdn$sample_from_output, 1L, y_pred, as.integer(output_dim), as.integer(mix_dim), temp=1.0)
#'   
#'   return(as.vector(y_samples))
#' }
