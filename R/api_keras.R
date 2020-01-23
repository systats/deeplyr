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
   if(self$meta$task == "linear") {
      if(is.null(self$params$output_dim)) self$params$output_dim <- 1
      if(is.null(self$params$output_fun)) self$params$output_fun <- "linear"
      if(is.null(self$params$loss)) self$params$loss <- "mse"
      if(is.null(self$params$metrics)) self$params$metrics <- "mean_squared_error"
      if(is.null(self$params$optimizer)) self$params$optimizer <- "adam"
   }
   if(self$meta$task == "binary") {
      if(is.null(self$params$output_dim)) self$params$output_dim <- 1
      if(is.null(self$params$output_fun)) self$params$output_fun <- "sigmoid"
      if(is.null(self$params$loss)) self$params$loss <- "binary_crossentropy"
      if(is.null(self$params$metrics)) self$params$metrics <- "accuracy"
      if(is.null(self$params$optimizer)) self$params$optimizer <- "adam"
      
   }
   if(self$meta$task == "multi") {
      if(is.null(self$params$output_dim)) self$params$output_dim <- length(unique(self$process$juice_y())) #1#ncol(self$data$train$y)
      if(is.null(self$params$output_fun)) self$params$output_fun <- "softmax"
      if(is.null(self$params$loss)) self$params$loss <- "sparse_categorical_crossentropy"
      if(is.null(self$params$metrics)) self$params$metrics <- "accuracy"
      if(is.null(self$params$optimizer)) self$params$optimizer <- "adam"
   }
   
   if(is.null(self$params$input_dim)) self$params$input_dim <- ncol(self$process$juice_x())
   if(is.null(self$params$batch_size)) self$params$batch_size <- 30
   if(is.null(self$params$verbose)) self$params$verbose <- 1

   # define classweights by default
   # if(self$meta$task != "linear"){
   #    if(!is.null(self$params$class_weights)){
   #       if(length(colnames(self$data$train$y)) == 0){
   #          class_weights <- class_weight()
   #       } else {
   #          class_weights <- class_weights()
   #       }
   #    }
   # }

   if(is.null(self$params$model)){
      
      message("new simple model")
      
      simple_model = function(input_dim = self$params$input_dim, output_dim = self$params$output_dim, output_fun = self$params$output_fun){
         keras::keras_model_sequential() %>%
            keras::layer_dense(units = 10, activation = "relu", input_shape = input_dim) %>%
            keras::layer_dense(units = output_dim, activation = output_fun)
      }

      summary(simple_model())
      self$params$model <- list(simple_model)
   }
   
   ### compile keras model
   model_params <- get_params(self$params$model[[1]], self$params)
   #self$params <- comp_params(self$params, model_param)

   model <- do.call(self$params$model[[1]], model_params) %>%
      keras::compile(
         loss = self$params[["loss"]],
         metric = self$params[["metric"]],
         optimizer = self$params[["optimizer"]]
      )
   
   x_train <- self$process$juice_x_matrix()
   y_train <- as.numeric(as.character(self$process$juice_y()))
   
   if(self$meta$task %in% c("binary", "multi")){
      if(min(y_train) == 1) y_train <- y_train - 1
   }
   
   keras_params <- list(
      object = model,
      x = x_train,
      y = y_train, #
      batch_size = self$params[["batch_size"]],
      class_weight = self$params[["class_weights"]],
      epochs = self$params$epochs, # old: x$epochs %error%  in combination with early stoping: free lunch!
      #callbacks = self$params$callbacks,
      verbose = self$params$verbose
   )

   do.call(keras::fit, compact(keras_params))
   
   return(model)
}

#' predict_keras
#' @export
predict_keras <- function(self, new_data){
   
   pred <- predict(self$model, self$process$stream_matrix(new_data)) %>% round(3)
   
   if(self$meta$task == "linear"){
     
      dplyr::tibble(pred = pred[,1])
     
   } else if(self$meta$task == "binary"){
      
     prob <- pred[,1]
      
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

#' save_keras
#' @export
save_keras <- function(file, name, path){
   keras::save_model_hdf5(object = file, filepath = glue::glue("{path}/{name}"), overwrite = T)
}

#' load_keras
#' @export
load_keras <- function(path) keras::load_model_hdf5(glue::glue("{path}/model"))


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
