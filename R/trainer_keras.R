# keras_model <- list(
#   simple_mlp = textlearnR::keras_simple_mlp,
#   deep_mlp = textlearnR::keras_deep_mlp,
#   simple_lstm = textlearnR::keras_simple_lstm,
#   #deep_lstm = textlearnR::keras_deep_lstm,
#   pooled_gru = textlearnR::keras_pooled_gru,
#   cnn_lstm = textlearnR::keras_cnn_lstm,
#   cnn_gru = textlearnR::keras_cnn_gru,
#   gru_cnn = textlearnR::keras_gru_cnn,
#   multi_cnn = textlearnR::keras_multi_cnn
# )


#' keras_trainer
#' 
#' @export
trainer_keras <- R6::R6Class("trainer_keras",
  private = list(
   class_weight = function(){
     
     self$param$class_weights <- self$data$train$y %>%
       tibble(var = .) %>%
       count(var) %>%
       mutate(n = max(n)/n) %>%
       pull(n) %>%
       as.list %>%
       set_names(c("0", "1"))
     
   },
   class_weights = function(){
     
     self$param$class_weights <- self$data$train$y %>%
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
     
   },
   get_param = function(model, param){
     
     out <- formals(model) %>%
       imap(~{
         if(.y %in% names(param)){
           .x <-  param[[.y]]
         }
         return(.x)
       })
     return(out)
   },
   comp_param = function(param1, param2){
     not_included <- param2[!(names(param2) %in% names(param1))]
     out <- param1 %>%
       imap(~{
         if(.y %in% names(param2)){
           .x <-  param2[[.y]]
         }
         return(.x)
       }) %>%
       c(not_included)
     return(out)
   },
   compile = function(){
     
     model_param <- private$get_param(self$param$model, self$param)
     self$param <- private$comp_param(self$param, model_param)
     
     print(self$param$model)
     
     self$model <- do.call(self$param$model, model_param) %>%
       keras::compile(
         loss = self$param$loss,
         metric = self$param$metric,
         optimizer = self$param$optimizer
       )
     
   },
   set_defaults = function(){
     
     if(is.null(self$param$batch_size)) self$param$batch_size <- 30
     if(is.null(self$param$verbose)) self$param$verbose <- 1
     if(is.null(self$param$callbacks)) self$param$callbacks <- c(keras::callback_early_stopping(monitor = "val_loss", patience = 1, mode = "auto"))
     
   },
   set_weights = function(){
     if(!is.null(self$param$class_weights)){
       if(length(colnames(self$data$train$y)) == 0){
         private$class_weight()
       } else {
         private$class_weights()
       }
     } else {
       self$param$class_weights <- NULL
     }
   }
  ),
  public = list(
   model = NULL,
   param = NULL,
   data = NULL,
   initialize = function(objective) {
     
     if (objective == "linear") {
       self$param$output_fun <- "linear"
       self$param$loss <- "mse"
       self$param$metrics <- "mean_squared_error"
       self$param$optimizer <- "adam" #optimizer_rmsprop(),
     } 
     # if (private$objective == "mixture"){
     #   self$param$output_fun <- "linear"
     #   #self$param$loss <- 
     #   self$param$metrics <- "mean_squared_error"
     #   self$param$optimizer <- "adam" #optimizer_rmsprop(),
     #   self$predict <- predict_mixture_keras
     #   
     # }
     if (objective == "binary") {
       self$param$output_fun <- "sigmoid"
       self$param$loss <- "binary_crossentropy"
       self$param$metrics <- "accuracy"
       self$param$optimizer <- "adam"
     }
     
     if (objective == "categorical") {
        
       self$param$output_fun <- "softmax"
       self$param$loss <- "categorical_crossentropy"
       self$param$metrics <- "accuracy"
       self$param$optimizer <- "adam"
       
     }
   },
   set = function(param, data){
     
     self$data <- data
     self$param <- c(self$param, param)
     
   },
   fit = function(){
      
     keras::use_session_with_seed(42)
     
     ### defaults if missing
     private$set_defaults()
     
     # define classweights by default
     private$set_weights()
     
     self$param$output_dim <- ncol(self$data$train$y)
     
     if(is.null(self$param$model)){
        
        simple_model = function(output_dim = 1, output_fun = "sigmoid"){
           keras::keras_model_sequential() %>%
              #keras::layer_dense(units = 10, activation = "relu", input_shape = ncol(self$data$train$x)) %>%
              keras::layer_dense(units = output_dim, activation = output_fun) # input_shape = ncol(self$data$train$x))
        }
        
        self$param$model <- simple_model
     }
     
     ### compile keras model
     private$compile()
     
     # val data?
     if(!is.null(self$data$val$x)){
       val_data <- list(as.matrix(self$data$val$x), self$data$val$y)
       self$param$validation_split <- NULL
     } else {
       self$param$validation_split <- .2
       val_data = NULL
     }
     
     keras_param <- list(
       object = self$model, 
       x = as.matrix(self$data$train$x), 
       y = self$data$train$y, 
       batch_size = self$param$batch_size,
       class_weight = self$param$class_weights,
       epochs = self$param$epochs, # old: x$epochs %error%  in combination with early stoping: free lunch!
       callbacks = self$param$callbacks, 
       validation_split = self$param$validation_split,
       validation_data = val_data, 
       verbose = self$param$verbose
     )
     
     do.call(keras::fit, compact(keras_param))
   },
   predict = function(x_test = NULL){
     
     if(is.null(x_test)) x_test <- as.matrix(self$data$test$x)
     
     pred <- predict(self$model, x_test)
     
     if(ncol(pred) == 1) pred <- pred[,1, drop=T]
     
     return(pred)
   }
  )
)

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
