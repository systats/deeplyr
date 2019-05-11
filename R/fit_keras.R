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


#' keras_learner
#' 
#' @export
fit_keras <- function(
  self, private,
  # x_train, y_train, 
  # validation_data = NULL, 
  # class_weights = T, 
  # batch_size = 100,
  # epochs = 1,
  ...
){
  
  if(private$backend == "keras"){
    # if(class_weights){
    #   class_weights <- compute_classweights(y_train, private$output_dim)
    # } else {
    #   class_weights <- NULL
    # }
  }
  
  
  model_param <- get_model_param(private$param$model, private$param)
  private$param <- complete_param(private$param, model_param)
  private$param$model <- do.call(private$param$model, model_param) %>%
    keras::compile(
      loss = private$param$loss,
      metric = private$param$metric,
      optimizer = private$param$optimizer
    )
  
  # validation_split = .2
  if(!is.null(private$x_val)){
    val_data <- list(private$x_val, private$y_val)
  } else {
    val_data = NULL
  }

  keras_param <- list(
    private$param$model, 
    private$x_train, 
    private$y_train, 
    batch_size = 100, 
    shuffle = T,
    #class_weight = class_weights,
    epochs = 1, # old: x$epochs %error%  in combination with early stoping: free lunch!
    callbacks = c(keras::callback_early_stopping(monitor = "val_loss", patience = 1, mode = "auto")),
    validation_data = val_data
  ) 

  #print(glimpse(keras_param))
  do.call(keras::fit, compact(keras_param))

  return(private$param$model)
}

#' @export
predict_keras <- function(model, x_test){
  pred <- predict(model, x_test)
  if(ncol(pred) == 1) pred <- pred[,1]
  return(pred)
}

# class_weights <- private$y_train %>%
#   tibble(var = .) %>%
#   count(var) %>%
#   mutate(n = max(n)/n) %>%
#   pull(n) %>%
#   as.list %>%
#   set_names(c("0", "1"))
# if(length(colnames(y_train)) == 0){
#   class_weights <- y_train %>% 
#     tibble(var = .) %>% 
#     count(var) %>%
#     mutate(n = max(n)/n) %>%
#     pull(n) %>% 
#     as.list %>%
#     set_names(c("0", "1"))
# } else {
#   class_weights <- y_train %>%
#     as_tibble() %>%
#     set_names(1:length(.)) %>%
#     imap(~{
#       out <- .x  %>%
#         tibble(var = .) %>%
#         count(var) %>%
#         mutate(n = max(n)/n) %>%
#         filter(var == 1) %>%
#         pull(n)
#       return(out)
#     }) %>%
#     c(list(`0` = 1), .)
# } 
