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
  self, private, cv = F,
  # x_train, y_train, 
  # validation_data = NULL, 
  # class_weights = T, 
  # batch_size = 100,
  # epochs = 1,
  ...
){

  #class_weights <- compute_classweights(y_train, private$output_dim)
  if(!is.null(private$param$class_weights)){
    if(length(colnames(private$y_train)) == 0){
      private$param$class_weights <- private$y_train %>%
        tibble(var = .) %>%
        count(var) %>%
        mutate(n = max(n)/n) %>%
        pull(n) %>%
        as.list %>%
        set_names(c("0", "1"))
    } else {
      private$param$class_weights <- private$y_train %>%
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
  } else {
    private$param$class_weights <- NULL
  }
  
  if(is.null(private$param$batch_size)) private$param$batch_size <- 30
  
  model_param <- get_model_param(private$param$model, private$param)
  private$param <- complete_param(private$param, model_param)
  
  private$param$model <- do.call(private$param$model, model_param)
  
  if(private$objective != "mixture"){
    private$param$model %>%
      keras::compile(
        loss = private$param$loss,
        metric = private$param$metric,
        optimizer = private$param$optimizer
      ) 
  }
  
  # validation_split = .2
  if(!is.null(private$x_val)){
    val_data <- list(private$x_val, private$y_val)
    validation_split <- NULL
  } else {
    validation_split <- .2
    val_data = NULL
  }
  
  if(is.null(private$param$verbose)) {
    private$param$verbose <- 1
  }

  if(is.null(private$param$callbacks)){
    callbacks <- c(keras::callback_early_stopping(monitor = "val_loss", patience = 1, mode = "auto"))
  } else {
    callbacks <- NULL
  }
  
  if(cv){
    ### N-Fold Cross Validation
    n_folds <- 5
    cv_dat <- private$train %>% 
      mutate(folds = sample(1:n_folds, n(), replace = T))
    
    cv_dat %>% 
      split(.$folds) %>%
      iwalk(~{
        
        ind <- which(cv_dat$folds == .y) 
        
        x_val <- private$x_train[ind,] %>% as.matrix()
        y_val <- private$y_train[ind,]
        
        x_train <- private$x_train[-ind,] %>% as.matrix()
        y_train <- private$y_train[-ind,]
        
        private$param$model %>%
          fit(
            x = x_train,
            y = y_train,
            batch_size = 1,
            epochs = 1,
            #class_weight = class_weights,
            validation_data = list(x_val, y_val)
          )
      })
    
  } else {
    ### Normal Training Mode
    keras_param <- list(
      private$param$model, 
      private$x_train, 
      private$y_train, 
      batch_size = private$param$batch_size,
      #shuffle = T,
      class_weight = private$param$class_weights,
      epochs = private$param$epochs, # old: x$epochs %error%  in combination with early stoping: free lunch!
      callbacks = callbacks, 
      validation_split = validation_split,
      validation_data = val_data, 
      verbose = private$param$verbose
    ) 
    
    do.call(keras::fit, compact(keras_param))
  }

  #print(glimpse(keras_param))

  return(private$param$model)
}

#' @export
predict_keras <- function(model, x_test){
  pred <- predict(model, x_test)
  if(ncol(pred) == 1) pred <- pred[,1]
  return(pred)
}

#' @export
predict_mixture_keras <- function(model, x_test, output_dim, mix_dim){
  
  mdn <- reticulate::import("mdn")
  np <- reticulate::import("numpy")
  
  y_pred <- predict(model, x_test)
  y_samples = np$apply_along_axis(mdn$sample_from_output, 1L, y_pred, as.integer(output_dim), as.integer(mix_dim), temp=1.0)
  
  return(as.vector(y_samples))
}


# class_weights <- private$y_train %>%
#   tibble(var = .) %>%
#   count(var) %>%
#   mutate(n = max(n)/n) %>%
#   pull(n) %>%
#   as.list %>%
#   set_names(c("0", "1"))
