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
  if(!is.null(self$param$class_weights)){
    if(length(colnames(self$splits$train$y)) == 0){
      self$param$class_weights <- self$splits$train$y %>%
        tibble(var = .) %>%
        count(var) %>%
        mutate(n = max(n)/n) %>%
        pull(n) %>%
        as.list %>%
        set_names(c("0", "1"))
    } else {
      self$param$class_weights <- self$splits$train$y %>%
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
    self$param$class_weights <- NULL
  }
  
  if(is.null(self$param$batch_size)) self$param$batch_size <- 30
  
  model_param <- get_model_param(self$param$model, self$param)
  self$param <- complete_param(self$param, model_param)
  
  self$param$model <- do.call(self$param$model, model_param)
  
  if(private$objective != "mixture"){
    self$param$model %>%
      keras::compile(
        loss = self$param$loss,
        metric = self$param$metric,
        optimizer = self$param$optimizer
      ) 
  }
  
  # validation_split = .2
  if(!is.null(self$splits$val$x)){
    val_data <- list(self$splits$val$x, self$splits$val$y)
    validation_split <- NULL
  } else {
    validation_split <- .2
    val_data = NULL
  }
  
  if(is.null(self$param$verbose)) {
    self$param$verbose <- 1
  }

  if(is.null(self$param$callbacks)){
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
        
        x_val <- self$splits$train$x[ind,] %>% as.matrix()
        y_val <- self$splits$train$y[ind,]
        
        x_train <- self$splits$train$x[-ind,] %>% as.matrix()
        y_train <- self$splits$train$y[-ind,]
        
        self$param$model %>%
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
      self$param$model, 
      self$splits$train$x, 
      self$splits$train$y, 
      batch_size = self$param$batch_size,
      #shuffle = T,
      class_weight = self$param$class_weights,
      epochs = self$param$epochs, # old: x$epochs %error%  in combination with early stoping: free lunch!
      callbacks = callbacks, 
      validation_split = validation_split,
      validation_data = val_data, 
      verbose = self$param$verbose
    ) 
    
    do.call(keras::fit, compact(keras_param))
  }

  #print(glimpse(keras_param))

  return(self$param$model)
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


# class_weights <- self$splits$train$y %>%
#   tibble(var = .) %>%
#   count(var) %>%
#   mutate(n = max(n)/n) %>%
#   pull(n) %>%
#   as.list %>%
#   set_names(c("0", "1"))
