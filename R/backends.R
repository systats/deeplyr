#' @export
backends <- R6::R6Class("learner",
  inherit = splitter, 
  private = list(
    backend = NULL,
    objective = NULL,
    eval_metrics = function(){
      self$perform  <- private$metrics %>%
        imap_dfc(~{
          tibble(value = .x(self$preds$target, self$preds$pred)[1]) %>%
            set_names(.y)
        })
    },
    eval_metrics_loss = function(){
      self$perform  <- list_metrics[["loss"]] %>%
        imap_dfc(~{
          tibble(value = .x(self$preds$target, self$preds$prob)[1]) %>%
            set_names(.y)
        }) %>% 
        bind_cols(self$perform)
    },
    eval_model_linear = function(){
      private$eval_metrics()
    },
    eval_model_binary = function(){
      
      ### input is a probability vector
      self$preds <- tibble(prob = self$preds) %>%
        mutate(pred = ifelse(prob > .5, 1, 0)) %>%
        bind_cols(as_tibble(private$test)) %>%
        mutate(target = private$test[[private$param$target]], model_id = private$param$model_id)
    
      private$eval_metrics()
      private$eval_metrics_loss()

      plot_binary_pos <- possibly(plot_binary, NULL)
      perform <- plot_binary_pos(self$preds$target, self$preds$pred, self$preds$prob)
      
      if(!is.null(perform)) ggsave(perform, filename = glue::glue("{private$new_path}/plot_perform.png"), width = 10, height = 5)
      
    },
    eval_model_categorical = function(){
      private$eval_metrics()
    }
  ),
  public = list(
    fit = NULL,
    predict = NULL,
    eval = NULL,
    set_backend = function(backend){
      
      input <- backend %>% 
        str_split(":") %>% unlist()
      
      private$backend <- input[1]
      private$objective <- input[2]
      private$metrics <- list_metrics[[private$objective]] # linear, binary, categorical
      
      
      if(private$backend == "keras"){
        
        self$fit <- fit_keras
        self$predict <- predict_keras
        
        if(private$objective == "linear"){
          
          private$param$output_fun <- "linear"
          private$param$loss <- "mse"
          private$param$metrics <- "mean_squared_error"
          private$param$optimizer <- "adam" #optimizer_rmsprop(),
          
        } else if(private$objective == "categorical"){
          
          private$param$output_fun <- "sigmoid"
          private$param$loss <- "binary_crossentropy"
          private$param$metrics <- "accuracy"
          private$param$optimizer <- "adam"
          
        } else if(private$objective == "binary"){
          
          private$param$output_fun <- "softmax"
          private$param$loss <- "categorical_crossentropy"
          private$param$metrics <- "accuracy"
          private$param$optimizer <- "adam"
          
        }
      }
      
      if(private$backend == "ranger"){
        
        private$param$model_name <- "ranger"
        
        if(private$objective == "linear"){
          
          self$fit <- fit_ranger_linear
          self$predict <- predict_ranger_linear
          
        } else {
          
          self$fit <- fit_ranger_binary
          self$predict <- predict_ranger_binary
        }
      }
      
      if(private$backend == "xgboost"){
        private$param$model_name <- "xgboost"
        
        self$fit <- fit_xgboost
        self$predict <- predict_xgboost

        if(private$objective == "linear"){
          private$param$objective <- "reg:squarederror"
        } else if(private$objective == "categorical"){
          private$param$objective <- "multi:softmax" # multi:softprob 
        } else if(private$objective == "binary"){
          private$param$objective <- "binary:logistic"
        }
      }
      
      
      if(private$objective == "linear"){
        self$eval <- private$eval_model_linear
      } else if(private$objective == "categorical"){
        self$eval <- private$eval_model_categorical
      } else if(private$objective == "binary"){
        self$eval <- private$eval_model_binary
      }
    }
  )
)
