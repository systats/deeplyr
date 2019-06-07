#' @export
backends <- R6::R6Class(
  "learner",
  inherit = splitter,
  private = list(
    backend = NULL,
    objective = NULL,
    plot = NULL,
    eval_metrics = function(metrics){
      metrics %>%
        imap_dfc( ~ {
          tibble(value = .x(self$preds$target, self$preds$pred)[1]) %>%
            set_names(.y)
        }) %>% 
        round(3)
    },
    eval_metrics_linear = function() {
      self$perform  <- list_metrics[["linear"]] %>% private$eval_metrics()
    },
    eval_metrics_binary = function() {
      self$perform  <- list_metrics[["binary"]] %>% private$eval_metrics()
    },
    eval_metrics_prob = function() {
      self$perform  <- list_metrics[["prob"]] %>%
        imap_dfc( ~ {
          tibble(value = .x(self$preds$target, self$preds$prob)[1]) %>%
            set_names(.y)
        }) %>%
        round(3) %>%
        bind_cols(self$perform) 
    },
    eval_metrics_categorical = function() {
      self$perform  <- list_metrics[["categorical"]]  %>% private$eval_metrics()
    },
    eval_model_linear = function() {
      
      self$preds <- tibble(pred = self$preds) %>%
        bind_cols(as_tibble(private$test)) %>%
        mutate(target = private$test[[private$param$target]],
               model_id = private$param$model_id)
      
      private$eval_metrics_linear()
    },
    eval_model_binary = function() {
      
      ### input is a probability vector
      self$preds <- tibble(prob = self$preds) %>%
        mutate(pred = ifelse(prob > .5, 1, 0)) %>%
        bind_cols(as_tibble(private$test)) %>%
        mutate(target = private$test[[private$param$target]],
               model_id = private$param$model_id)
      
      private$eval_metrics_binary()
      private$eval_metrics_prob()
      
    },
    eval_model_categorical = function() {
      
      
      if(length(self$preds) == length(private$test[[private$param$target]])) {
        self$preds <- tibble(pred = self$preds) %>%
          #mutate(pred = ifelse(prob > .5, 1, 0)) %>%
          bind_cols(as_tibble(private$test)) %>%
          mutate(target = private$test[[private$param$target]],
                 model_id = private$param$model_id)
      } else {
        #matrix(prob, nrow = length(target), ncol = 3)

        probs <- self$preds %>% 
          as_tibble() %>% 
          set_names(str_replace_all(colnames(.), "V", "prob_")) %>% 
          split(1:nrow(.))
        
        self$preds <- private$test %>% 
          as_tibble() %>%
          mutate(pred = probs %>% map_dbl(which.max)) %>% 
          mutate(prob = probs) %>%
          mutate(target = private$test[[private$param$target]],
                 model_id = private$param$model_id)
      }
      
      private$eval_metrics_categorical()
    }
  ),
  public = list(
    fit = NULL,
    predict = NULL,
    eval = NULL,
    set_backend = function(backend) {
      input <- backend %>%
        str_split(":") %>% unlist()
      
      private$backend <- input[1]
      private$objective <- input[2]
      private$metrics <-
        list_metrics[[private$objective]] # linear, binary, categorical
      
      ### Metrics and Performance Grafics
      if (private$objective %in% c("linear", "mixture")) {
        
        self$eval <- private$eval_model_linear
        private$plot <- plot_linear
        
      } else if (private$objective == "categorical") {
        
        self$eval <- private$eval_model_categorical
        private$plot <- plot_categorical
        
      } else if (private$objective == "binary") {
        
        self$eval <- private$eval_model_binary
        private$plot <- plot_binary
        
      }
      
      ### Backends
      if (private$backend == "keras") {
        
        self$fit <- fit_keras
        self$predict <- predict_keras
        
        if (private$objective == "linear") {
          
          private$param$output_fun <- "linear"
          private$param$loss <- "mse"
          private$param$metrics <- "mean_squared_error"
          private$param$optimizer <- "adam" #optimizer_rmsprop(),
          
        } else if (private$objective == "mixture"){
          
          private$param$output_fun <- "linear"
          #private$param$loss <- 
          private$param$metrics <- "mean_squared_error"
          private$param$optimizer <- "adam" #optimizer_rmsprop(),
          self$predict <- predict_mixture_keras
          
        } else if (private$objective == "binary") {
          
          private$param$output_fun <- "sigmoid"
          private$param$loss <- "binary_crossentropy"
          private$param$metrics <- "accuracy"
          private$param$optimizer <- "adam"
          
        } else if (private$objective == "categorical") {
          
          private$param$output_fun <- "softmax"
          private$param$loss <- "categorical_crossentropy"
          private$param$metrics <- "accuracy"
          private$param$optimizer <- "adam"
          
        }
      }
      
      if (private$backend == "ranger") {
        private$param$model_name <- "ranger"
        
        if (private$objective == "linear") {
          self$fit <- fit_ranger_linear
          self$predict <- predict_ranger_linear
          
        } else  if (private$objective == "binary") {
          self$fit <- fit_ranger_binary
          self$predict <- predict_ranger_binary
          
        } else {
          self$fit <- fit_ranger_categorical
          self$predict <- predict_ranger_categorical
          
        }
      }
      
      if (private$backend == "xgboost") {
        private$param$model_name <- "xgboost"
        
        self$fit <- fit_xgboost
        self$predict <- predict_xgboost
        
        if (private$objective == "linear") {
          #private$param$objective <- "reg:squarederror"
          
        } else if (private$objective == "categorical") {
          private$param$objective <- "multi:softmax" # multi:softprob
          
        } else if (private$objective == "binary") {
          private$param$objective <- "binary:logistic"
          
        }
      }
      
    }
  )
)
