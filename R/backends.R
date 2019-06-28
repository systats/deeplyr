#' @export
backends <- R6::R6Class(
  "learner",
  #inherit = splitter,
  private = list(
    backend = NULL,
    objective = NULL,
    plot = NULL
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

      ### EVAL
      self$eval <- evaluator$new(private$objective)
      
      ### Backends
      if (private$backend == "keras") {
        
        self$fit <- fit_keras
        self$predict <- predict_keras
        
        if (private$objective == "linear") {
          print(1)
          self$param$output_fun <- "linear"
          self$param$loss <- "mse"
          self$param$metrics <- "mean_squared_error"
          self$param$optimizer <- "adam" #optimizer_rmsprop(),
          
        } 
        
        if (private$objective == "mixture"){
          
          self$param$output_fun <- "linear"
          #self$param$loss <- 
          self$param$metrics <- "mean_squared_error"
          self$param$optimizer <- "adam" #optimizer_rmsprop(),
          self$predict <- predict_mixture_keras
          
        }
        
        if (private$objective == "binary") {
          
          self$param$output_fun <- "sigmoid"
          self$param$loss <- "binary_crossentropy"
          self$param$metrics <- "accuracy"
          self$param$optimizer <- "adam"
          
        }
        
        if (private$objective == "categorical") {
          print(3)
          self$param$output_fun <- "softmax"
          self$param$loss <- "categorical_crossentropy"
          self$param$metrics <- "accuracy"
          self$param$optimizer <- "adam"
          
        }
      }
      
      print(self$param)
      
      if (private$backend == "ranger") {
        self$param$model_name <- "ranger"
        
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
        self$param$model_name <- "xgboost"
        
        self$fit <- fit_xgboost
        self$predict <- predict_xgboost
        
        if (private$objective == "linear") {
          #self$param$objective <- "reg:squarederror"
          
        } else if (private$objective == "categorical") {
          self$param$objective <- "multi:softmax" # multi:softprob
          
        } else if (private$objective == "binary") {
          self$param$objective <- "binary:logistic"
          
        }
      }
      
    }
  )
)
