#' evaluator
#' @export
evaluator <- R6::R6Class("eval",
 private = list(
   # variables
   metrics = NULL,
   objective = NULL, 
   model_id = NA, 
   target = NULL,
   pred = NULL,
   # functions
   eval_model = NULL,
   plot_model = NULL,
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
   # Models
   eval_linear = function() {
     
     self$preds <- tibble(pred = private$pred) %>%
       mutate(target = private$target,
              model_id = private$model_id)
     
     private$eval_metrics_linear()
   },
   eval_binary = function() {
     
     ### input is a probability vector
     self$preds <- tibble(prob = private$pred) %>%
       mutate(pred = ifelse(prob > .5, 1, 0)) %>%
       mutate(target = private$target,
              model_id = private$model_id)
     
     private$eval_metrics_binary()
     private$eval_metrics_prob()
     
   },
   eval_categorical = function() {
     
     
     if(length(private$pred) == length(private$target)) {
       self$preds <- tibble(pred = private$pred) %>%
         mutate(target = private$target,
                model_id = private$model_id)
     } else {
       
       probs <- private$pred %>% 
         as_tibble() %>% 
         set_names(str_replace_all(colnames(.), "V", "prob_")) %>% 
         split(1:nrow(.))
       
       self$preds <- tibble(prob = probs) %>% 
         mutate(pred = probs %>% map_dbl(which.max)) %>%
         mutate(target = private$target,
                model_id = private$model_id)
     }
     
     private$eval_metrics_categorical()
   }
 ),
 public = list(
   # variables
   perform = NULL,
   plots = NULL,
   preds = NULL, 
   # functions
   initialize = function(objective = "binary") {
     private$objective <- objective
     
     ### Metrics and Performance Grafics
     if (private$objective %in% c("linear", "mixture")) {
       private$eval_model <- private$eval_linear
       private$plot_model <- plot_linear
     }
     
     if (private$objective == "categorical") {
       private$eval_model <- private$eval_categorical
       private$plot_model <- plot_categorical
     }
     
     if (private$objective == "binary") {
       private$eval_model <- private$eval_binary
       private$plot_model <- plot_binary
     } 
   },
   eval = function(target, pred, model_id = NULL){
     private$target <- target
     private$pred <- pred 
     private$model_id <- model_id 
     
     private$metrics <- list_metrics[[private$objective]] # linear, binary, categorical
     
     private$eval_model()
     self$plots <- private$plot_model(self$preds)
   }
 )
)