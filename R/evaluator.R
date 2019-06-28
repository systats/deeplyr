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
     self$plots <- plot_linear(self$preds)
   },
   eval_binary = function() {
     
     ### input is a probability vector
     self$preds <- tibble(prob = private$pred) %>%
       mutate(pred = ifelse(prob > .5, 1, 0)) %>%
       mutate(target = private$target,
              model_id = private$model_id)
     
     private$eval_metrics_binary()
     private$eval_metrics_prob()
     self$plots <- plot_binary(self$preds)
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
     self$plots <- plot_categorical(self$preds)
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
   },
   eval = function(target, pred, model_id = NULL){
     private$target <- target
     private$pred <- pred 
     private$model_id <- model_id 
     
     private$metrics <- list_metrics[[private$objective]] # linear, binary, categorical
     
     ### Metrics and Performance Grafics
     if (private$objective %in% c("linear", "mixture")) {
        private$eval_linear()
     }
     
     if (private$objective == "categorical") {
        private$eval_categorical()
     }
     
     if (private$objective == "binary") {
        private$eval_binary()
     }
   }
 )
)