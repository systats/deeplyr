#' GA
#' 
#' @export
ga <-  R6::R6Class("optimizer",
 #inherit = splitter, 
 private = list(
   x = NULL, 
   data = NULL,
   param = NULL,
   hyper = NULL,
   bounds = NULL,
   mkdir = NULL,
   backend = NULL,
   normalize_param = function(x){ 
     ### e.g. dropout ratios
     smaller_one <- function(x) ifelse(x <= 1, T, F)
     bigger_one <- function(x) ifelse(x > 1, T, F)
     
     ### only flatten integers 
     out <- x %>% 
       # round droput to 2 digits (maybe only 1?)
       purrr::map_if(smaller_one, round, 2) %>% 
       purrr::map_if(bigger_one, floor) %>% 
       as.numeric
     
     return(out)
   },
   complete_param = function(param1, param2){
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
   }
 ),
 public = list(
   ### Initalize variables
   ### Main Function
   initialize = function(mkdir, backend) {
     ### Combine and add parameters
     
     private$mkdir <- mkdir
     private$backend <- backend
     
     if(!dir.exists("models")){
       dir.create("models")
     }
     if(!dir.exists("models/results")){
       dir.create(glue::glue("models/{mkdir}"))
     }
   },
   set_param = function(param){
     private$param <- param
   },
   get_param = function(){
     return(private$param)
   },
   set_hyper = function(hyper){
     private$hyper <- hyper
   },
   set_data = function(data, x){
     private$data <- data
     private$x <- x
   },
   get_ga_bounds = function(){
     private$hyper %>% 
       purrr::map_dfr(~{
         tibble(
           name = .x$label %>% names, 
           desc = .x$label, 
           lower = .x$range$lower, 
           upper = .x$range$upper
         )
       })
   },
   optimize = function(...){
     ### Mapping dial parameters to GA
     private$bounds <- self$get_ga_bounds()
     
     fit_model <- function(params, data, x, backend, ...){
       jo <- deeplyr::learner$new(backend)
       jo$set_param(params)
       jo$set_data(data, x)
       jo$split(oos = F, val = F, by_index = "split")
       
       jo$train(cv = F)
       jo$test()
       out <- jo$report(...)
       return(out)
     }
     
     ### Fitting fun
     run_model <- function(x) { 
       
       # floor doubles to ineteger
       new_param <- private$normalize_param(x) %>%
         purrr::set_names(private$bounds$name)
       
       # Here are the static param!!!
       temp <- c(private$param, new_param)
       # print(pt)
       # print(private$data)
       
       out <- fit_model(
         param = temp, 
         data = private$data, 
         x = private$x, 
         backend = private$backend, 
         mkdir = private$mkdir,
         return_value = T
       )
       
       return(out$accuracy)
     }
     
     print(private$bounds$lower)
     print(private$bounds$upper)
     
     ### Main Run
     ga_out <- GA::ga(
       type = "real-valued", 
       fitness = run_model, 
       lower = private$bounds$lower, 
       upper = private$bounds$upper, 
       ...
     )
   }
 )
)