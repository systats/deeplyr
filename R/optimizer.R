#' @export
p_integer <- function(name, range){
   dials::new_quant_param(
      type = "integer",
      inclusive = c(T, T),
      range = range,
      label = c(name = glue::glue("# {name}"))
   ) 
}

#' @export
p_double <- function(name, range){
   dials::new_quant_param(
      type = "double",
      inclusive = c(T, T),
      range = range,
      label = c(name = glue::glue("# {name}"))
   ) 
}

#' GA
#' 
#' @export
ga <-  R6::R6Class("optimizer",
 #inherit = splitter, 
 private = list(
   fun = NULL,
   hyper = NULL,
   param = NULL,
   bounds = NULL,
   folder = NULL,
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
   initialize = function(folder) {
     ### Combine and add parameters
     
     private$folder <- folder
     
     if(!dir.exists("models")){
       dir.create("models")
     }
     if(!dir.exists(glue::glue("models/{folder}"))){
       dir.create(glue::glue("models/{folder}"))
     }
   },
   set = function(fun, hyper, param){
      self$set_fun(fun)
      self$set_hyper(hyper)
      self$set_param(param)
   },
   set_fun = function(fun){
      private$fun <- fun
   },
   set_hyper = function(hyper){
      private$hyper <- hyper
   },
   set_param = function(param){
     private$param <- param
   },
   get_param = function(){
     return(private$param)
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
     
     ### Fitting fun
     run_model <- function(x) { 
       # floor doubles to ineteger
       new_param <- private$normalize_param(x) %>%
         purrr::set_names(private$bounds$name)
       
       # Here are the static param!!!
       out <- private$fun(c(private$param, new_param), private$folder)
       return(out)
     }
     
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


#' GA
#' 
#' @export
fit_ga <- function(fun, hyper, param, folder = ".", ...){
   g <- deeplyr::ga$new(folder)
   g$set(fun, hyper, param)
   g$optimize(...)
   return(g)
}