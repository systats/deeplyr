#' @export
p_integer <- function(low, high){
   dials::new_quant_param(
      type = "integer",
      inclusive = c(T, T),
      range = c(low, high),
      label = c(name = "")
   ) 
}

#' @export
p_double <- function(low, high){
   dials::new_quant_param(
      type = "double",
      inclusive = c(T, T),
      range = c(low, high),
      label = c(name = "")
   ) 
}

#' @export
info <- function(param, perform) {
   
   param <- param %>% 
      purrr::imap_chr(~{glue::glue("{.y}={.x}")}) %>% 
      c(" ", .) %>%
      paste(collapse = " ")
   
   perform <- perform %>% 
      purrr::imap_chr(~{glue::glue("{.y}={.x}")}) %>% 
      c(" ", .) %>%
      paste(collapse = " ")
   
   crayon::blue("GA ") %+% 
      crayon::green(crayon::italic(glue::glue("[{Sys.time()}]"))) %+%
      param %+%
      crayon::blue(perform) %+% 
      "\n" %>%
      cat
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
   metric = NULL,
   minimize = NULL, 
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
   set = function(fun, hyper, param, metric, minimize){
      self$set_fun(fun)
      self$set_hyper(hyper)
      self$set_param(param)
      self$set_eval(metric, minimize)
   },
   set_eval = function(metric, minimize){
     private$metric <- metric
     private$minimize <- minimize
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
       purrr::imap_dfr(~{
         tibble(
           name = .y,
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
       
       perform <- private$fun(.x = c(private$param, new_param), folder = private$folder)
       
       info(new_param, perform)
       
       if(private$minimize){
         out <- perform[[private$metric]] * -1
       } else {
         out <- perform[[private$metric]]
       }
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


#' fit_ga
#' 
#' @export
fit_ga <- function(fun, hyper, param, metric, minimize, folder = ".", ...){
   g <- deeplyr::ga$new(folder)
   g$set(fun, hyper, param, metric, minimize)
   g$optimize(...)
   return(g)
}
