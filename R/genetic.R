#' p_integer
#' @export
p_integer <- function(low, high){
  dials::new_quant_param(
    type = "integer",
    inclusive = c(T, T),
    range = c(low, high),
    label = c(name = "")
  ) 
}

#' p_double
#' @export
p_double <- function(low, high){
  dials::new_quant_param(
    type = "double",
    inclusive = c(T, T),
    range = c(low, high),
    label = c(name = "")
  ) 
}

#' normalize_params
#' @export
normalize_params = function(x){ 
  ### e.g. dropout ratios
  smaller_one <- function(x) ifelse(x <= 1, T, F)
  bigger_one <- function(x) ifelse(x > 1, T, F)
  
  ### only flatten integers 
  x %>% 
    # round droput to 2 digits (maybe only 1?)
    purrr::map_if(smaller_one, round, 2) %>% 
    purrr::map_if(bigger_one, floor) %>% 
    as.numeric %>%
    as.list %>%
    set_names(1:length(.)) %>%
    as_tibble()
}


#' genetic
#' @export
genetic <-  R6::R6Class("genetic",
  public = list(
    
    ### Initalize variables
    parent = NULL,
    process = NULL,
    model = NULL,
    hypers = NULL,
    params = NULL,
    data = NULL, 
    metric = NULL,
    minimize = NULL, 
    bounds = NULL,
    path = NULL,
    
    ### Main Function
    initialize = function(parent){
      
      init_models()
      self$parent <- init_parent(parent)
      
    },
    set = function(process, hypers, params, data, metric, minimize){
      
      self$process <- process
      self$hypers <- hypers
      self$data <- data
      self$params <- params
      
      self$metric <- metric
      self$minimize <- minimize
      
    },
    
    compute = function(...){
      
      ### Mapping dial parameters to GA
      self$bounds <- self$hypers %>% 
        purrr::imap_dfr(~{
          dplyr::tibble(
            name = .y,
            lower = .x$range$lower, 
            upper = .x$range$upper
          )
        })
      
      ### Fitting fun
      run_model <- function(x) { 
        
        # creating new run path
        run_path <- init_run(self$parent$par_path)
        
        # floor doubles to ineteger
        new_params <- normalize_params(x) %>%
          purrr::set_names(self$bounds$name) %>%
          c(., self$params)
        
        process_pos <- purrr::possibly(self$process, list(accuracy = 0))  
        
        perform <- process_pos(
          params = new_params, 
          data = self$data,
          path = run_path
        )
        
        if(self$minimize){
          out <- perform[[self$metric]] * -1
        } else {
          out <- perform[[self$metric]]
        }
        return(out)
      }
      
      ### Main Run
      self$model <- GA::ga(
        type = "real-valued", 
        fitness = run_model, 
        lower = self$bounds$lower, 
        upper = self$bounds$upper,
        ...
      )
    },
    run = function(process, hypers, params, data, metric, minimize, ...){
      
      self$set(process, hypers, params, data, metric, minimize)
      self$compute(...)
      
    }
    # further = function(process, hypers, params, data, metric, minimize, ...){
    #   self$set(process, hypers, params, data, metric, minimize)
    #   self$compute(suggestions = )
    # }
  )
)