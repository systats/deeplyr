#' save_error
#' @export
save_error <- function(file, path) write_lines(file, glue::glue("{path}/error.txt"))

#' grid
#' @export
grid <- R6::R6Class("grid",
   private = NULL,
   public = list(
     parent = NULL,
     initialize = function(parent){
       init_models()
       self$parent <- init_parent(parent)
     },
     list_runs = function(){
       list_runs_pos <- purrr::possibly(list_runs, NULL)
       return(list_runs_pos(self$parent))
     },
     run = function(data, process, params){
       process_safe <- safely(process)
       params %>% 
         split(1:nrow(.)) %>%
         walk(~{
           run_path <- init_run(self$parent$par_path)
           out <- process_safe(.x, data, run_path)
           if(!is.null(out$error)) save_error(out$error, run_path)
         })
     }
   )
)
