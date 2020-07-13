#' init_models
#' @export 
init_models <- function(){
  if(!dir.exists("models")){
    dir.create("models")
  }
}

#' list_parents
#' @export 
list_parents <- function(path = NULL){
  
  init_models()
  
  if(!is.null(path)) {
    mydir <- glue::glue("{path}/models")
  } else {
    mydir <- "models"
  }
  
  dplyr::tibble(
    par_id = dir(mydir), 
    par_path = dir(mydir, full.names = T)
  ) %>%
  dplyr::mutate(
    par_name = stringr::str_remove(par_id, "\\d+_"), 
    par_number = stringr::str_extract(par_id, "\\d+") %>% as.numeric
  )
}


#' load_meta
#' @export 
load_meta <- function(path) dplyr::bind_cols(jsonlite::fromJSON(glue::glue("{path}/meta.json")))

#' load_params
#' @export 
load_params <- function(path) dplyr::bind_cols(jsonlite::fromJSON(glue::glue("{path}/params.json")))

#' load_metrics
#' @export 
load_metrics <- function(path) dplyr::bind_cols(jsonlite::fromJSON(glue::glue("{path}/metrics.json")))

#' load_cv
#' @export 
load_cv <- function(path) jsonlite::fromJSON(glue::glue("{path}/cv_metrics.json"))

#' load_evals
#load_evals <- function(path) get(load(glue::glue("{path}/evals.Rdata")))

#' load_list_files
#' @export 
load_list_files <- function(path) dir(glue::glue("{path}"))


#' list_runs
#' @export 
list_runs <- function(path) {
  
  run_id <- dir(path)
  run_path <- dir(path, full.names = T)
  
  dplyr::tibble(run_id, run_path) %>% 
    dplyr::mutate(
      meta = purrr::map(run_path, possibly(load_meta, NULL)),
      params = purrr::map(run_path, possibly(load_params, NULL)),
      metrics = purrr::map(run_path, possibly(load_metrics, NULL))
    )
}
