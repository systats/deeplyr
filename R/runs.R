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
list_runs <- function(.data){
  
  init_models()
  run_id <- dir(.data$par_path)  
  run_path <- dir(.data$par_path, full.names = T)
  
  dplyr::tibble(run_id, run_path) %>% 
    dplyr::mutate(
      meta = purrr::map(run_path, possibly(load_meta, NULL)), 
      params = purrr::map(run_path, possibly(load_params, NULL)), 
      metrics = purrr::map(run_path, possibly(load_metrics, NULL)),
      cv_metrics = purrr::map(run_path, possibly(load_cv, NULL))
      # evals = purrr::map(run_path, possibly(load_evals, NULL))
    ) %>%
    cbind(.data)
}


#' filter_parent
#' @export
filter_parent <- function(parent){
  if(is.numeric(parent)){
    # set parent experiment by number
    dplyr::filter(list_parents(), par_number == parent)
  } else if(is.character(parent)){
    # set by name
    dplyr::filter(list_parents(), par_id == parent | par_name == parent | par_path == parent)
  }
}


#' init_parent
#' @export
init_parent <- function(parent){
  
  ### check if models exists
  init_models()
  
  ### check if parent exists
  par <- filter_parent(parent)
  
  ### initalize parent
  if(nrow(par) == 0){
    num <- stringr::str_pad((nrow(list_parents()) + 1), width = 2, side = "left", pad = "0")
    dir.create(glue::glue("models/{nrow(list_parents())+1}_{parent}"))
    par <- filter_parent(parent)
  }
  
  return(par)
}


#' init_run
#' @export
init_run <- function(par_path){
  
  ### check if models exists
  init_models()
  
  runid <- stringr::str_sub(digest::digest(Sys.time()), 1, 8)
  run_path <- glue::glue("{par_path}/{runid}")
  dir.create(run_path)
  #dir.create(glue::glue("{run_path}/outputs"))
  return(run_path)
}
