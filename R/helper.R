#' range01
#' @export
range01 <- function(x){round((x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T)), 3)}

#' censor_bigger values
#' @export
censor_bigger <- function(x, k) ifelse(x > k, k, x)

#' censor_smaller values
#' @export
censor_smaller <- function(x, k) ifelse(x < k, k, x)

#' censor_quantiles values
#' @export
censor_quantiles <- function(x){
  
  q <- quantile(x, na.rm = T, probs = c(.05, .95))
  
  case_when(
    x < q[1] ~ q[1],
    x > q[2] ~ q[2],
    T ~ x
  )
}

#' is_numeric 
#' @export
is_numeric <- function(x){
  mean(!is.na(as.numeric(x))) > .5
}

#' silently 
#' @export
silently <- function(f){
  suppressMessages(suppressWarnings(f))
}


#' get_model_param
#' @export
get_model_param = function(model, param){
  
  # do_func_pos <- function(what, data){
  #   acceptable_args <- data[names(data) %in% (formals(what) %>% names)]
  #   do.call(what, acceptable_args %>% as.list)
  # }
  
  out <- formals(model) %>%
    imap(~{
      if(.y %in% names(param)){
        .x <-  param[[.y]]
      }
      return(.x)
    })
  return(out)
}

#' complete_param
#' @export
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


#' save_json
#' @export
save_json <- function(file, name, path) {
  file %>% 
    dplyr::as_tibble() %>%
    jsonlite::toJSON() %>% 
    jsonlite::fromJSON() %>% 
    jsonlite::toJSON(pretty = TRUE) %>% 
    writeLines(., glue::glue("{path}/{name}.json")) 
}

#' save_json_pos
#' @export
save_json_pos <- purrr::possibly(save_json, NULL)

#' save_rds
#' @export
save_rds <- function(file, name, path) saveRDS(file, file = glue::glue("{path}/{name}.rds"))

#' save_rds_pos
#' @export
save_rds_pos <- purrr::possibly(save_rds, NULL)
# save_rdata <- function(file, name, path) save(file, file = glue::glue("{path}/{name}.RData"))

#' load_json
#' @export
load_json <- function(path, name) dplyr::bind_cols(jsonlite::fromJSON(glue::glue("{path}/{name}.json")))

#' load_rds
#' @export
load_rds <- function(path, name) readr::read_rds(glue::glue("{path}/{name}.rds"))



