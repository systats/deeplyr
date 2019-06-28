#' create_container
#' 
#' @export
create_container = function(param, data = NULL, drop = NULL){
  d <- data_container$new()
  d$set_param(param)
  d$set_data(data)
  d$transform_drop(drop)
  return(d)
}

#' create_seq_container
#' 
#' @export
create_seq_container <- function(param, data = NULL, parallel = T){
  d <- data_container$new()
  d$set_param(param)
  d$set_data(data)
  d$transform_seq(parallel)
  return(d)
}



#' create_dtm_container
#' 
#' @export
create_dtm_container <- function(param, data = NULL){
  d <- data_container$new()
  d$set_param(param)
  d$set_data(data)
  d$transform_dtm(mode = "binary")
  return(d)
}
