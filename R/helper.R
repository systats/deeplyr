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