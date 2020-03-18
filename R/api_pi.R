#' fit_pi 
#' @export
fit_pi <- function(self){
  
  outcomes <- self$process$data$outcomes %>%
    dplyr::mutate_all(~ifelse(is.na(.x), 0, .x)) %>%
    as.matrix()
  
  predictors <- self$process$juice_x() %>%
    dplyr::mutate_all(as.character) %>%
    as.matrix()
  
  # teams <- x_train[, c("local_team_id", "visitor_team_id")] %>% dplyr::mutate_all(as.character) %>% as.matrix()
  # outcomes <- x_train[, c("local_team_score", "visitor_team_score")] %>% dplyr::mutate_all(~ifelse(is.na(.x), 0, .x)) %>% as.matrix()

  piratings <<- piratings::calculate_pi_ratings(predictors, outcomes) %>%
    as_tibble() %>% 
    purrr::set_names(c("local_pi", "visitor_pi")) %>%
    glimpse
  
  self$process$juice_x() %>%
    dplyr::bind_cols(piratings) %>% 
    dplyr::mutate(1:n()) %>%
    group_by(local_team_id) %>%
    slice(n()) %>%
    group_by(visitor_team_id) %>%
    slice(n()) %>%
    glimpse
}

#' predict_pi
#' @export
predict_pi <- function(self, new_data){
  
  if(is.null(self$model)){
    return(self$process$stream_id_x(new_data))
  }
  
  pi <- self$process$stream_id_x(new_data) %>%
    dplyr::left_join(self$model)
  
  return(pi)
}