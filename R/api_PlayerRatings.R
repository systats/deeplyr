#' fit_PlayerRatings 
#' @export
fit_PlayerRatings <- function(self){
  
  outcomes <- self$process$data$outcomes
  predictors <- self$process$juice_x()
  
  .data <- dplyr::tibble(
    week = 1:length(predictors[[1]]),
    team1 = predictors[[1]], 
    team2 = predictors[[2]],
    winner = dplyr::case_when(
      outcomes[[1]] > outcomes[[2]] ~ 1,
      outcomes[[1]] == outcomes[[2]] ~ .5,
      outcomes[[1]] < outcomes[[2]] ~ 0
    )
  )
  
  list(
    pr_elo = PlayerRatings::elo,
    pr_fide = PlayerRatings::fide,
    pr_glicko = PlayerRatings::glicko,
    pr_glicko2 = PlayerRatings::glicko2,
    pr_steph = PlayerRatings::steph
  ) %>%
    purrr::imap(~{ .x(.data)$ratings[,1:2] %>% purrr::set_names(c("team_id", .y))}) %>%
    purrr::reduce(dplyr::inner_join, by = "team_id")
  
}

#' predict_PlayerRatings
#' @export
predict_PlayerRatings <- function(self, new_data){
  
  if(is.null(self$model)){
    return(self$process$stream_id_x(new_data))
  }
  
  ratings <- self$process$stream_id_x(new_data) %>%
    dplyr::left_join(self$model %>% dplyr::rename_all(~paste0("local_", .x)), by = "local_team_id") %>%
    dplyr::left_join(self$model %>% dplyr::rename_all(~paste0("visitor_", .x)), by = "visitor_team_id")
  
  return(ratings)
}
