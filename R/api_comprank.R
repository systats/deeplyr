#' fit_comperank
#' @export
fit_comperank <- function(self){
  
  outcomes <- self$process$data$outcomes
  predictors <- self$process$data$predictors
  
  wide <- dplyr::tibble(
    game = 1:nrow(outcomes),
    player1 = as.numeric(predictors[[1]]), 
    score1 = as.numeric(outcomes[[1]]), 
    player2 = as.numeric(predictors[[2]]),
    score2 = as.numeric(outcomes[[2]])
  ) %>% 
  comperes::as_widecr()
  long <- comperes::as_longcr(wide)

  massy <- comperank::rate_massey(wide) 
  colley <- comperank::rate_colley(wide)
  
  h2h <- comperes::h2h_long(wide, !!!comperes::h2h_funs) %>%
    dplyr::select(-player2) %>%
    dplyr::group_by(player1) %>%
    dplyr::summarise_all(mean, na.rm = T) %>%
    dplyr::ungroup() %>%
    dplyr::rename(player = player1) %>%
    dplyr::rename_at(-1, ~paste0("h2h_", .x))
  
  keener <- comperank::rate_keener(long, !!! comperes::h2h_funs["mean_score"])
  markov <- comperank::rate_markov(long, !!! comperes::h2h_funs["mean_score"])
  od <- comperank::rate_od(long, if (player1[1] == player2[1]) 0 else mean(score1))
  elo <- comperank::rate_elo(long)
  
  list(massy, colley, keener, markov, od, elo, h2h) %>%
    purrr::reduce(dplyr::inner_join, by = "player") %>%
    dplyr::mutate_all(round, 3)
}



#' predict_comperank
#' @export
predict_comperank <- function(self, new_data){
  
  if(is.null(self$model)){
    return(self$process$stream_id_x(new_data))
  }
  
  ranks <- self$process$stream_id_x(new_data) %>%
    dplyr::left_join(
      self$model %>% 
        dplyr::rename(team_id = player) %>% 
        dplyr::rename_all(~paste0("local_", .x)), 
      by = "local_team_id"
    ) %>%
    dplyr::left_join(
      self$model %>% 
        dplyr::rename(team_id = player) %>% 
        dplyr::rename_all(~paste0("visitor_", .x)), 
      by = "visitor_team_id"
    )
  
  return(ranks)
}
