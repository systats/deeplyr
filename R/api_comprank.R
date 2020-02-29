#' fit_comprank
#' @export
fit_comprank <- function(self){
  
  outcomes <- self$process$data$outcomes
  predictors <- self$process$data$predictors
  
  wide <- tibble(
    game = 1:nrow(outcomes),
    player1 = as.numeric(predictors[[1]]), 
    score1 = as.numeric(outcomes[[1]]), 
    player2 = as.numeric(predictors[[2]]),
    score2 = as.numeric(outcomes[[2]])
  ) %>% 
  comperes::as_widecr()
  
  long <- comperes::as_longcr(wide)

  massy <- comperank::rank_massey(wide, keep_rating = T) 
  colley <- comperank::rank_colley(wide, keep_rating = T)
  keener <- comperank::rank_keener(long, !!! comperes::h2h_funs["mean_score"], keep_rating = T)
  markov <- comperank::rank_markov(long, !!! comperes::h2h_funs["num_wins"], keep_rating = T)
  od <- comperank::rank_od(long, if (player1[1] == player2[1]) 0 else mean(score1))
  elo <- comperank::rank_elo(long, keep_rating = T)

  list(massy, colley, keener, markov, od, elo) %>%
    purrr::reduce(dplyr::inner_join, by = "player") %>%
    dplyr::mutate_all(round, 3)
}


#' predict_comprank
#' @export
predict_comprank <- function(self, new_data){
  
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
