#' num_wins
#' @export
near <- function (x, y, tol = .Machine$double.eps^0.5) {
  abs(x - y) < tol
}

#' num_wins
#' @export
num_wins <- function (score_1, score_2, half_for_draw = F, na.rm = T){
  near_score <- deeplyr::near(score_1, score_2)
  sum(score_1[!near_score] > score_2[!near_score], na.rm = na.rm) + half_for_draw * 0.5 * sum(near_score, na.rm = na.rm)
}

#' my_h2h_funs
#' @export
my_h2h_funs <- list(
  mean_score_diff = rlang::expr(mean(score1 - score2)), 
  mean_score_diff_pos = rlang::expr(max(mean(score1 - score2), 0)), 
  mean_score = rlang::expr(mean(score1)), 
  sum_score_diff = rlang::expr(sum(score1 - score2)), 
  sum_score_diff_pos = rlang::expr(max(sum(score1 - score2), 0)), 
  sum_score = rlang::expr(sum(score1)), 
  num_wins = rlang::expr(deeplyr::num_wins(score1, score2, half_for_draw = F)), 
  num_wins2 = rlang::expr(deeplyr::num_wins(score1, score2, half_for_draw = T))
)

#' get_comprank_h2h
#' @export
get_comprank_h2h <- function(wide){
  
  comperes::h2h_long(wide, !!!deeplyr::my_h2h_funs) %>%
    dplyr::select(-player2) %>%
    dplyr::group_by(player1) %>%
    dplyr::summarise_all(mean, na.rm = T) %>%
    dplyr::ungroup() %>%
    dplyr::rename(player = player1) %>%
    dplyr::rename_at(-1, ~paste0("h2h_", .x))
}

#' get_comprank_h2h_pos
#' @export
get_comprank_h2h_pos <- purrr::possibly(get_comprank_h2h, NULL)

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

  h2h <- get_comprank_h2h_pos(wide)
  massy <- comperank::rate_massey(wide) 
  colley <- comperank::rate_colley(wide)
  keener <- comperank::rate_keener(long, !!! deeplyr::my_h2h_funs["mean_score"])
  markov <- comperank::rate_markov(long, !!! deeplyr::my_h2h_funs["mean_score"])
  od <- comperank::rate_od(long, if (player1[1] == player2[1]) 0 else mean(score1))
  elo <- comperank::rate_elo(long)
  
  list(massy, colley, keener, markov, od, elo,h2h) %>%
    purrr::compact() %>%
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
