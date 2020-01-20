#' fit_elo 
#' @export
fit_elo <- function(self){
  
  outcomes <- self$process$data$outcomes
  predictors <- self$process$juice_x()
  
  .data <- list(
    goals1 = as.numeric(outcomes[[1]]), 
    goals2 = as.numeric(outcomes[[2]]),
    team1 = as.character(predictors[[1]]), 
    team2 = as.character(predictors[[2]])
  )
  
  if(is.null(self$params$init)) self$params$init <- 1500
  if(is.null(self$params$k)) self$params$k <- 30
  
  elo_form <- as.formula("elo::score(goals1, goals2) ~ team1 + team2")
  elo::elo.run(elo_form, data = .data, k = self$params$k, initial.elos = self$params$init)
}

#' predict_elo 
#' @export
predict_elo <- function(self, new_data){
  
  if(is.null(self$model)){
    return(self$process$stream_id_x(new_data))
  }
  
  weights <- self$model %>%
    elo::final.elos() %>%
    as.list() %>%
    tibble::enframe(name = "team_id", value = "elo") %>%
    tidyr::unnest(cols = elo) %>%
    dplyr::mutate_all(as.numeric)
  
  self$process$stream_id_x(new_data) %>%
    tidyr::gather(side, team_id, -game_id) %>%
    dplyr::mutate(side = stringr::str_extract(side, "^local|^visitor")) %>%
    dplyr::left_join(weights, by = "team_id") %>%
    dplyr::distinct(game_id, team_id, .keep_all = T) %>%
    tidyr::pivot_wider(id_cols = game_id, names_from = side, values_from = team_id:ncol(.))
}