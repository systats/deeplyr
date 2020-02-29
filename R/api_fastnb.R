#' fit_fastnb
#' @export
fit_fastnb <- function(self){
  
  outcomes <- self$process$data$outcomes %>%
    set_names(c("local_team_score", "visitor_team_score")) %>%
    dplyr::mutate(
      winner100 = ifelse(local_team_score > visitor_team_score, 1, 0),
      winner110 = ifelse(local_team_score >= visitor_team_score, 1, 0),
      winner011 = ifelse(visitor_team_score >= local_team_score, 1, 0),
      winner001 = ifelse(visitor_team_score > local_team_score, 1, 0),
      score_diff = local_team_score - visitor_team_score,
      winner123 = dplyr::case_when(
        local_team_score > visitor_team_score ~ 1,
        local_team_score == visitor_team_score ~ 0,
        local_team_score < visitor_team_score ~ -1
      ) 
    )

  df <- self$process$data$predictors %>%
    dplyr::mutate(y = 1) %>%
    dplyr::mutate(
      local_team_id = as.factor(local_team_id),
      visitor_team_id = as.factor(visitor_team_id)
    )
  
  rec <- df %>%
    recipes::recipe(y ~ local_team_id + visitor_team_id, data = head(.)) %>%
    recipes::step_dummy(local_team_id, visitor_team_id) %>%
    recipes::prep(training = df, retain = T)
  
  x <- recipes::juice(rec) %>%
    dplyr::select(-y)
  
  list(
    rec = rec,
    local_fnb_win = fastNaiveBayes::fnb.bernoulli(x, outcomes$winner011), 
    local_fnb_win_draw = fastNaiveBayes::fnb.bernoulli(x, outcomes$winner001), 
    visitor_fnb_win = fastNaiveBayes::fnb.bernoulli(x, outcomes$winner001), 
    visitor_fnb_win_draw = fastNaiveBayes::fnb.bernoulli(x, outcomes$winner011), 
    fnb_ft_winner = fastNaiveBayes::fnb.multinomial(x, outcomes$winner123), 
    local_fnb_score = fastNaiveBayes::fnb.poisson(x, outcomes$local_team_score), 
    visitor_fnb_score = fastNaiveBayes::fnb.poisson(x, outcomes$visitor_team_score), 
    fnb_score_diff = fastNaiveBayes::fastNaiveBayes(x, outcomes$score_diff),
    local_fnb_score_event = fastNaiveBayes::fnb.multinomial(x, outcomes$local_team_score), 
    visitor_fnb_score_event = fastNaiveBayes::fnb.multinomial(x, outcomes$visitor_team_score), 
    fnb_score_event_diff = fastNaiveBayes::fnb.multinomial(x, outcomes$score_diff)
  )
}


#' predict_fastnb
#' @export
predict_fastnb <- function(self, new_data){
  
  if(is.null(self$model)){
    return(self$process$stream_id_x(new_data))
  }

  x <- recipes::bake(self$model[[1]], new_data = self$process$stream_id_x(new_data))
  
  self$model[-1] %>%
    purrr::imap_dfc(~{
      predict(.x, newdata = x) %>%
        as.character %>%
        as.numeric
    }) %>%
    cbind(self$process$stream_id_x(new_data), .)
}