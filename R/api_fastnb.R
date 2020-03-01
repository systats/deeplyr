#' fit_fastnb
#' @export
fit_fastnb <- function(self){
  
  outcomes <- self$process$data$outcomes %>%
    purrr::set_names(c("score1", "score2")) %>%
    dplyr::mutate_all(as.numeric) %>%
    dplyr::mutate(
      winner100 = ifelse(score1 > score2, 1, 0),
      winner110 = ifelse(score1 >= score2, 1, 0),
      winner011 = ifelse(score2 >= score1, 1, 0),
      winner001 = ifelse(score2 > score1, 1, 0),
      winner010 = ifelse(score2 > score1, 1, 0),
      winner123 = dplyr::case_when(
        score1 > score2 ~ 1,
        score1 == score2 ~ 0,
        score1 < score2 ~ -1
      ),
      both_score = ifelse(score1 > 0 & score2 > 0, 1, 0),
      over_under = ifelse((score1+score2) > 2.5, 1, 0)
    )

  ### Here we build the design matrix, because globally would be two sparse
  df <- self$process$data$predictors %>%
    dplyr::mutate_all(as.factor) %>%
    dplyr::mutate(y = 1)
  
  rec <- recipes::recipe(y ~ local_team_id + visitor_team_id, data = df) %>%
    recipes::step_dummy(local_team_id, visitor_team_id) %>%
    recipes::prep(df, retain = T)
  
  x <- recipes::juice(rec) %>%
    dplyr::select(-y) 
  
  list(
    rec = rec,
    local_fnb_win = fastNaiveBayes::fnb.bernoulli(x, outcomes$winner100), 
    local_fnb_win_draw = fastNaiveBayes::fnb.bernoulli(x, outcomes$winner110), 
    visitor_fnb_win = fastNaiveBayes::fnb.bernoulli(x, outcomes$winner001), 
    visitor_fnb_win_draw = fastNaiveBayes::fnb.bernoulli(x, outcomes$winner011), 
    fnb_draw = fastNaiveBayes::fnb.bernoulli(x, outcomes$winner010), 
    fnb_ft_winner = fastNaiveBayes::fnb.multinomial(x, outcomes$winner123), 
    local_fnb_score = fastNaiveBayes::fnb.poisson(x, outcomes$score1), 
    visitor_fnb_score = fastNaiveBayes::fnb.poisson(x, outcomes$score1), 
    local_fnb_score_event = fastNaiveBayes::fnb.multinomial(x, outcomes$score1), 
    fnb_both_score = fastNaiveBayes::fnb.bernoulli(x, outcomes$both_score), 
    fnb_over_under = fastNaiveBayes::fnb.bernoulli(x, outcomes$winner010)
  )
}


#' predict_fastnb
#' @export
predict_fastnb <- function(self, new_data){
  
  if(is.null(self$model)){
    return(self$process$stream_id_x(new_data))
  }
   
  x <- new_data %>%
    dplyr::mutate(
      local_team_id = as.factor(local_team_id),
      visitor_team_id = as.factor(visitor_team_id)
    ) %>% 
    recipes::bake(self$model[[1]], .)

  self$model[-1] %>%
    purrr::imap_dfc(~{
      predict(.x, newdata = x) %>%
        as.character %>%
        as.numeric
    }) %>%
    cbind(self$process$stream_id_x(new_data), .)
}