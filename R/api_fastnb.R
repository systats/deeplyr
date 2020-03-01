#' fit_fastnb
#' @export
fit_fastnb <- function(self){
  
  outcomes <- self$process$data$outcomes %>%
    purrr::set_names(c("score1", "score2")) %>%
    dplyr::mutate(
      winner100 = ifelse(score1 > score2, 1, 0),
      winner110 = ifelse(score1 >= score2, 1, 0),
      winner011 = ifelse(score2 >= score1, 1, 0),
      winner001 = ifelse(score2 > score1, 1, 0),
      winner010 = ifelse(score2 > score1, 1, 0),
      score_diff = score1 - score2,
      winner123 = dplyr::case_when(
        score1 > score2 ~ 1,
        score1 == score2 ~ 0,
        score1 < score2 ~ -1
      ) 
    )

  x <- self$process$data$predictors
  
  list(
    local_fnb_win = fastNaiveBayes::fnb.bernoulli(x, outcomes$winner100), 
    local_fnb_win_draw = fastNaiveBayes::fnb.bernoulli(x, outcomes$winner110), 
    visitor_fnb_win = fastNaiveBayes::fnb.bernoulli(x, outcomes$winner001), 
    visitor_fnb_win_draw = fastNaiveBayes::fnb.bernoulli(x, outcomes$winner011), 
    fnb_draw = fastNaiveBayes::fnb.bernoulli(x, outcomes$winner010), 
    fnb_ft_winner = fastNaiveBayes::fnb.multinomial(x, outcomes$winner123), 
    local_fnb_score = fastNaiveBayes::fnb.poisson(x, outcomes$score1), 
    visitor_fnb_score = fastNaiveBayes::fnb.poisson(x, outcomes$score1), 
    fnb_score_diff = fastNaiveBayes::fastNaiveBayes(x, outcomes$score_diff),
    local_fnb_score_event = fastNaiveBayes::fnb.multinomial(x, outcomes$score1), 
    visitor_fnb_score_event = fastNaiveBayes::fnb.multinomial(x, outcomes$score2), 
    fnb_score_event_diff = fastNaiveBayes::fnb.multinomial(x, outcomes$score_diff)
  )
}


#' predict_fastnb
#' @export
predict_fastnb <- function(self, new_data){
  
  if(is.null(self$model)){
    return(self$process$stream_id_x(new_data))
  }

  self$model %>%
    purrr::imap_dfc(~{
      predict(.x, newdata = self$process$stream(new_data)) %>%
        as.character %>%
        as.numeric
    })
}