#' get_local_long
get_local_long <- function(.data){
  .data %>%
    dplyr::transmute(
      #date_time,
      game_id,
      # league_id,
      team_id = local_team_id,
      #team_name = local_team_name,
      opponent_id = visitor_team_id,
      #opponent_name = visitor_team_name,
      ht_scored = local_ht_score,
      ht_conceded = visitor_ht_score,
      ft_scored = local_ft_score,
      ft_conceded = visitor_ft_score,
      side = "local"
    )
}

#' get_visitor_long
get_visitor_long <- function(.data){
  .data %>%
    dplyr::transmute(
      #date_time,
      game_id,
      #league_id,
      team_id = visitor_team_id,
      #team_name = visitor_team_name,
      opponent_id = local_team_id,
      #opponent_name = local_team_name,
      ht_scored = visitor_ht_score,
      ht_conceded = local_ht_score,
      ft_scored = visitor_ft_score,
      ft_conceded = local_ft_score,
      side = "visitor"
    )
}

#' transform_games_long
#' @export
transform_games_long <- function(g){
  dplyr::bind_rows(
    g %>% get_local_long,
    g %>% get_visitor_long
  ) %>%
    dplyr::arrange(team_id)
}

get_team_stats <- function(.data){
  
  .data %>% 
    dplyr::transmute(
      # ht_scored = ifelse(is.na(ht_scored), 0, ht_scored),
      # ht_conceded = ifelse(is.na(ht_conceded), 0, ht_conceded),

      # ht_score_ratio = round((ht_scored+1)/(ht_conceded+1), 3),
      # ht_score_percent = round(ht_scored/(ht_scored + ht_conceded), 3),
      # ht_score_percent = ifelse(is.nan(ht_score_percent), .5, ht_score_percent),
      # ht_score_diff = ht_scored - ht_conceded,
      # ht_won = ifelse(ht_scored > ht_conceded, 1, 0),
      # ht_draw = ifelse(ht_scored == ht_conceded, 1, 0),
      # ht_lost = ifelse(ht_scored < ht_conceded, 1, 0),
      # ht_won_scored = ifelse(ht_won, ht_scored, 0),
      # ht_won_conceded = ifelse(ht_won, ht_conceded, 0),
      # ht_lost_scored = ifelse(ht_lost, ht_scored, 0),
      # ht_lost_conceded = ifelse(ht_lost, ht_conceded, 0),
      # ht_over_under_05 = ifelse((ht_scored+ht_conceded) > .5, 1, 0),
      # ht_over_under_15 = ifelse((ht_scored+ht_conceded) > 1.5, 1, 0),
      # ht_over_under_25 = ifelse((ht_scored+ht_conceded) > 2.5, 1, 0),
      # ht_bt_score = ifelse((ht_scored > 0 & ht_conceded > 0), 1, 0),

      ft_score_ratio = round((ft_scored+1)/(ft_conceded+1), 3),
      ft_score_percent = round(ft_scored/(ft_scored + ft_conceded), 3),
      ft_score_percent = ifelse(is.nan(ft_score_percent), .5, ft_score_percent),
      ft_score_diff = ft_scored - ft_conceded,
      ft_won = ifelse(ft_scored > ft_conceded, 1, 0),
      ft_draw = ifelse(ft_scored == ft_conceded, 1, 0),
      ft_lost = ifelse(ft_scored < ft_conceded, 1, 0),
      ft_won_scored = ifelse(ft_won, ft_scored, 0),
      ft_won_conceded = ifelse(ft_won, ft_conceded, 0),
      ft_lost_scored = ifelse(ft_lost, ft_scored, 0),
      ft_lost_conceded = ifelse(ft_lost, ft_conceded, 0),
      ft_over_under_05 = ifelse((ft_scored + ft_conceded) > .5, 1, 0),
      ft_over_under_15 = ifelse((ft_scored + ft_conceded) > 1.5, 1, 0),
      ft_over_under_25 = ifelse((ft_scored + ft_conceded) > 2.5, 1, 0),
      ft_over_under_35 = ifelse((ft_scored + ft_conceded) > 3.5, 1, 0),
      ft_bt_score = ifelse((ft_scored > 0 & ft_conceded > 0), 1, 0)
      
    )
}


#' fit_soccerstats 
#' @export
fit_soccerstats <- function(self){
  
  sides <- self$process$juice() %>% 
    transform_games_long() %>%
    dplyr::group_by(side, team_id) %>%
    get_team_stats() %>%
    dplyr::select(contains("ft_"), contains("ht_"), contains("hft_")) %>%
    dplyr::summarise_all(.funs = list(mean = mean, sd = sd), na.rm = T) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_at(-1, round, 3)
  
  list(
    local = all %>% 
      dplyr::filter(side == "local") %>%
      dplyr::select(-side), 
    visitor = all %>% 
      dplyr::filter(side == "visitor") %>% 
      dplyr::select(-side)
  )
}

#' predict_soccerstats 
#' @export
predict_soccerstats <- function(self, new_data){
  
  new_data %>%
    dplyr::select(game_id, local_team_id, visitor_team_id) %>% 
    dplyr::left_join(self$model$local %>% dplyr::rename_all(~paste0("local_", .x)), by = "local_team_id") %>%
    dplyr::left_join(self$model$visitor %>% dplyr::rename_all(~paste0("visitor_", .x)), by = "visitor_team_id")
}





