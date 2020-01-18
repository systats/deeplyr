#' goal_model
#' @export
goal_model <- function (goals1, goals2, team1, team2, x1 = NULL, x2 = NULL, 
                        hfa = TRUE, dc = FALSE, rs = FALSE, fixed_params = NULL, 
                        weights = NULL, model = "poisson", optim_method = NULL,
                        initvals = NULL) 
{
   stopifnot(length(goals1) == length(goals2), length(goals2) == 
                length(team1), length(team1) == length(team2), length(goals1) >= 
                1, is.numeric(goals1), is.numeric(goals2), model %in% 
                c("poisson", "negbin", "gaussian", "ls"))
   if (dc) {
      if (!any(goals1 <= 1 & goals2 <= 1)) {
         stop("Dixon-Coles adjustment is not applicable when there are no instances both teams scoring 1 goal or less. ")
      }
      if (model %in% c("gaussian", "ls")) {
         stop("Dixon-Coles adjustment does not work with a Gaussian model.")
      }
   }
   if (!is.null(weights)) {
      stopifnot(is.numeric(weights), length(goals1) == length(weights), 
                all(weights >= 0), all(!is.na(weights)), !all(weights == 
                                                                 0))
   }
   team1 <- as.character(team1)
   team2 <- as.character(team2)
   all_teams <- sort(unique(c(unique(team1), unique(team2))), 
                     decreasing = FALSE)
   n_teams <- length(all_teams)
   parameter_list <- goalmodel:::initial_values(goals1 = goals1, goals2 = goals2, 
                                                team1 = team1, team2 = team2, all_teams = all_teams, 
                                                initvals = initvals)
   if (hfa) {
      parameter_list$hfa <- 0.1
   }
   if (dc) {
      parameter_list$rho <- 0.01
   }
   if (rs) {
      parameter_list$gamma <- 0
   }
   if (model == "negbin") {
      parameter_list$dispersion <- -10
   }
   if (model == "gaussian") {
      parameter_list$sigma <- log(stats::sd(c(goals1, goals2)) * 
                                     1.5)
   }
   additional_covariates <- c()
   if (!is.null(x1)) {
      stopifnot(is.matrix(x1))
      if (is.null(colnames(x1))) {
         stop("Error: The columns of the x1 matrix must be named.")
      }
      additional_covariates <- unique(c(additional_covariates, 
                                        colnames(x1)))
   }
   if (!is.null(x2)) {
      stopifnot(is.matrix(x2))
      if (is.null(colnames(x2))) {
         stop("Error: The columns of the x2 matrix must be named.")
      }
      additional_covariates <- unique(c(additional_covariates, 
                                        colnames(x2)))
   }
   if (length(additional_covariates) != 0) {
      parameter_list$beta <- rep(0.1, length(additional_covariates))
      names(parameter_list$beta) <- additional_covariates
   }
   if (!is.null(initvals)) {
      if ("hfa" %in% names(initvals)) {
         if (hfa) {
            parameter_list$hfa <- initvals$hfa
         }
         else {
            warning("Initial values for hfa is supplied, but hfa=FALSE. Will be ignored.")
         }
      }
      if ("rho" %in% names(initvals)) {
         if (dc) {
            parameter_list$rho <- initvals$rho
         }
         else {
            warning("Initial values for rho is supplied, but dc=FALSE. Will be ignored.")
         }
      }
      if ("gamma" %in% names(initvals)) {
         if (rs) {
            parameter_list$gamma <- initvals$gamma
         }
         else {
            warning("Initial values for gamma is supplied, but rs=FALSE. Will be ignored.")
         }
      }
      if ("dispersion" %in% names(initvals)) {
         if (model == "negbin") {
            parameter_list$dispersion <- initvals$dispersion
         }
         else {
            warning("Initial values for dispersion is supplied, but model is not negbin. Will be ignored.")
         }
      }
      if ("sigma" %in% names(initvals)) {
         if (model == "gaussian") {
            parameter_list$sigma <- initvals$sigma
         }
         else {
            warning("Initial values for sigma is supplied, but model is not gaussian. Will be ignored.")
         }
      }
      if ("beta" %in% names(initvals)) {
         if (length(additional_covariates) != 0) {
            inamesb <- intersect(names(initvals$beta), names(parameter_list$beta))
            parameter_list$beta[inamesb] <- initvals$beta[inamesb]
         }
      }
      check_plist(parameter_list, all_teams = all_teams)
   }
   if (!is.null(fixed_params)) {
      stopifnot(is.list(fixed_params))
      if (any(!names(fixed_params) %in% c("attack", "defense", 
                                          "beta", "intercept", "sigma", "rho", "dispersion", 
                                          "gamma", "hfa"))) {
         stop("In fixed_params: Invalid parameter name.")
      }
      if ("attack" %in% names(fixed_params)) {
         fixed_attack_params <- names(parameter_list$attack) %in% 
            names(fixed_params$attack)
         parameter_list$attack <- parameter_list$attack[!fixed_attack_params]
      }
      if ("defense" %in% names(fixed_params)) {
         fixed_defence_params <- names(parameter_list$defense) %in% 
            names(fixed_params$defense)
         parameter_list$defense <- parameter_list$defense[!fixed_defence_params]
      }
      if (any(!names(fixed_params) %in% c("attack", "defense"))) {
         parameter_list[names(fixed_params)] <- NULL
         if ("dispersion" %in% names(fixed_params)) {
            fixed_params$dispersion <- log(fixed_params$dispersion)
            if (model == "poisson") {
               warning("Dispersion parameter is fixed, but model is Poisson. The dispersion parameter will not have an effect.")
            }
            else if (model == "gaussian") {
               warning("Dispersion parameter is fixed, but model is Gaussian The dispersion parameter will not have an effect. The related parameter for the Gaussian model is sigma.")
            }
         }
      }
   }
   parameter_vector <- unlist(parameter_list)
   start_time <- Sys.time()
   optim_res <- optimx::optimr(par = parameter_vector, fn = goalmodel:::negloglik, 
                               goals1 = goals1, goals2 = goals2, team1 = team1, team2 = team2, 
                               x1 = x1, x2 = x2, fixed_params = fixed_params, model = model, 
                               all_teams = all_teams, param_skeleton = parameter_list, 
                               weights = weights, method = optim_method, control = list(maxit = 250))
   end_time <- Sys.time()
   est_time <- difftime(end_time, start_time, units = "secs")
   if (optim_res$convergence != 0) {
      warning("Did not converge!! Parameter estimates are unreliable.")
   }
   parameter_list_est <- utils::relist(optim_res$par, parameter_list)
   if (length(parameter_list_est$defense) != 0) {
      parameter_list_est$defense <- c(sum(parameter_list_est$defense) * 
                                         -1, parameter_list_est$defense)
      names(parameter_list_est$defense)[1] <- all_teams[1]
   }
   if (length(parameter_list_est$attack) != 0) {
      parameter_list_est$attack <- c(sum(parameter_list_est$attack) * 
                                        -1, parameter_list_est$attack)
      names(parameter_list_est$attack)[1] <- all_teams[1]
   }
   loglikelihood <- optim_res$value * -1
   npar_est <- length(optim_res$par)
   npar_fixed <- length(unlist(fixed_params))
   aic <- npar_est * 2 - 2 * loglikelihood
   all_goals <- c(goals1, goals2)
   if (is.null(weights)) {
      mean_goals <- mean(all_goals)
   }
   else {
      mean_goals <- stats::weighted.mean(all_goals, w = rep(weights, 
                                                            2))
   }
   if (model == "poisson") {
      if (is.null(weights)) {
         loglikelihood_saturated <- sum(stats::dpois(all_goals, 
                                                     lambda = all_goals, log = TRUE))
         loglikelihood_null <- sum(stats::dpois(all_goals, 
                                                lambda = mean_goals, log = TRUE))
      }
      else {
         loglikelihood_saturated <- sum(stats::dpois(all_goals, 
                                                     lambda = all_goals, log = TRUE) * rep(weights, 
                                                                                           2))
         loglikelihood_null <- sum(stats::dpois(all_goals, 
                                                lambda = mean_goals, log = TRUE) * weights)
      }
   }
   else if (model == "negbin") {
      if (is.null(weights)) {
         dispersion0_tmp <- MASS::theta.ml(y = all_goals, 
                                           mu = mean_goals, limit = 1000)
         loglikelihood_saturated <- sum(stats::dnbinom(all_goals, 
                                                       mu = all_goals, size = Inf, log = TRUE))
         loglikelihood_null <- sum(stats::dnbinom(all_goals, 
                                                  mu = mean_goals, size = dispersion0_tmp, log = TRUE))
      }
      else {
         dispersion0_tmp <- MASS::theta.ml(y = all_goals, 
                                           mu = mean_goals, weights = rep(weights, 2), 
                                           limit = 1000)
         loglikelihood_saturated <- sum(stats::dnbinom(all_goals, 
                                                       mu = all_goals, size = Inf, log = TRUE) * rep(weights, 
                                                                                                     2))
         loglikelihood_null <- sum(stats::dnbinom(all_goals, 
                                                  mu = mean_goals, size = dispersion0_tmp, log = TRUE) * 
                                      rep(weights, 2))
      }
   }
   else if (model == "gaussian") {
      if (is.null(weights)) {
         sigma0_tmp <- stats::sd(all_goals)
         loglikelihood_saturated <- NA
         loglikelihood_null <- sum(stats::dnorm(all_goals, 
                                                mean = mean_goals, sd = sigma0_tmp, log = TRUE))
      }
      else {
         sigma0_tmp <- sqrt(sum(rep(weights, 2) * (all_goals - 
                                                      mean_goals)^2))
         loglikelihood_saturated <- NA
         loglikelihood_null <- sum(stats::dnorm(all_goals, 
                                                mean = mean_goals, sd = sigma0_tmp, log = TRUE) * 
                                      rep(weights, 2))
      }
   }
   else if (model == "ls") {
      loglikelihood_saturated <- NA
      loglikelihood_null <- NA
   }
   deviance <- 2 * (loglikelihood_saturated - loglikelihood)
   deviance_null <- 2 * (loglikelihood_saturated - loglikelihood_null)
   if (model != "ls") {
      r_squared <- 1 - (deviance/deviance_null)
   }
   else {
      r_squared <- 1 - ((loglikelihood * -1)/(sum((all_goals - 
                                                      mean_goals)^2)))
   }
   ngames <- length(goals1)
   parameter_list_all <- goalmodel:::fill_fixed_params(parameter_list_est, 
                                                       fixed_params = fixed_params)
   parameter_list_all$defense <- parameter_list_all$defense[order(names(parameter_list_all$defense))]
   parameter_list_all$attack <- parameter_list_all$attack[order(names(parameter_list_all$attack))]
   if ("dispersion" %in% names(parameter_list_all)) {
      parameter_list_all$dispersion <- exp(parameter_list_all$dispersion)
   }
   if ("sigma" %in% names(parameter_list_all)) {
      parameter_list_all$sigma <- exp(parameter_list_all$sigma)
   }
   maxgoal <- max(max(goals1), max(goals2))
   out <- list(parameters = parameter_list_all, loglikelihood = loglikelihood, 
               npar_est = npar_est, npar_fixed = npar_fixed, aic = aic, 
               r_squared = r_squared, all_teams = all_teams, ngames = ngames, 
               est_time = est_time, model = model, optim_res = optim_res, 
               fixed_params = fixed_params, maxgoal = maxgoal)
   class(out) <- "goalmodel"
   return(out)
}


#' fit_goalmodel
#' @export
fit_goalmodel <- function(self){
   
   goals <- self$process$data$outcomes
   predictors <- self$process$juice_x()
   
   form <- list(
      goals1 = as.numeric(goals[[1]]), 
      goals2 = as.numeric(goals[[2]]),
      team1 = as.numeric(predictors[[1]]), 
      team2 = as.numeric(predictors[[2]])
   )
   
   ### linear outcome only!
   if(is.null(self$params$type)){
      form$model <- "poisson"
   } else {
      if(self$params$type == "poisson") form$model <- "poisson"
      if(self$params$type == "dc") form$dc <- T
      if(self$params$type == "rs") form$rs <- T
      if(self$params$type == "negbin") form$model <- "negbin"
      if(self$params$type == "gaus") form$model <- "gaussian"
   }
   ### due to performance issues
   #do.call(deeplyr::goal_model, form)
   do.call(goalmodel::goalmodel, form)
}



#' get_estimates 
#' @export
get_estimates <- function(model){
   model$parameters[c("attack", "defense")] %>% 
      purrr::imap(~tibble::enframe(.x, name = "team_id", .y)) %>% 
      purrr::reduce(dplyr::full_join, by = "team_id")
}

#' get_probs 
#' @export
get_probs <- function(model, x_test){
   
   results <- goalmodel::predict_result(model, team1 = x_test$team1, team2 = x_test$team2, return_df = T)
   dplyr::bind_rows(
      results %>% dplyr::select(team_id = team1, p = p1),
      results %>% dplyr::select(team_id = team2, p = p2)
   )
   
}

#' get_expg_pos 
#' @export
get_expg <- function(model, x_test){
   goals <- goalmodel::predict_expg(model, team1 = x_test$team1, team2 = x_test$team2, return_df = T) 
   dplyr::bind_rows(
      goals %>% dplyr::select(team_id = team1, expg = expg1),
      goals %>% dplyr::select(team_id = team2, expg = expg2)
   )
}

#' get_probs_pos 
#' @export
get_probs_pos <- purrr::possibly(get_probs, NULL)

#' get_estimates_pos 
#' @export
get_estimates_pos <- purrr::possibly(get_estimates, NULL)

#' get_expg_pos 
#' @export
get_expg_pos <- purrr::possibly(get_expg, NULL)

suf_to_pref <- function(x){
   pref <- x %>% stringr::str_extract("_[a-z]+$") %>% stringr::str_remove("_")
   paste0(pref, "_", stringr::str_remove(x, "_[a-z]+$"))
}

#' predict_goalmodel
#' @export
predict_goalmodel <- function(self, new_data){
   
   teams <- self$process$stream(new_data) %>% 
      set_names(c("team1", "team2"))
   
   # ### Extract probabilities
   probs <- get_probs_pos(self$model, teams)
   
   ### Extract expected goals
   xg <- get_expg_pos(self$model, teams)
   
   ### Extract attack and defense param
   estimates <- get_estimates_pos(self$model)
   
   ### join
   list(probs, xg, estimates) %>%
      purrr::discard(is.null) %>%
      purrr::map(mutate_all, as.numeric) %>%
      purrr::reduce(dplyr::left_join, by = "team_id") %>%
      dplyr::mutate_if(is.numeric, round, 3) %>%
      dplyr::mutate(side = rep(c("local", "visitor"), n()/2)) %>%
      ### could be more efficient
      tidyr::pivot_wider(names_from = side, values_from = team_id:(ncol(.)-1))
}


# tidyr::pivot_longer(cols = team1:team2, names_to = "team", values_to = "team_id") %>%
# tidyr::pivot_longer(cols = p1:p2, names_to = "side", values_to = "prob") %>%
# dplyr::filter(
#   (str_detect(team, "1") & str_detect(side, "1")) |
#     (str_detect(team, "2") & str_detect(side, "2"))
# ) %>%
# dplyr::select(-team, -side)
# tidyr::pivot_longer(cols = team1:team2, names_to = "team", values_to = "team_id") %>%
# tidyr::pivot_longer(cols = expg1:expg2, names_to = "side", values_to = "expg") %>%
# dplyr::filter(
#   (str_detect(team, "1") & str_detect(side, "1")) |
#     (str_detect(team, "2") & str_detect(side, "2"))
# ) %>%
# dplyr::select(-team, -side)



#' load_rpart
#' @export
load_goalmodel <- function(path){
}

#' save_rpart
#' @export
save_goalmodel <- function(file, name, path){
}