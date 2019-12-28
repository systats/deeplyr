#' browse_metrics
#' @export
browse_metrics <- function(){
   browseURL("https://github.com/mfrasco/Metrics")
}

#' eval_model
#' @export
eval_model <- function(self){
   if(self$task == "linear") {
      eval_linear(self$preds)
   } else if(self$task == "binary"){
      eval_binary(self$preds)
   } else if(self$task == "multi"){
      eval_multi(self$preds)
   }
}

#' compute_metrics
#' @export
compute_metrics <- function(metrics, actual, pred){
   metrics %>%
      purrr::map(possibly, otherwise = NULL) %>%
      purrr::map_dfc(~mean(.x(actual, pred)))
}

#' eval_linear
#' @export
eval_linear <- function(preds){
   list(
      mse = Metrics::mse,
      rmse = Metrics::rmse,
      mae = Metrics::mae,
      mape = Metrics::mape
      # se = Metrics::se,
      # ae = Metrics::ae,
      # ape = Metrics::ape,
      # smape = Metrics::smape
      # sle = Metrics::sle,
      # msle = Metrics::msle,
      # rmsle = Metrics::rmsle,
      # rse = Metrics::rse,
      # rrse = Metrics::rrse,
      # rae = Metrics::rae
   ) %>%
   compute_metrics(preds$target, preds$pred)
}

#' eval_binary
#' @export
eval_binary <- function(preds){
   pred <- list(
      accuracy = Metrics::accuracy,
      precision = Metrics::precision,
      recall = Metrics::recall,
      fbeta_score = Metrics::fbeta_score,
      ce = Metrics::ce,
      f1 = Metrics::f1
   ) %>%
   compute_metrics(preds$target, preds$pred)
   
   prob <- list(
      ll = Metrics::ll,
      logloss = Metrics::logLoss,
      auc = Metrics::auc
   ) %>%
   compute_metrics(preds$target, preds$prob1) 
   
   c(pred, prob)
}


#' rank_prob_score
#' https://opisthokonta.net/?p=1333
#' @export
rank_prob_score <- function(target, probs){
   ncat <- ncol(probs)
   npred <- nrow(probs)
   rps <- numeric(npred)
   
   for (rr in 1:npred){
      obsvec <- rep(0, ncat)
      obsvec[target[rr]] <- 1
      cumulative <- 0
      for (i in 1:ncat){
         cumulative <- cumulative + (sum(probs[rr,1:i]) - sum(obsvec[1:i]))^2
      }
      rps[rr] <- (1/(ncat-1))*cumulative
   }
   return(rps)
}

#' eval_multi
#' @export
eval_multi <- function(preds){
   pred <- list(
      accuracy = Metrics::accuracy,
      precision = Metrics::precision,
      recall = Metrics::recall,
      fbeta_score = Metrics::fbeta_score,
      ce = Metrics::ce,
      f1 = Metrics::f1
   ) %>%
   compute_metrics(preds$target, preds$pred)
   
   target <- preds$target
   if(any(target %in% 0)) target <- target + 1
   
   prob <- list(rps = rank_prob_score) %>%
      purrr::map(possibly, otherwise = NULL) %>%
      purrr::map_dfc(~mean(.x(target, preds %>% select(contains("prob")) %>% as.matrix), na.rm = T))
   
   c(pred, prob)
}
