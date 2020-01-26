browse_metrics <- function(){
  browseURL("https://github.com/mfrasco/Metrics")
}

#' model_eval
#' @export
model_eval <- function(self, y){
  if(self$meta$task == "linear") {
    eval_linear(self$preds, y)
  } else if(self$meta$task == "binary"){
    eval_binary(self$preds, y)
  } else if(self$meta$task == "multi"){
    eval_multi(self$preds, y)
  }
}

#' compute_metrics
#' @export
compute_metrics <- function(metrics, actual, pred){
  metrics %>%
    purrr::map(possibly, otherwise = NULL) %>%
    purrr::map_dfc(~mean(.x(actual, pred)))
}

#' mean_pred
#' @export
mean_pred <- function(x, y) mean(y)

#' mean_actual
#' @export
mean_actual <- function(x, y) mean(x)

#' sd_pred
#' @export
sd_pred <- function(x, y) stats::sd(y)

#' sd_actual
#' @export
sd_actual <- function(x, y) stats::sd(x)

#' eval_linear
#' @export
eval_linear <- function(preds, y){
  list(
    mse = Metrics::mse,
    rmse = Metrics::rmse,
    mae = Metrics::mae,
    mape = Metrics::mape,
    mean = mean_pred, 
    mean_actual = mean_actual,
    sd = sd_pred,
    sd_actual = sd_actual
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
    compute_metrics(preds[[y]], preds$pred)
}

#' eval_binary
#' @export
eval_binary <- function(preds, y){
  pred <- list(
    accuracy = Metrics::accuracy,
    precision = Metrics::precision,
    recall = Metrics::recall,
    fbeta_score = Metrics::fbeta_score,
    ce = Metrics::ce,
    f1 = Metrics::f1,
    mean = mean_pred, 
    mean_actual = mean_actual,
    sd = sd_pred,
    sd_actual = sd_actual
  ) %>%
    compute_metrics(as.numeric(as.character(preds[[y]])), as.numeric(as.character(preds$pred)))
  
  prob <- list(
    ll = Metrics::ll,
    logloss = Metrics::logLoss,
    auc = Metrics::auc
  ) %>%
    compute_metrics(as.numeric(as.character(preds[[y]])), preds$prob) 
  
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
eval_multi <- function(preds, y){
  pred <- list(
    accuracy = Metrics::accuracy,
    precision = Metrics::precision,
    recall = Metrics::recall,
    fbeta_score = Metrics::fbeta_score,
    ce = Metrics::ce,
    mean = mean_pred, 
    mean_actual = mean_actual,
    sd = sd_pred,
    sd_actual = sd_actual
    #f1 = Metrics::f1
  ) %>%
    compute_metrics(as.numeric(as.character(preds[[y]])), as.numeric(as.character(preds$pred)))
  
  prob <- list(rps = rank_prob_score) %>%
    purrr::map(possibly, otherwise = NULL) %>%
    purrr::map_dfc(~mean(.x(as.numeric(as.character(preds[[y]])), preds %>% select(contains("prob")) %>% as.matrix), na.rm = T))
  
  c(pred, prob)
}