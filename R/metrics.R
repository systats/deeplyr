#' list_metrics[["linear"]]
#' @export
list_metrics <- list(
  linear = list(
    se = Metrics::se,
    mse = Metrics::mse,
    rmse = Metrics::rmse,
    ae = Metrics::ae,
    mae = Metrics::mae,
    ape = Metrics::ape,
    mape = Metrics::mape,
    smape = Metrics::smape,
    sle = Metrics::sle,
    msle = Metrics::msle,
    rmsle = Metrics::rmsle,
    rse = Metrics::rse,
    rrse = Metrics::rrse,
    rae = Metrics::rae
  ),
  binary = list(
    precision = Metrics::precision,
    recall = Metrics::recall,
    fbeta_score = Metrics::fbeta_score,
    accuracy = Metrics::accuracy
  ),
  loss = list(
    ll = Metrics::ll,
    logloss = Metrics::logLoss,
    auc = Metrics::auc
  ),
  categorical = list(
    ce = Metrics::ce,
    accuracy = Metrics::accuracy,
    f1 = Metrics::f1
  )
)

#' @export
browse_metrics <- function(){
  browseURL("https://github.com/mfrasco/Metrics")
}