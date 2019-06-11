#' list_metrics[["linear"]]
#' @export
list_metrics <- list(
  linear = list(
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
  ),
  binary = list(
    precision = Metrics::precision,
    recall = Metrics::recall,
    fbeta_score = Metrics::fbeta_score,
    accuracy = Metrics::accuracy
  ),
  prob = list(
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