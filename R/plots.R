#' @export
plot_scatter <- function(actual, pred){
  
  dplyr::tibble(pred, actual) %>%
    ggplot2::ggplot(aes(pred, actual)) +
    ggplot2::geom_jitter(alpha = .5) +
    ggplot2::geom_smooth(method = "lm", se = F) +
    ggplot2::geom_smooth(color = "red") +
    ggplot2::geom_rug(position = "jitter", alpha = .05) +
    ggplot2::theme_classic()
  
}

#' @export
plot_marginal <- function(actual, pred){
  
  dplyr::tibble(actual, pred) %>%
    dplyr::mutate(resid = pred - actual) %>%
    dplyr::select(actual, resid, pred) %>%
    dplyr::mutate(actual = scale(actual)[,1], pred = scale(pred)[,1]) %>%
    tidyr::gather(term, value) %>%
    ggplot2::ggplot(ggplot2::aes(x = value, y = term, fill = term)) +
    ggridges::geom_density_ridges(color = NA, alpha = .5) +
    ggplot2::geom_rug(aes(colour = term), alpha = .1) + 
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none")
  
  #geom_density(color = NA, alpha = .5)
}

#' @export
plot_linear <- function(preds){
  pred <- preds$pred
  target <- preds$target
  
  plot_marginal_pos <- purrr::possibly(plot_marginal, NULL)
  plot_scatter_pos <- purrr::possibly(plot_scatter, NULL)
  
  plots <- list()
  plots$marginal <- plot_marginal(target, pred)
  plots$scatter <- plot_scatter(target, pred)
  return(plots)
}

#' plot_roc
#' 
#' plot_roc(all_preds$target, all_preds$prob)
#' @export
plot_roc <- function(actual, prob){
  dplyr::tibble(actual, prob)  %>%
    ggplot2::ggplot(aes(d = actual, m = prob)) +
    plotROC::geom_roc() +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "gray50", linetype = "dashed") +
    ggplot2::theme_classic() +
    ggplot2::coord_equal()
}

#' plot_roc_multi
#' 
#' plot_roc(all_preds$target, all_preds$prob)
#' @export
plot_roc_multi <- function(actual, pred, prob){
  
  roc_dat <- tibble(actual, pred, prob) %>% 
    tidyr::unnest(prob) %>% 
    tidyr::gather(class, prob, -actual, -pred)
  
  roc_multi <- roc_dat %>% 
    split(.$class) %>%
    purrr::imap_dfr(~{
      var <- .y
      dat <- roc_dat %>%
        dplyr::mutate(actual = ifelse(class == var, 1, 0)) %>% 
        dplyr::select(actual, prob)
      
      roc <- pROC::roc(dat$actual, dat$prob)
      
      coords <- data.frame(
        sens = (1- rev(roc$sensitivities)),
        spec = rev(roc$specificities)
      ) %>% 
        dplyr::mutate(class = stringr::str_extract(.y, "\\d+"))
      return(coords)
    })
  
  roc_multi %>% 
    ggplot2::ggplot(ggplot2::aes(sens, spec, colour = class)) +
    ggplot2::geom_line() +
    ggplot2::ggtitle("Paired ROC") +
    #scale_x_reverse() +
    ggplot2::geom_abline(a = 0, b = 1) +
    ggplot2::geom_point() +
    ggplot2::theme_classic() +
    ggplot2::labs(x = "FPR (1 - Sensitivity)", y = "TPR (Specificity)")
}

#' plot_confusion
#' 
#' plot_confusion(all_preds$pred, all_preds$target)
#' @export
plot_confusion <- function(actual, pred){
  
  acc <- Metrics::accuracy(actual, pred) %>% round(3)
  lvls <- unique(c(actual, pred))
  dplyr::tibble(actual, pred) %>% 
    dplyr::mutate(actual = factor(actual, levels = lvls), pred = factor(pred, levels = lvls)) %>%
    dplyr::group_by(actual, pred, .drop = F) %>% 
    dplyr::tally() %>%
    dplyr::ungroup() %>% 
    dplyr::group_by(actual, .drop = F) %>% 
    dplyr::mutate(n_true = sum(n)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(perc_real = round(n/n_true * 100, 1)) %>%
    dplyr::mutate(label = n) %>%
    dplyr::mutate(text_color = pred == actual) %>%
    ggplot(ggplot2::aes(actual, pred, fill = n)) + 
    ggplot2::geom_tile(alpha = 0.8) + 
    ggplot2::scale_fill_gradient(low = "white", high = "black")+
    ggplot2::coord_equal() + 
    ggplot2::labs(x = "Real value y", y = "Predicted value y hat") +
    ggplot2::geom_text(aes(label = label, colour = text_color)) +
    ggplot2::scale_color_manual(values = c("black", "white")) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::guides(colour = F, fill = F) +
    ggplot2::ggtitle(glue::glue("Test Accurcay: {acc}"))
}

#' @export
plot_sankey <- function(actual, pred){
  
  lvls <- unique(c(actual, pred))

  flow_dat <- tibble(actual, pred) %>% 
    dplyr::mutate(actual = factor(actual, levels = lvls), pred = factor(pred, levels = lvls)) %>%
    dplyr::count(pred, actual, sort = T) %>% 
    dplyr::mutate_if(is.double, as.factor) %>%
    dplyr::mutate(true = pred == actual, label = paste(actual, true)) %>% 
    dplyr::mutate(id = 1:n()) %>%
    ggforce::gather_set_data(1:2) %>% 
    dplyr::mutate(x = factor(x, levels = c("actual", "pred")))
  
  ggplot2::ggplot(flow_dat, ggplot2::aes(x, id = id, split = y, value = n)) +
    ggforce::geom_parallel_sets(aes(fill = true), alpha = 0.3, axis.width = 0.1) +
    ggforce::geom_parallel_sets_axes(axis.width = 0.1) +
    ggforce::geom_parallel_sets_labels(colour = 'white') +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none",
          line = ggplot2::element_blank()
          #text = element_blank()
    ) +
    ggplot2::labs(x = "", y = "")
}

#' @export
plot_probability <- function(actual, prob){
  dplyr::tibble(actual, prob) %>% 
    dplyr::mutate(actual = as.factor(actual)) %>%
    ggplot2::ggplot(ggplot2::aes(x = prob, fill = actual)) +
    ggplot2::geom_density(alpha = .5, color = NA) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none")
}

#' @export
plot_probability_multi <- function(actual, pred, prob){
  dplyr::tibble(actual, pred, prob) %>% 
    tidyr::unnest(prob) %>% 
    tidyr::gather(class, prob, -actual, -pred) %>%
    dplyr::mutate(actual = as.factor(actual), pred = as.factor(pred)) %>%
    ggplot2::ggplot(ggplot2::aes(x = prob, y = actual, fill = pred)) +
    #geom_density(alpha = .2, color = NA) +
    ggridges::geom_density_ridges(alpha = .5, color = NA) + 
    ggplot2::theme_classic() 
  #theme(legend.position = "none")
}


#' @export
plot_survival <- function(actual, pred, prob){
  
  preds <- dplyr::tibble(actual, pred, prob) %>% 
    dplyr::mutate(true = pred == actual) %>% 
    dplyr::group_by(true) %>% 
    dplyr::mutate(prob_abs = abs(pred - prob)) %>% 
    dplyr::ungroup() 
  
  fit <- survival::survfit(survival::Surv(prob_abs, true) ~ actual, data = preds)
  
  survminer::ggsurvplot(
    fit, 
    data = preds, 
    conf.int = TRUE,          # Add confidence interval
    pval = TRUE,              # Add p-value
    risk.table = TRUE,        # Add risk table
    risk.table.col = "strata",# Risk table color by groups
    risk.table.height = .3, # Useful to change when you have multiple groups
    ggtheme = ggplot2::theme_classic(),      # Change ggplot2 theme
    conf.int.style = "step",  # customize style of confidence intervals
    surv.median.line = "hv"
  )
}

#' @export
plot_survival_multi <- function(actual, pred, prob){
  
  surv_dat <- dplyr::tibble(actual, pred, prob) %>% 
    tidyr::unnest(prob) %>% 
    tidyr::gather(class, prob, -actual, -pred) %>% 
    dplyr::mutate(true = pred == actual)
  
  fit <- survival::survfit(survival::Surv(prob, true) ~ actual, data = surv_dat)
  
  survminer::ggsurvplot(
    fit, 
    data = surv_dat, 
    conf.int = TRUE,          # Add confidence interval
    pval = TRUE,              # Add p-value
    risk.table = TRUE,        # Add risk table
    risk.table.col = "strata",# Risk table color by groups
    risk.table.height = .3, # Useful to change when you have multiple groups
    ggtheme = ggplot2::theme_classic(),      # Change ggplot2 theme
    conf.int.style = "step",  # customize style of confidence intervals
    surv.median.line = "hv"
  )
}


#' @export
plot_importance <- function(model, feature_names){
  importance <- xgboost::xgb.importance(feature_names = feature_names, model = model)
}

#' @export
plot_binary <- function(preds){
  
  target <- preds$target
  pred <- preds$pred
  prob <- preds$prob

  plot_roc_pos <- purrr::possibly(plot_roc, NULL)
  plot_confusion_pos <- purrr::possibly(plot_confusion, NULL)
  plot_sankey_pos <- purrr::possibly(plot_sankey, NULL)
  plot_probability_pos <- purrr::possibly(plot_probability, NULL)
  plot_survival_pos <- purrr::possibly(plot_survival, NULL)
  plot_importance_pos <- purrr::possibly(plot_importance, NULL)
  
  plots <- list()
  plots$confusion <- plot_confusion_pos(target, pred)
  plots$roc <- plot_roc_pos(target, prob)
  
  plots$sankey <- plot_sankey_pos(target, pred)
  plots$prob <- plot_probability_pos(target, prob)
  plots$surv <- plot_survival_pos(target, pred, prob)

  #plots$importance <- plot_importance_pos(self$model, colnames(private$x_test))
  
  return(plots)
}

#' @export
plot_categorical = function(preds){
  
  plots <- list(
    confusion = plot_confusion, 
    multi_roc = plot_roc_multi,
    sankey = plot_sankey,
    multi_prob = plot_probability_multi, 
    multi_surv = plot_survival_multi
  ) %>% 
    imap(~{
      f <- .x
      fp <- formals(f) %>% names
      f_pos <- purrr::possibly(f, NULL)
      do.call(f_pos, list(actual = preds$target, pred = preds$pred, prob = preds$prob)[fp])
    })

  return(plots)
}
