#' @export
plot_scatter <- function(actual, pred){
  
  tibble(actual, pred) %>%
    ggplot(aes(actual, pred)) +
    geom_jitter(alpha = .5) +
    geom_smooth(method = "lm", se = F) +
    geom_smooth(color = "red") +
    geom_rug(position = "jitter", alpha = .05) +
    theme_classic()
  
}
#' @export
plot_marginal <- function(actual, pred){
  
  tibble(actual, pred) %>%
    mutate(resid = pred - actual) %>%
    select(actual, resid, pred) %>%
    mutate(actual = scale(actual)[,1], pred = scale(pred)[,1]) %>%
    gather(term, value) %>%
    ggplot(aes(x = value, y = term, fill = term)) +
    ggridges::geom_density_ridges(color = NA, alpha = .5) +
    geom_rug(aes(colour = term), alpha = .1) + 
    theme_classic() +
    theme(legend.position = "none")
  
  #geom_density(color = NA, alpha = .5)
}

#' @export
plot_linear <- function(preds){
  pred <- preds$pred
  target <- preds$target
  
  plot_marginal_pos <- possibly(plot_marginal, NULL)
  plot_scatter_pos <- possibly(plot_scatter, NULL)
  
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
  tibble(actual, prob)  %>%
    ggplot(aes(d = actual, m = prob)) +
    plotROC::geom_roc() +
    geom_abline(slope = 1, intercept = 0, color = "gray50", linetype = "dashed") +
    theme_classic() +
    coord_equal()
}

#' plot_roc_multi
#' 
#' plot_roc(all_preds$target, all_preds$prob)
#' @export
plot_roc_multi <- function(actual, pred, prob){
  
  roc_dat <- tibble(actual, pred, prob) %>% 
    unnest(prob) %>% 
    gather(class, prob, -actual, -pred)
  
  roc_multi <- roc_dat %>% 
    split(.$class) %>%
    imap_dfr(~{
      var <- .y
      dat <- roc_dat %>%
        mutate(actual = ifelse(class == var, 1, 0)) %>% 
        select(actual, prob)
      
      roc <- pROC::roc(dat$actual, dat$prob)
      
      coords <- data.frame(
        sens = (1- rev(roc$sensitivities)),
        spec = rev(roc$specificities)
      ) %>% 
        mutate(class = str_extract(.y, "\\d+"))
      return(coords)
    })
  
  roc_multi %>% 
    ggplot(aes(sens, spec, colour = class)) +
    geom_line() +
    ggtitle("Paired ROC") +
    #scale_x_reverse() +
    geom_abline(a = 0, b = 1) +
    geom_point() +
    theme_classic() +
    labs(x = "false_positive_fraction (1 - Sensitivity)", y = "true_positive_fraction (Specificity)")
}

#' plot_confusion
#' 
#' plot_confusion(all_preds$pred, all_preds$target)
#' @export
plot_confusion <- function(actual, pred){
  
  acc <- Metrics::accuracy(actual, pred) %>% round(3)
  lvls <- unique(c(actual, pred))
  tibble(actual, pred) %>% 
    mutate(actual = factor(actual, levels = lvls), pred = factor(pred, levels = lvls)) %>%
    dplyr::group_by(actual, pred, .drop = F) %>% 
    tally() %>%
    ungroup %>% 
    dplyr::group_by(actual, .drop = F) %>% 
    dplyr::mutate(n_true = sum(n)) %>% 
    ungroup() %>% 
    dplyr::mutate(perc_real = round(n/n_true * 100, 1)) %>%
    dplyr::mutate(label = n) %>%
    mutate(text_color = pred == actual) %>%
    ggplot(aes(actual, pred, fill = n)) + 
    ggplot2::geom_tile(alpha = 0.8) + 
    scale_fill_gradient(low = "white", high = "black")+
    coord_equal() + 
    labs(x = "Real value y", y = "Predicted value y hat") +
    ggplot2::geom_text(aes(label = label, colour = text_color)) +
    scale_color_manual(values = c("red", "green")) +
    theme_classic() +
    theme(legend.position = "none") +
    guides(colour = F, fill = F) +
    ggtitle(glue::glue("Test Accurcay: {acc}"))
}

#' @export
plot_sankey <- function(actual, pred){
  
  lvls <- unique(c(actual, pred))

  flow_dat <- tibble(actual, pred) %>% 
    mutate(actual = factor(actual, levels = lvls), pred = factor(pred, levels = lvls)) %>%
    count(pred, actual, sort = T) %>% 
    mutate_if(is.double, as.factor) %>%
    mutate(true = pred == actual, label = paste(actual, true)) %>% 
    mutate(id = 1:n()) %>%
    ggforce::gather_set_data(1:2) %>% 
    mutate(x = factor(x, levels = c("actual", "pred")))
  
  ggplot(flow_dat, aes(x, id = id, split = y, value = n)) +
    geom_parallel_sets(aes(fill = true), alpha = 0.3, axis.width = 0.1) +
    geom_parallel_sets_axes(axis.width = 0.1) +
    geom_parallel_sets_labels(colour = 'white') +
    theme_classic() +
    theme(legend.position = "none",
          line = element_blank()
          #text = element_blank()
    ) +
    labs(x = "", y = "")
}

#' @export
plot_probability <- function(actual, prob){
  tibble(actual, prob) %>% 
    mutate(actual = as.factor(actual)) %>%
    ggplot(aes(x = prob, fill = actual)) +
    geom_density(alpha = .5, color = NA) +
    theme_classic() +
    theme(legend.position = "none")
}

#' @export
plot_probability_multi <- function(actual, pred, prob){
  tibble(actual, pred, prob) %>% 
    unnest(prob) %>% 
    gather(class, prob, -actual, -pred) %>%
    mutate(actual = as.factor(actual), pred = as.factor(pred)) %>%
    ggplot(aes(x = prob, y = actual, fill = pred)) +
    #geom_density(alpha = .2, color = NA) +
    ggridges::geom_density_ridges(alpha = .5, color = NA) + 
    theme_classic() 
  #theme(legend.position = "none")
}


#' @export
plot_survival <- function(actual, pred, prob){
  
  preds <- tibble(actual, pred, prob) %>% 
    mutate(true = pred == actual) %>% 
    group_by(true) %>% 
    mutate(prob_abs = abs(pred - prob)) %>% 
    ungroup 
  
  fit <- survival::survfit(survival::Surv(prob_abs, true) ~ actual, data = preds)
  
  survminer::ggsurvplot(
    fit, 
    data = preds, 
    conf.int = TRUE,          # Add confidence interval
    pval = TRUE,              # Add p-value
    risk.table = TRUE,        # Add risk table
    risk.table.col = "strata",# Risk table color by groups
    risk.table.height = .3, # Useful to change when you have multiple groups
    ggtheme = theme_classic(),      # Change ggplot2 theme
    conf.int.style = "step",  # customize style of confidence intervals
    surv.median.line = "hv"
  )
}

#' @export
plot_survival_multi <- function(actual, pred, prob){
  
  surv_dat <- tibble(actual, pred, prob) %>% 
    unnest(prob) %>% 
    gather(class, prob, -actual, -pred) %>% 
    mutate(true = pred == actual)
  
  fit <- survival::survfit(survival::Surv(prob, true) ~ actual, data = surv_dat)
  
  survminer::ggsurvplot(
    fit, 
    data = surv_dat, 
    conf.int = TRUE,          # Add confidence interval
    pval = TRUE,              # Add p-value
    risk.table = TRUE,        # Add risk table
    risk.table.col = "strata",# Risk table color by groups
    risk.table.height = .3, # Useful to change when you have multiple groups
    ggtheme = theme_classic(),      # Change ggplot2 theme
    conf.int.style = "step",  # customize style of confidence intervals
    surv.median.line = "hv"
  )
}


#' @export
plot_importance <- function(model, feature_names){
  importance <- xgb.importance(feature_names = feature_names, model = model)
}

#' @export
plot_binary <- function(preds){
  
  target <- preds$target
  pred <- preds$pred
  prob <- preds$prob

  plot_roc_pos <- possibly(plot_roc, NULL)
  plot_confusion_pos <- possibly(plot_confusion, NULL)
  plot_sankey_pos <- possibly(plot_sankey, NULL)
  plot_probability_pos <- possibly(plot_probability, NULL)
  plot_survival_pos <- possibly(plot_survival, NULL)
  plot_importance_pos <- possibly(plot_importance, NULL)
  
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
