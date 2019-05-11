#' plot_roc
#' 
#' plot_roc(all_preds$target, all_preds$prob)
#' @export
plot_roc <- function(preds){
  preds %>%
    ggplot(aes(d = target, m = prob)) +
    plotROC::geom_roc() +
    geom_abline(slope = 1, intercept = 0, color = "gray50", linetype = "dashed") +
    theme_classic() +
    coord_equal()
}
#' plot_confusion
#' 
#' plot_confusion(all_preds$pred, all_preds$target)
#' @export
plot_confusion <- function(preds){
  
  lvls <- unique(c(preds$pred, preds$target))
  
  preds %>% 
    mutate(pred = factor(pred, levels = lvls), target = factor(target, levels = lvls)) %>%
    dplyr::count(pred, target) %>%
    dplyr::group_by(target) %>% 
    dplyr::mutate(n_true = sum(n)) %>% 
    ungroup() %>% 
    dplyr::mutate(perc_real = round(n/n_true * 100, 1)) %>%
    dplyr::mutate(label = n) %>%
    mutate(text_color = pred == target) %>%
    ggplot(aes(target, pred, fill = n)) + 
    ggplot2::geom_tile(alpha = 0.8) + 
    scale_fill_gradient(low = "white", high = "black")+
    coord_equal() + 
    labs(x = "Real value y", y = "Predicted value y hat") +
    ggplot2::geom_text(aes(label = label, colour = text_color)) +
    scale_color_manual(values = c("black", "white")) +
    theme_classic() +
    theme(legend.position = "none") +
    guides(colour = F, fill = F) 
}

#' @export
plot_sankey <- function(preds){
  
  lvls <- unique(c(preds$pred, preds$target))

  flow_dat <- preds %>% 
    mutate(pred = factor(pred, levels = lvls), target = factor(target, levels = lvls)) %>%
    count(pred, target, sort = T) %>% 
    mutate_if(is.double, as.factor) %>%
    mutate(true = pred == target, label = paste(target, true)) %>% 
    mutate(id = 1:n()) %>%
    gather_set_data(1:2) %>% 
    mutate(x = factor(x, levels = c("target", "pred")))
  
  ggplot(flow_dat, aes(x, id = id, split = y, value = n)) +
    geom_parallel_sets(aes(fill = true), alpha = 0.3, axis.width = 0.1) +
    geom_parallel_sets_axes(axis.width = 0.1) +
    geom_parallel_sets_labels(colour = 'white') +
    theme_classic()
}

#' @export
plot_probability <- function(preds){
  lvls <- unique(c(preds$pred, preds$target))
  preds %>% 
    mutate(pred = factor(pred, levels = lvls), target = factor(target, levels = lvls)) %>%
    ggplot(aes(x = prob, fill = target)) +
    geom_density(alpha = .5, color = NA) +
    theme_classic()
}

#' @export
plot_survival <- function(preds){
  
  fit <- survfit(Surv(prob, target) ~ pred, data = preds)
  
  ggsurvplot(
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
plot_binary = function(preds){
  
  plot_roc_pos <- possibly(plot_roc, NULL)
  plot_confusion_pos <- possibly(plot_confusion, NULL)
  plot_sankey_pos <- possibly(plot_sankey, NULL)
  plot_probability_pos <- possibly(plot_probability, NULL)
  plot_survival_pos <- possibly(plot_survival, NULL)
  
  plot <- list()
  plot$roc <- plot_roc_pos(preds)
  plot$confusion <- plot_confusion_pos(preds)
  plot$sankey <- plot_sankey_pos(preds)
  plot$prob <- plot_probability_pos(preds)
  plot$surv <- plot_survival_pos(preds)
  return(plot)
}