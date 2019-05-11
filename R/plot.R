#' plot_roc
#' 
#' plot_roc(all_preds$target, all_preds$prob)
#' @export
plot_roc <- function(target, prob){
  tibble(target, prob) %>% 
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
plot_confusion <- function(pred, real){
  tibble(pred = as.factor(pred), real = as.factor(real)) %>%
    dplyr::count(pred, real) %>%
    dplyr::group_by(real) %>% 
    dplyr::mutate(n_real = sum(n)) %>% 
    ungroup() %>% 
    dplyr::mutate(perc_real = round(n/n_real * 100, 1)) %>%
    dplyr::mutate(label = n) %>%
    mutate(text_color = pred == real) %>%
    ggplot(aes(real, pred, fill = n)) + 
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
#' plot_binary
#' 
#' @export
plot_binary <- function(target, pred, prob){
  roc <- plot_roc(target, prob)
  con <- plot_confusion(pred, target)
  gridExtra::grid.arrange(roc, con, nrow = 1)
}