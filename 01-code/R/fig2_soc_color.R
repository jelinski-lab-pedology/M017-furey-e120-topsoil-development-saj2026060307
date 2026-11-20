# Figure 2 / Section 3.1: SOC versus soil lightness (L*) -------------------------

#' OLS of ln(SOC) on ln(L*) for each moisture state (n = 42 samples each).
fit_soc_color <- function(color_carbon) {
  purrr::map(
    c(dry = "dry", moist = "moist"),
    \(s) lm(log(carbon_pct) ~ log(L), data = dplyr::filter(color_carbon, state == s))
  )
}

soc_color_panel <- function(data, model, x_label, l_grid, x_breaks, label_x) {
  fit <- tibble::tibble(L = l_grid, carbon_pct = exp(predict(model, data.frame(L = l_grid))))
  r2 <- round(summary(model)$r.squared, 2)
  y_breaks <- seq(0, 1.8, by = 0.1)
  ggplot2::ggplot(data, ggplot2::aes(x = L, y = carbon_pct, fill = hex)) +
    ggplot2::geom_point(size = 5, shape = 21) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_line(data = fit, ggplot2::aes(x = L, y = carbon_pct), inherit.aes = FALSE) +
    ggplot2::annotate("text", x = label_x, y = 1.5, label = paste("R^2==", r2), parse = TRUE, size = 5) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(c(0, 0.05)), limits = c(0, NA),
      breaks = y_breaks, labels = every_nth(y_breaks, 2, inverse = TRUE)
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(c(0.03, 0.02)),
      breaks = x_breaks, labels = every_nth(x_breaks, 2)
    ) +
    ggplot2::labs(x = x_label, y = "% Soil Carbon") +
    ggplot2::theme_grey(base_size = 10) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
}

plot_fig2 <- function(color_carbon, models, path) {
  moist <- soc_color_panel(
    dplyr::filter(color_carbon, state == "moist"), models$moist, "L* (Wet Soil)",
    l_grid = seq(19, 37, by = 0.2), x_breaks = 18:38, label_x = 30
  )
  dry <- soc_color_panel(
    dplyr::filter(color_carbon, state == "dry"), models$dry, "L* (Dry Soil)",
    l_grid = seq(34, 54, by = 0.2), x_breaks = 33:55, label_x = 47
  )
  fig <- cowplot::plot_grid(moist, dry, labels = c("(a)", "(b)"), label_size = 8, ncol = 1)
  save_figure(fig, path, width = 3.5, height = 5)
}
