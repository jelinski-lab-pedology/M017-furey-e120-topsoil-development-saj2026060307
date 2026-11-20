# Supplementary figures ------------------------------------------------------------

#' Figure S1: pairwise relationships among L*, a*, b* (all 840 depth means).
plot_figS1 <- function(color, path) {
  panel <- function(x, y, x_label, y_label) {
    ggplot2::ggplot(color, ggplot2::aes(x = .data[[x]], y = .data[[y]], shape = state, fill = hex)) +
      ggplot2::geom_point(size = 3) +
      ggplot2::scale_shape_manual(
        values = c(dry = 21, moist = 23), labels = c(dry = "dry", moist = "wet"),
        name = "Wet or Dry"
      ) +
      ggplot2::scale_fill_identity() +
      ggplot2::labs(x = x_label, y = y_label) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom")
  }
  fig <- cowplot::plot_grid(
    panel("L", "B", "L*", "b*"),
    panel("L", "A", "L*", "a*"),
    panel("B", "A", "b*", "a*"),
    ncol = 3
  )
  save_figure(fig, path, width = 8, height = 5)
}

#' Figure S2: baseline (1994) SOC by depth for the 20 study plots.
plot_figS2 <- function(soc, treatments, path) {
  dat <- soc |>
    dplyr::filter(year == 1994) |>
    dplyr::inner_join(treatments, by = "plot") |>
    dplyr::mutate(depth = forcats::fct_rev(factor(depth)))
  fig <- ggplot2::ggplot(dat, ggplot2::aes(x = depth, y = soc_pct, colour = fg, group = plot)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::coord_flip() +
    ggplot2::scale_colour_manual(values = fg_colours, name = "Functional Group") +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(c(0.01, 0.05))) +
    ggplot2::labs(x = "Depth (cm)", y = "Baseline % Soil Carbon 1994") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
  save_figure(fig, path, width = 6, height = 5)
}
