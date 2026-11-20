# Figure 6 / Section 3.4: rate of change of SOC (0-20 cm, 1994-2017) -----------------

soc_change_data <- function(rate, treatments) {
  dplyr::inner_join(rate, treatments, by = "plot")
}

#' OLS of rate on functional group; Tukey pairs; each group mean tested
#' against zero (t-tests, FDR-adjusted).
fit_soc_change <- function(dat) {
  model <- lm(rate ~ fg, data = dat)
  em <- emmeans::emmeans(model, ~fg)
  list(
    model = model,
    anova = stats::anova(model),
    contrasts = tibble::as_tibble(as.data.frame(pairs(em))),
    vs_zero = tibble::as_tibble(as.data.frame(emmeans::test(em, null = 0, adjust = "fdr")))
  )
}

plot_fig6 <- function(dat, path) {
  y_label <- expression(paste(
    "Soil C Accumulation Rate 0-20 cm", "(", "yr"^-1, ") ",
    "ln", "(", frac("C"[2017], "C"[1994]), ")", frac(1, 23),
  ))
  fig <- ggplot2::ggplot(dat, ggplot2::aes(x = fg, y = rate)) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::stat_summary(fun.data = ggplot2::mean_se, fill = "grey", colour = "black", geom = "bar", width = 0.8) +
    ggplot2::stat_summary(fun.data = ggplot2::mean_se, colour = "black", geom = "errorbar", width = 0.5) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Functional Group Composition", y = y_label) +
    ggplot2::theme_bw(base_size = 10)
  save_figure(fig, path, width = 3.5, height = 4)
}
