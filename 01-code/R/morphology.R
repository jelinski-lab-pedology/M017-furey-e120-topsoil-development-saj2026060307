# Figure 5 / Section 3.3: A-horizon thickness and Profile Darkness Index -------------

#' OLS: response ~ species richness + baseline SOC, Type III ANOVA, Tukey.
fit_morphology <- function(morph, response) {
  model <- lm(stats::as.formula(paste(response, "~ num_sp + soc_1994")), data = morph)
  list(
    model = model,
    anova = car::Anova(model, type = "III"),
    contrasts = tibble::as_tibble(as.data.frame(pairs(emmeans::emmeans(model, ~num_sp))))
  )
}

# Figure ------------------------------------------------------------------------------

#' Bar (mean +- SE) and points by species richness.
#'
#' @param contrasts Tukey table; pairs with p < 0.05 get a bracket and "*".
#' @param bracket_y Heights of the brackets, in order of increasing span.
morphology_panel <- function(morph, response, y_label, contrasts = NULL, bracket_y = NULL) {
  p <- ggplot2::ggplot(morph, ggplot2::aes(x = num_sp, y = .data[[response]])) +
    ggplot2::stat_summary(fun.data = ggplot2::mean_se, fill = "grey", colour = "black", geom = "bar", width = 0.8) +
    ggplot2::stat_summary(fun.data = ggplot2::mean_se, colour = "black", geom = "errorbar", width = 0.5) +
    ggplot2::geom_jitter(alpha = 0.2, height = 0, width = 0.1) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(c(0, 0.05))) +
    ggplot2::labs(x = "Number of Species", y = y_label)
  if (is.null(contrasts)) {
    return(p)
  }
  sig <- contrasts |>
    dplyr::filter(p.value < 0.05) |>
    dplyr::mutate(
      from = match(sub("num_sp", "", sub(" - .*", "", contrast)), richness_levels),
      to = match(sub(".* - num_sp", "", contrast), richness_levels),
      span = abs(to - from)
    ) |>
    dplyr::arrange(span, from)
  if (nrow(sig) == 0) {
    return(p)
  }
  if (is.null(bracket_y)) {
    top <- max(morph[[response]])
    bracket_y <- top * (1.05 + 0.07 * (seq_len(nrow(sig)) - 1))
  }
  for (i in seq_len(nrow(sig))) {
    p <- p +
      ggplot2::annotate(
        "segment",
        x = sig$from[i], xend = sig$to[i], y = bracket_y[i], yend = bracket_y[i], linewidth = 0.5
      ) +
      ggplot2::annotate("text", x = (sig$from[i] + sig$to[i]) / 2, y = bracket_y[i], label = "*", size = 6)
  }
  p
}

plot_fig5 <- function(morph, pdi_fit, path) {
  fig <- cowplot::plot_grid(
    morphology_panel(morph, "a_thickness", "A horizon Thickness (cm)"),
    morphology_panel(morph, "pdi", "Profile Darkness Index", pdi_fit$contrasts, bracket_y = c(16, 17)),
    labels = c("(a)", "(b)"), label_size = 8, ncol = 1
  )
  save_figure(fig, path, width = 3.5, height = 5)
}
