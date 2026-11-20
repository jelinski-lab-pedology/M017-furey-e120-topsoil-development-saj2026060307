# Figures 3 and 4 / Section 3.2: soil colour with depth by treatment --------------
#
# `trt` is the treatment column: "num_sp" (species richness; Figure 3) or
# "fg" (functional group; Figure 4).

model_data <- function(color, trt, state_) {
  color |>
    dplyr::filter(state == state_) |>
    dplyr::mutate(plot = factor(plot), depth_f = factor(depth))
}

# GAMs (depth-profile shape) ------------------------------------------------------

#' Fit one GAM per moisture state x CIELAB coordinate and predict 0-100 cm.
#'
#' value ~ s(depth, by = trt) + trt + s(plot, bs = "re"). Moist L* uses a
#' scaled-t family (heavy-tailed residuals); all others are Gaussian.
#' Predictions exclude the plot random effect.
predict_depth_gams <- function(color, trt) {
  combos <- tidyr::expand_grid(state = c("dry", "moist"), variable = c("L", "A", "B"))
  purrr::pmap(combos, function(state, variable) {
    dat <- model_data(color, trt, state)
    dat$value <- dat[[variable]]
    form <- stats::as.formula(paste0("value ~ s(depth, by = ", trt, ") + ", trt, " + s(plot, bs = 're')"))
    family <- if (state == "moist" && variable == "L") mgcv::scat(link = "identity") else stats::gaussian()
    model <- mgcv::gam(form, family = family, data = dat)
    grid <- tidyr::expand_grid(depth = 0:100, level = factor(levels(dat[[trt]]), levels = levels(dat[[trt]])))
    names(grid)[2] <- trt
    grid$plot <- factor(levels(dat$plot)[1], levels = levels(dat$plot))
    pred <- predict(model, newdata = grid, se.fit = TRUE, exclude = "s(plot)")
    grid |>
      dplyr::select(-plot) |>
      dplyr::mutate(state = state, variable = variable, fit = as.numeric(pred$fit), se = as.numeric(pred$se.fit))
  }) |>
    purrr::list_rbind()
}

# LMEs (discrete depths) ------------------------------------------------------------

#' Final LME models for L* by moisture state.
#'
#' Dry:   L ~ trt * depth + SOC_1994, random intercept for plot.
#' Moist: L ~ trt + depth + SOC_1994, random intercept for plot, with
#'        separate variance multipliers for treatment and depth (varComb).
#' Both fit by REML.
fit_depth_lme <- function(color, trt) {
  dry <- model_data(color, trt, "dry")
  moist <- model_data(color, trt, "moist")
  control <- list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000)
  weights <- nlme::varComb(
    nlme::varIdent(form = stats::as.formula(paste("~ 1 |", trt))),
    nlme::varIdent(form = ~ 1 | depth_f)
  )
  list(
    dry = do.call(nlme::lme, list(
      fixed = stats::as.formula(paste("L ~", trt, "* depth_f + soc_1994")),
      random = ~ 1 | plot, method = "REML", data = dry
    )),
    moist = do.call(nlme::lme, list(
      fixed = stats::as.formula(paste("L ~", trt, "+ depth_f + soc_1994")),
      random = ~ 1 | plot, weights = weights, control = control,
      method = "REML", data = moist
    ))
  )
}

#' Tukey contrasts between treatments: by depth (dry) and averaged over
#' depth (moist). Denominator df = plots - treatment levels - 1 (covariate),
#' i.e. the between-plot error df.
depth_lme_contrasts <- function(models, color, trt) {
  df <- dplyr::n_distinct(color$plot) - nlevels(color[[trt]]) - 1
  dry <- emmeans::emmeans(models$dry, stats::as.formula(paste("~", trt, "| depth_f")), df = df)
  moist <- emmeans::emmeans(models$moist, stats::as.formula(paste("~", trt)), df = df)
  list(
    dry = tibble::as_tibble(as.data.frame(pairs(dry))),
    moist = tibble::as_tibble(as.data.frame(pairs(moist)))
  )
}

# Figure ------------------------------------------------------------------------------

depth_profile_theme <- function(plot, colours) {
  plot +
    ggplot2::xlab("Depth (cm)") +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(legend.position = "none", axis.title = ggplot2::element_text(size = 10)) +
    ggplot2::scale_colour_manual(values = colours, name = "NumSp") +
    ggplot2::scale_x_reverse(breaks = seq(0, 100, by = 20), expand = ggplot2::expansion(c(0.02, 0.05)))
}

l_axis <- function(state, panel_limits) {
  if (state == "dry") {
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(0.01), breaks = seq(35, 55, by = 5), limits = panel_limits)
  } else {
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(0.01), breaks = seq(20, 35, by = 3), limits = c(20, 35))
  }
}

#' GAM-predicted L* profile; points filled with the predicted soil colour.
gam_panel <- function(pred, trt, state_, colours, label_y) {
  dat <- pred |> dplyr::filter(state == state_)
  ggplot2::ggplot(dat, ggplot2::aes(x = depth, y = L, fill = hex)) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::geom_point(size = 3, shape = 21, colour = dat$hex) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_line(ggplot2::aes(y = L + L_se, colour = .data[[trt]], group = .data[[trt]]), alpha = 0.8) +
    ggplot2::geom_line(ggplot2::aes(y = L - L_se, colour = .data[[trt]], group = .data[[trt]]), alpha = 0.8) +
    ggplot2::coord_flip() +
    purrr::map(names(colours), \(lev) {
      ggplot2::annotate("text", x = -7, y = label_y[[lev]], label = lev, colour = colours[[lev]], size = 5)
    })
}

#' Treatment mean +- SE of L* at each depth.
mean_panel <- function(color, trt, state_, colours, shapes) {
  dat <- color |>
    dplyr::filter(state == state_) |>
    dplyr::summarise(se = se_mean(L), L = mean(L), .by = c(depth, dplyr::all_of(trt)))
  aes_trt <- ggplot2::aes(colour = .data[[trt]], shape = .data[[trt]], fill = .data[[trt]])
  ggplot2::ggplot(dat, ggplot2::aes(x = depth, y = L)) +
    aes_trt +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::geom_errorbar(ggplot2::aes(ymax = L + se, ymin = L - se)) +
    ggplot2::geom_point(colour = "black") +
    ggplot2::coord_flip() +
    ggplot2::scale_shape_manual(values = shapes, name = "NumSp") +
    ggplot2::scale_fill_manual(values = colours, name = "NumSp")
}

#' Four-panel depth-profile figure (Figure 3 or 4).
#'
#' @param label_y Optional named list (dry, moist) of label positions for the
#'   treatment labels above panels a and b; defaults to the predicted L* at
#'   0 cm.
plot_depth_profiles <- function(color, gam_pred, trt, colours, shapes, path,
                                label_y = NULL, legend_position = c(0.85, 0.7),
                                legend_key_size = NULL, label_size = 10) {
  pred <- gam_pred |>
    dplyr::filter(variable %in% c("L", "A", "B")) |>
    tidyr::pivot_wider(names_from = variable, values_from = c(fit, se)) |>
    dplyr::rename(L = fit_L, A = fit_A, B = fit_B, L_se = se_L)
  pred$hex <- lab_to_hex_munsell(pred$L, pred$A, pred$B)$hex
  if (is.null(label_y)) {
    surface <- dplyr::filter(pred, depth == 0)
    label_y <- purrr::map(c(dry = "dry", moist = "moist"), \(s) {
      x <- dplyr::filter(surface, state == s)
      stats::setNames(as.list(x$L), as.character(x[[trt]]))
    })
  }
  legend_theme <- ggplot2::theme(
    axis.title.y = ggplot2::element_blank(),
    legend.position = legend_position,
    legend.box.background = ggplot2::element_rect(colour = "black", linewidth = 1, linetype = "solid"),
    legend.box.margin = ggplot2::margin(1, 1, 1, 1)
  )
  if (!is.null(legend_key_size)) {
    legend_theme <- legend_theme + ggplot2::theme(legend.key.size = ggplot2::unit(legend_key_size, "cm"))
  }
  a <- depth_profile_theme(gam_panel(pred, trt, "dry", colours, label_y$dry), colours) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
    l_axis("dry", c(35, 57.5))
  b <- depth_profile_theme(gam_panel(pred, trt, "moist", colours, label_y$moist), colours) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank()) +
    l_axis("moist")
  c <- depth_profile_theme(mean_panel(color, trt, "dry", colours, shapes), colours) +
    ggplot2::ylab("L* (Dry Soil)") +
    l_axis("dry", c(35, 57))
  d <- depth_profile_theme(mean_panel(color, trt, "moist", colours, shapes), colours) +
    ggplot2::ylab("L* (Wet Soil)") +
    legend_theme +
    l_axis("moist")
  fig <- cowplot::plot_grid(
    a, b, c, d,
    align = "hv", axis = "rt", rel_widths = c(1, 1), rel_heights = c(1, 1.1),
    label_size = label_size, labels = c("(a)", "(b)", "(c)", "(d)")
  )
  save_figure(fig, path, width = 7, height = 6)
}
