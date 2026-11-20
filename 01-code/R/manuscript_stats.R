# Every statistic quoted in the manuscript, in one table ------------------------------
#
# `manuscript` holds the text as printed in the 2026-06-27 version of the main
# manuscript, for side-by-side checking. Update it when the text changes.

# Formatting helpers --------------------------------------------------------------------

fmt_p <- function(p) {
  ifelse(p < 0.001, "p < 0.001", paste("p =", formatC(p, format = "f", digits = 3)))
}

fmt_F <- function(f_value, df1, df2, p) {
  sprintf("F(%s,%s) = %.2f, %s", df1, df2, f_value, fmt_p(p))
}

fmt_est <- function(est, se, p = NULL, digits = 2) {
  out <- sprintf(paste0("%.", digits, "f +- %.", digits, "f"), est, se)
  if (!is.null(p)) out <- paste0(out, ", ", fmt_p(p))
  out
}

# Result extractors -----------------------------------------------------------------------

#' R2 and overall F test of a simple linear regression.
lm_fit_text <- function(model) {
  s <- summary(model)
  f <- s$fstatistic
  p <- stats::pf(f[["value"]], f[["numdf"]], f[["dendf"]], lower.tail = FALSE)
  sprintf("R2 = %.2f, %s", s$r.squared, fmt_F(f[["value"]], f[["numdf"]], f[["dendf"]], p))
}

#' F test of one term from an anova()/car::Anova() table of an lm.
ols_term_text <- function(aov, term) {
  fmt_F(aov[term, "F value"], aov[term, "Df"], aov["Residuals", "Df"], aov[term, "Pr(>F)"])
}

#' F test of one term from a marginal anova() of an lme.
lme_term_text <- function(model, term) {
  aov <- anova(model, type = "marginal")
  fmt_F(aov[term, "F-value"], aov[term, "numDF"], aov[term, "denDF"], aov[term, "p-value"])
}

lme_coef_text <- function(model, term) {
  tt <- summary(model)$tTable
  fmt_est(tt[term, "Value"], tt[term, "Std.Error"], tt[term, "p-value"], digits = 1)
}

lm_coef_text <- function(model, term) {
  cf <- summary(model)$coefficients
  fmt_est(cf[term, "Estimate"], cf[term, "Std. Error"], cf[term, "Pr(>|t|)"], digits = 1)
}

#' A contrast from an emmeans pairs() table; `flip` reverses its sign.
contrast_text <- function(ct, label, flip = FALSE) {
  r <- ct[ct$contrast == label, ]
  fmt_est(if (flip) -r$estimate else r$estimate, r$SE, r$p.value)
}

#' All contrasts with p < 0.05, listed with their depth.
sig_contrasts_text <- function(ct) {
  sig <- dplyr::filter(ct, p.value < 0.05)
  paste(
    sprintf("%s cm: %s, %s", sig$depth_f, sig$contrast, fmt_est(sig$estimate, sig$SE, sig$p.value)),
    collapse = "; "
  )
}

#' Tukey p value for a pair of functional groups, in either order.
fg_pair_p_text <- function(ct, a, b) {
  fmt_p(ct$p.value[ct$contrast %in% c(paste(a, "-", b), paste(b, "-", a))])
}

#' Treatment mean tested against zero.
vs_zero_text <- function(vz, level) {
  r <- vz[vz$fg == level, ]
  fmt_est(r$emmean, r$SE, r$p.value, digits = 3)
}

# Table -------------------------------------------------------------------------------------

build_manuscript_stats <- function(soc, soc_color_models, lme_richness, lme_richness_ct,
                                   lme_fg, lme_fg_ct, morph, a_fit, pdi_fit, soc_change_fit,
                                   color) {
  plots <- soc |>
    dplyr::filter(year == 1994) |>
    dplyr::distinct(plot, num_sp) |>
    dplyr::count(num_sp)
  rc <- lme_richness_ct
  sc <- soc_change_fit

  rows <- list(
    # Methods
    c(
      "2.2", "Plots per richness level (0, 1, 2, 4, 8, 16); total",
      paste0(paste(plots$n, collapse = ", "), "; ", sum(plots$n)),
      "4, 32, 28, 29, 30 and 35; 158"
    ),
    # 3.1 SOC-colour relationship
    c(
      "3.1", "SOC ~ L* dry: R2, F", lm_fit_text(soc_color_models$dry),
      "R2 = 0.67, F(1,40) = 80.67, p < 0.001"
    ),
    c(
      "3.1", "SOC ~ L* moist: R2, F", lm_fit_text(soc_color_models$moist),
      "R2 = 0.80, F(1,40) = 162.3, p < 0.001"
    ),
    # 3.2.1 species richness
    c(
      "3.2.1", "Richness LME, dry: baseline SOC beta", lme_coef_text(lme_richness$dry, "soc_1994"),
      "beta = -42.6 +- 8.2, p < 0.001"
    ),
    c(
      "3.2.1", "Richness LME, moist: baseline SOC beta", lme_coef_text(lme_richness$moist, "soc_1994"),
      "beta = -24.3 +- 5.1, p < 0.001"
    ),
    c(
      "3.2.1", "Richness LME, dry: depth x richness", lme_term_text(lme_richness$dry, "num_sp:depth_f"),
      "F(40,340) = 1.49, p = 0.034"
    ),
    c(
      "3.2.1", "Richness LME, moist: richness", lme_term_text(lme_richness$moist, "num_sp"),
      "F(2,16) = 4.23, p = 0.034"
    ),
    c(
      "3.2.1", "Richness LME, moist: depth", lme_term_text(lme_richness$moist, "depth_f"),
      "F(20,380) = 48.1, p < 0.001"
    ),
    c(
      "3.2.1", "Richness LME, dry: significant Tukey contrasts (all depths)", sig_contrasts_text(rc$dry),
      "only at 0 cm: 0 - 16: 5.80 +- 1.70, p = 0.009; 1 - 16: 4.36 +- 1.38, p = 0.016"
    ),
    c("3.2.1", "Richness LME, moist: 0 - 1", contrast_text(rc$moist, "num_sp0 - num_sp1"), NA),
    c(
      "3.2.1", "Richness LME, moist: 0 - 16", contrast_text(rc$moist, "num_sp0 - num_sp16"),
      "reported as '0 vs. 1': 1.87 +- 0.74, p = 0.055"
    ),
    c(
      "3.2.1", "Richness LME, moist: 1 - 16", contrast_text(rc$moist, "num_sp1 - num_sp16"),
      "1.39 +- 0.60, p = 0.079"
    ),
    # 3.2.2 functional groups
    c(
      "3.2.2", "Functional-group LME, dry: baseline SOC", lme_term_text(lme_fg$dry, "soc_1994"),
      "F(1,15) = 11.1, p = 0.005"
    ),
    c(
      "3.2.2", "Functional-group LME, moist: baseline SOC", lme_term_text(lme_fg$moist, "soc_1994"),
      "F(1,15) = 7.2, p = 0.017"
    ),
    c(
      "3.2.2", "Functional-group LME, dry: depth x group", lme_term_text(lme_fg$dry, "fg:depth_f"),
      "F(60,320) = 1.60, p = 0.006"
    ),
    c(
      "3.2.2", "Functional-group LME, dry: significant Tukey contrasts (all depths)",
      sig_contrasts_text(lme_fg_ct$dry), "only the surface 0 vs 16-species difference"
    ),
    c(
      "3.2.2", "Functional-group LME, moist: depth", lme_term_text(lme_fg$moist, "depth_f"),
      "F(20,380) = 48.3, p < 0.001"
    ),
    c(
      "3.2.2", "Functional-group LME, moist: group", lme_term_text(lme_fg$moist, "fg"),
      "F(3,15) = 2.96, p = 0.066"
    ),
    # 3.3 morphology
    c(
      "3.3", "A-horizon thickness: mean +- sd (cm)",
      fmt_est(mean(morph$a_thickness), sd(morph$a_thickness), digits = 0), "48 +- 16"
    ),
    c(
      "3.3", "A-horizon thickness: richness", ols_term_text(a_fit$anova, "num_sp"),
      "F(2,16) = 0.63, p = 0.55"
    ),
    c(
      "3.3", "A-horizon thickness: baseline SOC", ols_term_text(a_fit$anova, "soc_1994"),
      "F(1,16) = 4.49, p = 0.050"
    ),
    c("3.3", "PDI: richness", ols_term_text(pdi_fit$anova, "num_sp"), "F(2,16) = 6.61, p = 0.008"),
    c("3.3", "PDI: baseline SOC", ols_term_text(pdi_fit$anova, "soc_1994"), "F(1,16) = 14.65, p = 0.001"),
    c(
      "3.3", "PDI: baseline SOC beta", lm_coef_text(pdi_fit$model, "soc_1994"),
      "beta = 28.5 +- 7.5, p = 0.001"
    ),
    c(
      "3.3", "PDI: 16 - 0 (Tukey)", contrast_text(pdi_fit$contrasts, "num_sp0 - num_sp16", flip = TRUE),
      "3.39 +- 1.08, p = 0.016"
    ),
    c(
      "3.3", "PDI: 16 - 1 (Tukey)", contrast_text(pdi_fit$contrasts, "num_sp1 - num_sp16", flip = TRUE),
      "2.56 +- 0.87, p = 0.024"
    ),
    c(
      "3.3", "PDI: 1 - 0 (Tukey)", contrast_text(pdi_fit$contrasts, "num_sp0 - num_sp1", flip = TRUE),
      "not different"
    ),
    # 3.4 SOC change
    c("3.4", "SOC rate ~ functional group", ols_term_text(sc$anova, "fg"), "p < 0.001"),
    c("3.4", "SOC rate Tukey: 0 - G", fg_pair_p_text(sc$contrasts, "0", "G"), "p = 0.014"),
    c("3.4", "SOC rate Tukey: 0 - GFL", fg_pair_p_text(sc$contrasts, "0", "GFL"), "p = 0.0001"),
    c("3.4", "SOC rate Tukey: 0 - L", fg_pair_p_text(sc$contrasts, "0", "L"), "p = 0.07"),
    c("3.4", "SOC rate Tukey: G - L", fg_pair_p_text(sc$contrasts, "G", "L"), "p = 0.84"),
    c("3.4", "SOC rate Tukey: G - GFL", fg_pair_p_text(sc$contrasts, "G", "GFL"), "p = 0.32"),
    c("3.4", "SOC rate Tukey: L - GFL", fg_pair_p_text(sc$contrasts, "L", "GFL"), "p = 0.064"),
    c(
      "3.4", "SOC rate mean vs 0 (FDR): bare (0)", vs_zero_text(sc$vs_zero, "0"),
      "-0.019 +- 0.004, p < 0.001"
    ),
    c("3.4", "SOC rate mean vs 0 (FDR): grass (G)", vs_zero_text(sc$vs_zero, "G"), "p = 0.34"),
    c("3.4", "SOC rate mean vs 0 (FDR): legume (L)", vs_zero_text(sc$vs_zero, "L"), "p = 0.83"),
    c(
      "3.4", "SOC rate mean vs 0 (FDR): 16-species (GFL)", vs_zero_text(sc$vs_zero, "GFL"),
      "0.015 +- 0.003, p < 0.001"
    ),
    # Supplement
    c(
      "Fig S1", "Number of depth-mean colour measurements", as.character(nrow(color)),
      "n = 420 wet and dry measurements"
    )
  )
  out <- as.data.frame(do.call(rbind, rows))
  names(out) <- c("section", "item", "result", "manuscript")
  tibble::as_tibble(out)
}
