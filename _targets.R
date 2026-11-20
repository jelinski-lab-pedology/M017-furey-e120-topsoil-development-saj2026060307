# Targets pipeline: Furey et al., "Jenny's thought experiment revisited:
# Plant biodiversity as a driver of soil melanization".
#
# Run from the project root (open the .Rproj in RStudio, or `cd` to this
# folder) with:   targets::tar_make()
# All paths below are relative to the project root.

library(targets)

tar_option_set(
  # Loaded for every target so that S3 methods (e.g. anova() and summary() for
  # nlme models, predict() for mgcv models) dispatch in any build order.
  packages = c("dplyr", "ggplot2", "nlme", "mgcv", "emmeans"),
  seed = 1 # fixes the horizontal jitter of points in Figure 5
)

tar_source("01-code/R")

raw <- "00-data/a-raw"
prepared <- "00-data/b-prepared"
figures <- "02-output/figures"
tables <- "02-output/tables"

list(
  # Inputs --------------------------------------------------------------------------
  tar_target(soc_file, fetch_soc_edi(file.path(raw, "edi/knb-lter-cdr.423.10_soil_carbon.csv")), format = "file"),
  tar_target(color_file, file.path(raw, "data_e120_quantitative_color.csv"), format = "file"),
  tar_target(carbon_file, file.path(raw, "data_e120_carbon_calibration_subset.csv"), format = "file"),
  tar_target(morph_file, file.path(raw, "data_e120_morphological_athick_pdi.csv"), format = "file"),

  # Prepared data -------------------------------------------------------------------
  tar_target(soc, clean_soc(soc_file)),
  tar_target(baseline, soc_baseline(soc)),
  tar_target(color_readings, read_color_readings(color_file)),
  tar_target(treatments, study_treatments(soc, unique(color_readings$Plot))),
  tar_target(color, color_depth_means(color_readings, treatments, baseline)),
  tar_target(calibration, color_carbon(carbon_file, color)),
  tar_target(morph, morphology_plots(morph_file, treatments, baseline)),
  tar_target(soc_change, soc_change_data(soc_rate(soc), treatments)),
  tar_target(
    prepared_files,
    c(
      write_csv_file(soc, file.path(prepared, "soc_1994_2017.csv")),
      write_csv_file(color, file.path(prepared, "color_depth_means.csv")),
      write_csv_file(calibration, file.path(prepared, "color_carbon_calibration.csv")),
      write_csv_file(morph, file.path(prepared, "morphology_plots.csv")),
      write_csv_file(soc_change, file.path(prepared, "soc_change_0_20cm.csv"))
    ),
    format = "file"
  ),

  # Section 3.1 / Figure 2: SOC vs L* -------------------------------------------------
  tar_target(soc_color_models, fit_soc_color(calibration)),
  tar_target(
    fig2,
    plot_fig2(calibration, soc_color_models, file.path(figures, "Fig2_soc_vs_lightness.pdf")),
    format = "file"
  ),

  # Section 3.2 / Figures 3-4: colour with depth -------------------------------------
  tar_target(gam_richness, predict_depth_gams(color, "num_sp")),
  tar_target(gam_fg, predict_depth_gams(color, "fg")),
  tar_target(lme_richness, fit_depth_lme(color, "num_sp")),
  tar_target(lme_fg, fit_depth_lme(color, "fg")),
  tar_target(lme_richness_ct, depth_lme_contrasts(lme_richness, color, "num_sp")),
  tar_target(lme_fg_ct, depth_lme_contrasts(lme_fg, color, "fg")),
  tar_target(
    fig3,
    plot_depth_profiles(
      color, gam_richness, "num_sp", richness_colours, richness_shapes,
      file.path(figures, "Fig3_lightness_depth_richness.pdf"),
      label_y = list(
        dry = list("0" = 43, "1" = 41, "16" = 37),
        moist = list("0" = 24.5, "1" = 23.4, "16" = 21.3)
      )
    ),
    format = "file"
  ),
  tar_target(
    fig4,
    plot_depth_profiles(
      color, gam_fg, "fg", fg_colours, fg_shapes,
      file.path(figures, "Fig4_lightness_depth_functional_group.pdf"),
      legend_position = c(0.85, 0.69), legend_key_size = 0.5, label_size = 8
    ),
    format = "file"
  ),

  # Section 3.3 / Figure 5: morphology -----------------------------------------------
  tar_target(a_fit, fit_morphology(morph, "a_thickness")),
  tar_target(pdi_fit, fit_morphology(morph, "pdi")),
  tar_target(fig5, plot_fig5(morph, pdi_fit, file.path(figures, "Fig5_morphology.pdf")), format = "file"),

  # Section 3.4 / Figure 6: SOC change -------------------------------------------------
  tar_target(soc_change_fit, fit_soc_change(soc_change)),
  tar_target(fig6, plot_fig6(soc_change, file.path(figures, "Fig6_soc_change.pdf")), format = "file"),

  # Supplementary figures ------------------------------------------------------------
  tar_target(figS1, plot_figS1(color, file.path(figures, "FigS1_cielab_covariation.pdf")), format = "file"),
  tar_target(figS2, plot_figS2(soc, treatments, file.path(figures, "FigS2_baseline_soc.pdf")), format = "file"),

  # Every statistic reported in the manuscript ----------------------------------------
  tar_target(
    manuscript_stats,
    build_manuscript_stats(
      soc, soc_color_models, lme_richness, lme_richness_ct, lme_fg, lme_fg_ct,
      morph, a_fit, pdi_fit, soc_change_fit, color
    )
  ),
  tar_target(
    manuscript_stats_file,
    write_csv_file(manuscript_stats, file.path(tables, "manuscript_statistics.csv")),
    format = "file"
  )
)
