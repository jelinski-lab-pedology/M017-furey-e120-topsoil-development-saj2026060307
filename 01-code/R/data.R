# Data acquisition and preparation ---------------------------------------------

# Soil organic carbon time series (Biodiversity II, all plots) ------------------
# Tilman, D. Soil carbon: Biodiversity II: Effects of Plant Biodiversity on
# Population and Ecosystem Processes. Environmental Data Initiative,
# package knb-lter-cdr.423, revision 10. Published revisions on EDI are
# immutable, so this URL and checksum identify one exact file.
soc_edi_url <- paste0(
  "https://pasta.lternet.edu/package/data/eml/knb-lter-cdr/423/10/",
  "3c9c99e98512331828e785da9139e61b"
)
soc_edi_md5 <- "70e39c21e1cd45b026aac01e7f822592"

#' Return the path to the EDI SOC file, downloading it only if missing.
#'
#' The file is cached in the repository. If it is absent it is downloaded from
#' EDI; in either case its MD5 checksum must match `md5`, so a failed or
#' altered download stops the pipeline with an explicit error.
fetch_soc_edi <- function(path, url = soc_edi_url, md5 = soc_edi_md5) {
  if (!file.exists(path)) {
    message("Downloading SOC data from EDI: ", url)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    tmp <- tempfile(fileext = ".csv")
    ok <- tryCatch(
      utils::download.file(url, tmp, mode = "wb", quiet = TRUE) == 0,
      error = function(e) FALSE
    )
    if (!ok || unname(tools::md5sum(tmp)) != md5) {
      stop(
        "Download of the EDI soil carbon file failed or returned unexpected ",
        "content (MD5 mismatch). Download it manually from\n  ", url,
        "\nand save it as\n  ", path,
        call. = FALSE
      )
    }
    file.copy(tmp, path)
  }
  if (unname(tools::md5sum(path)) != md5) {
    stop(
      "MD5 checksum of ", path, " does not match EDI revision 10 (", md5, ").",
      call. = FALSE
    )
  }
  path
}

# Plots dropped from the experiment (not continued), excluded from all analyses.
discontinued_plots <- c(
  32, 73, 127, 151, 153, 161, 189, 221, 255, 256, 272, 296, 322, 333
)

#' Read and clean the EDI SOC time series (1994 and 2017 only).
clean_soc <- function(path) {
  utils::read.delim(path, na.strings = c("", " ", "NA"), strip.white = TRUE) |>
    dplyr::filter(Year %in% c(1994, 2017), !Plot %in% discontinued_plots) |>
    dplyr::transmute(
      year = Year,
      plot = Plot,
      depth = stringr::str_remove(Depth, " ?cm$"),
      num_sp = NumSp,
      c4 = C4,
      legume = Legume,
      soc_pct = pTotalCs
    )
}

#' Baseline (1994) SOC: mean of the four depth increments per plot.
soc_baseline <- function(soc) {
  soc |>
    dplyr::filter(year == 1994) |>
    dplyr::summarise(soc_1994 = mean(soc_pct, na.rm = TRUE), .by = plot)
}

#' Rate of change of SOC concentration at 0-20 cm, 1994-2017 (Eq. 3).
soc_rate <- function(soc) {
  soc |>
    dplyr::filter(depth == "0-20") |>
    dplyr::select(plot, year, soc_pct) |>
    tidyr::pivot_wider(names_from = year, values_from = soc_pct, names_prefix = "soc_") |>
    dplyr::mutate(rate = (log(soc_2017) - log(soc_1994)) / (2017 - 1994))
}

#' Treatments for the 20 study plots, derived from the EDI planting data.
#'
#' Species richness (0, 1, 16) and functional group: 0 = bare ground,
#' G = C4 grass monoculture, L = legume monoculture, GFL = 16-species mixture.
study_treatments <- function(soc, study_plots) {
  soc |>
    dplyr::filter(year == 1994, plot %in% study_plots) |>
    dplyr::distinct(plot, num_sp, c4, legume) |>
    dplyr::mutate(
      fg = dplyr::case_when(
        num_sp == 0 ~ "0",
        num_sp == 16 ~ "GFL",
        num_sp == 1 & c4 == 1 ~ "G",
        num_sp == 1 & legume == 1 ~ "L"
      ),
      num_sp = factor(num_sp, levels = richness_levels),
      fg = factor(fg, levels = fg_levels)
    ) |>
    dplyr::select(plot, num_sp, fg)
}

# Quantitative colour (Nix Pro) -------------------------------------------------

#' Read individual colour readings (data-package file).
read_color_readings <- function(path) {
  readr::read_csv(path, show_col_types = FALSE)
}

#' Average readings to one CIELAB value per plot x state x 5-cm depth.
#'
#' HEX and Munsell are derived from the averaged CIELAB coordinates.
color_depth_means <- function(readings, treatments, baseline) {
  means <- readings |>
    dplyr::summarise(
      dplyr::across(c(L, A, B), mean),
      n_readings = dplyr::n(),
      .by = c(Plot, State, Depth)
    ) |>
    dplyr::rename(plot = Plot, state = State, depth = Depth)
  means |>
    dplyr::bind_cols(lab_to_hex_munsell(means$L, means$A, means$B)) |>
    dplyr::left_join(treatments, by = "plot") |>
    dplyr::left_join(baseline, by = "plot") |>
    dplyr::arrange(num_sp, plot, state, depth)
}

# Carbon calibration subset -----------------------------------------------------

#' Total carbon for the 42-sample calibration subset, duplicates averaged,
#' joined to the colour of the same sample in both moisture states.
color_carbon <- function(path, color) {
  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::summarise(carbon_pct = mean(Carbon), .by = c(Plot, Depth)) |>
    dplyr::rename(plot = Plot, depth = Depth) |>
    dplyr::inner_join(color, by = c("plot", "depth"), relationship = "one-to-many")
}

# Morphology --------------------------------------------------------------------

#' Plot-level A-horizon thickness and PDI (data-package file).
#'
#' Both are computed from the field horizon descriptions (Eqs. 1 and 2):
#' A-horizon thickness is the summed thickness of all A and AB horizons;
#' PDI is the sum over all horizons from 0 to 100 cm of
#' thickness / (moist Munsell value x moist chroma + 1).
morphology_plots <- function(path, treatments, baseline) {
  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::transmute(
      plot = readr::parse_number(Plot),
      a_thickness = Athickness,
      pdi = PDI_Total
    ) |>
    dplyr::left_join(treatments, by = "plot") |>
    dplyr::left_join(baseline, by = "plot")
}
