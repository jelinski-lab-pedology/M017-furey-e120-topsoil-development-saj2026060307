# Shared helpers ---------------------------------------------------------------

#' Standard error of the mean.
se_mean <- function(x) {
  sqrt(var(x) / length(x))
}

#' Blank out every nth label in an axis label vector.
#'
#' With `inverse = FALSE` the 1st, (nth + 1)th, ... labels are blanked;
#' with `inverse = TRUE` only those labels are kept.
every_nth <- function(x, nth, inverse = FALSE) {
  blank <- seq_along(x) %% nth == 1
  if (inverse) blank <- !blank
  x[blank] <- ""
  x
}

#' Convert CIELAB to sRGB HEX codes and nearest Munsell chips.
#'
#' @param L,A,B Numeric vectors of CIELAB coordinates.
#' @return A tibble with columns `hex` and `munsell`.
lab_to_hex_munsell <- function(L, A, B) {
  srgb <- grDevices::convertColor(cbind(L, A, B), from = "Lab", to = "sRGB")
  munsell <- aqp::rgb2munsell(srgb, colorSpace = "CIE2000", nClosest = 1)
  tibble::tibble(
    hex = grDevices::rgb(srgb),
    munsell = paste0(munsell$hue, " ", munsell$value, "/", munsell$chroma)
  )
}

#' Save a ggplot / cowplot object and return the path (for file targets).
save_figure <- function(plot, path, width, height) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(path, plot = plot, width = width, height = height, units = "in")
  path
}

#' Write a data frame to CSV and return the path (for file targets).
write_csv_file <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(x, path, na = "")
  path
}

# Treatment levels, colours, and shapes ----------------------------------------
# Colours follow RColorBrewer "Dark2" as used in the published figures.

richness_levels <- c("0", "1", "16")
richness_colours <- c("0" = "#1b9e77", "1" = "#d95f02", "16" = "#7570b3")
richness_shapes <- c("0" = 21, "1" = 22, "16" = 23)

fg_levels <- c("0", "G", "L", "GFL")
fg_colours <- c("0" = "#1b9e77", "G" = "#d95f02", "L" = "#e7298a", "GFL" = "#7570b3")
fg_shapes <- c("0" = 21, "G" = 22, "L" = 24, "GFL" = 23)
