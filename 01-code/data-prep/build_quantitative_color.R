# Build data_e120_quantitative_color.csv from the Nix Pro instrument exports.
#
# This is a one-off data-preparation script. It is NOT part of the targets
# pipeline: its output is the file submitted to the Cedar Creek / EDI data
# package, and the pipeline reads that file.
#
# Steps
#   1. Read the 40 per-plot exports in 00-data/instrument-exports/NixData/
#      (20 plots x {dry, moist}).
#   2. Lab re-run substitution: three dry samples were re-measured on
#      2021-01-04 because the lab technician judged the original readings too
#      variable (plot 265, 20 cm; plot 126, 70 and 80 cm). All original
#      readings at those depths are replaced by all re-run readings
#      (00-data/instrument-exports/reruns-colordata_e120soilmorph.csv).
#      Replaced rows are marked "lab re-run" in the Description column.
#   3. Label correction: in the moist export for plot 166, the 2nd and 3rd
#      readings of the 95 cm sample were recorded as "90-2" and "90-3". The
#      measurement sequence (time stamps fall between 95-1 and 100-1) and the
#      values identify them as 95 cm. They are relabelled "95-2" and "95-3" and
#      marked "label corrected (recorded as 90-x)" in the Description column.
#   4. Add Plot, State, Depth, and Rep (reading number) columns, convert CMYK
#      percentages to fractions, and write one long table.
#
# Run from the project root:  Rscript 01-code/data-prep/build_quantitative_color.R

library(dplyr)
library(purrr)
library(readr)
library(stringr)

export_dir <- "00-data/instrument-exports/NixData"
rerun_file <- "00-data/instrument-exports/reruns-colordata_e120soilmorph.csv"
out_file <- "00-data/a-raw/data_e120_quantitative_color.csv"

# Samples replaced by lab re-runs (dry state only).
reruns_used <- tibble(Plot = c(265, 126, 126), Depth = c(20, 70, 80))

# Mislabelled readings: identified by plot, state, recorded label, and time stamp.
relabels <- tibble(
  Plot = c(166, 166),
  State = c("moist", "moist"),
  `Color Name` = c("90-2", "90-3"),
  `Time Stamp` = c("2021-08-04 15:10:01", "2021-08-04 15:10:13"),
  new_name = c("95-2", "95-3")
)

# Column names as exported by the Nix software. Duplicated names (L, B, C, Y)
# are made unique as L.1, B.1, C.1, Y.1. A few exports were re-saved with
# R-mangled headers, so names are assigned by position after checking width.
nix_columns <- make.unique(
  c(
    "Color Name", "Description", "Time Stamp", "L", "A", "B", "L", "C", "H",
    "X", "Y", "Z", "R", "G", "B", "HEX", "C", "M", "Y", "K",
    "ACES-R", "ACES-G", "ACES-B", "ACEScg-R", "ACEScg-G", "ACEScg-B",
    "Lin.sRGB-R", "Lin.sRGB-G", "Lin.sRGB-B"
  ),
  sep = "."
)

read_nix_export <- function(path) {
  dat <- read.csv(path, check.names = FALSE, colClasses = "character")
  dat <- dat[, names(dat) != "", drop = FALSE] # trailing comma in some exports
  stopifnot(ncol(dat) == length(nix_columns))
  names(dat) <- nix_columns
  dat
}

add_ids <- function(dat, plot, state) {
  id <- str_split_fixed(dat$`Color Name`, "-", 2)
  tibble(
    Plot = plot,
    State = state,
    Depth = as.numeric(id[, 1]),
    Rep = as.integer(id[, 2])
  ) |>
    bind_cols(dat)
}

files <- list.files(export_dir, pattern = "_colordata\\.csv$", full.names = TRUE)

readings <- map(files, function(path) {
  parts <- str_match(basename(path), "^(\\d+)_(dry|moist)_colordata\\.csv$")
  add_ids(read_nix_export(path), plot = as.numeric(parts[2]), state = parts[3])
}) |>
  list_rbind()

reruns <- read_nix_export(rerun_file)
reruns <- add_ids(reruns, plot = as.numeric(reruns$Description), state = "dry") |>
  mutate(Description = "lab re-run") |>
  semi_join(reruns_used, by = c("Plot", "Depth"))

stopifnot(nrow(reruns) == 9)

readings <- readings |>
  anti_join(mutate(reruns_used, State = "dry"), by = c("Plot", "State", "Depth")) |>
  bind_rows(reruns)

readings <- readings |>
  left_join(relabels, by = c("Plot", "State", "Color Name", "Time Stamp")) |>
  mutate(
    Description = if_else(
      is.na(new_name), Description, paste0("label corrected (recorded as ", `Color Name`, ")")
    ),
    Depth = if_else(is.na(new_name), Depth, as.numeric(str_extract(new_name, "^\\d+"))),
    Rep = if_else(is.na(new_name), Rep, as.integer(str_extract(new_name, "\\d+$"))),
    `Color Name` = coalesce(new_name, `Color Name`)
  ) |>
  select(-new_name)

stopifnot(sum(str_detect(readings$Description, "label corrected"), na.rm = TRUE) == nrow(relabels))

# CMYK is exported as percentages ("47%"); store as fractions (0.47).
cmyk <- c("C.1", "M", "Y.1", "K")
readings <- readings |>
  mutate(across(all_of(cmyk), \(x) as.numeric(str_remove(x, "%")) / 100)) |>
  mutate(across(L:last_col() & !all_of(c("HEX", cmyk)), as.numeric)) |>
  arrange(Plot, State, Depth, Rep, `Time Stamp`)

# Every plot x state x depth (20 x 2 x 21) must have at least one reading.
stopifnot(
  n_distinct(readings$Plot) == 20,
  nrow(distinct(readings, Plot, State, Depth)) == 20 * 2 * 21
)

write_csv(readings, out_file, na = "")
message("Wrote ", nrow(readings), " readings to ", out_file)
