# Plant biodiversity as a driver of soil melanization (Cedar Creek e120)

Code and data to reproduce every figure and statistic in:

> Furey, G. N., Jelinski, N. A., LaBine, K., Effertz, W., & Tilman, D. (in review).
> Jenny's thought experiment revisited: Plant biodiversity as a driver of soil
> melanization. *Soil Science Society of America Journal*.

| | |
|---|---|
| Repository | https://github.com/jelinski-lab-pedology/M017-furey-e120-topsoil-development-saj2026060307 |
| Archived code (Zenodo, all versions) | https://doi.org/10.5281/zenodo.XXXXXXX |
| Data package (EDI) | https://doi.org/10.6073/pasta/XXXX |
| SOC time series (EDI) | Tilman, D. Soil carbon: Biodiversity II. knb-lter-cdr.423.10 |

## Quick start

Requirements: R ≥ 4.1 (developed and tested with R 4.3.2).

1. Download or clone this repository and open `M017-furey-e120-topsoil-development.Rproj`
   in RStudio (or start R with this folder as the working directory).
2. Install the package versions used for the paper:

   ```r
   install.packages("renv")
   renv::restore()
   ```

3. Run the pipeline:

   ```r
   targets::tar_make()
   ```

This rebuilds all prepared data, figures, and statistics in under a minute. All
paths are relative to the project root, so the folder can live anywhere on your
computer. The pipeline only needs to be run from the project root.

The [targets](https://books.ropensci.org/targets/) package rebuilds only what is
out of date. To inspect one result use `targets::tar_read()`, e.g.
`targets::tar_read(manuscript_stats)`; `targets::tar_visnetwork()` draws the
dependency graph.

## Where each result comes from

| Manuscript item | Output file | Code (`01-code/R/`) |
|---|---|---|
| Figure 1 (conceptual diagram) | not included (see Licence) | — |
| Figure 2, Section 3.1 | `02-output/figures/Fig2_soc_vs_lightness.pdf` | `fig2_soc_color.R` |
| Figure 3, Section 3.2.1 | `02-output/figures/Fig3_lightness_depth_richness.pdf` | `depth_profiles.R` |
| Figure 4, Section 3.2.2 | `02-output/figures/Fig4_lightness_depth_functional_group.pdf` | `depth_profiles.R` |
| Figure 5, Section 3.3 | `02-output/figures/Fig5_morphology.pdf` | `morphology.R` |
| Figure 6, Section 3.4 | `02-output/figures/Fig6_soc_change.pdf` | `soc_change.R` |
| Figure S1 | `02-output/figures/FigS1_cielab_covariation.pdf` | `supp_figures.R` |
| Figure S2 | `02-output/figures/FigS2_baseline_soc.pdf` | `supp_figures.R` |
| All statistics in the text | `02-output/tables/manuscript_statistics.csv` | `manuscript_stats.R` |
| Tables 1 and S1 | descriptive; no computation | — |

`manuscript_statistics.csv` lists every number quoted in the results, by
manuscript section, next to the text as printed (`manuscript` column), so the
two can be compared line by line.

## Repository layout

```
_targets.R                 pipeline definition (start here)
01-code/R/                 functions used by the pipeline, one file per analysis
01-code/data-prep/         one-off script that builds the colour data file
00-data/a-raw/             input data (identical to the published data package)
00-data/a-raw/edi/         cached copy of the EDI soil carbon time series
00-data/instrument-exports/ original Nix Pro exports and lab reports (provenance)
00-data/b-prepared/        prepared data written by the pipeline
02-output/                 figures and tables written by the pipeline
renv.lock, DESCRIPTION     R package versions and dependencies
```

## Data

| File (`00-data/a-raw/`) | Contents |
|---|---|
| `data_e120_quantitative_color.csv` | Nix Pro CIELAB readings, 20 plots × 21 depths (0–100 cm, 5-cm increments) × dry/moist, ~3 readings each |
| `data_e120_carbon_calibration_subset.csv` | Total carbon (dry combustion) for the 42-sample calibration subset (+ analytical duplicates) |
| `data_e120_morphological_athick_pdi.csv` | Plot-level A-horizon thickness (A + AB horizons) and Profile Darkness Index (all horizons, 0–100 cm, moist Munsell colours; Eq. 2) |
| `edi/knb-lter-cdr.423.10_soil_carbon.csv` | SOC time series for all Biodiversity II plots (EDI) |

**Soil carbon time series.** This dataset is published on EDI and is not
duplicated in our data package. The pipeline uses the cached copy in
`00-data/a-raw/edi/`; if that file is missing it is downloaded from EDI
(package `knb-lter-cdr.423`, revision 10). Either way the file's MD5 checksum is
verified, and the pipeline stops with instructions if it does not match.
Fourteen plots that were discontinued are excluded (`discontinued_plots` in
`01-code/R/data.R`). Plot treatments (species richness and functional group) are
taken from the planting information in this file.

**Quantitative colour.** `data_e120_quantitative_color.csv` is built from the
instrument exports by `01-code/data-prep/build_quantitative_color.R`. Three dry
samples were re-measured in January 2021 because the original readings were
judged too variable by the lab technician (plot 265 at 20 cm; plot 126 at 70 and
80 cm). The re-run readings replace the originals and are marked `lab re-run` in
the `Description` column. In the moist export for plot 166, two readings of the
95 cm sample were recorded with the 90 cm label ("90-2", "90-3"); the measurement
sequence (time stamps) and values identify them as 95 cm, so they are relabelled
"95-2" and "95-3" and marked `label corrected (recorded as 90-x)` in the
`Description` column. Analyses use the mean of all readings for each plot ×
moisture state × depth.

## Code style

Code follows the [tidyverse style guide](https://style.tidyverse.org/), applied
with `styler::style_dir("01-code")` and checked with `lintr::lint_dir("01-code")`
(settings in `.lintr`).

## Licence

Code: MIT (see `LICENSE`). Data: see the licence of the EDI data packages.
Figure 1 of the main manuscript reproduces Jenny (1958), © Ecological Society of America, used in the
article with permission; it is not covered by this repository's licence. It is not generated by code and
is not included in this repository; see the published article.

## Contact

Nic Jelinski (University of Minnesota - jeli0026 at umn.edu) and George Furey (NMBU - george.neil.furey at nmbu.no).
