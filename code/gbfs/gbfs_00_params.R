source("code/00_data_load.R")
source("code/00_helpers.R")

# Raw data aggregation --------------------------------------------------------------------------
BASE_FOLDER <- "data/collection/gbfs"
OUTPUT_FOLDER <- "data/analysis/gbfs"
DATE_START <- 20260926
DATE_END <- 20261026

PERIODS_FULL <- tibble::tribble(
    ~day_type, ~hour_start, ~hour_end, # [hour_start, hour_end[
    "all", 0L, 24L,
    "week", 0L, 24L,
    "weekend", 0L, 24L
)

PERIODS_HOURLY <- tidyr::expand_grid(
    day_type = c("all", "week", "weekend"),
    hour_start = 0L:23L
) |>
    dplyr::mutate(hour_end = hour_start + 1L)

PERIODS <- dplyr::bind_rows(PERIODS_FULL, PERIODS_HOURLY)

REGIONS_GBFS <- c(
    # Bird
    "bird_almada", "bird_cascais", "bird_lisboa",
    # Lime
    "lime_almada", "lime_lisboa", "lime_oeiras"
    # Bolt MISSING
)

REGIONS <- c(REGIONS_GBFS, "emel_gira")
