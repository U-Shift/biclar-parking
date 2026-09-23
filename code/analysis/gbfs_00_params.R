source("code/00_data_load.R")
source("code/00_helpers.R")

# Raw data aggregation --------------------------------------------------------------------------
BASE_FOLDER <- "data/collection/gbfs"
OUTPUT_FOLDER <- "data/analysis/gbfs"
DATE_START <- 20260827
DATE_END <- 20260902

PERIODS <- tibble::tribble(
    ~day_type,  ~hour_start, ~hour_end, # [hour_start, hour_end[
    "all",      0L,          24L,
    "week",     6L,          9L,
    "week",     16L,         20L,
    "week",     20L,         24L,
    "weekend",  0L,          24L
)

REGIONS_GBFS <- c(
    # Bird
    "bird_almada", "bird_cascais", "bird_lisboa",
    # Lime
    "lime_almada", "lime_lisboa", "lime_oeiras"
    # Bolt MISSING
)

REGIONS <- c(REGIONS_GBFS, "emel_gira")
