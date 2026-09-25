# Code to create the minimum ratios model.
# For Almada and Seixal, the values used are based on municipality regulations (RMUE),
# while for the rest of the LMA values are based on literature recommendations.
# Refer to https://github.com/U-Shift/biclar-parking/issues/9 (municipal regulations) and
# https://github.com/U-Shift/biclar-parking/issues/7 (literature recommendations) for more details.

library(dplyr)
library(sf)
library(mapview)

source("code/00_data_load.R")
source("code/99_results_load.R")
grid_csv <- read.csv("useful_data/grid.csv")


# Function to calculate minimum number of parking spots in each grid cell -------------------
### FUNCTION IN TESTS, OUTPUT IS NOT FINAL
# Parameters:
# grid_gpkg - file with grid geometry.
# grid_csv - file with grid mid - municipality id (if grid_gpkg already has the mid the function can be changed).
# buildings_grid - file with all building characteristics per grid cell - the most important characteristics for the
# residential ratios are area and number of households.
# ratios - csv file with minimum and maximum ratios for each municipality, as well as the unit they're in (e.g. 0.1 spots/m2)
minimum_ratios <- function(grid_gpkg, grid_csv, buildings_grid, ratios){
  parking_spots <- grid_gpkg |> mutate(
    mid = grid_csv$mid,
    total_area = buildings_grid$atlas_total_footprint_m2,
    residential_pct = buildings_grid$census_pct_residential/100,
    residential_area_m2 = total_area * residential_pct,
    n_households = buildings_grid$census_dwellings_total
    ) |>
    left_join(ratios, by = "mid") |>
    mutate(residential_parking_min = dplyr::case_when(
      residential_unit == "m2" ~ residential_min*residential_area_m2*residential_pct,
      residential_unit == "household" ~ residential_min*n_households
    )) |>
    mutate(residential_parking_max = dplyr::case_when(
      residential_unit == "m2" ~ residential_max*residential_area_m2*residential_pct,
      residential_unit == "household" ~ residential_max*n_households
    ))
  return(parking_spots)
}

residential_ratios <- read.csv("data/models/minimum_ratios/residential.csv")
mrm_residential_parking_spots <- minimum_ratios(grid, grid_csv, grid_dasymetric_buildings, residential_ratios)
mapview(mrm_residential_parking_spots, zcol="residential_parking_min")
