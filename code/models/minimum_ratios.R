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
# grid_csv - file with grid mid (if grid_gpkg already has the mid the function can be changed).
# buildings_grid - file with all building characteristics per grid cell.
# Most important for residential ratios are area and number of households.
# ratios - csv file with minimum and maximum ratios for each municipality, as well as the unit they're in (e.g. 0.1 spots/m2)
residential_minimum_ratios <- function(grid_gpkg, grid_csv, buildings_grid, ratios){
  parking_spots <- grid_gpkg |> mutate(
    mid = grid_csv$mid,
    residential_area = buildings_grid$atlas_total_floor_area_m2
    ) |>
    left_join(ratios, by = "mid") |>
    mutate(
      parking_spots_min = residential_area * minimum,
      parking_spots_max = residential_area * maximum
    )
  return(parking_spots)
}

residential_ratios <- read.csv("data/models/minimum_ratios/residential.csv")

parking_spots_grid <- minimum_ratios(grid, grid_csv, grid_dasymetric_buildings, residential_ratios)
