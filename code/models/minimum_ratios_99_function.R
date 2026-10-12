# Script to create the minimum ratios model.
# For the municipalities Almada and Seixal, the values used are based on municipality regulations (RMUE).
# For the rest of the LMA, values are based on literature recommendations, since no bicycle parking regulations exist.
# Refer to https://github.com/U-Shift/biclar-parking/issues/9 (municipal regulations) and
# https://github.com/U-Shift/biclar-parking/issues/7 (literature recommendations) for more details.
# Complete methodology available at:
# https://docs.google.com/document/d/1DSM_PcinC-S8Q3kiweb7OUQ05ckQvSMSx0THY80E_eI/edit?usp=drive_link

# Code steps:
# 1. Function to calculate residential minimum ratios. (WRAPPING UP)
# 2. Function to calculate commercial and office minimum ratios. (NOT STARTED)
# 3. Function to calculate POI minimum ratios. (IN PROGRESS)
# 4. Aggregation of outputs from each function.

library(dplyr)
library(sf)
library(mapview)

source("code/00_data_load.R")
source("code/99_results_load.R")
grid_csv <- read.csv("useful_data/grid.csv")

# 1. Residential minimum ratios function -------------------
# Function to calculate minimum number of parking spots for residential areas in each grid cell
### NOTE: Function working but inputs still being corrected. Function output not final.
# Parameters:
# grid_gpkg - file with grid geometry.
# grid_csv - file with grid mid - municipality id (if grid_gpkg already has the mid the function can be changed).
# buildings_grid - file with all building characteristics per grid cell - the most important characteristics for the
# residential ratios are area and number of households.
# ratios - csv file with minimum and maximum ratios for each municipality, as well as the unit they're in (e.g. 0.1 spots/m2)
residential_minimum_ratios <- function(grid_gpkg, grid_csv, buildings_grid, ratios){
  parking_spots <- grid_gpkg |> mutate(
    mid = grid_csv$mid,
    n_households = buildings_grid$census_dwellings_total,
    ### Area units - deprecated
    # total_area = buildings_grid$atlas_total_footprint_m2,
    # census_pct_residential = ifelse(is.na(buildings_grid$census_pct_residential),0,buildings_grid$census_pct_residential),
    # residential_pct = census_pct_residential/100,
    # residential_area_m2 = total_area * residential_pct,
    # census_buildings_total = buildings_grid$census_buildings_total,
    # atlas_building_count = buildings_grid$atlas_building_count,
    # census_atlas_ratio = ifelse(atlas_building_count==0,1,census_buildings_total/atlas_building_count)
    ###
    ) |>
    left_join(ratios, by = "mid") |>
    mutate(residential_parking_min = dplyr::case_when(
      residential_unit == "household" ~ residential_min*n_households
    )) |>
    mutate(residential_parking_max = dplyr::case_when(
      residential_unit == "household" ~ residential_max*n_households
    ))
  return(parking_spots)
}

residential_ratios <- read.csv("data/models/minimum_ratios/residential.csv")
mrm_residential_parking_spots <- residential_minimum_ratios(grid, grid_csv, grid_dasymetric_buildings, residential_ratios)

mapview(mrm_residential_parking_spots, zcol="residential_parking_min")

# write.csv(mrm_residential_parking_spots |> st_drop_geometry(), "data/models/minimum_ratios/result.csv", row.names = FALSE)

# 2. Commercial/Service minimum ratios function -------------------

# 3. POI minimum ratios function -------------------
# Function to calculate minimum number of parking spots for POIs in each grid cell
### FUNCTION IN TESTS, OUTPUT NOT FINAL
# Parameters:
# grid_gpkg - file with grid geometry.
# poi_grid - file with number of POIs of each type per grid cell.
# ratios - csv file with minimum and maximum parking ratio for each type of POI, as well as the respective unit.

# poi_minimum_ratios <- function(grid_gpkg, poi_grid, ratios){
# }

poi_grid <- st_read("data/processed/poi/grid/poi_grid_counts.gpkg")
poi_ratios <- read.csv("data/models/minimum_ratios/poi.csv")
#mrm_poi_parking_spots <- poi_minimum_ratios(grid, poi_grid, poi_ratios)



