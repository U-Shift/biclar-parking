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
library(tidyr)
library(mapview)

source("code/00_data_load.R")
source("code/99_results_load.R")
grid_csv <- read.csv("useful_data/grid.csv")

# 1. Residential minimum ratios function -------------------
# Function to calculate minimum number of parking spots for residential areas in each grid cell
# Parameters:
# grid_gpkg - file with grid geometry.
# grid_csv - file with grid mid - municipality id (if grid_gpkg already has the mid the function can be changed).
# buildings_grid - file with all building characteristics per grid cell - the most important characteristics for the
# residential ratios is number of households.
# ratios - csv file with minimum and maximum ratios for each municipality, as well as the unit they're in (e.g. 0.1 spots/m2)
residential_mr <- function(grid_gpkg, grid_csv, buildings_grid, ratios){
  parking_spots <- grid_gpkg |>
    mutate(
      mid = grid_csv$mid,
      n_households = buildings_grid$households,
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
    mutate(
      residential_parking_min = residential_min*n_households,
      residential_parking_max = residential_max*n_households
    )
  return(parking_spots)
}

# grid_dasymetric <- read.csv("data/base/daysimetric/grid_dasymetric_combined.csv")
residential_ratios <- read.csv("data/models/minimum_ratios/residential.csv")
mrm_residential <- residential_mr(grid, grid_csv, grid_dasymetric, residential_ratios)

View(mrm_residential)
mapview(mrm_residential, zcol="residential_parking_min")

# 2. POI minimum ratios function -------------------
# Function to calculate minimum number of parking spots for POIs in each grid cell.
# Parameters:
# poi_grid - csv file with the area of each type of POI in each grid cell.
# ratios - csv file with minimum and maximum parking ratio for each type of POI, as well as the respective unit.

poi_mr <- function(poi_grid, ratios){
  parking_long <- poi_grid |>
    st_drop_geometry() |>
    select(gid, starts_with("area_")) |>
    pivot_longer(      # turn the table longer, by creating a row for each poi category in each grid cell.
      cols = starts_with("area_"),
      names_to = "category",
      names_prefix = "area_",
      values_to = "area"
    ) |>
    left_join(ratios, by = "category") |>
    mutate(
      poi_parking_min = area*poi_min,
      poi_parking_max = area*poi_max
    )

  parking_wide <- parking_long |>
    # Select all columns except poi_min, poi_max and poi_unit, since these ratios are the same everywhere, only depending on category.
    # They can be consulted in "data/models/minimum_ratios/poi.csv"
    select(-poi_min, -poi_max, -poi_unit) |>
    pivot_wider( # now turn the table wider again, to get one variable per category
      names_from = category,
      values_from = c(area, poi_parking_min, poi_parking_max),
      names_glue = "{.value}_{category}"
    )
  parking_total <- parking_long |>  # create an object with only the totals
    group_by(gid) |>
    summarise(
      poi_parking_min_total = sum(poi_parking_min, na.rm = TRUE),
      poi_parking_max_total = sum(poi_parking_max, na.rm = TRUE)
    ) |>
    ungroup()

  parking_final <- grid |>  # now join the ratios both by category and total
    left_join(parking_wide, by = "gid") |>
    left_join(parking_total, by = "gid")
  return(parking_final)
}


# grid_pois_area <- read.csv("data/processed/poi/buildings/poi_buildings_grid.csv")
poi_ratios <- read.csv("data/models/minimum_ratios/poi.csv")
mrm_poi <- poi_mr(grid_pois_area, poi_ratios)

View(mrm_poi)
mapview(mrm_poi, zcol = "poi_parking_max_total")


# 3. PT minimum ratios function -------------------------------------------


# 4. Schools minimum ratios function --------------------------------------


# 5. Write outputs --------------------------------------------------------
write.csv(mrm_residential |> st_drop_geometry(), "data/outputs/minimum_ratios/residential.csv", row.names = FALSE)
write.csv(mrm_poi |> st_drop_geometry(), "data/outputs/minimum_ratios/poi.csv", row.names = FALSE)

