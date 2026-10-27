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
      parking_min_residential = n_households*min,
      parking_max_residential = n_households*max
    ) |>
    select(-min, -max, -unit)

  return(parking_spots)
}

# grid_dasymetric <- read.csv("data/base/daysimetric/grid_dasymetric_combined.csv")
residential_ratios <- read.csv("data/models/minimum_ratios/residential.csv")
mrm_residential <- residential_mr(grid, grid_csv, grid_dasymetric, residential_ratios)

View(mrm_residential)
mapview(mrm_residential, zcol="parking_min_residential")

# 2. POI minimum ratios function -------------------
# Function to calculate minimum number of parking spots for points of interest (POIs) in each grid cell.
# Parameters:
# poi_grid - csv file with the area of each type of POI in each grid cell.
# ratios - csv file with minimum and maximum parking ratio for each type of POI, as well as the respective unit.

poi_mr <- function(poi_grid, ratios){
  parking_long <- poi_grid |>
    st_drop_geometry() |>
    pivot_longer(      # turn the table longer, by creating a row for each poi category in each grid cell.
      cols = starts_with("area_"),
      names_to = "category",
      names_prefix = "area_",
      values_to = "area"
    ) |>
    left_join(ratios, by = "category") |>
    mutate(
      parking_min = area*min,
      parking_max = area*max
    )

  parking_wide <- parking_long |>
    # Select all columns except min, max and unit, since these ratios are the same everywhere, only depending on category.
    # They can be consulted in "data/models/minimum_ratios/poi.csv"
    select(-min, -max, -unit) |>
    pivot_wider( # now turn the table wider again, to get one variable per category
      names_from = category,
      values_from = c(area, parking_min, parking_max),
      names_glue = "{.value}_{category}"
    )
  parking_total <- parking_long |>  # create an object with only the totals
    group_by(gid) |>
    summarise(
      parking_min_poi = sum(parking_min, na.rm = TRUE),
      parking_max_poi = sum(parking_max, na.rm = TRUE)
    ) |>
    ungroup()

  parking_final <- grid |>  # now join the ratios both by category and total
    left_join(parking_wide, by = "gid") |>
    left_join(parking_total, by = "gid")
  return(parking_final)
}

# grid_pois_area <- read.csv("data/processed/poi/buildings/poi_buildings_grid.csv")
poi_ratios <- read.csv("data/models/minimum_ratios/poi.csv")
mrm_poi <- poi_mr(select(grid_pois_area, c(gid, starts_with("area_"))), poi_ratios)

View(mrm_poi)
mapview(mrm_poi, zcol = "poi_parking_max_total")


# 3. PT minimum ratios function -------------------------------------------
# Function to calculate minimum number of parking spots for public transport (PT) stops/stations in each grid cell.
# Parameters:
# pt_grid - csv file with the number of PT validations per mode in each grid cell.
# ratios - csv file with minimum and maximum parking ratio for each type of PT station, as well as the respective unit.

pt_mr <- function(pt_grid, ratios){
  parking_long <- pt_grid |>
    pivot_longer(      # turn the table longer, by creating a row for each pt mode ("category") in each grid cell.
      cols = starts_with("n_"),
      names_to = "category",
      names_prefix = "n_",
      values_to = "n_mode"
    ) |>
    left_join(ratios, by = "category") |>
    mutate(
      parking_min = n_mode*min,
      parking_max = n_mode*max
    )

  parking_wide <- parking_long |>
    # Select all columns except min, max and unit, since these ratios are the same everywhere, only depending on category.
    # They can be consulted in "data/models/minimum_ratios/pt.csv"
    select(-min, -max, -unit) |>
    pivot_wider( # now turn the table wider again, to get one variable per mode ("category")
      names_from = category,
      values_from = c(n_mode, parking_min, parking_max),
      names_glue = "{.value}_{category}"
    )
  parking_total <- parking_long |>  # create an object with only the totals
    group_by(gid) |>
    summarise(
      parking_min_pt = sum(parking_min, na.rm = TRUE),
      parking_max_pt = sum(parking_max, na.rm = TRUE)
    ) |>
    ungroup()

  parking_final <- grid |>  # now join the ratios both by mode ("category") and total
    left_join(parking_wide, by = "gid") |>
    left_join(parking_total, by = "gid")

  parking_final[is.na(parking_final)] <- 0
  return(parking_final)
}

# grid_ticketing_global <- read.csv("data/analysis/ticketing/grid_ticketing_global.csv")
pt_ratios <- read.csv("data/models/minimum_ratios/pt.csv")
mrm_pt <- pt_mr(grid_ticketing_global, pt_ratios)

View(mrm_pt)
mapview(mrm_pt, zcol = "parking_min_pt")

# 4. Schools minimum ratios function --------------------------------------



# 5. Write outputs --------------------------------------------------------
write.csv(mrm_residential |> st_drop_geometry(), "data/outputs/minimum_ratios/residential.csv", row.names = FALSE)
write.csv(mrm_poi |> st_drop_geometry(), "data/outputs/minimum_ratios/poi.csv", row.names = FALSE)
write.csv(mrm_pt |> st_drop_geometry(), "data/outputs/minimum_ratios/pt.csv", row.names = FALSE)

