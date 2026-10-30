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
      parking_min_residential = n_households*residential_min,
      parking_max_residential = n_households*residential_max
    )
  return(parking_spots)
}

# grid_dasymetric <- read.csv("data/base/daysimetric/grid_dasymetric_combined.csv")
residential_ratios <- read.csv("data/models/minimum_ratios/residential.csv")
mrm_residential <- residential_mr(grid, grid_meta, grid_dasymetric, residential_ratios)

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
      parking_min = area*poi_min,
      parking_max = area*poi_max
    )

  parking_wide <- parking_long |>
    pivot_wider( # now turn the table wider again, to get one variable per category
      names_from = category,
      values_from = c(poi_min, poi_max, area, parking_min, parking_max),
      names_glue = "{.value}_{category}"
    )
  parking_total <- parking_long |>  # create an object with only the totals
    group_by(gid) |>
    summarise(
      parking_min_poi_total = sum(parking_min, na.rm = TRUE),
      parking_max_poi_total = sum(parking_max, na.rm = TRUE)
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
mapview(mrm_poi, zcol = "parking_max_poi_total")


# 3. PT minimum ratios function -------------------------------------------
# Function to calculate minimum number of parking spots for public transport (PT) stops/stations in each grid cell.
# Parameters:
# pt_grid - csv file with the number of PT validations per mode in each grid cell.
# ratios - csv file with minimum and maximum parking ratio for each type of PT station, as well as the respective unit.

pt_mr <- function(pt_grid, ratios){
  pt_grid_new <- grid |>
    left_join(pt_grid, by = "gid") |>
    st_drop_geometry()
  pt_grid_new[is.na(pt_grid_new)] <- 0

  parking_long <- pt_grid_new |>
    pivot_longer(      # turn the table longer, by creating a row for each pt mode ("category") in each grid cell.
      cols = starts_with("n_"),
      names_to = "category",
      names_prefix = "n_",
      values_to = "n_mode"
    ) |>
    left_join(ratios, by = "category") |>
    mutate(
      parking_min = n_mode*pt_min,
      parking_max = n_mode*pt_max
    )

  parking_wide <- parking_long |>
    pivot_wider( # now turn the table wider again, to get one variable per mode ("category")
      names_from = category,
      values_from = c(pt_min, pt_max, n_mode, parking_min, parking_max),
      names_glue = "{.value}_{category}"
    ) |>
    select(-h3_address)

  parking_total <- parking_long |>  # create an object with only the totals
    group_by(gid) |>
    summarise(
      parking_min_pt_total = sum(parking_min, na.rm = TRUE),
      parking_max_pt_total = sum(parking_max, na.rm = TRUE)
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
mapview(mrm_pt, zcol = "parking_min_pt_total")

# 4. Schools minimum ratios function --------------------------------------

schools_mr <- function(schools_grid, university_grid, ratios){
  # Starting by standardizing and unifying schools/universities data
  schools_grid <- schools_grid |>
    rename(n_students_schools = n_students) |>
    select(-X)
  university_grid <- university_grid |>
    rename(n_students_university = n_students) |>
    select(-X)
  all_schools_grid <- schools_grid |>
    left_join(university_grid, by = "gid")

  # Now joining the ratios
  ratios_wide <- ratios |>
    pivot_wider(
      names_from = "category",
      values_from = c(schools_min, schools_max),
      names_glue = "{.value}_{category}"
    )
  all_schools_grid <- all_schools_grid |>
    cross_join(ratios_wide) |>
    mutate(
      parking_min_childcare = n_students_childcare*schools_min_childcare,
      parking_min_primary = n_students_primary_school*schools_min_primary,
      parking_min_secondary = n_students_secondary_school*schools_min_secondary,
      parking_min_university = n_students_university*schools_min_university,

      parking_max_childcare = n_students_childcare*schools_max_childcare,
      parking_max_primary = n_students_primary_school*schools_max_primary,
      parking_max_secondary = n_students_secondary_school*schools_max_secondary,
      parking_max_university = n_students_university*schools_max_university,

      parking_min_schools_total = parking_min_childcare + parking_min_primary + parking_min_secondary + parking_min_university,
      parking_max_schools_total = parking_max_childcare + parking_max_primary + parking_max_secondary + parking_max_university
    )

  parking_total <- grid |>
    left_join(all_schools_grid, by = "gid")

  return(parking_total)
}

schools_ratios <- read.csv("data/models/minimum_ratios/schools.csv")
mrm_schools <- schools_mr(grid_pois_schools, grid_pois_universities, schools_ratios)

View(mrm_schools)
mapview(mrm_schools, zcol = "parking_min_schools_total")


# 5. Aggregate all data ---------------------------------------------------
mrm_total <- grid |>
  left_join(mrm_residential |> st_drop_geometry()) |>
  left_join(mrm_poi |> st_drop_geometry()) |>
  left_join(mrm_pt |> st_drop_geometry()) |>
  left_join(mrm_schools |> st_drop_geometry())
mrm_total <- mrm_total |>
  mutate(
    parking_min_total = parking_max_residential + parking_min_poi_total + parking_min_pt_total + parking_min_schools_total,
    parking_max_total = parking_max_residential + parking_max_poi_total + parking_max_pt_total + parking_max_schools_total
  )
View(mrm_total)
mapview(mrm_total, zcol = "parking_min_total")
mapview(mrm_total, zcol = "parking_max_total")

# 6. Write outputs --------------------------------------------------------
write.csv(mrm_residential |> st_drop_geometry(), "data/outputs/minimum_ratios/residential.csv", row.names = FALSE)
write.csv(mrm_poi |> st_drop_geometry(), "data/outputs/minimum_ratios/poi.csv", row.names = FALSE)
write.csv(mrm_pt |> st_drop_geometry(), "data/outputs/minimum_ratios/pt.csv", row.names = FALSE)
write.csv(mrm_schools |> st_drop_geometry(), "data/outputs/minimum_ratios/schools.csv", row.names = FALSE)
write.csv(mrm_total |> st_drop_geometry(), "data/outputs/minimum_ratios/total.csv", row.names = FALSE)

