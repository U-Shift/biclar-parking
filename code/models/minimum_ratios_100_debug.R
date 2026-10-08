# Load data 
source("code/00_data_load.R")
source("code/99_results_load.R")
model = read.csv("data/models/minimum_ratios/result.csv")

model_debug = grid |> 
    # Convert to sf for visualisation
    left_join(model, by="gid") |>
    # Get demographics, to compare with parking dimensioning
    left_join(grid_dasymetric |> select(gid, population, buildings, households) |> rename_with(~ paste0("dasymetric_population_", .), -gid), by="gid") |>
    # Relate dimensions
    mutate(
      parking_min_to_population_ratio = ifelse(
        dasymetric_population_population>0, 
        residential_parking_min / dasymetric_population_population, 
        0
      ),
      parking_max_to_population_ratio = ifelse(
        dasymetric_population_population>0, 
        residential_parking_max / dasymetric_population_population, 
        0
      ),
      building_ratio = ifelse(
        atlas_building_count>0, 
        census_buildings_total / atlas_building_count, 
        0
      )
    ) # |> 
  # Remove outliers (for better visualisation, should not be needed when problems fixed)
  filter(
    building_ratio < quantile(model_debug$building_ratio, probs = c(0.99), na.rm = TRUE) &
    parking_min_to_population_ratio < quantile(model_debug$parking_min_to_population_ratio, probs = c(0.99), na.rm = TRUE)
  )

summary(model_debug$building_ratio)
quantile(model_debug$building_ratio, probs = c(0.99), na.rm = TRUE)
summary(model_debug$parking_min_to_population_ratio)
quantile(model_debug$parking_min_to_population_ratio, probs = c(0.99), na.rm = TRUE)

# Visualise results
mapview(model_debug, zcol="residential_parking_min", layer.name = "Min. ratios model result: lower bound") + 
  mapview(model_debug, zcol="residential_parking_max", hide = TRUE, layer.name = "Min. ratios model result: upper bound") + 
  mapview(model_debug, zcol="residential_area_m2", hide = TRUE, layer.name = "Residential area (m²)") + 
  mapview(model_debug, zcol="dasymetric_population_population", hide = TRUE, layer.name = "Population (dasymetric)") + 
  mapview(model_debug, zcol="dasymetric_population_buildings", hide = TRUE, layer.name = "Buildings (dasymetric)") + 
  mapview(model_debug, zcol="dasymetric_population_households", hide = TRUE, layer.name = "Households (dasymetric)") + 
  mapview(model_debug, zcol="parking_min_to_population_ratio", hide = TRUE, layer.name = "Parking min to population ratio") + 
  mapview(model_debug, zcol="parking_max_to_population_ratio", hide = TRUE, layer.name = "Parking max to population ratio") +
  mapview(model_debug, zcol="building_ratio", hide = TRUE, layer.name = "Building ratio")
