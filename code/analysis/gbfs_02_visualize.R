source("code/00_data_load.R")

folder <- "data/analysis/gbfs" # or specific operator: "data/analysis/gbfs/emel_gira", "data/analysis/gbfs/bird_almada"
period <- "all"
hour_start <- 0
hour_end <- 24

data <- read.csv(paste0(folder, "/grid_bikes_", period, "_", hour_start, "-", hour_end, ".csv"))
data_grid <- grid |>
  left_join(data, by = "gid") |>
  filter(n_updates > 0, n_vehicles > 0)

# Quick spatial overview
mapview(municipalities, alpha.regions = 0, color = "black", lwd = 1.5) +
  mapview(data_grid, zcol = "avg_vehicles_per_update", layer.name = "Avg vehicles/update", legend = TRUE)

# Detailed breakdown by available vehicle types and metrics
m <- mapview(municipalities, alpha.regions = 0, color = "black", lwd = 1.5) +
  mapview(data_grid, zcol = "avg_vehicles_per_update", layer.name = "Avg vehicles/update", legend = TRUE) +
  mapview(data_grid, zcol = "n_vehicles", layer.name = "Total vehicle counts", legend = TRUE, hide = TRUE) +
  mapview(data_grid, zcol = "n_updates", layer.name = "Total updates", legend = TRUE, hide = TRUE)

if ("n_operators" %in% names(data_grid)) {
  m <- m + mapview(data_grid, zcol = "n_operators", layer.name = "Operators count", legend = TRUE, hide = TRUE)
}

if ("avg_bicycle_per_update" %in% names(data_grid)) {
  m <- m + mapview(data_grid, zcol = "avg_bicycle_per_update", layer.name = "Avg bicycles/update", legend = TRUE, hide = TRUE)
}

if ("avg_scooter_per_update" %in% names(data_grid)) {
  m <- m + mapview(data_grid, zcol = "avg_scooter_per_update", layer.name = "Avg scooters/update", legend = TRUE, hide = TRUE)
}

m
