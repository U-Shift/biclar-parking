
folder = "data/analysis/gbfs/emel_gira"
folder = "data/analysis/gbfs"
period = "week"
hour_start = 6
hour_end = 9

data = read.csv(paste0(folder, "/grid_metrics_", period, "_", hour_start, "-", hour_end, ".csv"))
data_grid = grid |> left_join(data, by = "gid") |> filter(n_updates > 0) |>
  mutate(
    bikes_rented_capacity_ratio = round(bikes_rented / capacity, 2),
    bikes_returned_capacity_ratio = round(bikes_returned / capacity, 2)
  )

mapview(municipalities, alpha.regions=0) +
  mapview(data_grid, zcol="bikes_rented_capacity_ratio", layer.name = "Rented/capacity ratio", legend=TRUE)
mapview(municipalities, alpha.regions=0) +
  mapview(data_grid, zcol="bikes_returned_capacity_ratio", layer.name = "Returned/capacity ratio", legend=TRUE)

mapview(municipalities, alpha.regions=0) +
  mapview(data_grid, zcol="bikes_n_max", layer.name = "Max nr bikes", legend=TRUE)

mapview(municipalities, alpha.regions=0) +
  mapview(data_grid, zcol="occupancy_ratio_max", layer.name = "Max bike occupancy", legend=TRUE)

mapview(data_grid, zcol="n_stations", layer.name = "Number of stations", legend=TRUE) +
  mapview(data_grid, zcol="capacity", layer.name = "Capacity", legend=TRUE, hide=TRUE) +
  mapview(data_grid, zcol="bikes_rented", layer.name = "Bikes rented", legend=TRUE, hide = TRUE) +
  mapview(data_grid, zcol="bikes_returned", layer.name = "Bikes returned", legend=TRUE, hide =TRUE)


