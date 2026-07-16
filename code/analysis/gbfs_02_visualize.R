
folder = "data/analysis/gbfs/emel_gira"
period = "all"
hour_start = 0
hour_end = 24

data = read.csv(paste0(folder, "/grid_metrics_", period, "_", hour_start, "-", hour_end, ".csv"))
data_grid = grid |> left_join(data, by = "gid") |> filter(n_updates > 0)

mapview(data_grid, zcol="n_stations", layer.name = "Number of stations", legend=TRUE) +
  mapview(data_grid, zcol="capacity", layer.name = "Capacity", legend=TRUE, hide=TRUE) +
  mapview(data_grid, zcol="bikes_rented", layer.name = "Bikes rented", legend=TRUE, hide = TRUE) + 
  mapview(data_grid, zcol="bikes_returned", layer.name = "Bikes returned", legend=TRUE, hide =TRUE)


