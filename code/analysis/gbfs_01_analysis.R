# Analyse gbfs data at grid level
# Run with: $ Rscript code/analysis/gbfs_01_analysis.R
source("code/00_data_load.R")
source("code/00_helpers.R")

# PERIODS should be loaded from gbfs_00_aggregation.R
PERIODS = data.frame(
  day_type = character(), # Week, weekend, all
  hour_start = integer(),
  hour_end = integer()
)
PERIODS = rbind(PERIODS, data.frame(day_type = "all", hour_start = 0, hour_end = 24))
PERIODS = rbind(PERIODS, data.frame(day_type = "week", hour_start = 6, hour_end = 9))
PERIODS = rbind(PERIODS, data.frame(day_type = "week", hour_start = 16, hour_end = 19))
PERIODS = rbind(PERIODS, data.frame(day_type = "week", hour_start = 19, hour_end = 24))
PERIODS = rbind(PERIODS, data.frame(day_type = "weekend", hour_start = 0, hour_end = 24))
assertthat::assert_that(exists("PERIODS"), msg = "PERIODS not found. Run gbfs_00_aggregation.R first.")

GBFS_LIST = data.frame(
  region = character(),
  dockless = logical()
)
GBFS_LIST[1,] = c("emel_gira", FALSE)
GBFS_LIST[2,] = c("bird_cascais", TRUE)
GBFS_LIST[3,] = c("bird_lisboa", TRUE)

grid_all = data.frame() 

# Analyse for each operator
for (i in 1:nrow(GBFS_LIST)) { # i = 1
  region = GBFS_LIST[i, "region"]
  dockless = as.logical(GBFS_LIST[i, "dockless"])
  print(paste0("Processing region: ", region, " (dockless: ", dockless, ")"))
  folder = paste0("data/analysis/gbfs/", region, "/")
  # Load stations/grid association 
  stations_in_grid = read.csv(paste0(folder, "stations_in_grid.csv"))

  # Aggregate stats at grid level
  gtfs_stations = stations_in_grid |>
    group_by(gid) |>
    summarise(n_stations = n())
  grid_analysis = grid |>
    left_join(gtfs_stations, by = "gid")
  # mapview(grid_analysis, zcol="n_stations", legend=TRUE, layer.name="Number of stations")

  # For each period, merge stats 
  for (p in 1:nrow(PERIODS)) { # p = 1
    period = PERIODS[p, ]
    message(paste0("Processing period: ", period$day_type, " (", period$hour_start, " to ", period$hour_end, ")"))
    # Load aggregated stats for the period
    period_stats = read.csv(paste0(folder, "station_metrics_", period$day_type, "_", period$hour_start, "-", period$hour_end, ".csv")) |>
      left_join(stations_in_grid, by = c("id" = "sid")) |>
      group_by(gid) |>
      summarise(
        n_updates = sum(n_updates),
        capacity = sum(capacity, na.rm = TRUE),
        bikes_rented = sum(bikes_rented, na.rm = TRUE),
        bikes_returned = sum(bikes_returned, na.rm = TRUE)
        # TODO! Improve with additional metrics
      ) |>
      left_join(grid_analysis |> st_drop_geometry() |> select(gid, n_stations), by = "gid")
    # View(period_stats)
    
    write.csv(period_stats, paste0(folder, "grid_metrics_", period$day_type, "_", period$hour_start, "-", period$hour_end, ".csv"), row.names = FALSE)

    grid_all = grid_all |> 
      bind_rows(
        period_stats |>
          mutate(
            region = region,
            dockless = dockless,
            day_type = period$day_type,
            hour_start = period$hour_start,
            hour_end = period$hour_end
          )
      )
  }
}

# Aggregate all operators at grid level, by period
for (p in 1:nrow(PERIODS)) { # p = 1
  period = PERIODS[p, ]
  message(paste0("Aggregating all operators for period: ", period$day_type, " (", period$hour_start, " to ", period$hour_end, ")"))
  grid_all_period = grid_all |>
    filter(day_type == period$day_type & hour_start == period$hour_start & hour_end == period$hour_end) |>
    group_by(gid) |>
    summarise(
      n_operators = n(),
      n_stations = sum(n_stations, na.rm = TRUE),
      n_updates = sum(n_updates, na.rm = TRUE),
      capacity = sum(capacity, na.rm = TRUE),
      bikes_rented = sum(bikes_rented, na.rm = TRUE),
      bikes_returned = sum(bikes_returned, na.rm = TRUE),
      # TODO! Improve with additional metrics
      any_dockless = any(dockless),
      any_dock = !any(dockless)
    )
  
  write.csv(grid_all_period, paste0("data/analysis/gbfs/grid_metrics_", period$day_type, "_", period$hour_start, "-", period$hour_end, ".csv"), row.names = FALSE)
}