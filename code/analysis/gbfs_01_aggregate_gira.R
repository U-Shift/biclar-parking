# Aggregates raw data collection for GIRA
# Run with: $ Rscript code/analysis/gbfs_01_aggregate_gira.R > code/analysis/gbfs_01_aggregate_gira_$(date +%Y%m%d).log 2>&1

source("code/analysis/gbfs_00_params.R")

## Utils --------------------------------------------------------------------------

load_station_collection <- function(
  base_folder, subfolder,
  date_start, date_end,
  parse_file_fn = read.csv
) {
  day_folders <- list.dirs(paste0(base_folder, "/collection/", subfolder), recursive = FALSE)
  # Pre-allocate list
  all_days <- vector("list", length(day_folders))

  for (i in seq_along(day_folders)) {
    f <- day_folders[[i]]
    day <- as.integer(basename(f))
    files <- list.files(f, pattern = "\\.csv$", full.names = TRUE)

    if (day < date_start | day > date_end) {
      message(sprintf("Skipping day %s (%d files)", day, length(files)))
      next
    }
    day_date <- as.Date(as.character(day), format = "%Y%m%d")
    day_type_val <- if (lubridate::wday(day_date) %in% c(1, 7)) "weekend" else "week"

    message(sprintf("Processing day %s (%d files)", day, length(files)))
    all_days[[i]] <- dplyr::bind_rows(lapply(files, function(file) {
      ts_str <- tools::file_path_sans_ext(basename(file))
      df <- parse_file_fn(file)
      df$timestamp <- ts_str
      df$day <- basename(dirname(file))
      df$day_type <- day_type_val
      df$hour <- as.integer(substr(ts_str, 1, 2))
      df
    }))
  }

  result <- dplyr::bind_rows(all_days)
  # Free the intermediate list before returning the combined frame
  rm(all_days)
  invisible(gc())
  result
}

compute_period_metrics <- function(station_status, stations_in_grid, output_folder, periods) {
  for (p in seq_len(nrow(periods))) {
    period <- periods[p, ]
    message(sprintf(
      "Aggregating GIRA data for period %s, from %02d:00 to %02d:00",
      period$day_type, period$hour_start, period$hour_end
    ))

    status_period <- station_status |>
      filter(
        (day_type == period$day_type | period$day_type == "all"),
        hour >= period$hour_start,
        hour < period$hour_end
      )

    if (nrow(status_period) == 0) {
      warning(sprintf(
        "No data for period %s, from %02d:00 to %02d:00",
        period$day_type, period$hour_start, period$hour_end
      ))
      next
    }

    grid_aggregation <- aggregate_stations_at_grid(status_period, stations_in_grid)
    out_path <- paste0(
      output_folder, "/grid_bikes_",
      period$day_type, "_", period$hour_start, "-", period$hour_end, ".csv"
    )
    write.csv(grid_aggregation, out_path, row.names = FALSE)
    rm(grid_aggregation)
    invisible(gc())
  }
}

aggregate_stations_at_grid <- function(status, stations_in_grid) {
  n_updates <- n_distinct(status$day, status$timestamp)
  n_days <- n_distinct(status$day)

  # Join stations to grid cells via serialNumber = sid
  status_grid <- status |>
    inner_join(stations_in_grid, by = c("serialNumber" = "sid"))

  status_grid |>
    group_by(gid) |>
    summarise(
      n_vehicles = sum(bikes, na.rm = TRUE),
      n_updates = n_updates,
      n_days = n_days,
      avg_vehicles_per_update = round(n_vehicles / n_updates, 2),
      n_bicycle = sum(bikes, na.rm = TRUE),
      n_updates_bicycle = n_updates,
      avg_bicycle_per_update = round(n_bicycle / n_updates, 2),
      .groups = "drop"
    )
}

## GIRA --------------------------------------------------------------------------
message("\nProcessing GIRA data")
output_folder <- paste0("data/analysis/gbfs/emel_gira")
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

# Load metadata
folder <- paste0("data/collection/gbfs/emel_gira")
station_points <- read.csv(paste0(folder, "/stations_information.csv")) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  rename(id = serialNumber)

# Associate station points with grid
stations_in_grid <- st_join(
  station_points |> select(id) |> rename(sid = id),
  grid |> select(gid),
  join = st_within
) |> st_drop_geometry()
write.csv(stations_in_grid, paste0(output_folder, "/stations_in_grid.csv"), row.names = FALSE)

# Load data collection
station_status_collection <- load_station_collection(
  base_folder   = folder,
  subfolder     = "station_information",
  date_start    = DATE_START,
  date_end      = DATE_END,
  parse_file_fn = read.csv
)

# Aggregate and write per-period metrics to grid_bikes_<period>.csv
compute_period_metrics(station_status_collection, stations_in_grid, output_folder, PERIODS)

rm(station_status_collection, stations_in_grid, station_points)
invisible(gc())
