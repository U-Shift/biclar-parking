# Aggregates raw data collection for GIRA
# Run with: $ Rscript code/analysis/gbfs_01_aggregate_gira.R > code/analysis/gbfs_01_aggregate_gira_$(date +%Y%m%d).log 2>&1

source("code/analysis/gbfs_00_params.R")

## Utils --------------------------------------------------------------------------

load_station_collection <- function(
  base_folder, subfolder,
  date_start, date_end,
  parse_file_fn = arrow::read_parquet
) {
  status_dir <- file.path(base_folder, "collection", subfolder)

  if (!dir.exists(status_dir)) {
    warning(sprintf("Directory '%s' does not exist.", status_dir))
    return(tibble::tibble(
      serialNumber = integer(),
      bikes = integer(),
      docks = integer(),
      assetStatus = integer(),
      datetime = as.POSIXct(character()),
      timestamp = character(),
      day = character(),
      day_type = character(),
      hour = integer()
    ))
  }

  parquet_files <- list.files(status_dir, pattern = "^\\d{8}\\.parquet$", full.names = TRUE)
  today_str <- format(Sys.Date(), "%Y%m%d")

  day_strs <- sub("\\.parquet$", "", basename(parquet_files))
  day_nums <- as.integer(day_strs)

  # Eligible past-date parquet files strictly before today (< today_str, ignoring current day CSV)
  valid_idx <- grepl("^\\d{8}$", day_strs) & (day_strs < today_str)
  in_range_idx <- valid_idx & (day_nums >= as.integer(date_start)) & (day_nums <= as.integer(date_end))

  skipped_files <- parquet_files[valid_idx & !in_range_idx]
  for (sf in skipped_files) {
    message(sprintf("Skipping day %s (outside date range)", sub("\\.parquet$", "", basename(sf))))
  }

  target_files <- parquet_files[in_range_idx]

  if (length(target_files) == 0) {
    message(sprintf("No eligible past-date parquet files found in '%s' for date range %s to %s.",
                    status_dir, date_start, date_end))
    return(tibble::tibble(
      serialNumber = integer(),
      bikes = integer(),
      docks = integer(),
      assetStatus = integer(),
      datetime = as.POSIXct(character()),
      timestamp = character(),
      day = character(),
      day_type = character(),
      hour = integer()
    ))
  }

  all_days <- vector("list", length(target_files))

  for (i in seq_along(target_files)) {
    f <- target_files[[i]]
    day_str <- sub("\\.parquet$", "", basename(f))
    day_date <- as.Date(day_str, format = "%Y%m%d")
    day_type_val <- if (lubridate::wday(day_date) %in% c(1, 7)) "weekend" else "week"

    message(sprintf("Processing day %s (%s)", day_str, basename(f)))
    df <- parse_file_fn(f)

    df$day <- day_str
    df$day_type <- day_type_val
    if (!"hour" %in% names(df) && "datetime" %in% names(df)) {
      df$hour <- lubridate::hour(df$datetime)
    }
    if (!"timestamp" %in% names(df) && "datetime" %in% names(df)) {
      df$timestamp <- format(df$datetime, "%H%M%S")
    }

    all_days[[i]] <- df
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
folder <- file.path(BASE_FOLDER, "emel_gira")

station_points <- read.csv(file.path(folder, "stations_information.csv")) |>
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
  date_end      = DATE_END
)

if (nrow(station_status_collection) == 0) {
  message("No station status data to aggregate for GIRA.")
} else {
  # Aggregate and write per-period metrics to grid_bikes_<period>.csv
  compute_period_metrics(station_status_collection, stations_in_grid, output_folder, PERIODS)
}

rm(station_status_collection, stations_in_grid, station_points)
invisible(gc())
