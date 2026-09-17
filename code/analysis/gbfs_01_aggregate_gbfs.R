# Aggregates raw data collection (free bikes positions) for operators producing
# counts of free bikes (parked, not in use) per grid cell
# Run with: $ Rscript code/analysis/gbfs_01_aggregate_gbfs.R > code/analysis/gbfs_01_aggregate_gbfs_$(date +%Y%m%d).log 2>&1

source("code/analysis/gbfs_00_params.R")

## Utils --------------------------------------------------------------------------

load_bikes <- function(
  base_folder, subfolder,
  date_start, date_end,
  parse_file_fn = arrow::read_parquet
) {
  status_dir <- file.path(base_folder, "collection", subfolder)

  if (!dir.exists(status_dir)) {
    warning(sprintf("Directory '%s' does not exist.", status_dir))
    return(tibble::tibble(
      lat = numeric(),
      lon = numeric(),
      vehicle_type = integer(),
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
      lat = numeric(),
      lon = numeric(),
      vehicle_type = integer(),
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

compute_period_metrics <- function(bike_collection, vehicle_types, output_folder, periods) {
  for (p in seq_len(nrow(periods))) { # p = 2
    period <- periods[p, ]
    message(sprintf(
      "Aggregating data for period %s, from %02d:00 to %02d:00",
      period$day_type, period$hour_start, period$hour_end
    ))

    bike_collection_period <- bike_collection |>
      filter(
        # "all" does not filter by day_type, only by hour
        (day_type == period$day_type | period$day_type == "all"),
        hour >= period$hour_start,
        hour < period$hour_end
      )

    if (nrow(bike_collection_period) == 0) {
      warning(sprintf(
        "No data for period %s, from %02d:00 to %02d:00",
        period$day_type, period$hour_start, period$hour_end
      ))
      next
    }

    grid_aggregation <- aggregate_bikes_at_grid(bike_collection_period, vehicle_types)
    out_path <- paste0(
      output_folder, "/grid_bikes_",
      period$day_type, "_", period$hour_start, "-", period$hour_end, ".csv"
    )
    write.csv(grid_aggregation, out_path, row.names = FALSE)
    rm(grid_aggregation)
    invisible(gc())
  }
}


aggregate_bikes_at_grid <- function(bikes, types = NULL) {
  bikes_row_n <- bikes |> mutate(pid = row_number())
  bikes_grid <- st_join(
    bikes_row_n |> select(pid),
    grid |> select(gid),
    join = st_within
  ) |>
    st_drop_geometry() |>
    left_join(bikes_row_n |> st_drop_geometry(), by = "pid")

  if (!is.null(types)) {
    bikes_grid <- bikes_grid |>
      left_join(types |> select(id, form_factor), by = c("vehicle_type" = "id")) |>
      mutate(form_factor = ifelse(is.na(form_factor), paste0("type_", vehicle_type), form_factor))
  } else {
    bikes_grid <- bikes_grid |>
      mutate(form_factor = paste0("type_", vehicle_type))
  }

  # All grids have same n_updates and n_days, regardless of having or not bikes in that period
  n_updates <- n_distinct(bikes$day, bikes$timestamp)
  n_days <- n_distinct(bikes$day)

  by_type <- bikes_grid |>
    group_by(gid, form_factor) |>
    summarise(
      n = n(),
      n_updates = n_updates,
      per_update = round(n / n_updates, 2),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = form_factor,
      values_from = c(n, n_updates, per_update),
      names_glue = "{.value}_{form_factor}",
      values_fill = 0
    ) |>
    rename_with(
      ~ paste0("avg_", sub("^per_update_", "", .x), "_per_update"),
      starts_with("per_update_")
    )

  overall <- bikes_grid |>
    group_by(gid) |>
    summarise(
      n_vehicles = n(),
      n_updates = n_updates,
      n_days = n_days,
      avg_vehicles_per_update = round(n_vehicles / n_updates, 2),
      .groups = "drop"
    )

  left_join(overall, by_type, by = "gid")
}


## GBFS --------------------------------------------------------------------------
for (r in REGIONS_GBFS) { # r = REGIONS_GBFS[[1]]
  message("\nProcessing GBFS data for ", r)
  output_folder <- paste0("data/analysis/gbfs/", r)
  if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

  # Load metadata for region
  folder <- file.path(BASE_FOLDER, r)

  # Load vehicle types
  vehicle_types <- read.csv(file.path(folder, "vehicle_types.csv"))

  # Load data collection
  bikes_raw <- load_bikes(
    base_folder   = folder,
    subfolder     = "free_bike_status",
    date_start    = DATE_START,
    date_end      = DATE_END
  )

  if (nrow(bikes_raw) == 0) {
    message(sprintf("No bike collection data to aggregate for %s.", r))
    next
  }

  bikes_collection <- bikes_raw |> st_as_sf(coords = c("lon", "lat"), crs = 4326)
  rm(bikes_raw)

  # Aggregate and write per-period metrics
  compute_period_metrics(bikes_collection, vehicle_types, output_folder, PERIODS)

  # Free all large objects before next region iteration
  rm(bikes_collection, vehicle_types)
  invisible(gc())
}
