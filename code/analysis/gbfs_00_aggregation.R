# Aggregates raw data collection for operators producing stats for each bike station (different periods)
# Run with: $ Rscript code/analysis/gbfs_00_aggregation.R > code/analysis/gbfs_00_aggregation_20260730.log 2>&1

source("code/00_data_load.R")
source("code/00_helpers.R")

# Raw data aggregation --------------------------------------------------------------------------
DATE_START <- 20260507
DATE_END <- 20260715

PERIODS <- tibble::tribble(
  ~day_type,  ~hour_start, ~hour_end,
  "all",      0L,          24L,
  "week",     6L,          9L,
  "week",     16L,         19L,
  "week",     19L,         24L,
  "weekend",  0L,          24L
)

## Utils --------------------------------------------------------------------------

aggregate_station_updates <- function(updates) {
  updates |>
    # Make sure id, timestamp are unique (preserve all columns for first match)
    distinct(id, timestamp, .keep_all = TRUE) |>
    filter(is_installed) |>
    select(-is_installed) |>
    arrange(id, timestamp) |>
    group_by(id) |>
    mutate(
      capacity = free_bikes + free_docks,
      occupation_ratio = round(free_bikes / capacity, 2),
      # Compare to previous update
      changed = free_bikes != lag(free_bikes),
      bikes_rented = pmax(lag(free_bikes) - free_bikes, 0, na.rm = TRUE),
      bikes_returned = pmax(free_bikes - lag(free_bikes), 0, na.rm = TRUE),
      time_since_previous = as.numeric(difftime(timestamp, lag(timestamp), units = "secs"))
    ) |>
    ungroup() |>
    filter(time_since_previous < 5 * 60) |>
    mutate(
      # Enrich with day type and hour (from timestamp, seconds epoch)
      datetime = as.POSIXct(timestamp, origin = "1970-01-01", tz = "Europe/Lisbon"),
      day_type = case_when(
        lubridate::wday(datetime) %in% c(1, 7) ~ "weekend",
        TRUE ~ "week"
      ),
      hour = lubridate::hour(datetime)
    ) |>
    select(-datetime)
}

aggregate_station_stats <- function(stats) {
  stats |>
    group_by(id) |>
    summarise(
      n_updates = n(),
      capacity = max(capacity, na.rm = TRUE),
      n_changes = sum(changed, na.rm = TRUE),
      bikes_rented = sum(bikes_rented, na.rm = TRUE),
      bikes_returned = sum(bikes_returned, na.rm = TRUE),
      # Time between updates avg and sd
      time_between_updates_mean = round(mean(time_since_previous, na.rm = TRUE), 2),
      time_between_updates_sd = round(sd(time_since_previous, na.rm = TRUE), 2),
      # Free bikes metrics
      !!!analyse_col_dispersion(free_bikes, measures_extra = "cv"),
      # Free docks metrics
      !!!analyse_col_dispersion(free_docks, measures_extra = "cv"),
      # Occupation ratio metrics
      !!!analyse_col_dispersion(occupation_ratio, measures_extra = "cv")
    )
}


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
    message(sprintf("Processing day %s (%d files)", day, length(files)))
    all_days[[i]] <- dplyr::bind_rows(lapply(files, parse_file_fn))
  }

  result <- dplyr::bind_rows(all_days)
  # Free the intermediate list before returning the combined frame
  rm(all_days); invisible(gc())
  result
}

write_period_metrics <- function(enriched, output_folder, periods) {
  for (p in seq_len(nrow(periods))) {
    period <- periods[p, ]
    message(sprintf(
      "Aggregating data for period %s, from %02d:00 to %02d:00",
      period$day_type, period$hour_start, period$hour_end
    ))

    enriched_period <- enriched |>
      filter(
        # "all" does not filter by day_type, only by hour
        (day_type == period$day_type | period$day_type == "all"),
        hour >= period$hour_start,
        hour < period$hour_end
      )

    if (nrow(enriched_period) == 0) {
      warning(sprintf(
        "No data for period %s, from %02d:00 to %02d:00",
        period$day_type, period$hour_start, period$hour_end
      ))
      next
    }

    station_metrics_period <- enriched_period |> aggregate_station_stats()
    out_path <- paste0(
      output_folder, "/station_metrics_",
      period$day_type, "_", period$hour_start, "-", period$hour_end, ".csv"
    )
    write.csv(station_metrics_period, out_path, row.names = FALSE)
  }
}

## Bird --------------------------------------------------------------------------
regions <- c("cascais", "lisboa")

for (r in regions) { # r = regions[[1]]
  message("\nProcessing Bird data for ", r)
  output_folder <- paste0("data/analysis/gbfs/bird_", r)
  if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

  # Load metadata for region
  folder <- paste0("data/collection/gbfs/bird_", r)
  # station_areas = st_read(paste0(folder, "/station_information_area.gpkg"))
  station_points <- st_read(paste0(folder, "/station_information.gpkg"))
  # nrow(station_points)
  # mapview(station_points, layer.name="Bird Station Points", col.regions="#ffcc66")
  # mapview(station_areas, ...) + mapview(station_points, ...)
  station_data <- read.csv(paste0(folder, "/station_information.csv"))
  # assertthat::are_equal(nrow(station_areas), nrow(station_points))
  # vehicle_types = read.csv(paste0(folder, "/vehicle_types.csv"))

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
    subfolder     = "station_status",
    date_start    = DATE_START,
    date_end      = DATE_END
  )

  # Enrich raw data
  station_status_collection_enriched <- station_status_collection |>
    rename(
      timestamp  = last_reported,
      free_bikes = num_bikes_available,
      free_docks = num_docks_available
    ) |>
    select(id, timestamp, free_bikes, free_docks, is_installed) |>
    aggregate_station_updates()
  # Raw collection no longer needed — free before aggregation
  rm(station_status_collection); invisible(gc())
  # nrow(station_status_collection_enriched)
  # summary(station_status_collection_enriched)
  # View(station_status_collection_enriched)

  # Aggregate and write per-period metrics
  write_period_metrics(station_status_collection_enriched, output_folder, PERIODS)

  # Save overall (unfiltered) metrics to disk
  station_metrics_all <- station_status_collection_enriched |> aggregate_station_stats()
  write.csv(station_metrics_all, paste0(output_folder, "/station_metrics_all.csv"), row.names = FALSE)

  # Free all large objects before next region iteration
  rm(station_points, station_data, stations_in_grid,
     station_status_collection_enriched, station_metrics_all)
  invisible(gc())
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
# nrow(station_points) # 202
# mapview(station_points, layer.name="GIRA Station Points", col.regions="#ffcc66")

# Associate station points with grid
stations_in_grid <- st_join(
  station_points |> select(id) |> rename(sid = id),
  grid |> select(gid),
  join = st_within
) |> st_drop_geometry()
write.csv(stations_in_grid, paste0(output_folder, "/stations_in_grid.csv"), row.names = FALSE)

# GIRA-specific file parser: derives the epoch timestamp from the file path (day/HHMMSS.csv)
parse_gira_file <- function(s) {
  day <- as.integer(gsub(".*/collection/station_information/(\\d+)/.*", "\\1", s))
  hhmmss <- gsub(".*station_information/.*/(\\d{6})\\.csv", "\\1", s)
  ts <- as.POSIXct(paste0(day, hhmmss), format = "%Y%m%d%H%M%S", tz = "Europe/Lisbon")
  read.csv(s) |> dplyr::mutate(timestamp = as.numeric(ts))
}

# Load data collection
station_status_collection <- load_station_collection(
  base_folder   = folder,
  subfolder     = "station_information",
  date_start    = DATE_START,
  date_end      = DATE_END,
  parse_file_fn = parse_gira_file
)

# Enrich raw data
station_status_collection_enriched <- station_status_collection |>
  rename(
    id         = serialNumber,
    free_bikes = bikes
  ) |>
  mutate(
    free_docks   = docks - free_bikes,
    is_installed = as.logical(assetStatus)
  ) |>
  select(id, timestamp, free_bikes, free_docks, is_installed) |>
  aggregate_station_updates()
# Raw collection no longer needed — free before aggregation
rm(station_status_collection); invisible(gc())
# nrow(station_status_collection_enriched)
# summary(station_status_collection_enriched)
# View(station_status_collection_enriched)

# Aggregate and write per-period metrics
write_period_metrics(station_status_collection_enriched, output_folder, PERIODS)

# Save overall (unfiltered) metrics to disk
station_metrics_all <- station_status_collection_enriched |> aggregate_station_stats()
write.csv(station_metrics_all, paste0(output_folder, "/station_metrics_all.csv"), row.names = FALSE)
