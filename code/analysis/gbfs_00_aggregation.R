# Aggregates raw data collection for operators producing stats for each bike station (different periods)
# Run with: $ Rscript code/analysis/gbfs_00_aggregation.R
source("code/00_data_load.R")
source("code/00_helpers.R")

# Raw data aggregation --------------------------------------------------------------------------
DATE_START = 20260507
DATE_END = 20260715

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

## Utils --------------------------------------------------------------------------
aggregate_station_updates <- function(updates) {
  return(
    updates |>
      # Make sure id, timstamp are unique (preserve all columns for first match)
    distinct(id, timestamp, .keep_all = TRUE) |>
    filter(is_installed) |>
    select(-is_installed) |>
    arrange(id, timestamp) |>
    group_split(id) |>
    purrr::map_dfr(~ .x |>
      mutate(
        capacity = free_bikes + free_docks,
        occupation_ratio = round(free_bikes / capacity, 2),
        # compare to previous update
        changed = free_bikes != lag(free_bikes),
        bikes_rented = pmax(lag(free_bikes) - free_bikes, 0, na.rm = TRUE),
        bikes_returned = pmax(free_bikes - lag(free_bikes), 0, na.rm = TRUE),
        time_since_previous = as.numeric(difftime(timestamp, lag(timestamp), units = "secs"))
      )
    ) |>
    filter(time_since_previous < 5*60) |>
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
  )
}
aggregate_station_stats <- function(stats) {
  return(
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
      !!!analyse_col_dispersion(free_bikes, measures_extra="cv"),
      # Free docks metrics
      !!!analyse_col_dispersion(free_docks, measures_extra="cv"),
      # Occupation ratio metrics
      !!!analyse_col_dispersion(occupation_ratio, measures_extra="cv")
    )
  )
} 

## Bird --------------------------------------------------------------------------
regions = c("cascais", "lisboa")

for (r in regions) { # r = regions[[1]]
  message("\nProcessing Bird data for ", r)
  output_folder = paste0("data/analysis/gbfs/bird_", r)
  if(!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }

  # Load metadata for region
  folder <- paste0("data/collection/gbfs/bird_", r)
  # station_areas = st_read(paste0(folder, "/station_information_area.gpkg"))
  station_points = st_read(paste0(folder, "/station_information.gpkg"))
  # nrow(station_areas)
  # nrow(station_points)
  # mapview(station_points, layer.name="Bird Station Points", col.regions="#ffcc66")
  # mapview(station_areas, layer.name="Bird Station Areas", col.regions="#00cc99") + mapview(station_points, layer.name="Bird Station Points", col.regions="#ffcc66")
  station_data = read.csv(paste0(folder, "/station_information.csv"))
  # assertthat::are_equal(nrow(station_areas), nrow(station_points))
  # vehicle_types = read.csv(paste0(folder, "/vehicle_types.csv"))

  # Associate station points with grid
  stations_in_grid = st_join(
    station_points |> select(id) |> rename(sid = id), 
    grid |> select(gid), 
    join = st_within
  ) |> st_drop_geometry()
  write.csv(stations_in_grid, paste0(output_folder, "/stations_in_grid.csv"), row.names = FALSE)
    
  # Load data collection
  station_status_folders = list.dirs(paste0(folder, "/collection/station_status"), recursive = FALSE)
  station_status_collection = data.frame()
  for (f in station_status_folders) { # f = station_status_folders[[2]]
    station_status_files = list.files(f, pattern = "*.csv", full.names = TRUE)
    day = as.integer(gsub(".*station_status/(.*)", "\\1", f))
    if (day < DATE_START | day > DATE_END) {
      message(sprintf("Skipping station status for day %s, with %d files", day, length(station_status_files)))
      next
    }
    message(sprintf("Processing station status for day %s, with %d files", day, length(station_status_files)))
    for (s in station_status_files) { # s = station_status_files[[1]]
      # message(sprintf("Processing station status file: %s for hour %s", s, hour))
      station_status = read.csv(s)
      station_status_collection = rbind(station_status_collection, station_status)
    }
  }

  # Enrich raw data 
  # nrow(station_status_collection)
  station_status_collection_enriched = station_status_collection |> 
    rename(
      timestamp = last_reported,
      free_bikes = num_bikes_available,
      free_docks = num_docks_available
    ) |>
    select(id, timestamp, free_bikes, free_docks, is_installed) |>
    aggregate_station_updates()
  # nrow(station_status_collection_enriched)
  # summary(station_status_collection_enriched)
  # View(station_status_collection_enriched)

  # Aggregate data
  for (p in 1:nrow(PERIODS)) { # p = 1
    period = PERIODS[p, ]
    message(sprintf("Aggregating data for period %s, from %02d:00 to %02d:00", period$day_type, period$hour_start, period$hour_end))
    station_status_collection_enriched_period = station_status_collection_enriched |>
      filter(
        # Consider "all" should not filter by day_type, but only by hour
        (day_type == period$day_type | period$day_type == "all"),
        hour >= period$hour_start,
        hour < period$hour_end
      )
    # nrow(station_status_collection_enriched_period)
    if (nrow(station_status_collection_enriched_period) == 0) {
      warning(sprintf("No data for period %s, from %02d:00 to %02d:00", period$day_type, period$hour_start, period$hour_end))
      next
    }
    # summary(station_status_collection_enriched_period)
    # View(station_status_collection_enriched_period)

    # Aggregate station metrics
    station_metrics_period = station_status_collection_enriched_period |> aggregate_station_stats()
    # View(station_metrics_period)
    write.csv(station_metrics_period, paste0(output_folder, "/station_metrics_", period$day_type, "_", period$hour_start, "-", period$hour_end, ".csv"), row.names = FALSE)
  }

  station_metrics = station_status_collection_enriched |> aggregate_station_stats()
  # View(station_metrics) 
}


## GIRA --------------------------------------------------------------------------
message("\nProcessing GIRA data")
output_folder = paste0("data/analysis/gbfs/emel_gira")
if(!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# Load metadata for region
folder <- paste0("data/collection/gbfs/emel_gira")
station_points = read.csv(paste0(folder, "/stations_information.csv")) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  rename(id = serialNumber)
# nrow(station_points) # 202
# mapview(station_points, layer.name="Bird Station Points", col.regions="#ffcc66")

# Associate station points with grid
stations_in_grid = st_join(
  station_points |> select(id) |> rename(sid = id), 
  grid |> select(gid), 
  join = st_within
) |> st_drop_geometry()
write.csv(stations_in_grid, paste0(output_folder, "/stations_in_grid.csv"), row.names = FALSE)
  
# Load data collection
station_status_folders = list.dirs(paste0(folder, "/collection/station_information"), recursive = FALSE)
station_status_collection = data.frame()
for (f in station_status_folders) { # f = station_status_folders[[2]]
  station_status_files = list.files(f, pattern = "*.csv", full.names = TRUE)
  day = as.integer(gsub(".*station_information/(.*)", "\\1", f))
  if (day < DATE_START | day > DATE_END) {
    message(sprintf("Skipping station status for day %s, with %d files", day, length(station_status_files)))
    next
  }
  message(sprintf("Processing station status for day %s, with %d files", day, length(station_status_files)))
  for (s in station_status_files) { # s = station_status_files[[1]]
    # message(sprintf("Processing station status file: %s for hour %s", s, hour))
    hhmmss = gsub(".*station_information/.*/(\\d{6})\\.csv", "\\1", s)
    timestamp = as.POSIXct(paste0(day, hhmmss), format = "%Y%m%d%H%M%S", tz = "Europe/Lisbon")
    station_status = read.csv(s) |> mutate(
      timestamp = as.numeric(timestamp)
    )
    station_status_collection = rbind(station_status_collection, station_status)
  }
}

# Enrich raw data 
# nrow(station_status_collection)
station_status_collection_enriched = station_status_collection |> 
  rename(
    id = serialNumber,
    free_bikes = bikes
  ) |>
  mutate(
    free_docks = docks - free_bikes,
    is_installed = as.logical(assetStatus)
) |>
  select(id, timestamp, free_bikes, free_docks, is_installed) |>
  aggregate_station_updates()
# nrow(station_status_collection_enriched)
# summary(station_status_collection_enriched)
# View(station_status_collection_enriched)

# Aggregate data
for (p in 1:nrow(PERIODS)) { # p = 1
  period = PERIODS[p, ]
  message(sprintf("Aggregating data for period %s, from %02d:00 to %02d:00", period$day_type, period$hour_start, period$hour_end))
  station_status_collection_enriched_period = station_status_collection_enriched |>
    filter(
      # Consider "all" should not filter by day_type, but only by hour
      (day_type == period$day_type | period$day_type == "all"),
      hour >= period$hour_start,
      hour < period$hour_end
    )
  # nrow(station_status_collection_enriched_period)
  if (nrow(station_status_collection_enriched_period) == 0) {
    warning(sprintf("No data for period %s, from %02d:00 to %02d:00", period$day_type, period$hour_start, period$hour_end))
    next
  }
  # summary(station_status_collection_enriched_period)
  # View(station_status_collection_enriched_period)

  # Aggregate station metrics
  station_metrics_period = station_status_collection_enriched_period |> aggregate_station_stats()
  # View(station_metrics_period)
  write.csv(station_metrics_period, paste0(output_folder, "/station_metrics_", period$day_type, "_", period$hour_start, "-", period$hour_end, ".csv"), row.names = FALSE)
}

station_metrics = station_status_collection_enriched |> aggregate_station_stats()
# View(station_metrics) 