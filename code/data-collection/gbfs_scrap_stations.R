regions <- data.frame(
    name = character(),
    base_folder = character(),
    url_base = character()
)
regions <- regions |> bind_rows(data.frame(
    name = "Bird Lisboa",
    base_folder = "data/collection/gbfs/bird_lisboa",
    url_base = "https://mds.bird.co/gbfs/v2/public/lisbon/"
))
regions <- regions |> bind_rows(data.frame(
    name = "Bird Cascais",
    base_folder = "data/collection/gbfs/bird_cascais",
    url_base = "https://mds.bird.co/gbfs/v2/public/cascais/"
))

library(dplyr)
library(sf)
library(mapview)
library(geojsonsf)


station_info <- read.csv(paste0(base_folder, "/station_information.csv")) |> select(id, station_id)
vehicle_types <- read.csv(paste0(base_folder, "/vehicle_types.csv")) |> select(id, vehicle_type_id)


for (i in 1:nrow(regions)) {
    base_folder <- regions$base_folder[i]
    url_base <- regions$url_base[i]
    name <- regions$name[i]


    message(sprintf("Scrapping %s station info...", name))
    # 1. Get station status
    station_status <- jsonlite::fromJSON(paste0(url_base, "station_status.json"))$data$stations |>
        left_join(station_info, by = "station_id") |>
        # Fallback to avoid missing id of new stations
        mutate(station_id_fallback = ifelse(is.na(id), station_id, NA)) |>
        # Unlist vehicle_types_available, creating one row per each
        tidyr::unnest_longer(vehicle_types_available, values_to = "vehicle") |>
        tidyr::unnest_wider(vehicle, names_sep = "_") |>
        # Associate with vehicle_types
        dplyr::left_join(vehicle_types |> rename(vehicle_id = id), by = c("vehicle_vehicle_type_id" = "vehicle_type_id")) |>
        # Remove GBFS ids
        dplyr::select(-station_id, -vehicle_vehicle_type_id) |>
        # Pivot wider to get columns for each vehicle type count
        tidyr::pivot_wider(
            names_from = vehicle_id,
            values_from = vehicle_count,
            names_glue = "vehicle_{vehicle_id}_count",
            values_fill = list(vehicle_count = 0)
        ) |>
        dplyr::select(id, dplyr::everything()) # Make sure id is first column

    # nrow(station_status)
    # names(station_status)
    # summary(station_status)
    # View(station_status |> sample_n(10))

    # 2. Store data ------------------------------------------------------------
    message("Saving data...")
    date <- Sys.Date()
    store_folder <- paste(base_folder, "/", "collection/station_status/", gsub("-", "", date), sep = "")
    if (!dir.exists(store_folder)) dir.create(store_folder, recursive = TRUE)

    write.csv(station_status, sprintf("%s/%s.csv", store_folder, format(Sys.time(), "%H%M%S")), row.names = FALSE)

    message("Done! :)\n")
}
