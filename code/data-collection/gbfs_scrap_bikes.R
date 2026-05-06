library(dplyr)
library(sf)
library(mapview)
library(geojsonsf)

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



for (i in 1:nrow(regions)) {
    base_folder <- regions$base_folder[i]
    url_base <- regions$url_base[i]
    name <- regions$name[i]

    message(sprintf("Scrapping %s bike info...", name))

    vehicle_types <- read.csv(paste0(base_folder, "/vehicle_types.csv")) |>
        select(id, vehicle_type_id)

    # 1. Get station status
    free_bike_status <- jsonlite::fromJSON(paste0(url_base, "free_bike_status.json"))$data$bikes |>
        left_join(vehicle_types |> rename(vehicle_type = id), by = "vehicle_type_id") |>
        # Remove GBFS ids
        select(-bike_id, -vehicle_type_id) |>
        mutate(
            current_range_km = round(current_range_meters / 1000, 2),
            current_fuel_percent = round(current_fuel_percent, 2)
        ) |>
        select(-current_range_meters)

    # summary(free_bike_status)
    # nrow(free_bike_status)
    # names(free_bike_status)
    # View(free_bike_status |> sample_n(10))


    # 2. Store data ------------------------------------------------------------
    message("Saving data...")
    date <- Sys.Date()
    store_folder <- paste(base_folder, "/", "collection/free_bike_status/", gsub("-", "", date), sep = "")
    if (!dir.exists(store_folder)) dir.create(store_folder, recursive = TRUE)

    write.csv(free_bike_status, sprintf("%s/%s.csv", store_folder, format(Sys.time(), "%H%M%S")), row.names = FALSE)

    message("Done! :)\n")
}
