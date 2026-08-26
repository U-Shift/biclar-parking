# Set up cron: $ crontab -e
# Add the following line: * * * * * cd /home/goncalo/Repos/biclar-parking && Rscript /home/goncalo/Repos/biclar-parking/code/data-collection/gbfs_scrap_bikes.R
# To validate: $ crontab -l

# Count files gathered per day: $ find data/collection/gbfs/bird_lisboa/collection/free_bike_status/ -mindepth 1 -maxdepth 1 -type d -exec sh -c 'printf "%s: %d files\n" "$1" $(find "$1" -maxdepth 1 -type f | wc -l)' _ {} \; | sort

# Count files gathered per day: $ find data/collection/gbfs/bird_cascais/collection/free_bike_status/ -mindepth 1 -maxdepth 1 -type d -exec sh -c 'printf "%s: %d files\n" "$1" $(find "$1" -maxdepth 1 -type f | wc -l)' _ {} \; | sort


# Script

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
regions <- regions |> bind_rows(data.frame(
    name = "Bird Almada",
    base_folder = "data/collection/gbfs/bird_almada",
    url_base = "https://mds.bird.co/gbfs/v2/public/almada-portugal/"
))
regions <- regions |> bind_rows(data.frame(
    name = "Lime Almada",
    base_folder = "data/collection/gbfs/lime_almada",
    url_base = "https://data.lime.bike/api/partners/v1/gbfs/almada/"
))
regions <- regions |> bind_rows(data.frame(
    name = "Lime Lisboa",
    base_folder = "data/collection/gbfs/lime_lisboa",
    url_base = "https://data.lime.bike/api/partners/v1/gbfs/lisbon/"
))
regions <- regions |> bind_rows(data.frame(
    name = "Lime Oeiras",
    base_folder = "data/collection/gbfs/lime_oeiras",
    url_base = "https://data.lime.bike/api/partners/v1/gbfs/Oeiras/"
))


for (i in 1:nrow(regions)) { # i = 3
    base_folder <- regions$base_folder[i]
    if (!dir.exists(base_folder)) dir.create(base_folder, recursive = TRUE)
    url_base <- regions$url_base[i]
    name <- regions$name[i]

    message(sprintf("Scrapping %s bike info...", name))

    vehicle_types <- read.csv(paste0(base_folder, "/vehicle_types.csv")) |>
        select(any_of(c("id", "vehicle_type_id", "vehicle_type")))

    # 1. Get station status
    free_bike_status <- jsonlite::fromJSON(paste0(url_base, "free_bike_status.json"))$data$bikes
    if ("vehicle_type_id" %in% names(free_bike_status)) {
        free_bike_status <- free_bike_status |>
            left_join(vehicle_types |> rename(vehicle_type = id), by = "vehicle_type_id")
    } else if ("vehicle_type" %in% names(free_bike_status)) {
        free_bike_status <- free_bike_status |>
            left_join(vehicle_types, by = "vehicle_type") |>
            select(-vehicle_type) |>
            rename(vehicle_type = id)
    }

    free_bike_status <- free_bike_status |>
        # Remove GBFS ids
        select(lat, lon, vehicle_type)

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
