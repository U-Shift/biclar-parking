library(dplyr)
library(sf)
library(mapview)
library(geojsonsf)

regions <- data.frame(
    name = character(),
    base_folder = character(),
    url_base = character(),
    vehicle_types = I(list())
)

# BIRD Lisbon: https://mobilitydatabase.org/feeds/gbfs/gbfs-bikr-lisbon
regions <- regions |> bind_rows(data.frame(
    name = "Bird Lisboa",
    base_folder = "data/collection/gbfs/bird_lisboa",
    url_base = "https://mds.bird.co/gbfs/v2/public/lisbon/"
))

# BIRD Cascais https://mobilitydatabase.org/feeds/gbfs/gbfs-bird-cascais
regions <- regions |> bind_rows(data.frame(
    name = "Bird Cascais",
    base_folder = "data/collection/gbfs/bird_cascais",
    url_base = "https://mds.bird.co/gbfs/v2/public/cascais/"
))

# BIRD Almada: https://mds.bird.co/gbfs/v2/public/almada-portugal/gbfs.json
regions <- regions |> bind_rows(data.frame(
    name = "Bird Almada",
    base_folder = "data/collection/gbfs/bird_almada",
    url_base = "https://mds.bird.co/gbfs/v2/public/almada-portugal/"
))

# LIME Almada: https://data.lime.bike/api/partners/v1/gbfs/almada/gbfs.json
regions <- regions |> bind_rows(data.frame(
    name = "Lime Almada",
    base_folder = "data/collection/gbfs/lime_almada",
    vehicle_types = I(list(c("bike", "scooter")))
))

# LIME Lisbon: https://data.lime.bike/api/partners/v1/gbfs/lisbon/gbfs.json
regions <- regions |> bind_rows(data.frame(
    name = "Lime Lisbon",
    base_folder = "data/collection/gbfs/lime_lisboa",
    vehicle_types = I(list(c("bike", "scooter")))
))

# LIME Oeiras: https://data.lime.bike/api/partners/v1/gbfs/Oeiras/gbfs.json
regions <- regions |> bind_rows(data.frame(
    name = "Lime Oeiras",
    base_folder = "data/collection/gbfs/lime_oeiras",
    vehicle_types = I(list(c("bike", "scooter")))
))

for (i in 1:nrow(regions)) { # i = 1
    base_folder <- regions$base_folder[i]
    if (!dir.exists(base_folder)) dir.create(base_folder, recursive = TRUE)
    url_base <- regions$url_base[i]
    name <- regions$name[i]

    message(sprintf("Processing %s initial info...", name))

    # 1. Get vehicle types ------------------------------------------------------------
    if (is.na(url_base) || url_base == "") {
        vec <- unlist(regions$vehicle_types[[i]])
        vehicle_types <- data.frame(
            vehicle_type = vec,
            form_factor = vec,
            id = seq_along(vec)
        )
        write.csv(vehicle_types, paste0(base_folder, "/vehicle_types.csv"), row.names = FALSE)
        next
    }

    vehicle_types <- jsonlite::fromJSON(paste0(url_base, "vehicle_types.json"))$data$vehicle_types |> mutate(id = row_number())
    write.csv(vehicle_types, paste0(base_folder, "/vehicle_types.csv"), row.names = FALSE)

    # 2. Get stations information -----------------------------------------------------
    station_information <- jsonlite::fromJSON(paste0(url_base, "station_information.json"))$data$stations
    if (length(station_information) > 0) {
        station_information <- station_information |> mutate(id = row_number())
        write.csv(station_information |> select(-lat, -lon, -station_area), paste0(base_folder, "/station_information.csv"), row.names = FALSE)

        station_information_sf <- station_information |> st_as_sf(coords = c("lon", "lat"), crs = 4326)
        # mapview::mapview(station_information_sf, zcol="is_virtual_station")
        st_write(station_information_sf |> select(id), paste0(base_folder, "/station_information.gpkg"), delete_dsn = TRUE)
        st_write(station_information_sf |> select(id), paste0(base_folder, "/station_information.geojson"), delete_dsn = TRUE)

        station_area_json <- jsonlite::toJSON(station_information$station_area, auto_unbox = TRUE)
        station_area_sfc <- geojsonsf::geojson_sfc(station_area_json)
        station_information_sf_area <- station_information |> st_set_geometry(station_area_sfc)
        # mapview::mapview(station_information_sf_area)
        st_write(station_information_sf_area |> select(id), paste0(base_folder, "/station_information_area.gpkg"), delete_dsn = TRUE)
        st_write(station_information_sf_area |> select(id), paste0(base_folder, "/station_information_area.geojson"), delete_dsn = TRUE)
    }

    # 3. Geofencing zones ------------------------------------------------------------
    geofencing_zones <- jsonlite::fromJSON(paste0(url_base, "geofencing_zones.json"))$data$geofencing_zones$features |>
        mutate(id = row_number()) |>
        # Unnest properties$rules to their own columns (without prefix)
        tidyr::unnest_wider(properties, names_sep = "_") |>
        mutate(properties_rules = properties_rules[[1]]) |>
        tidyr::unnest_wider(properties_rules, , names_sep = "_") |>
        dplyr::rename_with(~ stringr::str_remove(., "properties_rules_"), dplyr::starts_with("properties_rules_"))
    write.csv(geofencing_zones |> select(-geometry), paste0(base_folder, "/geofencing_zones.csv"), row.names = FALSE)

    # summary(geofencing_zones |> select(-geometry))
    # names(geofencing_zones)
    # View((geofencing_zones |> sample_n(1)))


    geofencing_zones_json <- jsonlite::toJSON(geofencing_zones$geometry)
    geofencing_zones_sfc <- geojsonsf::geojson_sfc(geofencing_zones_json)
    geofencing_zones_sf <- geofencing_zones |> st_set_geometry(geofencing_zones_sfc)

    # mapview(geofencing_zones_sf, zcol = "ride_allowed")
    # mapview(geofencing_zones_sf, zcol = "ride_through_allowed")
    # mapview(geofencing_zones_sf, zcol = "station_parking")

    st_write(geofencing_zones_sf |> select(id), paste0(base_folder, "/geofencing_zones.gpkg"), delete_dsn = TRUE)
    st_write(geofencing_zones_sf |> select(id), paste0(base_folder, "/geofencing_zones.geojson"), delete_dsn = TRUE)
}
