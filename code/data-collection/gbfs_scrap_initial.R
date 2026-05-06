# Lisbon: https://mobilitydatabase.org/feeds/gbfs/gbfs-bikr-lisbon
# base_folder <- "data/collection/gbfs/bird_lisboa"
# url_base <- "https://mds.bird.co/gbfs/v2/public/lisbon/"

# Cascais https://mobilitydatabase.org/feeds/gbfs/gbfs-bird-cascais
base_folder <- "data/collection/gbfs/bird_cascais"
url_base <- "https://mds.bird.co/gbfs/v2/public/cascais/"

if (!dir.exists(base_folder)) dir.create(base_folder, recursive = TRUE)

library(dplyr)
library(sf)
library(mapview)
library(geojsonsf)

# 1. Get vehicle types ------------------------------------------------------------
vehicle_types <- jsonlite::fromJSON(paste0(url_base, "vehicle_types.json"))$data$vehicle_types |> mutate(id = row_number())
write.csv(vehicle_types, paste0(base_folder, "/vehicle_types.csv"), row.names = FALSE)

# 2. Get stations information -----------------------------------------------------
station_information <- jsonlite::fromJSON(paste0(url_base, "station_information.json"))$data$stations |> mutate(id = row_number())
write.csv(station_information |> select(-lat, -lon, -station_area), paste0(base_folder, "/station_information.csv"), row.names = FALSE)

station_information_sf <- station_information |> st_as_sf(coords = c("lon", "lat"), crs = 4326)
# mapview::mapview(station_information_sf, zcol="is_virtual_station")
st_write(station_information_sf |> select(id), paste0(base_folder, "/station_information.gpkg"))
st_write(station_information_sf |> select(id), paste0(base_folder, "/station_information.geojson"))

station_area_json <- jsonlite::toJSON(station_information$station_area, auto_unbox = TRUE)
station_area_sfc <- geojsonsf::geojson_sfc(station_area_json)
station_information_sf_area <- station_information |> st_set_geometry(station_area_sfc)
# mapview::mapview(station_information_sf_area)
st_write(station_information_sf_area |> select(id), paste0(base_folder, "/station_information_area.gpkg"))
st_write(station_information_sf_area |> select(id), paste0(base_folder, "/station_information_area.geojson"))


# 3. Geofencing zones ------------------------------------------------------------
geofencing_zones <- jsonlite::fromJSON(paste0(url_base, "geofencing_zones.json"))$data$geofencing_zones$features |>
    mutate(id = row_number()) |>
    # Unnest properties$rules to their own columns (without prefix)
    tidyr::unnest_wider(properties, names_sep = "_") |>
    mutate(properties_rules = properties_rules[[1]]) |>
    tidyr::unnest_wider(properties_rules, , names_sep = "_") |>
    dplyr::rename_with(~ stringr::str_remove(., "properties_rules_"), dplyr::starts_with("properties_rules_"))
write.csv(geofencing_zones |> select(-geometry), paste0(base_folder, "/geofencing_zones.csv"), row.names = FALSE)

summary(geofencing_zones |> select(-geometry))
names(geofencing_zones)
# View((geofencing_zones |> sample_n(1)))


geofencing_zones_json <- jsonlite::toJSON(geofencing_zones$geometry)
geofencing_zones_sfc <- geojsonsf::geojson_sfc(geofencing_zones_json)
geofencing_zones_sf <- geofencing_zones |> st_set_geometry(geofencing_zones_sfc)

mapview(geofencing_zones_sf, zcol = "ride_allowed")
mapview(geofencing_zones_sf, zcol = "ride_through_allowed")
mapview(geofencing_zones_sf, zcol = "station_parking")

st_write(geofencing_zones_sf |> select(id), paste0(base_folder, "/geofencing_zones.gpkg"))
st_write(geofencing_zones_sf |> select(id), paste0(base_folder, "/geofencing_zones.geojson"))
