library(dplyr)
library(sf)
library(mapview)
library(httr)
library(jsonlite)

# Shared Setup & Boundary -----------------------------------
municipalities <- st_read("data/geo/municipalities_union.gpkg")
municipalities_wgs84 <- st_transform(municipalities, 4326)

# Query bikeindex.org -----------------------------------
# Docs: https://bikeindex.org/documentation/api_v3#!/search/GET_version_search_format_get_0

## > Define query parameters -----------------------------------------
bbox <- st_bbox(municipalities |> st_transform(3763))
bbox_width_km <- (bbox$xmax - bbox$xmin) / 1000
bbox_height_km <- (bbox$ymax - bbox$ymin) / 1000
bbox_width_miles <- bbox_width_km * 0.62137119
bbox_height_miles <- bbox_height_km * 0.62137119

distance_miles <- ceiling(max(bbox_width_miles, bbox_height_miles))
centroid_wgs84 <- st_centroid(municipalities) |> st_transform(4326)
coords <- st_coordinates(centroid_wgs84)
lat_center <- coords[2]
lon_center <- coords[1]

## > Query API -------------------------------------------
bikeindex_url <- "https://bikeindex.org/api/v3/search"
bikeindex_res <- GET(
    url = bikeindex_url,
    query = list(
        page = 1,
        per_page = 100,
        location = paste0(lat_center, ",", lon_center),
        distance = distance_miles,
        stolenness = "proximity"
    )
)

bikeindex_raw <- fromJSON(content(bikeindex_res, as = "text", encoding = "UTF-8"))$bikes
message(sprintf("Fetched %d stolen bikes from BikeIndex API", nrow(bikeindex_raw)))

## > Standardize BikeIndex sf object --------------------------------
bikes_bikeindex <- bikeindex_raw |>
    mutate(
        provider = "bikeindex",
        year = as.integer(year),
        lat = sapply(stolen_coordinates, function(x) if (is.null(x) || length(x) < 2) NA_real_ else x[[1]]),
        lon = sapply(stolen_coordinates, function(x) if (is.null(x) || length(x) < 2) NA_real_ else x[[2]])
    ) |>
    filter(!is.na(lat) & !is.na(lon)) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    st_filter(municipalities_wgs84, .predicate = st_within) |>
    select(-where(is.list))
# select(provider, bike_id, title, manufacturer, model, year, date_stolen)


# Query cicla.pt -----------------------------------

## > Helper to fetch missing coordinates from zip codes ------------------
fetch_coords_from_cp <- function(postal_code) {
    api_key <- Sys.getenv("CTT_CODIGO_POSTAL_API_KEY")
    if (api_key == "") {
        stop("CTT_CODIGO_POSTAL_API_KEY environment variable is not set.")
    }

    cp_clean <- trimws(postal_code)
    if (grepl("\\d{4}-\\d{3}", cp_clean)) {
        cp_clean <- regmatches(cp_clean, regexpr("\\d{4}-\\d{3}", cp_clean))
    } else if (grepl("\\b\\d{7}\\b", cp_clean)) {
        cp_raw <- regmatches(cp_clean, regexpr("\\b\\d{7}\\b", cp_clean))
        cp_clean <- paste0(substr(cp_raw, 1, 4), "-", substr(cp_raw, 5, 7))
    }

    if (!grepl("^\\d{4}-\\d{3}$", cp_clean)) {
        warning(sprintf("Invalid postal code format: '%s'", postal_code))
        return(c(lat = NA_real_, lon = NA_real_))
    }

    url <- sprintf("https://www.cttcodigopostal.pt/api/v1/%s/%s", api_key, cp_clean)
    res <- GET(url)

    if (status_code(res) != 200) {
        warning(sprintf("Failed to fetch coordinates for postal code %s (HTTP %d)", cp_clean, status_code(res)))
        return(c(lat = NA_real_, lon = NA_real_))
    }

    res_data <- fromJSON(content(res, as = "text", encoding = "UTF-8"))

    if (is.null(res_data) || length(res_data) == 0 || nrow(res_data) == 0) {
        warning(sprintf("No results returned for postal code %s", cp_clean))
        return(c(lat = NA_real_, lon = NA_real_))
    }

    if (nrow(res_data) > 1) {
        warning(sprintf("More than one result returned for postal code %s (%d results). Using the first result.", cp_clean, nrow(res_data)))
    }

    first_res <- res_data[1, ]
    return(c(lat = as.numeric(first_res$latitude), lon = as.numeric(first_res$longitude)))
}


## > Query API -------------------------------------------
cicla_url <- "https://api.cicla.pt/api/bikes?filters%5Bstolen%5D%5B$eq%5D=true&sort=createdAt:desc&populate=*"
cicla_res <- GET(url = cicla_url)

cicla_raw <- fromJSON(content(cicla_res, as = "text", encoding = "UTF-8"))
message(sprintf("Fetched %d stolen bikes from cicla.pt API", nrow(cicla_raw)))

# Fill missing coordinates using CTT postal code API
cicla_cp <- cicla_raw$stolen_details$postal
fetched_coords <- purrr::map2_dfr(cicla_raw$latitude, cicla_cp, function(lat_val, cp_val) {
    if ((is.na(lat_val) || lat_val == "" || lat_val == 0) && !is.na(cp_val) && cp_val != "") {
        coords <- fetch_coords_from_cp(cp_val)
        tibble(lat = coords["lat"], lon = coords["lon"])
    } else {
        tibble(lat = as.numeric(lat_val), lon = as.numeric(cicla_raw$longitude))
    }
})

## > Standardize Cicla.pt sf object --------------------------------
bikes_cicla <- cicla_raw |>
    mutate(
        provider = "cicla.pt",
        year = as.integer(substr(createdAt, 1, 4)),
        lat_clean = ifelse(is.na(latitude) | latitude == "", fetched_coords$lat, as.numeric(latitude)),
        lon_clean = ifelse(is.na(longitude) | longitude == "", fetched_coords$lon, as.numeric(longitude))
    ) |>
    filter(!is.na(lat_clean) & !is.na(lon_clean)) |>
    st_as_sf(coords = c("lon_clean", "lat_clean"), crs = 4326) |>
    st_filter(municipalities_wgs84, .predicate = st_within) |>
    select(-where(is.list))
# select(provider, bike_id, title, manufacturer, model, year, date_stolen)


# Combine All Stolen Bikes -----------------------------------
stolen_bikes_all <- bind_rows(bikes_bikeindex, bikes_cicla)

message(sprintf("Total combined stolen bikes in region: %d", nrow(stolen_bikes_all)))
mapview(stolen_bikes_all, zcol = "provider")

## > Aggregate at grid level -----------------------------------
grid_stolen <- grid |>
    st_transform(4326) |>
    st_join(stolen_bikes_all, join = st_intersects) |>
    group_by(gid) |>
    summarise(
        n_stolen = sum(!is.na(provider)),
        n_bikeindex = sum(provider == "bikeindex", na.rm = TRUE),
        n_cicla = sum(provider == "cicla.pt", na.rm = TRUE)
    )

mapview(grid_stolen |> filter(n_stolen > 0), zcol = "n_stolen")

write.csv(grid_stolen |> st_drop_geometry() |> filter(n_stolen > 0), "data/analysis/stolen/stolen_bikes_grid.csv", row.names = FALSE)
