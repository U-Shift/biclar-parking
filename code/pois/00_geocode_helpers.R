# Geocoding helpers for Schools and Universities (POI)
# Uses OSM (Nominatim) -> ArcGIS -> Google Maps Geocoding API fallback chain.
#
# To use Google Maps Geocoding as a fallback (3rd resource):
# 1. Go to Google Cloud Console (https://console.cloud.google.com/) and create/select a project.
# 2. Enable billing (Google provides a recurring $200 monthly credit, covering ~40,000 requests/month).
# 3. Enable the "Geocoding API" under APIs & Services > Library.
# 4. Create an API key under APIs & Services > Credentials (optionally restrict it to "Geocoding API").
# 5. Add the key to your environment: run `usethis::edit_r_environ()` or create ~/.Renviron or .Renviron:
#    GOOGLE_MAPS_KEY="AIzaSy..."
# 6. Restart your R session so Sys.getenv("GOOGLE_MAPS_KEY") picks it up.

library(dplyr)
library(tibble)
library(logr)

# 1. Query OSM (Nominatim)
query_osm <- function(name, municipio, amenity_types = c("school", "college", "kindergarten", "university")) {
    Sys.sleep(1) # Respect Nominatim 1 req/sec rate limit
    q <- paste(name, municipio, "Portugal", sep = ", ")

    res <- tryCatch(
        suppressWarnings(osmdata::getbb(q, format_out = "data.frame")),
        error = function(e) NULL
    )

    if (is.null(res) || !is.data.frame(res) || nrow(res) == 0) {
        return(NULL)
    }

    # Filter to school / university amenities to avoid false positives (e.g. bus stops or street names)
    if ("class" %in% names(res) && any(res$class == "amenity")) {
        res <- res |> filter(class == "amenity", type %in% amenity_types)
    }

    # If multiple matches exist, prefer 'node' over 'way' / 'relation' when it resolves unambiguously
    if (nrow(res) > 1 && "osm_type" %in% names(res) && any(res$osm_type == "node")) {
        res_nodes <- res |> filter(osm_type == "node")
        if (nrow(res_nodes) == 1) {
            res <- res_nodes
        }
    }

    # Only keep if there is a single unambiguous result
    if (nrow(res) == 1) {
        logr::log_info(sprintf("OSM match found for %s in %s", name, municipio))
        return(tibble(
            lat = as.numeric(res$lat[1]),
            lon = as.numeric(res$lon[1])
        ))
    }
    NULL
}

# 2. Query ArcGIS Geocoding REST API (Fallback 1)
query_arcgis <- function(name, municipio) {
    Sys.sleep(0.2)
    q <- paste(name, municipio, "Portugal", sep = ", ")
    url <- paste0(
        "https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/findAddressCandidates?SingleLine=",
        URLencode(q),
        "&f=json&maxLocations=1&outFields=Addr_type,Type,Score,Match_addr"
    )

    res <- tryCatch(
        jsonlite::fromJSON(url),
        error = function(e) NULL
    )

    if (!is.null(res$candidates) && is.data.frame(res$candidates) && nrow(res$candidates) > 0) {
        cand <- res$candidates[1, ]
        addr_type <- if ("attributes" %in% names(cand) && "Addr_type" %in% names(cand$attributes)) {
            cand$attributes$Addr_type[1]
        } else if ("attributes.Addr_type" %in% names(cand)) {
            cand$attributes.Addr_type[1]
        } else {
            ""
        }
        score <- cand$score[1]

        # Only accept confident POI or street address matches (excludes broad municipality/locality matches)
        if (score >= 80 && addr_type %in% c("POI", "PointAddress", "StreetAddress")) {
            logr::log_info(sprintf("ArcGIS match found for %s in %s (score: %.1f, type: %s)",
                                   name, municipio, score, addr_type))
            return(tibble(
                lat = as.numeric(cand$location$y[1]),
                lon = as.numeric(cand$location$x[1])
            ))
        }
    }
    NULL
}

# 3. Query Google Maps Geocoding API (Fallback 2)
# Requires GOOGLE_MAPS_KEY environment variable.
# Returns NULL if no key is set or if location cannot be resolved.
query_google <- function(name, municipio, api_key = Sys.getenv("GOOGLE_MAPS_KEY")) {
    if (api_key == "") {
        return(NULL)
    }

    Sys.sleep(0.1) # Respect Google rate limits
    q <- paste(name, municipio, "Portugal", sep = ", ")
    url <- paste0(
        "https://maps.googleapis.com/maps/api/geocode/json?address=",
        URLencode(q),
        "&key=", api_key
    )

    res <- tryCatch(
        jsonlite::fromJSON(url),
        error = function(e) NULL
    )

    if (!is.null(res) && !is.null(res$status) && res$status == "OK" && length(res$results) > 0) {
        types <- unlist(res$results$types[1])
        # Avoid broad locality/administrative fallbacks when specific place isn't found
        is_broad <- any(types %in% c("locality", "administrative_area_level_1", "administrative_area_level_2")) &&
            !any(types %in% c("point_of_interest", "school", "establishment", "premise", "subpremise", "university"))
        
        if (!is_broad) {
            logr::log_info(sprintf("Google Maps match found for %s in %s", name, municipio))
            return(tibble(
                lat = as.numeric(res$results$geometry$location$lat[1]),
                lon = as.numeric(res$results$geometry$location$lng[1])
            ))
        }
    }
    NULL
}

# 4. Composite geocoder with fallback chain
geocode_poi <- function(name, municipio, amenity_types = c("school", "college", "kindergarten", "university")) {
    # 1. Try OSM first
    res <- query_osm(name, municipio, amenity_types = amenity_types)
    if (!is.null(res)) return(res)

    # 2. Fallback to ArcGIS
    res_arcgis <- query_arcgis(name, municipio)
    if (!is.null(res_arcgis)) return(res_arcgis)

    # 3. Fallback to Google Maps
    res_google <- query_google(name, municipio)
    if (!is.null(res_google)) return(res_google)

    logr::log_warning(sprintf("No match found (OSM, ArcGIS & Google) for %s in %s", name, municipio))
    NULL
}

# Backward compatibility aliases for existing code
query_osm_school <- query_osm
query_arcgis_school <- query_arcgis
query_google_school <- query_google
geocode_school <- geocode_poi
