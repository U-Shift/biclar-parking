# Set up cron: $ crontab -e
# Add the following line: * * * * * cd /home/goncalo/Repos/biclar-parking && Rscript /home/goncalo/Repos/biclar-parking/code/data-collection/emel_gira.R
# To validate: $ crontab -l

# Count files gathered per day: $ find data/collection/gbfs/emel_gira/collection/station_information/ -mindepth 1 -maxdepth 1 -type d -exec sh -c 'printf "%s: %d files\n" "$1" $(find "$1" -maxdepth 1 -type f | wc -l)' _ {} \; | sort

# Script

# Load required libraries
library(httr2)
library(jsonlite)
library(dplyr)

# --- Configuration ---
# Updated backend: GIRA switched from deprecated EMEL GraphQL service to Vaimoo Firestore API
firestore_project <- "vaimoorotterdam"
firestore_api_key <- "AIzaSyAmKfHdjYUhzYmg7qSZtRwwYE92HQQlmJ4"
gira_tenant <- "P1/EML/EML/"
firestore_url <- sprintf(
    "https://firestore.googleapis.com/v1/projects/%s/databases/(default)/documents:runQuery?key=%s",
    firestore_project,
    firestore_api_key
)

# --- Retrieve Stations via Vaimoo Firestore API ---
message("Fetching GIRA stations data from Vaimoo Firestore API...")

firestore_query <- list(
    structuredQuery = list(
        from = list(list(collectionId = "docking-stations")),
        where = list(
            fieldFilter = list(
                field = list(fieldPath = "Tenant"),
                op = "EQUAL",
                value = list(stringValue = gira_tenant)
            )
        )
    )
)

stations_req <- request(firestore_url) %>%
    req_method("POST") %>%
    req_headers(`Content-Type` = "application/json") %>%
    req_body_json(firestore_query)

decode_firestore_value <- function(val) {
    if (is.null(val)) return(NA)
    if (!is.null(val$stringValue)) return(val$stringValue)
    if (!is.null(val$integerValue)) return(as.numeric(val$integerValue))
    if (!is.null(val$doubleValue)) return(as.numeric(val$doubleValue))
    if (!is.null(val$booleanValue)) return(val$booleanValue)
    if (!is.null(val$geoPointValue)) return(val$geoPointValue)
    if (!is.null(val$timestampValue)) return(val$timestampValue)
    return(NA)
}

decode_firestore_doc <- function(doc) {
    fields <- doc$fields
    if (is.null(fields)) return(NULL)

    code <- decode_firestore_value(fields$DockingStationId)
    name <- decode_firestore_value(fields$Name)
    is_active <- decode_firestore_value(fields$IsActive)
    service_status <- decode_firestore_value(fields$ServiceStatus)
    available_bikes <- decode_firestore_value(fields$AvailableBikes)
    dock_limit <- decode_firestore_value(fields$DockLimit)
    free_docks <- decode_firestore_value(fields$FreeDocks)

    loc <- fields$Location$geoPointValue
    lat <- if (!is.null(loc$latitude)) loc$latitude else NA_real_
    lon <- if (!is.null(loc$longitude)) loc$longitude else NA_real_

    street <- decode_firestore_value(fields$Street)
    street_id <- decode_firestore_value(fields$StreetBuildingIdentifier)
    city <- decode_firestore_value(fields$City)

    desc_parts <- c(street_id, street, city)
    desc_parts <- desc_parts[!is.na(desc_parts) & desc_parts != ""]
    description <- paste(desc_parts, collapse = " ")

    asset_status <- ifelse(isTRUE(is_active) & (!is.na(service_status) & service_status == "AVAILABLE"), "active", "repair")

    tibble::tibble(
        code = as.character(code),
        description = description,
        latitude = lat,
        longitude = lon,
        name = as.character(name),
        bikes = as.integer(max(0, available_bikes, na.rm = TRUE)),
        docks = as.integer(max(0, dock_limit, na.rm = TRUE)),
        freeDocks = as.integer(max(0, free_docks, na.rm = TRUE)),
        serialNumber = as.character(code),
        assetStatus = asset_status
    )
}

# Perform Firestore request and parse into data.frame
tryCatch(
    {
        stations_resp <- req_perform(stations_req)
        raw_data <- resp_body_json(stations_resp)

        stations_list <- purrr::map(raw_data, ~ if (!is.null(.x$document)) decode_firestore_doc(.x$document) else NULL)
        stations_df <- dplyr::bind_rows(stations_list)

        if (is.null(stations_df) || !is.data.frame(stations_df) || nrow(stations_df) == 0) {
            stop("Failed to parse stations into a data.frame from the response.")
        }

        message(sprintf("Successfully retrieved %d stations!", nrow(stations_df)))

        # Export/make stations_df available (if running interactively)
        # or assign it to the global environment
        assign("stations_df", stations_df, envir = .GlobalEnv)
    },
    error = function(e) {
        stop(paste("Failed to retrieve stations data:", e$message))
    }
)


# summary(stations_df)
# nrow(stations_df)
# View(stations_df)

# write.csv(stations_df |> select(-bikes, -docks, -freeDocks, -assetStatus), "data/collection/gbfs/emel_gira/stations_information.csv", row.names = FALSE)
# stations_sf <- stations_df |>
#    select(-bikes, -docks, -freeDocks, -assetStatus) |>
#    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
# mapview::mapview(stations_sf)
# sf::st_write(stations_sf, "data/collection/gbfs/emel_gira/stations_information.gpkg")
# sf::st_write(stations_sf, "data/collection/gbfs/emel_gira/stations_information.geojson")

# 3. Store data ------------------------------------------------------------
message("Saving data...")
base_folder <- "data/collection/gbfs/emel_gira"

date <- Sys.Date()
store_folder <- paste(base_folder, "/", "collection/station_information/", gsub("-", "", date), sep = "")
if (!dir.exists(store_folder)) dir.create(store_folder, recursive = TRUE)

write.csv(
    stations_df |>
        select(serialNumber, bikes, docks, assetStatus) |>
        mutate(
            assetStatus = ifelse(assetStatus == "active", 1, 0)
        ),
    paste0(store_folder, "/", format(Sys.time(), "%H%M%S"), ".csv"),
    row.names = FALSE
)

message("Done! :)")

