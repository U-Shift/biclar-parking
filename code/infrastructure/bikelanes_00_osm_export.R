# Bike infrastructure classification
# Purpose     Classify bike infrastructure based on its quality
# Scale       Lines

library(dplyr)
library(sf)
library(osmactive) # Assuming this provides classification helpers
library(tidyr)
library(stringr)

# Pre-requisites: 00a_impt_data_handle.R and 00b_data_load.R should be run first
# to have impt_read/write and general context.

# 1. Load context data ----------------------------------------------------
# Use the project's standard boundary
limit <- impt_read("/geo/municipios_union_2024.geojson") |>
    sf::st_make_valid()

# 2. Define Helper Functions ----------------------------------------------

# Tags to extract from OSM
et_active <- function() {
    c(
        "maxspeed",
        "oneway",
        "bicycle",
        "cycleway",
        "cycleway:left",
        "cycleway:right",
        "cycleway:both",
        "lanes",
        "lanes:both_ways",
        "lanes:forward",
        "lanes:backward",
        "lanes:bus",
        "lanes:bus:conditional",
        "width",
        "segregated",
        "sidewalk",
        "footway",
        "service",
        "surface",
        "tracktype",
        "smoothness",
        "access",
        "foot"
    )
}

# Function to get network from OSM
get_travel_network <- function(place, extra_tags = et_active(), columns_to_remove = c("waterway", "aerialway", "barrier", "manmade"), ...) {
    osm_highways <- osmextract::oe_get(place = place, extra_tags = extra_tags, ...)
    osm_highways |>
        dplyr::filter(!is.na(highway)) |>
        dplyr::filter(is.na(service)) |>
        dplyr::select(-dplyr::matches(columns_to_remove))
}

# Core classification logic
classify_cycle_infrastructure_portugal <- function(osm) {
    CYCLE_TRACK <- "Cycle track or lane"
    ADVISORY <- "Advisory lane"
    PROTECTED_ACTIVE <- "Protected Active"
    MIXED_TRAFFIC <- "Mixed traffic"

    osm |>
        # 1. Preliminary classification
        dplyr::mutate(detailed_segregation = dplyr::case_when(
            highway == "cycleway" ~ CYCLE_TRACK,
            highway == "path" & bicycle == "designated" ~ CYCLE_TRACK,
            # highway == "footway" & bicycle == "yes" ~ PROTECTED_ACTIVE,
            highway == "pedestrian" & bicycle == "designated" ~ PROTECTED_ACTIVE,
            segregated == "yes" ~ CYCLE_TRACK,
            segregated == "no" ~ CYCLE_TRACK,
            TRUE ~ MIXED_TRAFFIC
        )) |>
        # 2. Analyse cycleway tags
        tidyr::unite("cycleway_chars", dplyr::starts_with("cycleway"), sep = "|", remove = FALSE) |>
        dplyr::mutate(detailed_segregation2 = dplyr::case_when(
            stringr::str_detect(cycleway_chars, "separate") & detailed_segregation == "Mixed traffic" ~ CYCLE_TRACK,
            stringr::str_detect(cycleway_chars, "buffered_lane") & detailed_segregation == "Mixed traffic" ~ CYCLE_TRACK,
            stringr::str_detect(cycleway_chars, "segregated") & detailed_segregation == "Mixed traffic" ~ CYCLE_TRACK,
            TRUE ~ detailed_segregation
        )) |>
        # Accumulate further classifications
        dplyr::mutate(detailed_segregation2 = dplyr::case_when(
            stringr::str_detect(cycleway_chars, "shared_lane") ~ ADVISORY,
            stringr::str_detect(cycleway_chars, "lane") & detailed_segregation == "Mixed traffic" ~ CYCLE_TRACK,
            stringr::str_detect(cycleway_chars, "track") & detailed_segregation == "Mixed traffic" ~ CYCLE_TRACK,
            TRUE ~ detailed_segregation2 # Use previous state as default
        )) |>
        # 3. Clarify sharing with pedestrians
        dplyr::mutate(detailed_segregation4 = dplyr::case_when(
            detailed_segregation2 == CYCLE_TRACK & highway %in% c("cycleway", "path") & foot %in% c("designated", "permissive", "private", "use_sidepath", "yes") & (is.na(sidewalk) | sidewalk == "no") & (is.na(segregated) | segregated == "no") ~ PROTECTED_ACTIVE,
            detailed_segregation2 == CYCLE_TRACK & highway == "footway" & bicycle %in% c("yes", "designated") ~ PROTECTED_ACTIVE,
            detailed_segregation2 == CYCLE_TRACK & highway == "pedestrian" & bicycle %in% c("yes", "designated") ~ PROTECTED_ACTIVE,
            TRUE ~ detailed_segregation2
        )) |>
        dplyr::mutate(cycle_segregation = factor(
            detailed_segregation4,
            levels = c(CYCLE_TRACK, ADVISORY, PROTECTED_ACTIVE, MIXED_TRAFFIC),
            ordered = TRUE
        ))
}

# 3. Execution ------------------------------------------------------------

# Download and clip OSM data
# Note: "Portugal" here refers to the osmextract provider.

aml_cycleways <- get_travel_network(
    place = "Portugal",
    # boundary = limit,
    # boundary_type = "clipsrc",
    force_vectortranslate = TRUE
) |>
    st_filter(limit) # Perform the spatial clip/filter

# Replace : by _ in column names for easier logic
aml_cycleways <- aml_cycleways |>
    dplyr::rename_with(~ gsub(":", "_", .x))

# Apply classification
# In the testing phase, Rosa used get_cycling_network() and distance_to_road()
# Assuming aml_cycleways can be used directly for classification here:
cycle_net_pt <- classify_cycle_infrastructure_portugal(aml_cycleways)

# 4. Cleanup and Save -----------------------------------------------------

# Filter to keep only relevant infrastructure and columns
cycle_net_pt_clean <- cycle_net_pt |>
    select(osm_id, name, highway, geometry, cycle_segregation) |>
    filter(cycle_segregation != "Mixed traffic")

mapview(cycle_net_pt_clean, zcol = "cycle_segregation")

# Write output to the mobility folder
impt_write(cycle_net_pt_clean, "/mobility/cycle_network_class.gpkg")
