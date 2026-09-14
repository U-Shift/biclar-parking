# Bike infrastructure classification
# Purpose     Classify bike infrastructure based on its quality
# Scale       Lines

library(dplyr)
library(sf)
library(osmactive) # Assuming this provides classification helpers
library(tidyr)
library(stringr)
library(osmextract)

source("code/00_data_load.R")

# 0. Download OSM extract ----------------------------------------------------
# (use day tag on filename to get the latest file, as oe_download caches files and GeoFrabrick is updated daily)
osm_file <- oe_download(
    "https://download.geofabrik.de/europe/portugal-latest.osm.pbf",
    file_basename = sprintf("%s_%s.osm.pbf", "PT", format(Sys.Date(), "%Y%m%d"))
)
# 1. Define Helper Functions ----------------------------------------------

# Tags to extract from OSM
et_active <- function() {
    c(
        # Base
        "highway",
        # For cyclelane classification
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

columns_to_remove <- c("waterway", "aerialway", "barrier", "manmade")

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
boundary <- sf::st_read("data/geo/municipalities_union_plus_500m.gpkg")
# mapview(boundary)
road_osm <- osmextract::oe_read(osm_file, boundary = boundary, quiet = TRUE, extra_tags = et_active()) |>
    select(-other_tags) |>
    filter(!is.na(highway)) |>
    filter(is.na(service)) |>
    select(-dplyr::matches(columns_to_remove)) |>
    st_filter(boundary)
# View(road_osm |> sample_n(10))
# nrow(road_osm)

# Apply classification
# In the testing phase, Rosa used get_cycling_network() and distance_to_road()
# Assuming aml_cycleways can be used directly for classification here:
cycle_net_pt <- classify_cycle_infrastructure_portugal(road_osm)

# 4. Cleanup and Save -----------------------------------------------------
cycle_segregation_levels <- c(
    "Cycle track or lane",
    "Protected Active",
    "Advisory lane"
)

# Filter to keep only relevant infrastructure and columns
cycle_net_pt_clean <- cycle_net_pt |>
    select(osm_id, name, highway, geometry, cycle_segregation) |>
    filter(cycle_segregation != "Mixed traffic") |>
    mutate(cycle_segregation = factor(cycle_segregation, levels = cycle_segregation_levels, ordered = TRUE)) |>
    arrange(osm_id)

mapview(cycle_net_pt_clean, zcol = "cycle_segregation", layer.name = "OSM UPDATED cycle segregation") +
    mapview(municipalities, alpha.regions = 0.1)

# Write output to the mobility folder
sf::st_write(
    cycle_net_pt_clean,
    sprintf("data/raw/cycling_network/osm/osm_%s.gpkg", format(Sys.Date(), "%Y%m%d")),
    delete_dsn = TRUE
)

# 5. Compare with IMPT to identify new cyclelanes -------------------------------------------------
cycle_net_impt <- st_read("data/raw/impt_mobility/cycle_network_class.gpkg") |>
    sf::st_set_geometry(attr(cycle_net_pt_clean, "sf_column")) |>
    mutate(cycle_segregation = factor(cycle_segregation, levels = cycle_segregation_levels, ordered = TRUE))
mapview(cycle_net_impt, zcol = "cycle_segregation", layer.name = "IMPT OSM cycle segregation") +
    mapview(municipalities, alpha.regions = 0.1)

compare_networks <- function(network_old, network_new) {
    message(sprintf("Old network rows: %d", nrow(network_old)))
    message(sprintf("New network rows: %d", nrow(network_new)))

    old_ids <- unique(network_old$osm_id)
    new_ids <- unique(network_new$osm_id)

    # Segments present in new network: either "existing" or "new"
    new_tagged <- network_new |>
        dplyr::mutate(status = dplyr::if_else(osm_id %in% old_ids, "existing", "new"))

    # Segments only in old network: "suppressed"
    suppressed <- network_old |>
        dplyr::filter(!osm_id %in% new_ids) |>
        dplyr::mutate(status = "suppressed")

    # Combine ensuring matching columns and valid sf object
    common_cols <- intersect(names(new_tagged), names(suppressed))
    combined <- dplyr::bind_rows(
        new_tagged |> dplyr::select(dplyr::all_of(common_cols)),
        suppressed |> dplyr::select(dplyr::all_of(common_cols))
    )

    message("Summary of segment comparison:")
    print(table(combined$status))

    combined
}

comparison <- compare_networks(cycle_net_impt, cycle_net_pt_clean)

mapview(comparison, zcol = "status", layer.name = "OSM network update status") +
    mapview(comparison |> filter(status == "existing"), layer.name = "Existing", hide = TRUE) +
    mapview(comparison |> filter(status == "new"), layer.name = "New", hide = TRUE) +
    mapview(comparison |> filter(status == "suppressed"), layer.name = "Suppressed", hide = TRUE)
