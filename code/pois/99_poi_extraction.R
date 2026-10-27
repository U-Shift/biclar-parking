# Introduction -----------------------------
# 99_poi_extraction.R
#
# Purpose: build a POI layer for the AML bicycle-parking siting model, covering
# the demand-generator categories in the Danish Cycling Federation (2008) and
# VSS/Velokonferenz Schweiz (2008) norm tables. Two output products:
#   (1) individual POI points, one GPKG per category + one combined file
#   (2) POI counts aggregated onto the shared H3 grid, one column per category
#
# NOTE: Schools and Universities are treated as separate from other POIs, due to an extra attribute: number of students.
#       Code for these is found in "biclar-parking/code/pois/02_schools_univ_agg"
# Steps:
# 0. Base code to define LMA bounds and standardize columns.
# 1. Get all data possible from Carris Metropolitana - NOT USED
# 1.1. Facilities (using OSM)
# 1.2. PT Stations (using TML data instead)
# 1.3. Bus stops (using TML data instead)
# 2. Get the remaining POIs from OSM.
# 3. Combine all layers.
# 4. Aggregated by grid cell.
#
# Source policy (per team decision):
#   - carrismetropolitana/datasets is privileged wherever a category exists there
#   - OSM (Geofabrik, via osmextract) is used only for categories absent from Carris
#
# Conventions: plain sf::st_read()/st_write() (no IMPT helpers), h3jsr for grid ops.


library(sf)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(tidyr)
library(osmextract)
library(h3jsr)


# 0. CONFIG -- fill these in before running --------------------------

aml_boundary_path <- "data/base/daysimetric/clipping_boundary.geojson"
grid_path         <- "data/geo/grid.gpkg"            # h3 id column is `h3_address`
h3_resolution     <- 9L                              # confirmed via h3jsr::get_res(grid_check$h3_address[1])

out_points_dir <- "data/processed/poi/points"
out_grid_dir   <- "data/processed/poi/grid"
dir.create(out_points_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_grid_dir,   recursive = TRUE, showWarnings = FALSE)

crs_work <- 3763  # ETRS89 / Portugal TM06 -- adjust if the project has standardised on something else

aml_boundary <- st_read(aml_boundary_path, quiet = TRUE) |> st_transform(crs_work)
grid         <- st_read(grid_path, quiet = TRUE) |> st_transform(crs_work)

standard_cols <- c("poi_id", "name", "category", "subcategory", "source", "municipality", "geometry")


# 1. CARRIS METROPOLITANA -------------------------------------------------

# cm_repo_raw <- "https://raw.githubusercontent.com/carrismetropolitana/datasets/latest"
#
# read_cm_csv <- function(rel_path, category, subcategory) {
#   url <- file.path(cm_repo_raw, rel_path)
#   df <- read_csv(url, show_col_types = FALSE)
#   df |>
#     filter(!is.na(lat), !is.na(lon)) |>
#     transmute(
#       poi_id       = as.character(id),
#       name         = name,
#       category     = category,
#       subcategory  = subcategory,
#       source       = "carrismetropolitana",
#       municipality = municipality_name,
#       lon = lon, lat = lat
#     ) |>
#     st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
#     st_transform(crs_work)
# }
#
# ## 1.1. Stations (train/metro/light-rail/boat) --------
#
# cm_stations <- bind_rows(
#   read_cm_csv("connections/train_stations/train_stations.csv", "station", "train"),
#   read_cm_csv("connections/subway_stations/subway_stations.csv", "station", "metro"),
#   read_cm_csv("connections/light_rail_stations/light_rail_stations.csv","station", "light_rail"),
#   read_cm_csv("connections/boat_stations/boat_stations.csv", "station", "boat")
# )
#
# ## 1.2. Bus stops and terminals (from GTFS) --------
# ## Bus stops live in the GTFS static feed, not the "datasets" repo. Primary
# ## producer URL per Carris' own API docs; falls back to the GitHub mirror if
# ## that redirect ever changes.
#
# gtfs_zip <- tempfile(fileext = ".zip")
# gtfs_ok <- tryCatch({
#   download.file("https://api.carrismetropolitana.pt/gtfs", gtfs_zip, mode = "wb", quiet = TRUE)
#   TRUE
# }, error = function(e) FALSE)
#
# if (!gtfs_ok) {
#   download.file("https://github.com/carrismetropolitana/gtfs/raw/live/CarrisMetropolitana.zip",
#                 gtfs_zip, mode = "wb", quiet = TRUE)
# }
#
# gtfs_dir <- tempfile()
# unzip(gtfs_zip, exdir = gtfs_dir)
#
# stops_raw <- read_csv(file.path(gtfs_dir, "stops.txt"), show_col_types = FALSE)
#
# # location_type 0/NA = stop or platform; 1 = station (parent). We want the
# # boarding stops themselves -- terminals show up as clusters of stops sharing
# # a name/parent_station, which is exactly what the norm table treats as one
# # "terminal", so no separate terminal aggregation is done here.
# cm_bus_stops <- stops_raw |>
#   filter(is.na(location_type) | location_type == 0) |>
#   filter(!is.na(stop_lat), !is.na(stop_lon)) |>
#   transmute(
#     poi_id = as.character(stop_id), name = stop_name,
#     category = "bus_stop", subcategory = NA_character_,
#     source = "carrismetropolitana_gtfs", municipality = NA_character_,
#     lon = stop_lon, lat = stop_lat
#   ) |>
#   st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
#   st_transform(crs_work) |>
#   st_filter(aml_boundary)  # GTFS feed extends slightly beyond AML in places


# 2. OSM -- everything CM doesn't cover -------------------------------
## Geofabrik "Portugal" extract, spatially filtered to the AML boundary so we
## don't pull the whole country into memory. oe_get() caches the .osm.pbf, so
## re-runs are cheap.

aml_boundary_4326 <- st_transform(aml_boundary, 4326) |> st_bbox()

osm_tags <- c("amenity", "shop", "tourism", "leisure", "office", "healthcare", "landuse")

osm_points_raw <- oe_get(
  place = aml_boundary_4326,
  layer = "points",
  extra_tags = osm_tags,
  boundary_type = "spat",
  force_download = TRUE,
  force_vectortranslate = TRUE,
  quiet = FALSE
)

osm_polygons_raw <- oe_get(
  place = aml_boundary_4326,
  layer = "multipolygons",
  extra_tags = osm_tags,
  boundary_type = "spat",
  force_download = TRUE,
  force_vectortranslate = TRUE,
  quiet = FALSE
)

osm_polygons_tagged <- osm_polygons_raw |>
  filter(!is.na(amenity) | !is.na(shop) | !is.na(tourism) | !is.na(leisure) | !is.na(office) | !is.na(healthcare) | !is.na(landuse))

osm_polygon_centroids <- osm_polygons_tagged |>
  st_transform(crs_work) |>
  st_make_valid() |>
  st_filter(aml_boundary) |>
  st_centroid()

osm_all <- bind_rows(
  osm_points_raw |> st_transform(crs_work),
  osm_polygon_centroids
)

# Classify OSM features into the norm-table taxonomy. Only categories NOT
# already sourced from Carris are kept (see mapping table in the chat reply).
classify_osm <- function(df) {
  df |>
    mutate(
      category = case_when(
        shop == "mall" | building == "retail"              ~ "retail_large",
        !is.na(shop) & !shop %in% c("mall")                ~ "retail_small",
        !is.na(healthcare)                                 ~ "healthcare_centres",
        amenity %in% c("cinema", "theatre", "arts_centre",
                       "place_of_worship", "library")      ~ "cultural",
        tourism %in% c("hotel", "hostel", "guest_house")   ~ "hotel_hostel",
        amenity %in% c("cafe", "restaurant", "fast_food")  ~ "cafe_restaurant",
        leisure %in% c("sports_centre", "fitness_centre",
                       "swimming_pool", "stadium")         ~ "sports_facility",
        !is.na(office)                                     ~ "office_industry",
        landuse == "industrial"                            ~ "office_industry",
        leisure %in% c("park", "garden", "nature_reserve") ~ "recreational_area",
        amenity %in% c("bar","nightclub")                  ~ "bar",
        TRUE                                               ~ NA_character_
      ),
      subcategory = case_when(
        !is.na(healthcare) & healthcare != "hospital" &
        healthcare != "pharmacy" & healthcare != "clinic"  ~ "healthcare_other",
        healthcare == "hospital"                           ~ "hospital",
        healthcare == "pharmacy"                           ~ "pharmacy",
        healthcare == "clinic"                             ~ "clinic",
        amenity == "cinema"                                ~ "cinema",
        amenity == "theatre"                               ~ "theatre",
        amenity == "arts_centre"                           ~ "cultural_other",
        amenity == "place_of_worship"                      ~ "church",
        amenity == "library"                               ~ "library",
        amenity == "cafe"                                  ~ "cafe",
        amenity %in% c("restaurant","fast_food")           ~ "restaurant",
        !is.na(office)                                     ~ "office",
        landuse == "industrial"                            ~ "industry",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(category)) |>
    transmute(
      poi_id = as.character(osm_id), name = name,
      category, subcategory,
      source = "osm", municipality = NA_character_,
      geometry = geometry
    ) |>
    subset(!is.na(name))
}

osm_poi <- classify_osm(osm_all)


# 3. Combine, clip, export individual points ---------------------------

poi_all <- osm_poi |>
  select(all_of(standard_cols)) |>
  st_filter(aml_boundary) |>
  mutate(poi_id = row_number()) |>
  distinct(poi_id, name, category, .keep_all = TRUE)  # guards against feed re-runs, not cross-source dedup

# validation printout -- check before exporting
poi_all |> st_drop_geometry() |> count(category, source, sort = TRUE)

# one GPKG per category
poi_all |>
  st_drop_geometry() |>
  distinct(category) |>
  pull(category) |>
  walk(function(cat) {
    st_write(
      poi_all |> filter(category == cat),
      file.path(out_points_dir, paste0(cat, ".gpkg")),
      delete_dsn = TRUE, quiet = TRUE
    )
  })

# combined file
st_write(poi_all, file.path(out_points_dir, "poi_all.gpkg"), delete_dsn = TRUE, quiet = TRUE)


# 4. Aggregate to the H3 grid ------------------------------------------

poi_wgs84 <- st_transform(poi_all, 4326)

poi_h3 <- poi_wgs84 |>
  mutate(h3_address = point_to_cell(poi_wgs84, res = h3_resolution)) |>
  st_drop_geometry()

poi_grid_counts <- poi_h3 |>
  count(h3_address, category) |>
  pivot_wider(names_from = category, values_from = n, values_fill = 0, names_prefix = "poi_")

grid_with_counts <- grid |>
  left_join(poi_grid_counts, by = "h3_address") |>
  mutate(across(starts_with("poi_"), \(x) replace_na(x, 0)))

st_write(grid_with_counts, file.path(out_grid_dir, "poi_grid_counts.gpkg"), delete_dsn = TRUE, quiet = TRUE)
write_csv(grid_with_counts |> st_drop_geometry(), file.path(out_grid_dir, "poi_grid_counts.csv"))
