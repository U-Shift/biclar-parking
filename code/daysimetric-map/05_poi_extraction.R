## =============================================================================
## 05_poi_extraction.R
##
## Purpose: build a POI layer for the AML bicycle-parking siting model, covering
## the demand-generator categories in the Danish Cycling Federation (2008) and
## VSS/Velokonferenz Schweiz (2008) norm tables. Two output products:
##   (1) individual POI points, one GPKG per category + one combined file
##   (2) POI counts aggregated onto the shared H3 grid, one column per category
##
## Source policy (per team decision):
##   - carrismetropolitana/datasets is privileged wherever a category exists there
##   - OSM (Geofabrik, via osmextract) is used only for categories absent from Carris
##
## Conventions: plain sf::st_read()/st_write() (no IMPT helpers), h3jsr for grid ops.
## =============================================================================

library(sf)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(tidyr)
library(osmextract)
library(h3jsr)

## -----------------------------------------------------------------------------
## 0. CONFIG -- fill these in before running
## -----------------------------------------------------------------------------

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

## -----------------------------------------------------------------------------
## 1. CARRIS METROPOLITANA -- facilities (schools, shopping centers, museums, theaters)
## -----------------------------------------------------------------------------

cm_repo_raw <- "https://raw.githubusercontent.com/carrismetropolitana/datasets/latest"

read_cm_csv <- function(rel_path, category, subcategory) {
  url <- file.path(cm_repo_raw, rel_path)
  df <- read_csv(url, show_col_types = FALSE)
  df |>
    filter(!is.na(lat), !is.na(lon)) |>
    transmute(
      poi_id       = as.character(id),
      name         = name,
      category     = category,
      subcategory  = subcategory,
      source       = "carrismetropolitana",
      municipality = municipality_name,
      lon = lon, lat = lat
    ) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    st_transform(crs_work)
}

cm_facilities <- bind_rows(
  read_cm_csv("facilities/schools/schools.csv",                 "schools",        NA),
  read_cm_csv("facilities/shopping_centers/shopping_centers.csv","retail",         "shopping_center"),
  read_cm_csv("facilities/museums/museums.csv",                 "museum",         NA),
  read_cm_csv("facilities/theaters/theaters.csv",                "cinema_theatre", "theatre")
)

## -----------------------------------------------------------------------------
## 2. CARRIS METROPOLITANA -- stations (train/metro/light-rail/boat) + Park&Ride
## -----------------------------------------------------------------------------

cm_stations <- bind_rows(
  read_cm_csv("connections/train_stations/train_stations.csv",         "station", "train"),
  read_cm_csv("connections/subway_stations/subway_stations.csv",       "station", "metro"),
  read_cm_csv("connections/light_rail_stations/light_rail_stations.csv","station", "light_rail"),
  read_cm_csv("connections/boat_stations/boat_stations.csv",           "station", "boat")
)

cm_park_ride <- read_cm_csv("connections/parking/parking.csv", "park_and_ride", NA)

## -----------------------------------------------------------------------------
## 3. CARRIS METROPOLITANA -- bus stops and terminals (GTFS, not in datasets repo)
## -----------------------------------------------------------------------------
## Bus stops live in the GTFS static feed, not the "datasets" repo. Primary
## producer URL per Carris' own API docs; falls back to the GitHub mirror if
## that redirect ever changes.

gtfs_zip <- tempfile(fileext = ".zip")
gtfs_ok <- tryCatch({
  download.file("https://api.carrismetropolitana.pt/gtfs", gtfs_zip, mode = "wb", quiet = TRUE)
  TRUE
}, error = function(e) FALSE)

if (!gtfs_ok) {
  download.file("https://github.com/carrismetropolitana/gtfs/raw/live/CarrisMetropolitana.zip",
                gtfs_zip, mode = "wb", quiet = TRUE)
}

gtfs_dir <- tempfile()
unzip(gtfs_zip, exdir = gtfs_dir)

stops_raw <- read_csv(file.path(gtfs_dir, "stops.txt"), show_col_types = FALSE)

# location_type 0/NA = stop or platform; 1 = station (parent). We want the
# boarding stops themselves -- terminals show up as clusters of stops sharing
# a name/parent_station, which is exactly what the norm table treats as one
# "terminal", so no separate terminal aggregation is done here.
cm_bus_stops <- stops_raw |>
  filter(is.na(location_type) | location_type == 0) |>
  filter(!is.na(stop_lat), !is.na(stop_lon)) |>
  transmute(
    poi_id = as.character(stop_id), name = stop_name,
    category = "bus_stop", subcategory = NA_character_,
    source = "carrismetropolitana_gtfs", municipality = NA_character_,
    lon = stop_lon, lat = stop_lat
  ) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(crs_work) |>
  st_filter(aml_boundary)  # GTFS feed extends slightly beyond AML in places

## -----------------------------------------------------------------------------
## 4. OSM -- everything Carris doesn't cover
## -----------------------------------------------------------------------------
## Geofabrik "Portugal" extract, spatially filtered to the AML boundary so we
## don't pull the whole country into memory. oe_get() caches the .osm.pbf, so
## re-runs are cheap.

aml_boundary_4326 <- st_transform(aml_boundary, 4326)

osm_tags <- c("amenity", "shop", "tourism", "leisure", "office", "healthcare", "landuse")

osm_points_raw <- oe_get(
  place = "Portugal",
  layer = "points",
  extra_tags = osm_tags,
  boundary = aml_boundary_4326,
  boundary_type = "spat",
  quiet = FALSE
)

osm_polygons_raw <- oe_get(
  place = "Portugal",
  layer = "multipolygons",
  extra_tags = osm_tags,
  quiet = FALSE
)
# NB: boundary/boundary_type="spat" is unreliable here at the GDAL level, and an
# exact spatial filter against the full national layer (670k+ rows, including
# huge coastline/admin polygons) is unstable in GEOS. Filter by tag first --
# cheap, and it drops the irrelevant giant geometries -- then do the spatial
# clip on the much smaller remainder.
osm_polygons_tagged <- osm_polygons_raw |>
  filter(!is.na(amenity) | !is.na(shop) | !is.na(tourism) | !is.na(leisure) |
           !is.na(office) | !is.na(healthcare) | !is.na(landuse))

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
        amenity == "kindergarten"                                   ~ "childcare",
        amenity %in% c("college", "university")                     ~ "college_university",
        !is.na(shop) & !shop %in% c("mall")                          ~ "retail_other",
        amenity %in% c("doctors", "dentist") | !is.na(healthcare)    ~ "urban_professions",
        amenity == "cinema"                                          ~ "cinema_theatre",
        tourism %in% c("hotel", "hostel", "guest_house")             ~ "hotel_hostel",
        amenity == "restaurant"                                      ~ "restaurant",
        leisure %in% c("sports_centre", "fitness_centre",
                       "swimming_pool")                              ~ "sports_facility",
        leisure == "stadium"                                         ~ "stadium",
        !is.na(office)                                                ~ "office_industry",
        landuse == "industrial"                                       ~ "office_industry",
        leisure %in% c("park", "garden", "nature_reserve") |
          tourism == "attraction"                                     ~ "recreational_area",
        amenity == "library"                                          ~ "library",
        amenity == "nightclub"                                        ~ "disco_dance_bar",
        landuse == "cemetery" | amenity == "grave_yard"               ~ "cemetery",
        amenity == "place_of_worship"                                 ~ "church",
        tourism == "zoo"                                              ~ "zoo",
        TRUE                                                          ~ NA_character_
      ),
      subcategory = case_when(
        !is.na(shop) & category == "retail_other" ~ shop,
        !is.na(healthcare)                         ~ healthcare,
        !is.na(office)                             ~ office,
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(category)) |>
    transmute(
      poi_id = as.character(osm_id), name = name,
      category, subcategory,
      source = "osm", municipality = NA_character_,
      geometry = geometry
    )
}

osm_poi <- classify_osm(osm_all)

## -----------------------------------------------------------------------------
## 5. Combine, clip, export individual points
## -----------------------------------------------------------------------------

poi_all <- bind_rows(
  cm_facilities, cm_stations, cm_park_ride, cm_bus_stops, osm_poi
) |>
  select(all_of(standard_cols)) |>
  st_filter(aml_boundary) |>
  distinct(poi_id, category, .keep_all = TRUE)  # guards against feed re-runs, not cross-source dedup

# validation printout -- check before exporting
poi_all |> st_drop_geometry() |> count(category, source, sort = TRUE) |> print(n = 40)

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

## -----------------------------------------------------------------------------
## 6. Aggregate to the H3 grid
## -----------------------------------------------------------------------------

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
