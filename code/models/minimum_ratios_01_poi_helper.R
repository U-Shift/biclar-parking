# Script to get floor area of each POI in LMA

library(dplyr)
library(osmdata)
library(osmextract)

source("code/00_data_load.R")
source("code/99_results_load.R")

poi_all <- st_read("data/processed/poi/points/poi_all.gpkg", quiet=TRUE)
poi_grid <- st_read("data/processed/poi/grid/poi_grid_counts.gpkg", quiet=TRUE)
buildings_lma <- st_read("data/base/daysimetric/lisbon_metro_buildings_footprints.geojson") |> st_transform(3763)


# 1. Extract buildings from OSM --------------------------------------------

aml_boundary <- st_read("data/geo/municipalities_union_bbox_plus_500m.gpkg") |> st_transform(4326) |> st_bbox()

### Using osmdata (not appropriate for such a large area)
# osm_buildings_raw <- opq(bbox = aml_boundary) |>
#   add_osm_feature(key = "building") |>
#   osmdata_sf()

### Using osmextract (not working)
osm_buildings_raw <- oe_get(
  place = aml_boundary,
  layer = "multipolygons",
  query = "SELECT * FROM multipolygons WHERE building IS NOT NULL", # select all attributes from the polygons with a valid building attribute
  boundary = aml_boundary,
  boundary_type = "spat",
  extra_tags = "building",
  quiet = FALSE
)
osm_buildings <- osm_buildings_raw |>
  select(osm_id, building, name, geometry) |>
  st_transform (3763)
osm_buildings$area <- as.numeric(st_area(osm_buildings))

# Buildings with area below 25m2 are to be removed, since they occupy a significant amount of memory and barely
# contribute to our analysis. A validation that the vast majority (if not all) buildings below 25m2 were not used
# as POIs was made. Although most  buildings with area between 25 and 50m2 are also not "useful", these are kept
# since there are some old/historical areas (e.g. Alfama) where many buildings are tiny, but still have POIs in them.
osm_buildings <- osm_buildings |> filter(area >= 25)

#mapview(filter(osm_buildings, area >= 25 & area <= 30)) # don't try to view all buildings at once, the server will most likely crash!
#write_sf(osm_buildings, "data/base/osm_buildings.gpkg", delete_dsn = TRUE)

# 2. Get buildings of each POI --------------------------------------------

poi_in_building <- poi_all |> filter(!category %in% c("bus_stop","cemetery","park_and_ride","recreational_area","sports_facility","station","zoo"))
# poi_buildings <- st_join(poi_in_building, buildings_lma)
# poi_error <- poi_buildings |> filter(is.na(poi_buildings$source.y))

mapview(filter(poi_buildings,!is.na(poi_buildings$source.y)), col.regions="darkgreen")+mapview(filter(poi_buildings,is.na(poi_buildings$source.y)), col.regions="red")

# Schools -----------------------------------------------------------------
poi_schools <- st_read("data/processed/poi/schools/schools_students.gpkg", quiet=TRUE)
