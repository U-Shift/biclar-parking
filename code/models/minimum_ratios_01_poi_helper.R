# Script to get floor area of each POI in LMA

source("code/00_data_load.R")
source("code/99_results_load.R")

poi_all <- st_read("data/processed/poi/points/poi_all.gpkg", quiet=TRUE)
poi_grid <- st_read("data/processed/poi/grid/poi_grid_counts.gpkg", quiet=TRUE)
buildings_lma <- st_read("data/base/daysimetric/lisbon_metro_buildings_footprints.geojson") |> st_transform(3763)


# 1. Extract buildings from OSM --------------------------------------------

aml_boundary <- st_read("data/geo/municipalities_union_bbox_plus_50m.gpkg") |> st_transform(4326) |> st_bbox()

osm_buildings_fetch <- opq(bbox = aml_boundary) |>
  add_osm_feature(key = "building") |>
  osmdata_sf()
osm_buildings <- osm_buildings_fetch$osm_polygons |> select(osm_id, building, name, geometry)



# 2. Get buildings of each POI --------------------------------------------

poi_in_building <- poi_all |> filter(!category %in% c("bus_stop","cemetery","park_and_ride","recreational_area","sports_facility","station","zoo"))
# poi_buildings <- st_join(poi_in_building, buildings_lma)
# poi_error <- poi_buildings |> filter(is.na(poi_buildings$source.y))

mapview(filter(poi_buildings,!is.na(poi_buildings$source.y)), col.regions="darkgreen")+mapview(filter(poi_buildings,is.na(poi_buildings$source.y)), col.regions="red")

# Schools -----------------------------------------------------------------
poi_schools <- st_read("data/processed/poi/schools/schools_students.gpkg", quiet=TRUE)
