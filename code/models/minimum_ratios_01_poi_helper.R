# Script to get floor area of each POI in LMA

poi_all <- st_read("data/processed/poi/points/poi_all.gpkg", quiet=TRUE)
poi_grid <- st_read("data/processed/poi/grid/poi_grid_counts.gpkg", quiet=TRUE)
buildings_lma <- st_read("data/base/daysimetric/lisbon_metro_buildings_footprints.geojson") |> st_transform(3763)

poi_in_building <- poi_points |> filter(!category %in% c("bus_stop","cemetery","park_and_ride","station","zoo"))
poi_buildings <- st_join(poi_in_building, buildings_lma)
poi_error <- poi_buildings |> filter(is.na(poi_buildings$source.y))

mapview(filter(poi_buildings,!is.na(poi_buildings$source.y)), col.regions="darkgreen")+mapview(filter(poi_buildings,is.na(poi_buildings$source.y)), col.regions="red")

# Schools -----------------------------------------------------------------
poi_schools <- st_read("data/processed/poi/schools/schools_students.gpkg", quiet=TRUE)
