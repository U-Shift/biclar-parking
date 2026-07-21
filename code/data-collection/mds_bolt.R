# Fetch JSON from mds.bolt.eu/gbfs/2/23/geofencing_zones
data <- jsonlite::fromJSON("https://mds.bolt.eu/gbfs/2/23/geofencing_zones")
names(data$data$geofencing_zones)

# Store data$data$geofencing_zones as geojson
data_geojson = tempfile(fileext = ".geojson")
jsonlite::write_json(data$data$geofencing_zones, data_geojson, auto_unbox = TRUE)

data_sf = sf::st_read(data_geojson)
nrow(data_sf) # 678
mapview(data_sf)

# fetch https://mds.bolt.eu/gbfs/2/23/free_bike_status 
free_bike_status = jsonlite::fromJSON("https://mds.bolt.eu/gbfs/2/23/free_bike_status")
names(free_bike_status$data$bikes)

bikes_sf = free_bike_status$data$bikes |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
nrow(bikes_sf)
mapview(bikes_sf)
