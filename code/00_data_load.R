# This script loads base layers useful for any analysis
# To load it, use source("code/00_data_load.R") in your script

# Geo ---------------------------------------------------------------------
grid <- sf::st_read("data/geo/grid.gpkg")
parishes <- sf::st_read("data/geo/parishes.gpkg")
municipalities <- sf::st_read("data/geo/municipalities.gpkg")

