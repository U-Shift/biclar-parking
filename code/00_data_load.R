# This script loads base layers useful for any analysis
# To load it, use source("code/00_data_load.R") in your script

library(dplyr)
library(sf)
library(mapview)

# Geo ---------------------------------------------------------------------
grid <- sf::st_read("data/geo/grid.gpkg")
parishes <- sf::st_read("data/geo/parishes.gpkg")
municipalities <- sf::st_read("data/geo/municipalities.gpkg")

grid_meta <- read.csv("useful_data/grid.csv")
parishes_meta <- read.csv("useful_data/grid.csv")
municipalities_meta <- read.csv("useful_data/municipalities.csv")

conversion_dicofre <- read.csv("https://raw.githubusercontent.com/U-Shift/IMPT-data/refs/heads/main/useful_data/dicofre_16_24_conversion.csv")
conversion_dicofre_weight <- read.csv("https://raw.githubusercontent.com/U-Shift/IMPT-data/refs/heads/main/useful_data/dicofre_16_24_conversion_full_with_weights.csv")


# LMA cycleway network #22 ----------------------------------------------
cycleways <- sf::st_read("data/base/existing_cycleways/aml_osm_cycleways.gpkg")
cycleways_grid <- sf::st_read("data/base/existing_cycleways/osm_cycleways_grid.gpkg")

# LMA bicycle parking locations #21 -------------------------------------
parking <- sf::st_read("data/base/existing_parking/aml_osm_parking.gpkg")
parking_grid <- sf::st_read("data/base/existing_parking/osm_parking_grid.gpkg")
