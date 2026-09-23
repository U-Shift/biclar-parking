# This script should include the outputs of all analysis performed

# GBFS data #29
grid_gbfs_bikes <- read.csv("data/analysis/gbfs/grid_bikes_all_0-24.csv")

# Bike thefts #14
grid_stolen = read.csv("data/analysis/stolen/stolen_bikes_grid.csv")

# Ticketing data #23
grid_ticketing_global = read.csv("data/analysis/ticketing/grid_ticketing_global.csv")
grid_ticketing_interfaces = read.csv("data/analysis/ticketing/grid_ticketing_interfaces.csv")

# Dasymetric population map (COS + Global Building Atlas combined) - 03_dasymetric_combined.R
grid_dasymetric = read.csv("data/base/daysimetric/grid_dasymetric_combined.csv")

# --- 04_building_attributes.R -------------------------------------------------
# Building characteristics on the H3 grid: BGRI-derived counts (dasymetric,
# reusing 03's weight hierarchy) + Global Building Atlas per-building stats.
grid_building_attributes     <- sf::st_read("data/base/daysimetric/grid_building_attributes.gpkg", quiet = TRUE)
grid_building_attributes_csv <- read.csv("data/base/daysimetric/grid_building_attributes.csv")
