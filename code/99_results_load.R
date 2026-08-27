# This script should include the outputs of all analysis performed

# GBFS data #29
grid_gbfs_morning = read.csv("data/analysis/gbfs/grid_metrics_week_6-9.csv")

# Bike thefts #14
grid_stolen = read.csv("data/analysis/stolen/stolen_bikes_grid.csv")

# Ticketing data #23
grid_ticketing_global = read.csv("data/analysis/ticketing/grid_ticketing_global.csv")
grid_ticketing_interfaces = read.csv("data/analysis/ticketing/grid_ticketing_interfaces.csv")

# Dasymetric population map (COS + Global Building Atlas combined) - 03_dasymetric_combined.R
grid_dasymetric = sf::st_read("data/base/daysimetric/grid_dasymetric_combined.gpkg", quiet = TRUE)
grid_dasymetric_csv = read.csv("data/base/daysimetric/grid_dasymetric_combined.csv")
