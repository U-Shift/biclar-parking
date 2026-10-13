# This script should include the outputs of all analysis performed

# GBFS data #29
grid_gbfs_bikes <- read.csv("data/analysis/gbfs/grid_bikes_all_0-24.csv")

# Bike thefts #14
grid_thefts <- read.csv("data/processed/thefts/grid_thefts.csv")

# Ticketing data #23
grid_ticketing_global <- read.csv("data/analysis/ticketing/grid_ticketing_global.csv")
grid_ticketing_interfaces <- read.csv("data/analysis/ticketing/grid_ticketing_interfaces.csv")

# Dasymetric population map (COS + Global Building Atlas combined) #20 (03_dasymetric_combined.R)
grid_dasymetric <- read.csv("data/base/daysimetric/grid_dasymetric_combined.csv")
grid_dasymetric_cos <- read.csv("data/base/daysimetric/grid_dasymetric_cos_only.csv")

# POIS
# Schools #37
grid_pois_schools <- read.csv("data/processed/poi/schools/grid_schools_students.csv")
# Others #34
grid_pois <- read.csv("data/processed/poi/grid/poi_grid_counts.csv")

# MODELS
# Minimum ratios 
model_mr <- read.csv("data/outputs/minimum_ratios/residential.csv")
