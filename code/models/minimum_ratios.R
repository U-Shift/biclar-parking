# Code to create the minimum ratios model.
# For Almada and Seixal, the values used are based on municipality regulations (RMUE),
# while for the rest of the LMA values are based on literature recommendations.
# Refer to https://github.com/U-Shift/biclar-parking/issues/9 (municipal regulations) and
# https://github.com/U-Shift/biclar-parking/issues/7 (literature recommendations) for more details.

library(dplyr)
library(sf)
library(mapview)

source("code/00_data_load.R")
source("code/99_results_load.R")
