library(dplyr)
library(mapview)

source("code/00_data_load.R")
census_bgri <- sf::st_read("data/base/daysimetric/BGRI21_170.gpkg")
grid_dasymetric <- read.csv("data/base/daysimetric/grid_dasymetric_combined.csv")

sum(census_bgri$N_INDIVIDUOS)
sum(grid_dasymetric$population)

census_bgri_test = census_bgri |> filter(DTMNFR21=="150701") |>
    mutate(
        fragment_adults = N_INDIVIDUOS_15_24 + N_INDIVIDUOS_25_64,
        dep_jovem = ifelse(fragment_adults>0, N_INDIVIDUOS_0_14/fragment_adults * 100, 0),
        dep_elder = ifelse(fragment_adults > 0, N_INDIVIDUOS_65_OU_MAIS / fragment_adults * 100, 0),
        perc_woman = ifelse(N_INDIVIDUOS > 0, N_INDIVIDUOS_M / N_INDIVIDUOS, 0),
    ) 

# View(census_bgri_test)
sum(census_bgri_test$N_INDIVIDUOS)


grid_dasymetric_test = grid_dasymetric |> left_join(grid_meta, by= c("id" = "gid")) |> filter(pid==150701)
nrow(grid_dasymetric_test)
names(grid_dasymetric_test)
sum(grid_dasymetric_test$population)

grid_dasymetric_test_sf = grid |> left_join(grid_dasymetric_test, by= c("gid" = "id")) |> filter(!is.na(population))

mapview(census_bgri_test, zcol="N_INDIVIDUOS") + 
    mapview(census_bgri_test, zcol="dep_jovem", hide=TRUE) + 
    mapview(census_bgri_test, zcol="dep_elder", hide=TRUE) + 
    mapview(census_bgri_test, zcol="perc_woman", hide=TRUE) +
    mapview(grid_dasymetric_test_sf, zcol="population", hide = TRUE) + 
    mapview(grid_dasymetric_test_sf, zcol="youth_dependency", hide=TRUE) + 
    mapview(grid_dasymetric_test_sf, zcol="elderly_dependency", hide=TRUE) + 
    mapview(grid_dasymetric_test_sf, zcol="women_percentage", hide=TRUE) + 
    mapview(grid_dasymetric_test_sf, zcol="weight_sources", hide=TRUE) + 
    mapview(grid_dasymetric_test_sf, zcol="weight_total", hide=TRUE)
