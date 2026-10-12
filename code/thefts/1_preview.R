library(mapview)
library(sf)

# source("code/00_data_load.R")

filter_inside_aml <- function(data_sf) {
    data_sf <- data_sf |>
        st_join(municipalities |> select(mid), left = TRUE)
    table(data_sf$mid)
    data_sf <- data_sf |>
        filter(!is.na(mid))
    return(data_sf)
}

# GNR -----------------------------------------------------------
# > North
gnr_thefts_north <- read.csv("data/raw/thefts/gnr_aml_north.csv") |>
    st_as_sf(
        coords = c("lon", "lat"),
        crs = 4326
    )
nrow(gnr_thefts_north) # 41
testthat::expect_equal(nrow(gnr_thefts_north), 41)
gnr_thefts_north <- filter_inside_aml(gnr_thefts_north)
nrow(gnr_thefts_north) # 31

mapview(municipalities |> filter(mid <= 1116), col.regions = "gray", apha.regions = 0.5, legend = FALSE) +
    mapview(gnr_thefts_north, col.regions = "#0099ff", color = "#0099ff", legend = FALSE, alpha = 1)

# > South
gnr_thefts_south <- read.csv("data/raw/thefts/gnr_aml_south.csv") |>
    st_as_sf(
        coords = c("lon", "lat"),
        crs = 4326
    )
nrow(gnr_thefts_south) # 710
testthat::expect_equal(nrow(gnr_thefts_south), 710)
gnr_thefts_south <- filter_inside_aml(gnr_thefts_south)
nrow(gnr_thefts_south) # 619

mapview(municipalities |> filter(mid > 1116), col.regions = "gray", apha.regions = 0.5, legend = FALSE) +
    mapview(gnr_thefts_south, col.regions = "#660066", color = "#660066", legend = FALSE, alpha = 1)

# PSP -----------------------------------------------------------
# Pending data provision

# AGGREGATE ALL SOURCES -----------------------------------------------------------
thefts_aggregated <- rbind(
    gnr_thefts_north |> mutate(source = "GNR", dataset = "gnr_aml_north"),
    gnr_thefts_south |> mutate(source = "GNR", dataset = "gnr_aml_south")
)
nrow(thefts_aggregated) # 650
testthat::expect_equal(nrow(thefts_aggregated), nrow(gnr_thefts_north) + nrow(gnr_thefts_south))
length(unique(thefts_aggregated$mid)) # 14 out of 18

mapview(municipalities, col.regions = "gray", apha.regions = 0.5, legend = FALSE) +
    mapview(thefts_aggregated, zcol = "dataset")

sf::st_write(thefts_aggregated, "data/processed/thefts/thefts_aggregated.gpkg", delete_layer = TRUE)

thefts_aggregated_municipalities <- thefts_aggregated |> left_join(read.csv("useful_data/municipalities.csv") |> mutate(mid = as.character(mid)))
plot(as.factor(thefts_aggregated_municipalities$name))
table(thefts_aggregated_municipalities$name)

# > Aggregate at grid
grid_thefts <- grid
grid_thefts$n_thefts <- lengths(st_intersects(grid, st_transform(thefts_aggregated, st_crs(grid))))
grid_thefts <- grid_thefts |> select(gid, n_thefts)

mapview(grid_thefts |> filter(n_thefts != 0), zcol = "n_thefts")
# mapview(thefts_aggregated, col.regions = "#0099ff", color = "#0099ff", legend = FALSE, alpha = 1)

st_write(grid_thefts, "data/processed/thefts/grid_thefts.gpkg", delete_layer = TRUE)
write.csv(grid_thefts |> st_drop_geometry(), "data/processed/thefts/grid_thefts.csv", row.names = FALSE)
