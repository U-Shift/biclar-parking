# Script to get floor area of each POI in LMA

library(sf)
library(dplyr)
library(tidyr)
library(osmdata)
library(osmextract)

source("code/00_data_load.R")
source("code/99_results_load.R")

poi_all <- st_read("data/processed/poi/points/poi_all.gpkg", quiet=TRUE)
poi_grid <- st_read("data/processed/poi/grid/poi_grid_counts.gpkg", quiet=TRUE)
poi_ratios <- read_csv("data/models/minimum_ratios/poi.csv")


# 1. Extract buildings from OSM --------------------------------------------

aml_boundary <- st_read("data/geo/municipalities_union_bbox_plus_500m.gpkg") |> st_transform(4326) |> st_bbox()

### Using osmdata (not appropriate for such a large area)
# osm_buildings_raw <- opq(bbox = aml_boundary) |>
#   add_osm_feature(key = "building") |>
#   osmdata_sf()

### Using osmextract (working)
osm_buildings_raw <- oe_get(
  place = aml_boundary,
  layer = "multipolygons",
  query = "SELECT * FROM multipolygons WHERE building IS NOT NULL", # select all attributes from the polygons with a valid "building" attribute
  boundary = aml_boundary,
  boundary_type = "spat", # spatial boundary
  extra_tags = "building",
  force_download = TRUE,
  force_vectortranslate = TRUE,
  quiet = FALSE
)
osm_buildings <- osm_buildings_raw |>
  select(osm_id, building, name, geometry) |>
  st_transform (3763)
osm_buildings$osm_area <- as.numeric(st_area(osm_buildings))

# Buildings with area below 25m2 are to be removed, since they occupy a significant amount of memory and barely
# contribute to our analysis. A validation that the vast majority (if not all) buildings below 25m2 were not used
# as POIs was made. Although most  buildings with area between 25 and 50m2 are also not "useful", these are kept
# since there are some old/historical areas (such as Alfama) where many buildings are tiny, but still have POIs in them.
# Also assign a unique id to each building (replacing attribute "osm_id", given most of it was NA), in order to
# be able to check which POIs share a building.
osm_buildings <- osm_buildings |> filter(osm_area >= 25) |> mutate(osm_id = row_number())

#mapview(filter(osm_buildings, area >= 25 & area <= 30)) # don't try to view all buildings at once, the server will most likely crash!
#write_sf(osm_buildings, "data/base/osm_buildings.gpkg", delete_dsn = TRUE)

# 2. Get buildings of each POI --------------------------------------------
# Prefferably run this section all at once (object poi_buildings is changed a lot).

poi_in_building <- poi_all |>
  filter(!category %in% c("recreational_area")) # POIs that are not buildings

poi_buildings <- poi_in_building |> st_join( # join POIs with buildings, maintaining both poi and osm IDs.
  subset(osm_buildings, select = c("osm_id", "osm_area", "geometry")),
  join = st_within,
  left = TRUE
  ) |>
  group_by(poi_id) |> slice(1) |> ungroup() # in order to remove POIs that were associated with multiple buildings that overlap one another.

poi_buildings <- poi_buildings |> # divide area of building if multiple POIs occupy it.
  group_by(osm_id) |>
  mutate(n_poi = ifelse(is.na(osm_id), NA, n()), # count number of POIs inside each building
         poi_area = ifelse(is.na(osm_id), NA_real_, osm_area/n_poi),
         ) |>
  ungroup()

poi_buildings <- poi_buildings |> # assign median values of each category for POIs that fall outside buildings (poi_area == NA)
  group_by(category) |>
  mutate(poi_area = coalesce(poi_area, median(poi_area, na.rm = TRUE))) # coalesce to maintain values where they exist, and to add the medians to NAs


# After the first iteration of this whole script, it was noticed that there were some extreme outliers on maximum POI areas, as is shown below:

# Percentile          p50             p90             p95           p100
# Area value          186.7           761.0           1451.8        67167.4

# This way, values above p95 in each category are removed in the next lines.

poi_buildings <- poi_buildings |> # remove outliers and give them the p95 value
  group_by(category) |>
    mutate(
    p95      = quantile(poi_area, 0.95, na.rm = TRUE),
    poi_area = if_else(poi_area > p95, p95, poi_area)
  ) |>
  ungroup() |>
  select(-p95)

poi_buildings <- poi_buildings |>
  select(poi_id, name, category, subcategory, poi_area, geometry)


View(poi_buildings)
quantile(poi_buildings$poi_area, probs = seq(0,1,0.05))
mapview(filter(poi_buildings, category == "office_industry"), zcol = "poi_area")

st_write(poi_buildings, "data/processed/poi/buildings/poi_buildings.gpkg", delete_dsn = TRUE, quiet = TRUE)
write_csv(poi_buildings |> st_drop_geometry(), "data/processed/poi/buildings/poi_buildings.csv")


# 3. Aggregate to grid ---------------------------------------------------------

poi_grid_helper <- poi_buildings |>
  st_transform(4326) |>
  st_join(
    grid["gid"],
    join = st_within) |>
  st_drop_geometry() |>
  group_by(gid, category) |>
  summarise(
    n = n(),
    area = sum(poi_area),
    .groups = "drop"
  )
poi_grid_helper_2 <- poi_grid_helper |>
  pivot_wider(          # to turn the table wider, with its attributes being the nº of POIs and area per category and grid cell
    names_from = category,
    values_from = c(n, area),
    values_fill = 0,
    names_glue = "{.value}_{category}" # to get "n_category" and "area_category"
  )
poi_buildings_grid <- grid |>
  left_join(poi_grid_helper_2, by = "gid")
poi_buildings_grid[is.na(poi_buildings_grid)] <- 0


mapview(poi_buildings_grid, zcol="area_office_industry")
st_write(poi_buildings_grid, "data/processed/poi/buildings/poi_buildings_grid.gpkg", delete_dsn = TRUE, quiet = TRUE)
write_csv(poi_buildings_grid |> st_drop_geometry(), "data/processed/poi/buildings/poi_buildings_grid.csv")



