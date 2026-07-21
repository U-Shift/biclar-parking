# Aggregation levels base layers set up: municipality, parish, hex (res 9)

library(sf)
library(dplyr)
library(mapview)


# Parish and municipality ------------------------------------------------------------

## 1. Fetch latest CAOP from DGT ----------------------------------------------------
# (Source: https://www.dgterritorio.gov.pt/atividades/cartografia/cartografia-tematica/caop)

# > Download zip
link_DGT_CAOP <- "https://geo2.dgterritorio.gov.pt/caop/CAOP_Continente_2025-gpkg.zip"
CAOP_zip <- tempfile(fileext = ".zip")
download.file(link_DGT_CAOP, destfile = CAOP_zip, mode = "wb")
zip::zip_list(CAOP_zip) # Validate the contents of the zip file

# > Unzip gpkg
CAOP_gpkg <- tempdir()
unzip(CAOP_zip, exdir = CAOP_gpkg)
list.files(CAOP_gpkg)

# > Read gpkg
CAOP <- sf::st_read(file.path(CAOP_gpkg, "Continente_CAOP2025.gpkg"))
# mapview(CAOP)
st_crs(CAOP) # 3763

# > Filter by NUT II Lisboa
nrow(CAOP) # 3392
CAOP_GLPS <- CAOP |>
  filter(nuts2 %in% c("Grande Lisboa", "Península de Setúbal")) |>
  select(-id, -nuts1, -nuts3, -tipo_area_administrativa, -distrito_ilha, -perimetro_km) |>
  st_transform(crs = 4326)
nrow(CAOP_GLPS) # 189
names(CAOP_GLPS) 
# mapview(CAOP_GLPS)

# sf::st_write(CAOP_GLPS, "data/geo/CAOP_GLPS.gpkg", delete_dsn = TRUE)
# CAOP_GLPS <- sf::st_read("data/geo/CAOP_GLPS.gpkg")


## 2. Fetch latest COS from DGT ----------------------------------------------------
# (Source: https://www.dgterritorio.gov.pt/dados-abertos)

# > Download zip
link_DGT_COS <- "https://geo2.dgterritorio.gov.pt/cos/S2/COS2023/COS2023v1-S2-gpkg.zip"
COS_zip <- tempfile(fileext = ".zip")
download.file(link_DGT_COS, destfile = COS_zip, mode = "wb")
zip::zip_list(COS_zip)

# > Unzip gpkg
COS_gpkg <- tempdir()
unzip(COS_zip, exdir = COS_gpkg)
list.files(COS_gpkg)
COS <- sf::st_read(file.path(COS_gpkg, "COS2023v1-S2.gpkg")) |> st_transform(crs = 4326)
# mapview(COS)
names(COS)


## 3. Adjust geometries to remove water from parish areas ----------------------------------------------------

# > Get COS inside CAOP_GLPS
nrow(COS) # 783760
COS_GLPS <- COS |>
  st_intersection(CAOP_GLPS |> st_union()) |>
  st_make_valid() |>
  mutate(n_row = row_number())
nrow(COS_GLPS) # 31104
# mapview(COS_GLPS)
table(COS_GLPS$COS23_n4_L)

# > Remove water from COS
WATER_LABELS <- c("Desembocaduras fluviais", "Zonas entremarés", "Sapais", "Pauis e turfeiras")
# FARMING_LABELS <- c("Arrozais", "Culturas temporárias de sequeiro e regadio", "Pastagens melhoradas")
mapview(COS_GLPS |> filter(COS23_n4_L %in% WATER_LABELS), layer.name="Water", col.regions="#00ccff") + 
  # mapview(COS_GLPS |> filter(COS23_n4_L %in% FARMING_LABELS), layer.name="Farming", col.regions="#00ff00") +
  # mapview(COS_GLPS |> filter(COS23_n4_L == "Rede rodoviária"), layer.name="Roads", col.regions="#808080") + 
  mapview(COS_GLPS |> filter(!COS23_n4_L %in% c(WATER_LABELS)), layer.name="Land", col.regions="#996633")
COS_GLPS_LAND <- COS_GLPS |>
  filter(!(COS23_n4_L %in% WATER_LABELS))
nrow(COS_GLPS_LAND) # 30975
# mapview(COS_GLPS_LAND)

# > Remove bboxs from COS_GLPS_LAND
bbox = list(
  c(38.698032, -9.179598, 38.682482, -9.174718), # Ponte 25 de Abril
  c(38.736986, -9.008036, 38.783230, -9.081920), # Ponte Vasco da Gama South
  c(38.786945, -9.090903, 38.782942, -9.075899) # Ponte Vasco da Gama North
)
for (i in seq_along(bbox)) { # i = 1
  bbox_i = st_as_sfc(
    st_bbox(c(xmin = bbox[[i]][2], xmax = bbox[[i]][4], ymin = bbox[[i]][3], ymax = bbox[[i]][1]), 
    crs = st_crs(4326))
  )
  # mapview(bbox_i)
  COS_GLPS_LAND <- COS_GLPS_LAND |>
    st_difference(bbox_i)
}
mapview(COS_GLPS_LAND, layer.name="COS Land", col.regions="#996633")


# > Cut CAOP using COS land geometry
sf_use_s2(FALSE)
COS_GLPS_LAND_UNION = COS_GLPS_LAND |>
  st_union()
sf_use_s2(TRUE)
mapview(COS_GLPS_LAND_UNION, layer.name="COS Land Union", col.regions="#996633") +
  mapview(COS_GLPS, layer.name="COS", col.regions="#00cc99") +
  mapview(COS_GLPS_LAND, layer.name="COS Land", col.regions="#ffcc66") +
  mapview(CAOP_GLPS, layer.name="CAOP GLPS", col.regions="#015160")
# sf::st_write(COS_GLPS_LAND, "data/geo/COS_GLPS_LAND.gpkg", delete_dsn = TRUE)
# COS_GLPS_LAND = sf::st_read("data/geo/COS_GLPS_LAND.gpkg")


# > Cut CAOP_GLPS with COS_GLPS_LAND
CAOP_GLPS_LAND <- CAOP_GLPS |>
  st_intersection(COS_GLPS_LAND_UNION) |>
  st_make_valid()
nrow(CAOP_GLPS_LAND) # 188
# mapview(CAOP_GLPS_LAND)


## 4. Make sure each parish has one unique polygon ----------------------------------------------------
# > For parishes with multiple polygons, choose the one with greatest area_ha, without loosing other attributes
CAOP_GLPS_LAND_UNIQUE_dtmnfr <- CAOP_GLPS_LAND |>
  group_by(dtmnfr) |>
  slice_max(order_by = area_ha, n = 1, with_ties = FALSE) |>
  ungroup()
nrow(CAOP_GLPS_LAND_UNIQUE_dtmnfr) # 124
mapview(CAOP_GLPS_LAND_UNIQUE_dtmnfr)

mapview(
  CAOP_GLPS,
  # Hide elements, for printscreen
  homebutton = FALSE, legend = TRUE, layer.control.show = FALSE,
  layer.name = "Original CAOP",
  # Color
  col.regions = "#015160"
) + mapview(
  CAOP_GLPS_LAND,
  # Hide elements, for printscreen
  homebutton = FALSE, legend = TRUE, layer.control.show = FALSE,
  # Color
  col.regions = "#ff33cc",
  layer.name = "CAOP adjusted to COS land"
)


# sf::st_write(CAOP_GLPS_LAND_UNIQUE_dtmnfr, "data/geo/CAOP_GLPS_LAND_UNIQUE_dtmnfr.gpkg", delete_dsn = TRUE)
# CAOP_GLPS_LAND_UNIQUE_dtmnfr = sf::st_read("data/geo/CAOP_GLPS_LAND_UNIQUE_dtmnfr.gpkg")

## 5. Compute parishes area and generate final geometry ----------------------------------------------------

parishes_metric <- CAOP_GLPS_LAND_UNIQUE_dtmnfr |>
  st_transform(crs = 3763) |>
  mutate(area_km2 = round(as.numeric(st_area(st_geometry(geom))) / 1e6, 2)) |>
  st_drop_geometry()

parishes <- CAOP_GLPS_LAND_UNIQUE_dtmnfr |>
  select(dtmnfr, freguesia, nuts3_cod) |>
  rename(
    pid =  dtmnfr,
    name = freguesia,
    nid = nuts3_cod
  ) |>
  mutate(
    mid = substr(as.character(pid), 1, 4),
    nid = paste("PT", substr(as.character(nid), 1, 2), sep = "")
  ) |>
  left_join(parishes_metric |> select(dtmnfr, area_km2), by = c("pid" = "dtmnfr"))
parishes

# mapview(parishes, layer.name = "Parishes", zcol = "area_km2")

# sf::st_write(parishes |> select(pid, area_km2), "data/geo/parishes.gpkg", delete_dsn = TRUE)
# parishes <- sf::st_read("data/geo/parishes.gpkg")
# write.csv(parishes |> st_drop_geometry(), "useful_data/parishes.csv", row.names = FALSE)

## 6. Group by municipality ----------------------------------------------------

municipalities <- CAOP_GLPS_LAND |>
  mutate(
    mid = substr(as.character(dtmnfr), 1, 4),
    nid = paste("PT", substr(as.character(nuts3_cod), 1, 2), sep = ""),
    name = municipio
  ) |>
  group_by(mid, name, nid) |>
  summarise(geometry = st_union(geom))
nrow(municipalities) # 18
# mapview(municipalities, zcol="name")
# mapview(municipalities, zcol="nid")

municipios_metric <- municipalities |>
  st_transform(crs = 3763) |>
  mutate(area_km2 = round(as.numeric(st_area(st_geometry(geometry))) / 1e6, 2)) |>
  st_drop_geometry() |>
  ungroup()

municipalities <- municipalities |>
  left_join(municipios_metric |> select(mid, area_km2), by = "mid")

# sf::st_write(municipalities |> select(mid, area_km2), "data/geo/municipalities.gpkg", delete_dsn = TRUE)
# municipalities = sf::st_read("data/geo/municipalities.gpkg")
write.csv(municipalities |> st_drop_geometry(), "useful_data/municipalities.csv", row.names = FALSE)

## 7. Get union for whole area (for export tools) ----------------------------------------------------

municipalities_union <- municipalities |>
  sf::st_union() |>
  sf::st_make_valid()
# mapview(municipalities_union)

# sf::st_write(municipalities_union, "data/geo/municipalities_union.geojson", delete_dsn = TRUE)
# sf::st_write(municipalities_union, "data/geo/municipalities_union.gpkg", delete_dsn = TRUE)
# municipalities_union = sf::st_read("data/geo/municipalities_union.gpkg")

plus_distances <- c(50, 100, 150, 500, 1000, 2000, 5000)
for (dist in plus_distances) {
  municipalities_union_bbox_plus <- municipalities_union |>
    st_transform(crs = 3763) |>
    st_buffer(dist = dist) |>
    st_transform(crs = 4326)
  # mapview(municipalities_union_bbox_plus)
  sf::st_write(municipalities_union_bbox_plus, paste0("data/geo/municipalities_union_plus_", dist, "m.gpkg"), delete_dsn = TRUE)
}

# for the bbox (to Copernicus)
municipalities_union_bbox <- st_as_sfc(st_bbox(municipalities_union))
# mapview(municipalities_union_bbox)
st_write(municipalities_union_bbox, "data/geo/municipalities_union_bbox.gpkg", delete_dsn = TRUE)

plus_distances <- c(50, 100, 150, 500, 1000, 2000, 5000)
for (dist in plus_distances) {
  municipalities_union_bbox_plus <- municipalities_union_bbox |>
    st_transform(crs = 3763) |>
    st_buffer(dist = dist) |>
    st_transform(crs = 4326)
  # mapview(municipalities_union_bbox_plus)
  st_write(municipalities_union_bbox_plus, paste0("data/geo/municipalities_union_bbox_plus_", dist, "m.gpkg"), delete_dsn = TRUE)
}

# H3 Grid ----------------------------------------------------

# TODO! From here
## 1. Get H3 grid matching the study area ----------------------------------------------------
# We will keep using the TML grid for now, but in the future we can produce a new one with h3, which is replicable and has nice properties (equal area, hierarchical, etc).
library(h3jsr)

h3_res <- 9 # 400m diameter (https://h3geo.org/docs/core-library/restable/)

# > Use buffer to make sure all land is covered by grid
municipalities_union_bbox_plus = sf::st_read("data/geo/municipalities_union_plus_500m.gpkg") # 25112 cells
# municipalities_union_bbox_plus = sf::st_read("data/geo/municipalities_union.gpkg") # 24156 cells

grid_ids <- municipalities_union_bbox_plus |> # municipalities_union |>
  polygon_to_cells(res = h3_res)
grid <- grid_ids$`0` |>
  cell_to_polygon(simple = FALSE)
nrow(grid)

# > Remove grid that does not overlap with municipalities_union
grid_overlap <- grid |>
  st_make_valid() |>
  st_intersection(municipalities_union) |>
  st_drop_geometry()
grid <- grid |>
  filter(h3_address %in% grid_overlap$h3_address)
mapview(grid)

mapview(municipalities_union, layer.name = "Municipalities Union", col.regions = "#015160", legend=TRUE) +
  mapview(municipalities_union_bbox_plus, layer.name = "Municipalities Union + buffer", col.regions = "#00ccff", legend=TRUE) +
  mapview(grid, layer.name = "H3 Grid", col.regions = "#ffcc00", legend=FALSE, zcol=NULL) 
nrow(grid) # 25380


grid <- grid |>
  mutate(gid = seq(1:nrow(grid))) # give an ID to each cell
# mapview(grid, zcol="gid", layer.name = "H3 Grid with ID", legend=FALSE)

# sf::st_write(grid, "data/geo/grid.gpkg", delete_dsn = TRUE)
# grid = sf::st_read("data/geo/grid.gpkg")


## 2. Extend grid with parish and municipality data ----------------------------------------------------

# > Find centroids
grid_centroids = st_centroid(grid)
nrow(grid_centroids) # 25380
# mapview(grid_centroids)
# sf::st_write(grid_centroids, "data/geo/grid_centroids.gpkg", delete_dsn = TRUE)
# grid_centroids = sf::st_read("data/geo/grid_centroids.gpkg")

# > Overlap with CAOP to get parish information
grid_nuts = st_join(
  grid_centroids |> select(gid), 
  CAOP_GLPS |> select(dtmnfr) |> rename(pid=dtmnfr), join = st_within)
summary(grid_nuts)

# > For missing parish info (because the centroid falls outside any parish polygon due to buffer), get closest
grid_nuts_missing_parish <- grid_nuts |> filter(is.na(pid))
nrow(grid_nuts_missing_parish) # 743
mapview(grid_nuts_missing_parish, layer.name = "Grid without parish ID", col.regions = "#ff0000", legend=FALSE)

closest_parish <- st_nearest_feature(grid_nuts_missing_parish, CAOP_GLPS)
grid_nuts_missing_parish <- grid_nuts_missing_parish |>
  mutate(pid = CAOP_GLPS$dtmnfr[closest_parish])

mapview(grid_nuts_missing_parish, layer.name = "Grid with closest parish ID", col.regions = "#00ff00", legend=FALSE) +
  mapview(CAOP_GLPS, layer.name = "CAOP GLPS", col.regions = "#015160")

# > Join back to grid_nuts
grid_nuts <- grid_nuts |>
  left_join(
    grid_nuts_missing_parish |> st_drop_geometry() |> select(gid, pid),
    by = "gid",
    suffix = c("", "_closest")
  ) |>
  mutate(pid = ifelse(is.na(pid), pid_closest, pid)) |>
  select(-pid_closest)
nrow(grid_nuts |> filter(is.na(pid))) # 0
# mapview(grid_nuts, layer.name = "Grid with parish ID", zcol="pid", legend=FALSE)

# > Get mid and nid from parishes
grid_nuts_meta <- grid_nuts |>
  st_drop_geometry() |>
  left_join(
    parishes |> st_drop_geometry() |> select(pid, mid, nid),
    by = "pid"
  ) |>
  left_join(grid |> st_drop_geometry() |> select(gid, h3_address), by = "gid")
grid_nuts_meta

# write.csv(grid_nuts_meta, "useful_data/grid.csv", row.names = FALSE)
