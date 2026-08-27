# Data preparation - GRID ONLY variant
# Extracted from 01_data_prep.R: CAOP admin boundaries -> limit -> H3 grid.
# Deliberately excludes: OSM roads, trip jittering, BGRI census, POIs, GTFS, DEM, r5r routing.
#
# What this script actually produces, end to end: starting from Portugal's official
# administrative boundary dataset (CAOP), it isolates the Lisbon Metropolitan Area (AML) -
# the study area for this project - as a single boundary polygon, then covers that boundary
# with a grid of H3 hexagonal cells. That hexagon grid is the common spatial unit every
# later script in this project aggregates data onto (building volumes, census population,
# eventually the population-per-cell dasymetric map). Nothing here touches population,
# buildings, or land use yet - this script only builds the boundary and the grid.
#
# Difference from the original CAOP block: the "/mqat/..." Lisbon river-clip refinement
# (original lines 25-35) is dropped. We use the unclipped CAOP Lisbon boundary instead,
# since the /mqat/ dataset (a separate U-Shift project) was never sourced for this task.
# Practical effect: Lisbon's freguesia polygons will include some river area that the
# original pipeline trims out. This does not affect grid generation itself.

required_packages <- c("sf", "dplyr", "mapview", "h3jsr")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) install.packages(missing_packages)

library(sf)
library(dplyr)
library(mapview)
library(h3jsr)

# CAOP polygons ------------------------------------------------------------
# CAOP (Carta Administrativa Oficial de Portugal) is DGT's official Portuguese
# administrative boundary dataset - it defines every freguesia (civil parish, the smallest
# administrative unit in Portugal) and municipio (municipality) as a polygon, nationwide.
# This section's job is to cut that national dataset down to just the freguesias that make
# up the Lisbon Metropolitan Area (AML).

# The raw national file - every freguesia in mainland Portugal, not yet filtered to AML.
CAOP_PT <- impt_read("/original/Continente_CAOP2024_1.gpkg")

# AML = the "Grande Lisboa" and "Península de Setúbal" NUTS2/NUTS3-level subdivisions
# combined - this is the actual study-area filter. The select(-...) drops CAOP columns not
# needed downstream (internal ids, higher-level NUTS codes, etc.). Reprojecting to EPSG:4326
# (WGS84 lat/lon) here fixes the CRS every later script in this project assumes for geo files
# written to /geo/ - see the target_crs pattern in 02c_dasymetric_population.R, which
# transforms to a metric CRS only transiently for area/distance math and always converts
# back to 4326 before writing output, matching this convention.
CAOP_GLPS <- CAOP_PT |>
  filter(nuts2 %in% c("Grande Lisboa", "Península de Setúbal")) |>
  select(-id, -nuts1, -nuts3, -tipo_area_administrativa, -distrito_ilha, -perimetro_km) |>
  st_transform(crs = 4326)
names(CAOP_GLPS)

# NOTE: original script's Lisbon-river-clip step (rbind with /mqat/ freguesias) skipped here.
# CAOP_GLPS is used as-is, unclipped.

# Some freguesias are stored as more than one polygon (e.g. a parish with a small detached
# exclave, or a digitizing artifact) - group_by + slice_max keeps only the largest polygon
# per freguesia id (dtmnfr), so each freguesia is represented by exactly one row/geometry.
# This is a simplification (a tiny detached fragment of a parish gets dropped), not a
# precise dissolve - acceptable here since the grid this script builds doesn't depend on
# freguesia boundaries being pixel-perfect, only on the overall AML boundary further below.
CAOP_GLPS_UNIQUE_dtmnfr <- CAOP_GLPS |>
  group_by(dtmnfr) |>
  slice_max(order_by = area_ha, n = 1, with_ties = FALSE) |>
  ungroup()

freguesias <- CAOP_GLPS_UNIQUE_dtmnfr

# Visual sanity check only - compares the full (possibly multi-polygon) freguesias against
# the deduplicated single-polygon version, so you can eyeball whether the slice_max() step
# above dropped anything that actually mattered. Not required for the script to proceed.
mapview(
  CAOP_GLPS,
  homebutton = FALSE, legend = FALSE, layer.control.show = FALSE,
  col.regions = "#015160"
) + mapview(
  freguesias,
  homebutton = FALSE, legend = FALSE, layer.control.show = FALSE,
  col.regions = "#015160"
)

impt_write(CAOP_GLPS, "/geo/freguesias_2024.gpkg")
impt_write(CAOP_GLPS_UNIQUE_dtmnfr, "/geo/freguesias_2024_unique.gpkg")
# Re-reading immediately after writing (rather than just continuing to use the in-memory
# CAOP_GLPS_UNIQUE_dtmnfr object) is a pattern repeated throughout this script and the rest
# of the project - it round-trips the object through impt_write/impt_read so that what's
# used downstream is guaranteed to be exactly what was saved to disk, not an in-memory
# version that might differ subtly (e.g. column order, attribute cleanup sf/GDAL applies on
# write) from what a later script will get when it loads the same file fresh.
freguesias <- impt_read("/geo/freguesias_2024_unique.gpkg")

# A municipio (municipality) is the administrative level directly above freguesia - each
# municipality is made up of several freguesias. Dissolving (st_union) all of a
# municipality's freguesia polygons together collapses those internal parish boundaries,
# leaving one polygon per municipality instead of one per parish.
municipios <- CAOP_GLPS |>
  group_by(municipio) |>
  summarise(geometry = st_union(geom)) |>
  st_transform(crs = 4326)

# NOTE: original script did `municipios <- municipios[-6, ] # Remove strange Lisbon` here.
# That row-removal was a workaround for an artifact of the /mqat/ rbind step we've dropped,
# so it is NOT applied here. Sanity check instead: AML = Grande Lisboa (9) + Península de
# Setúbal (9) = 18 municipalities. Verify below before trusting this layer downstream.
nrow(municipios) # expect 18 - if not, inspect `municipios` (e.g. mapview(municipios)) before proceeding
mapview(municipios)

impt_write(municipios, "/geo/municipios_2024.gpkg")
municipios <- impt_read("/geo/municipios_2024.gpkg")

# One more dissolve, this time across ALL 18 municipalities into a single polygon - this is
# the actual AML study-area boundary used everywhere else in this project (e.g. as the
# outline the H3 grid below is generated to cover, and as the clip boundary in
# 01c_cos_residential_prep.R and the bbox source in 02b_buildings_volume_prep.R).
# st_make_valid() repairs any small topology issues (e.g. self-intersections at the seams
# where municipality polygons used to touch) that a dissolve of this many polygons can
# introduce.
municipios_union <- municipios |>
  sf::st_union() |>
  sf::st_make_valid()
impt_write(municipios_union, "/geo/municipios_union_2024.geojson")
municipios_union <- impt_read("/geo/municipios_union_2024.geojson")

# A rectangular bounding box around the AML boundary (not the boundary shape itself) -
# originally intended (per the comment below, from the source project) to define the extent
# for a Copernicus DEM (elevation) download, a step this grid-only script doesn't include.
# Kept here since it's a cheap byproduct and other scripts in the original pipeline expect
# it to exist at this path even though nothing in this project's active workflow reads it.
municipios_union_bbox <- st_as_sfc(st_bbox(municipios_union))
# NOTE: original line here was `st_write(municipios_union_bbox, st, delete_dsn = TRUE)`,
# which references an undefined `st` object - a latent bug in the source script. Fixed here
# to use impt_write consistently, since 00b_data_load.R expects this exact file to exist.
impt_write(municipios_union_bbox, "/geo/municipios_union_bbox_2024.geojson")

# Bridge: the grid section below (and 00b_data_load.R normally) expects a variable named
# `limit`. We're not running 00b_data_load.R in this standalone script, so define it directly.
# (00b_data_load.R was subsequently removed from this project entirely during cleanup, since
# none of the active dasymetric-pipeline scripts source it - this bridge line is now simply
# how `limit` gets defined, full stop, not a workaround for skipping a script that still exists.)
limit <- municipios_union

# H3 Grid --------------------------------------------------------------------
# H3 (from Uber) is a system for covering the globe with hexagonal cells at different
# "resolutions" (zoom levels) - unlike a plain square grid, its cells are close to equal
# area everywhere and nest hierarchically across resolutions. This project uses it as the
# common spatial unit that population, buildings, and (eventually) accessibility/jobs
# indicators all get aggregated onto - every H3 cell that falls inside the AML boundary
# becomes one row of the final dasymetric population map.

# Resolution: https://h3geo.org/docs/core-library/restable/
# Higher resolution = smaller, more numerous cells (finer spatial detail, more compute).
# This project settled on 10 (~150m across) after starting from lower resolutions - see the
# commented alternatives below, kept deliberately as a record of that choice, not dead code
# to clean up. Changing h3_res here means every downstream script that reads
# "/geo/grelha_h3_r10*" needs its file paths updated to match (see the r8/r9/r10 filename
# convention followed throughout this project).
h3_res <- 10 # 150m diameter
# h3_res = 9 # 400m diameter
# h3_res = 8 # 1060m diameter - use for the MVP

# polygon_to_cells() finds every H3 cell (at the chosen resolution) that overlaps the AML
# boundary polygon, returning their H3 addresses (a string id encoding a cell's location
# and resolution) - not yet actual polygon geometries. cell_to_polygon() then converts those
# addresses into the hexagon shapes themselves, which is what makes GRID_h3 a proper sf
# object you can map, intersect, and aggregate onto like any other spatial layer.
GRID_h3 <- limit |>
  polygon_to_cells(res = h3_res, simple = FALSE)
GRID_h3 <- GRID_h3$h3_addresses |>
  cell_to_polygon(simple = FALSE)
nrow(GRID_h3)

# Every downstream script joins data onto grid cells by this plain integer `id`, not the H3
# address string directly - simpler to work with in joins/group_by than the string form.
# h3_index keeps the id <-> h3_address lookup on its own (no geometry) in case the actual H3
# address is ever needed later (e.g. to look up a cell's neighbors or parent cell using H3's
# own hierarchy, which id numbers alone can't do).
GRID_h3 <- GRID_h3 |>
  mutate(id = seq(1:nrow(GRID_h3))) # give an ID to each cell
h3_index <- GRID_h3 |> st_drop_geometry() # save h3_address for later

mapview(GRID_h3)

# Only `id` (not the h3_address string) is kept in the geometry file that gets read
# throughout the rest of the project - the address itself lives separately in h3_index.Rds,
# joinable back onto the grid geometry by id whenever actually needed.
impt_write(GRID_h3 |> select(id), "/geo/grelha_h3_r10.gpkg")
impt_write(h3_index, "/geo/grelha_h3_r10_index.Rds")

## Grid centroids
# A single representative point per hexagon (its centroid), rather than the full hexagon
# shape - useful for anything that needs one coordinate per cell instead of an area (e.g.
# point-in-polygon lookups, distance calculations, or plotting cell locations without the
# visual clutter of full hexagon outlines). Not currently consumed by any of this project's
# active scripts, but written out for the same reason as municipios_union_bbox above: a
# cheap byproduct that other scripts in the wider pipeline convention expect to exist.

GRID_h3_centroids <- st_centroid(GRID_h3) |>
  select(id, h3_address)

impt_write(GRID_h3_centroids, "/geo/grelha_h3_r10_centroids.gpkg")
