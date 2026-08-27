# =============================================================================
# SERVER-ONLY SCRIPT — kept as documentation
# =============================================================================
# This script uses impt_read()/impt_write() which require the data to be
# mounted via `mount --bind` on the server, or adapted for local use.
#
# Its output already exists on the server at:
#   /data/biclar-parking/base/daysimetric/lisbon_metro_buildings_footprints.geojson
#
# You do NOT need to re-run this script unless that file is lost, the Global
# Building Atlas source changes, or the AML boundary is updated.
# 03_dasymetric_combined.R reads from that path directly via the sshfs-mounted
# ./data/ folder.
# =============================================================================

# Buildings volume prep - AML-wide polygon footprints
# Adapted from code/test-code/buildings_height.R (removed from this project during cleanup -
# see git history if you need to compare against the original point-centroid version).
#
# What this script actually does, end to end: fetches real building footprint polygons and
# heights for the whole AML from the Global Building Atlas (a global, remotely-sensed
# building dataset - not survey/census data), computes each building's volume
# (footprint area x height) as a proxy for how much floor space/population it could plausibly
# hold, and saves the result as one polygon-with-attributes layer. This is the raw ingredient
# 02c_dasymetric_population.R uses to distribute each census block's population across grid
# cells in proportion to how much residential building volume sits in each cell - the
# assumption being that bigger/taller buildings house more people than smaller ones.
#
# Differences from the original script:
# - bbox extended from the hardcoded Lisbon-only box c(-9.50, 38.40, -8.70, 39.10) to the
#   full AML extent. The original hardcoded bbox was found to exclude most of Montijo's land
#   area (its eastern zone, ~291.6 km2) and sat right at the edge for Sintra/Mafra/Palmela -
#   see earlier coverage check.
# - SERVER VARIANT: the AML boundary itself now comes from data/geo/municipalities_union.gpkg
#   (a server-provided file) instead of this project's own municipios_union_2024.geojson,
#   since 01_data_prep_grid_only.R (which produced that file) is no longer part of this
#   workflow on the shared server - see 02c_dasymetric_population.R for the equivalent
#   change there.
# - Keeps building POLYGON geometry (footprints) instead of collapsing to point centroids -
#   needed for area-overlap-based dasymetric disaggregation against the H3 grid and BGRI.
# - SERVER VARIANT: writes to data/base/daysimetric/lisbon_metro_buildings_footprints.geojson
#   rather than /pois/ - matching where 02c_dasymetric_population.R now reads it from, and
#   this project's other derived outputs on the server. Still does NOT touch or overwrite
#   lisbon_metro_buildings_height.geojson, which 02_jobs_imob.R still depends on (odjitter
#   needs points, not polygons, for its subpoints_origins/destinations argument).
# - No download/unzip block needed: this reads live from the public S3 parquet source via
#   DuckDB's httpfs extension, same as the original script - nothing to pre-fetch locally.

required_packages <- c("duckdb", "sf", "dplyr")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) install.packages(missing_packages)

library(duckdb)
library(sf)
library(dplyr)

# AML boundary + bbox --------------------------------------------------------
# The building-data source below is queried by a rectangular bounding box, not the actual
# AML polygon shape - st_bbox() reduces the (irregular, coastline-following) AML boundary to
# the smallest rectangle that fully contains it. This means the query below will also pull
# in some buildings that sit inside that rectangle but outside the true AML boundary (e.g. in
# the rectangle's corners). That overshoot is harmless here: 02c_dasymetric_population.R
# later intersects this buildings layer against the grid and COS layers, both of which are
# clipped to the real AML shape, so any building outside the true boundary simply never
# matches anything and drops out of the final result - it's filtered out downstream, not
# something this script needs to handle itself.

aml_boundary <- sf::st_read("data/geo/municipalities_union.gpkg", quiet = TRUE)
aml_bbox <- st_bbox(aml_boundary)
aml_bbox # sanity check before querying - compare against the coverage check we did earlier

# Global Building Atlas LOD1 fetch (polygons kept) ---------------------------
# Source: https://source.coop/tge-labs/globalbuildingatlas-lod1
# Tile "w010_n40_w005_n35" covers lon -10..-5, lat 35..40 - the full AML extent falls
# inside this single tile (same as the original script), so only the bbox FILTER values
# passed into the SQL query change, not the tile file itself.
#
# How the fetch actually works: the building dataset is stored as a "parquet" file (a
# columnar data format) sitting in public cloud storage (S3), tens of millions of buildings
# for the whole tile. Rather than downloading that entire file, this function uses DuckDB -
# an in-process database engine - to run a SQL query directly against the remote file over
# HTTP, so only the rows (buildings) that actually fall in the AML bounding box are ever
# transferred.

fetch_building_polygons <- function(bbox, parquet_file_dir) {
  parquet_s3_path <- paste0("s3://us-west-2.opendata.source.coop/tge-labs/globalbuildingatlas-lod1/", parquet_file_dir, ".parquet")

  q_xmin <- bbox[["xmin"]]
  q_ymin <- bbox[["ymin"]]
  q_xmax <- bbox[["xmax"]]
  q_ymax <- bbox[["ymax"]]

  con <- dbConnect(duckdb())
  # spatial: adds geometry/GIS functions (ST_AsWKB etc. below) to DuckDB's SQL dialect.
  # httpfs: lets DuckDB read files over http(s)/S3 as if they were local, which is what
  # makes querying the remote parquet file directly (instead of downloading it) possible.
  dbExecute(con, "INSTALL spatial; LOAD spatial; INSTALL httpfs; LOAD httpfs; SET httpfs_client_implementation='httplib'; SET s3_region='us-west-2'; SET preserve_insertion_order=false;")
  # httpfs_client_implementation='httplib' works around a DuckDB 1.5+ SSL bug on S3 HTTP
  # requests - see https://github.com/duckdb/duckdb-httpfs/issues/282

  # The dataset stores each building's own bounding box as a "bbox" struct column - the
  # WHERE clause below uses that to keep only buildings fully contained within the AML query
  # bbox. geometry is excluded from the plain SELECT and re-added as WKB (Well-Known Binary,
  # a standard binary geometry encoding) via ST_AsWKB/TRY_CAST, since that's the form R's sf
  # package can decode directly further down - DuckDB's native geometry type can't be handed
  # straight to R.
  query <- paste0("
    SELECT * EXCLUDE (geometry),
           ST_AsWKB(TRY_CAST(geometry AS GEOMETRY)) as geom_wkb
    FROM read_parquet('", parquet_s3_path, "')
    WHERE bbox.xmin >= ", q_xmin, " AND bbox.xmax <= ", q_xmax, "
      AND bbox.ymin >= ", q_ymin, " AND bbox.ymax <= ", q_ymax)

  message("Fetching AML-wide building polygons (larger extent than the original Lisbon-only bbox, expect a longer fetch)...")
  raw_data <- dbGetQuery(con, query) |>
    filter(!is.na(geom_wkb)) # remove NULLs created by TRY_CAST (rows whose geometry failed to cast cleanly)
  dbDisconnect(con) # stop the connection to free resources

  if (nrow(raw_data) == 0) {
    message("No buildings found for this area.")
    return(NULL)
  }

  # geom_wkb is decoded back into an actual sf geometry column here (crs = 4326 because
  # that's the coordinate system the source dataset stores coordinates in) - this is what
  # turns raw_data from a plain data frame into a proper spatial (sf) object.
  # Transform to SF polygons, keep footprint geometry (no centroid collapse)
  buildings_polygons <- raw_data |>
    mutate(geometry = st_as_sfc(geom_wkb, crs = 4326)) |>
    st_as_sf() |>
    mutate(
      # CAVEAT worth knowing about: EPSG:3857 (Web Mercator) is NOT an equal-area
      # projection - it inflates area increasingly with distance from the equator. At AML's
      # latitude (~38.7N) that inflation is roughly 1.6x, i.e. footprint_m2/total_floor_area_m2
      # /volume_m3 below are systematically ~60% larger than the true ground-measured values.
      # (The rest of this project correctly uses EPSG:3763, a Portugal-specific projected CRS,
      # for area/distance math elsewhere - e.g. 02c_dasymetric_population.R's target_crs -
      # this is the one place that doesn't.) This does NOT bias the final population
      # estimate in 02c_dasymetric_population.R, because that script's prorating step divides
      # a fragment's area (computed correctly, in EPSG:3763) by this same building's
      # footprint_m2 - since volume_m3 and footprint_m2 both carry the same ~1.6x inflation,
      # it cancels out in that ratio. It WOULD matter if footprint_m2/volume_m3/
      # total_floor_area_m2 from this file are ever reported or used as real-world figures on
      # their own (e.g. "total residential building volume in the AML") - those numbers, as
      # stored, are not actually true square/cubic meters.
      footprint_m2 = round(as.numeric(st_area(st_transform(geometry, 3857)))), # metric CRS for area
      est_floors = pmax(1, round(height / 3)), # average 3m height per floor, minimum of 1 floor
      total_floor_area_m2 = round(footprint_m2 * est_floors),
      volume_m3 = round(footprint_m2 * height)
    ) |>
    # `var` is the source dataset's height-prediction uncertainty/variance (per the Global
    # Building Atlas paper), not a building-use/type field despite the generic name - not
    # used by any calculation in this project, kept for provenance/QA. `source` (data
    # provenance, e.g. which imagery/method produced this building) is likewise passed
    # through unused.
    select(source, id, height, var, est_floors, footprint_m2, total_floor_area_m2, volume_m3, geometry)

  return(buildings_polygons)
}

lisbon_file_dir <- "w010_n40_w005_n35" # same tile as buildings_height.R - covers full AML

aml_buildings_footprints <- fetch_building_polygons(aml_bbox, lisbon_file_dir)

# Quick sanity checks before writing - row count in the hundreds of thousands and plausible
# building heights/volumes are the expected signal that the fetch actually worked; if
# nrow() is suspiciously low or height/volume look degenerate (all zero, all NA), that's
# worth investigating before relying on the exported file downstream.
nrow(aml_buildings_footprints)
summary(aml_buildings_footprints$height)
summary(aml_buildings_footprints$volume_m3)

# export ---------------------------------------------------------------
dir.create("data/base/daysimetric", recursive = TRUE, showWarnings = FALSE)
sf::st_write(aml_buildings_footprints,
             "data/base/daysimetric/lisbon_metro_buildings_footprints.geojson",
             delete_dsn = TRUE)
