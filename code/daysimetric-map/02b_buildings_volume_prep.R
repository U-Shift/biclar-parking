# =============================================================================
# Fetch Global Building Atlas footprints & estimate building volume
#
# Run with: $ Rscript code/daysimetric-map/02b_buildings_volume_prep.R > code/daysimetric-map/02b_buildings_volume_prep_$(date +%Y%m%d).log 2>&1
#
# Procedure:
#   1. Load AML boundary and compute its rectangular bounding box.
#   2. Initialize DuckDB with spatial and httpfs extensions to query S3 directly.
#   3. Query remote Global Building Atlas LOD1 parquet file filtered by AML bbox.
#   4. Convert WKB geometries to sf polygons and calculate building metrics:
#      - footprint_m2 (metric area using EPSG:3857)
#      - est_floors (height / 3m)
#      - total_floor_area_m2 (footprint * floors)
#      - volume_m3 (footprint * height)
#   5. Export building footprints with volumes as GeoJSON.
#
# Inputs:
#   - data/geo/municipalities_union.gpkg
#   - Remote S3 Parquet: globalbuildingatlas-lod1 (tile w010_n40_w005_n35)
#
# Output:
#   - data/base/daysimetric/lisbon_metro_buildings_footprints.geojson
#
# WARNING:
#   Output already exists on the server (data/base/daysimetric/). Do NOT
#   re-run this script unless the output file is missing, the Global Building
#   Atlas source changes, or the AML boundary is updated.
# =============================================================================

# 1. Setup -------------------------------------------------------------------

required_packages <- c("duckdb", "sf", "dplyr")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) install.packages(missing_packages)

library(duckdb)
library(sf)
library(dplyr)

# 2. Get AML bounding box ----------------------------------------------------

aml_boundary <- sf::st_read("data/geo/municipalities_union.gpkg", quiet = TRUE)
aml_bbox <- st_bbox(aml_boundary)
aml_bbox

# 3. Query Global Building Atlas via DuckDB S3 HTTPFS ------------------------

fetch_building_polygons <- function(bbox, parquet_file_dir) {
  parquet_s3_path <- paste0("s3://us-west-2.opendata.source.coop/tge-labs/globalbuildingatlas-lod1/", parquet_file_dir, ".parquet")

  q_xmin <- bbox[["xmin"]]
  q_ymin <- bbox[["ymin"]]
  q_xmax <- bbox[["xmax"]]
  q_ymax <- bbox[["ymax"]]

  con <- dbConnect(duckdb())
  # Setup DuckDB with spatial and httpfs extensions
  dbExecute(con, "INSTALL spatial; LOAD spatial; INSTALL httpfs; LOAD httpfs; SET httpfs_client_implementation='httplib'; SET s3_region='us-west-2'; SET preserve_insertion_order=false;")

  # Filter parquet directly by bbox over S3 HTTPFS
  query <- paste0("
    SELECT * EXCLUDE (geometry),
           ST_AsWKB(TRY_CAST(geometry AS GEOMETRY)) as geom_wkb
    FROM read_parquet('", parquet_s3_path, "')
    WHERE bbox.xmin >= ", q_xmin, " AND bbox.xmax <= ", q_xmax, "
      AND bbox.ymin >= ", q_ymin, " AND bbox.ymax <= ", q_ymax)

  message("Fetching AML-wide building polygons (larger extent than the original Lisbon-only bbox, expect a longer fetch)...")
  raw_data <- dbGetQuery(con, query) |>
    filter(!is.na(geom_wkb))
  dbDisconnect(con)

  if (nrow(raw_data) == 0) {
    message("No buildings found for this area.")
    return(NULL)
  }

  # Convert WKB to sf geometry and calculate building metrics
  buildings_polygons <- raw_data |>
    mutate(geometry = st_as_sfc(geom_wkb, crs = 4326)) |>
    st_as_sf() |>
    mutate(
      footprint_m2 = round(as.numeric(st_area(st_transform(geometry, 3763)))), # metric CRS for Portugal (EPSG:3763)
      est_floors = pmax(1, round(height / 3)), # average 3m height per floor, minimum of 1 floor
      total_floor_area_m2 = round(footprint_m2 * est_floors),
      volume_m3 = round(footprint_m2 * height)
    ) |>
    select(source, id, height, var, est_floors, footprint_m2, total_floor_area_m2, volume_m3, geometry)

  return(buildings_polygons)
}

# 4. Fetch AML buildings -----------------------------------------------------

lisbon_file_dir <- "w010_n40_w005_n35" # Tile covering lon -10..-5, lat 35..40

aml_buildings_footprints <- fetch_building_polygons(aml_bbox, lisbon_file_dir)

nrow(aml_buildings_footprints)
summary(aml_buildings_footprints$height)
summary(aml_buildings_footprints$volume_m3)

# 5. Export ------------------------------------------------------------------

dir.create("data/base/daysimetric", recursive = TRUE, showWarnings = FALSE)
sf::st_write(aml_buildings_footprints,
  "data/base/daysimetric/lisbon_metro_buildings_footprints.geojson",
  delete_dsn = TRUE
)
