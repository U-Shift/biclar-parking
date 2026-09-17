# =============================================================================
# Prep residential COS polygons and AML clipping boundary
#
# Procedure:
#   1. Dissolve CAOP municipalities/freguesias into a unified AML boundary mask.
#   2. Load COS land-use dataset and clean geometries to strict polygons.
#   3. Validate schema and filter land-use parcels by residential classes (n4).
#   4. Clip filtered residential parcels to the AML clipping boundary.
#   5. Export clipping boundary (.geojson) and residential layer (.gpkg).
#
# Inputs:
#   - data/geo/CAOP_GLPS.gpkg
#   - data/geo/COS_GLPS_LAND.gpkg
#
# Outputs:
#   - data/base/daysimetric/clipping_boundary.geojson
#   - data/base/daysimetric/cos_residential_aml.gpkg
#
# WARNING:
#   Outputs already exist on the server (data/base/daysimetric/). Do NOT
#   re-run this script unless output files are missing or raw CAOP/COS
#   source data changes. 03_dasymetric_combined.R reads these directly.
# =============================================================================

# 1. Setup -------------------------------------------------------------------

required_packages <- c("sf", "dplyr", "mapview")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) install.packages(missing_packages)

library(sf)
library(dplyr)
library(mapview)

# 2. Clipping boundary -------------------------------------------------------

# Dissolve CAOP municipalities/freguesias into a single AML boundary
caop_glps <- sf::st_read("data/geo/CAOP_GLPS.gpkg", quiet = TRUE)
message(sprintf("CAOP_GLPS.gpkg: %d row(s) before dissolve", nrow(caop_glps)))

clip_boundary <- caop_glps |>
  st_union() |>
  st_make_valid()

dir.create("data/base/daysimetric", recursive = TRUE, showWarnings = FALSE)
sf::st_write(clip_boundary, "data/base/daysimetric/clipping_boundary.geojson", delete_dsn = TRUE)

# 3. Load & clean COS land-use layer -----------------------------------------

cos_raw <- sf::st_read("data/geo/COS_GLPS_LAND.gpkg", quiet = TRUE)

# Ensure geometries are strictly polygonal
geom_types <- st_geometry_type(cos_raw)
geom_type_counts <- table(geom_types)
message(sprintf(
  "COS_GLPS_LAND.gpkg geometry type(s) found: %s",
  paste(names(geom_type_counts), geom_type_counts, sep = ": ", collapse = ", ")
))

if (!all(geom_types %in% c("POLYGON", "MULTIPOLYGON"))) {
  message("Non-polygon geometry present at the source - extracting polygonal parts only via st_collection_extract('POLYGON')")
  n_before <- nrow(cos_raw)
  cos_raw <- st_collection_extract(cos_raw, "POLYGON")
  cos_raw <- cos_raw[!st_is_empty(cos_raw), ]
  message(sprintf(
    "st_collection_extract + empty-geometry drop: %d rows before -> %d rows after (types now: %s)",
    n_before, nrow(cos_raw), paste(unique(st_geometry_type(cos_raw)), collapse = ", ")
  ))
}

# Verify schema and residential classification labels
message("COS_GLPS_LAND.gpkg columns: ", paste(names(cos_raw), collapse = ", "))

required_col <- "COS23_n4_L"
if (!required_col %in% names(cos_raw)) {
  stop(sprintf(
    paste(
      "COS_GLPS_LAND.gpkg has no '%s' column (columns found: %s).",
      "This script's residential filter is written for COS's detailed n4 classification",
      "labels - if this file uses a different/simplified land-use schema (plausible given",
      "the 'LAND' naming), the filter list below needs to be rewritten against its actual",
      "categories before this script can produce correct output. Stopping here instead of",
      "silently filtering on a column that doesn't exist.",
      sep = " "
    ),
    required_col, paste(names(cos_raw), collapse = ", ")
  ))
}

expected_residential_labels <- c(
  "Áreas edificadas residenciais contínuas predominantemente verticais",
  "Áreas edificadas residenciais contínuas predominantemente horizontais",
  "Áreas edificadas residenciais descontínuas",
  "Áreas edificadas residenciais descontínuas esparsas"
)
actual_values <- unique(cos_raw[[required_col]])
found_labels <- intersect(expected_residential_labels, actual_values)

if (length(found_labels) == 0) {
  stop(sprintf(
    paste(
      "Found a '%s' column, but NONE of the expected residential category labels are",
      "present in it. Actual unique values: %s.",
      "The residential filter list below needs to be rewritten against this file's actual",
      "categories before this script can produce correct output.",
      sep = " "
    ),
    required_col, paste(actual_values, collapse = " | ")
  ))
} else if (length(found_labels) < length(expected_residential_labels)) {
  warning(sprintf(
    paste(
      "Only %d of the %d expected residential category labels were found in '%s'",
      "(missing: %s) - the residential filter below may be incomplete for this file.",
      "Proceeding anyway, but review whether the missing categories should be added",
      "under different label text.",
      sep = " "
    ),
    length(found_labels), length(expected_residential_labels), required_col,
    paste(setdiff(expected_residential_labels, found_labels), collapse = ", ")
  ))
}

# 4. Filter residential classes ----------------------------------------------

cos_residential <- cos_raw |> filter(COS23_n4_L %in% expected_residential_labels)

mapview(cos_residential)

# 5. Clip to AML boundary ----------------------------------------------------

cos_residential_aml <- cos_residential |> st_transform(crs = st_crs(clip_boundary))
cos_residential_aml <- cos_residential_aml[clip_boundary, ]

if ("ID" %in% names(cos_residential_aml)) {
  cos_residential_aml <- cos_residential_aml |> select(-ID)
}

mapview(cos_residential_aml, zcol = "COS23_n4_L")

# 6. Export ------------------------------------------------------------------

sf::st_write(cos_residential_aml, "data/base/daysimetric/cos_residential_aml.gpkg", delete_dsn = TRUE)
