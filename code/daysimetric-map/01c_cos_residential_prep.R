# =============================================================================
# SERVER-ONLY SCRIPT — kept as documentation
# =============================================================================
# This script uses impt_read()/impt_write() which require the data to be
# mounted via `mount --bind` on the server, or adapted for local use.
#
# Its outputs already exist on the server at:
#   /data/biclar-parking/base/daysimetric/cos_residential_aml.gpkg
#   /data/biclar-parking/base/daysimetric/clipping_boundary.geojson
#
# You do NOT need to re-run this script unless those files are lost or the
# COS source data changes. 03_dasymetric_combined.R reads from those paths
# directly via the sshfs-mounted ./data/ folder.
# =============================================================================

# COS residential layer prep - GRID ONLY variant support script
# Extracted/adapted from the commented-out block at 02_census_grid_with_cos.R:6-37.
# Produces the two files that 02_census_grid_with_cos.R expects to already exist:
#   /base/daysimetric/clipping_boundary.geojson  (AML boundary used to clip COS)
#   /base/daysimetric/cos_residential_aml.gpkg   (residential-only COS polygons, clipped to AML)
#
# SERVER VARIANT - adapted to run on the shared team server (ushift@alfa) instead of a local
# solo setup. Differences from the local version this was adapted from:
# - CAOP/COS now come from data/geo/CAOP_GLPS.gpkg and data/geo/COS_GLPS_LAND.gpkg -
#   pre-existing files on the server, not the raw national DGT downloads this script
#   originally expected. CAOP_GLPS.gpkg's naming mirrors this project's own CAOP_GLPS
#   variable (CAOP already filtered to Grande Lisboa + Setúbal), so it may be several rows
#   (one per freguesia/municipio) rather than a single dissolved boundary - dissolved
#   defensively below regardless, since union-ing an already-single polygon is a no-op.
#   COS_GLPS_LAND.gpkg's "LAND" naming suggests it may be a simplified/reclassified land-use
#   scheme rather than COS's full detailed n4 taxonomy this filter logic was written
#   against - see the schema check right after loading it below, which fails loudly rather
#   than silently filtering on a column/values that don't actually mean what this script
#   assumes they mean.
# - All derived outputs now go under data/base/daysimetric/ instead of data/geo/, matching
#   this project's other outputs on the shared server.
#
# Differences from the original commented block (still true here):
# - No download/unzip step: assumes the source files are already placed as described above.
# - The original block read a pre-existing "/geo/clipping_boundary.geojson" that was never
#   produced by any script we found (a gated-server-only file). Here we instead build it
#   from CAOP_GLPS.gpkg (see above) and write it out under the expected filename so
#   downstream scripts still find it in that path.
#
# What this script actually does, end to end: COS (Carta de Uso e Ocupação do Solo) is DGT's
# nationwide land-use/land-cover classification - every parcel of land in Portugal is tagged
# with a category (residential, industrial, agricultural, forest, water, etc.). This script
# (1) narrows that classification down to just the categories that represent residential
# buildings, and (2) clips the result to the AML study-area boundary. The output,
# cos_residential_aml.gpkg, is what 02c_dasymetric_population.R clips the Global Building
# Atlas buildings layer against, so only buildings sitting in a residential-zoned area count
# toward the population estimate - a warehouse or office block elsewhere in the COS dataset
# contributes nothing. (02_census_grid_with_cos.R, the script this was originally adapted
# from, was later removed from this project during cleanup - 02c_dasymetric_population.R is
# now the only actual consumer of this output.)

required_packages <- c("sf", "dplyr", "mapview")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) install.packages(missing_packages)

library(sf)
library(dplyr)
library(mapview)

# Clipping boundary ---------------------------------------------------------

# CAOP_GLPS.gpkg is a server-provided file, expected to already be filtered to the AML
# study area (Grande Lisboa + Setúbal) - but not necessarily already dissolved to one
# polygon. st_union() here is defensive: if it's already a single polygon this is a no-op,
# if it's multiple freguesia/municipio rows (matching what this project's own CAOP_GLPS
# variable used to be, before dissolving) this collapses it into the one boundary polygon
# needed as a clip mask below.
caop_glps <- sf::st_read("data/geo/CAOP_GLPS.gpkg", quiet = TRUE)
message(sprintf("CAOP_GLPS.gpkg: %d row(s) before dissolve", nrow(caop_glps)))
clip_boundary <- caop_glps |> st_union() |> st_make_valid()
dir.create("data/base/daysimetric", recursive = TRUE, showWarnings = FALSE)
sf::st_write(clip_boundary, "data/base/daysimetric/clipping_boundary.geojson", delete_dsn = TRUE)

# COS raw ---------------------------------------------------------------
# Server-provided, pre-clipped/pre-processed COS layer - NOT the raw national DGT download
# this script originally read. Its actual classification schema is unverified (see the
# schema check immediately below) - "GLPS_LAND" in the filename suggests it may already be
# simplified/reclassified rather than COS's full detailed n4 taxonomy.
cos_raw <- sf::st_read("data/geo/COS_GLPS_LAND.gpkg", quiet = TRUE)

# GEOMETRY TYPE CHECK - found the actual root cause of the residential-clip loop being far
# slower on the server than locally: cos_residential_mask (built from this file, in
# 02c_dasymetric_population.R) came out as a GEOMETRYCOLLECTION after st_union(), not a
# clean POLYGON/MULTIPOLYGON - st_intersection() against a GEOMETRYCOLLECTION is known to be
# slower/less predictable in GEOS than against clean polygons. Checking here, at the source,
# in case COS_GLPS_LAND.gpkg itself already loads as mixed/ambiguous geometry (its GDAL
# geometry type reads "GEOMETRY", not a pure polygon type) - i.e. the source file may already
# contain stray points/lines mixed in with the residential polygons, rather than this being
# introduced later by st_union() in 02c. st_collection_extract() keeps only the polygonal
# parts, discarding any point/line geometries that don't belong in a land-use polygon layer
# anyway.
geom_types <- st_geometry_type(cos_raw)
geom_type_counts <- table(geom_types)
message(sprintf(
  "COS_GLPS_LAND.gpkg geometry type(s) found: %s",
  paste(names(geom_type_counts), geom_type_counts, sep = ": ", collapse = ", ")
))
if (!all(geom_types %in% c("POLYGON", "MULTIPOLYGON"))) {
  # Catches both an actual GEOMETRYCOLLECTION per row AND the plainer case of some rows
  # just being a bare POINT/LINESTRING mixed in among polygon rows (GDAL reports the whole
  # layer's type as ambiguous "GEOMETRY" in either case) - st_geometry_type() never returns
  # the literal string "GEOMETRY" itself (that's the layer-level declaration, not a
  # per-feature value), so checking against the actual POLYGON/MULTIPOLYGON allowlist here
  # is what correctly catches both cases.
  message("Non-polygon geometry present at the source - extracting polygonal parts only via st_collection_extract('POLYGON')")
  n_before <- nrow(cos_raw)
  cos_raw <- st_collection_extract(cos_raw, "POLYGON")
  # Safety net: a row that was a bare POINT/LINESTRING (not wrapped in a GEOMETRYCOLLECTION)
  # has no polygonal part to extract - st_collection_extract may leave that as an empty
  # geometry rather than dropping the row outright, so filter those out explicitly to
  # guarantee the result is genuinely clean, not just "no longer literally a collection."
  cos_raw <- cos_raw[!st_is_empty(cos_raw), ]
  message(sprintf(
    "st_collection_extract + empty-geometry drop: %d rows before -> %d rows after (types now: %s)",
    n_before, nrow(cos_raw), paste(unique(st_geometry_type(cos_raw)), collapse = ", ")
  ))
}

# SCHEMA CHECK - fail loudly rather than silently produce wrong output. The residential
# filter below is written against COS's detailed n4 classification (column COS23_n4_L,
# with four specific Portuguese label strings). Since this file's actual schema hasn't been
# inspected, verify both the column and the expected values actually exist before filtering.
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

# Residential filter ---------------------------------------------------------
# COS's "n4" classification level distinguishes residential built-up land by density/form -
# continuous vertical (e.g. dense apartment blocks), continuous horizontal (e.g. terraced
# housing), and discontinuous/sparse discontinuous (lower-density, more scattered housing).
# All four are kept here since all represent residential use, just at different densities -
# a later stage (02c_dasymetric_population.R) uses each matched building's own volume as the
# actual weight, so this filter's job is only to answer "is this parcel residential at all",
# not to distinguish how residential.
cos_residential <- cos_raw |> filter(COS23_n4_L %in% expected_residential_labels)

mapview(cos_residential)

# Clip to AML -----------------------------------------------------------
# cos_residential above is still nationwide (or at least not necessarily AML-only) - clip
# it down to just the AML study area. st_transform() first makes sure the two layers share
# a CRS (a spatial filter/subset like the bracket indexing below silently produces wrong
# results, not an error, if the two layers are in different CRSs). The bracket subset
# `x[y, ]` keeps every cos_residential polygon that intersects the clip boundary at all - it
# does NOT cut polygons exactly at the AML edge, so a residential parcel straddling the
# boundary is kept whole, slightly overshooting the study area rather than being clipped
# mid-polygon.
cos_residential_aml <- cos_residential |> st_transform(crs = st_crs(clip_boundary))
cos_residential_aml <- cos_residential_aml[clip_boundary, ]
# Drop COS's own row-id column if present - unverified whether COS_GLPS_LAND.gpkg carries
# the same "ID" column as the raw national COS download did, so this is now conditional
# rather than assumed.
if ("ID" %in% names(cos_residential_aml)) {
  cos_residential_aml <- cos_residential_aml |> select(-ID)
}

mapview(cos_residential_aml, zcol = "COS23_n4_L")

# export ---------------------------------------------------------------

sf::st_write(cos_residential_aml, "data/base/daysimetric/cos_residential_aml.gpkg", delete_dsn = TRUE)
