# COS residential layer prep - GRID ONLY variant support script
# Extracted/adapted from the commented-out block at 02_census_grid_with_cos.R:6-37.
# Produces the two files that 02_census_grid_with_cos.R expects to already exist:
#   /geo/clipping_boundary.geojson  (AML boundary used to clip COS)
#   /geo/cos_residential_aml.gpkg   (residential-only COS polygons, clipped to AML)
#
# Differences from the original commented block:
# - No download/unzip step: assumes COS2023v1-S2.gpkg is already placed at data/original/
#   (per the public DGT source noted in 02_census_grid_with_cos.R:6).
# - The original block read a pre-existing "/geo/clipping_boundary.geojson" that was never
#   produced by any script we found (a gated-server-only file). Here we instead build it
#   from `municipios_union_2024.geojson`, produced by 01_data_prep_grid_only.R, and write
#   it out under the expected filename so downstream scripts still find it in that path.
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

# Built from the AML union produced by 01_data_prep_grid_only.R, saved under the
# filename 02_census_grid_with_cos.R expects.
clip_boundary <- impt_read("/geo/municipios_union_2024.geojson")
impt_write(clip_boundary, "/geo/clipping_boundary.geojson")

# COS raw ---------------------------------------------------------------
# The full national COS dataset, unfiltered - every land parcel in Portugal with its
# land-use classification, not yet limited to AML or to residential categories.
cos_raw <- impt_read("/original/COS2023v1-S2.gpkg")
# COS23_n4_L is the human-readable classification label (Portuguese), COS23_n4_C is its
# numeric/coded equivalent - printed here just to see the full set of category values COS
# uses, so the residential filter below can be checked against the actual label text.
unique(cos_raw$COS23_n4_L)
unique(cos_raw$COS23_n4_C)

# Residential filter ---------------------------------------------------------
# COS's "n4" classification level distinguishes residential built-up land by density/form -
# continuous vertical (e.g. dense apartment blocks), continuous horizontal (e.g. terraced
# housing), and discontinuous/sparse discontinuous (lower-density, more scattered housing).
# All four are kept here since all represent residential use, just at different densities -
# a later stage (02c_dasymetric_population.R) uses each matched building's own volume as the
# actual weight, so this filter's job is only to answer "is this parcel residential at all",
# not to distinguish how residential.
cos_residential <- cos_raw |> filter(COS23_n4_L %in% c(
  "Áreas edificadas residenciais contínuas predominantemente verticais",
  "Áreas edificadas residenciais contínuas predominantemente horizontais",
  "Áreas edificadas residenciais descontínuas",
  "Áreas edificadas residenciais descontínuas esparsas"
))

mapview(cos_residential)

# Clip to AML -----------------------------------------------------------
# cos_residential above is still nationwide - clip it down to just the AML study area.
# st_transform() first makes sure the two layers share a CRS (a spatial filter/subset like
# the bracket indexing below silently produces wrong results, not an error, if the two
# layers are in different CRSs). The bracket subset `x[y, ]` keeps every cos_residential
# polygon that intersects the clip boundary at all - it does NOT cut polygons exactly at the
# AML edge, so a residential parcel straddling the boundary is kept whole, slightly
# overshooting the study area rather than being clipped mid-polygon.
cos_residential_aml <- cos_residential |> st_transform(crs = st_crs(clip_boundary))
cos_residential_aml <- cos_residential_aml[clip_boundary, ]
cos_residential_aml <- cos_residential_aml |> select(-ID) # drop COS's own row-id column, unused downstream

mapview(cos_residential_aml, zcol = "COS23_n4_L")

# export ---------------------------------------------------------------

impt_write(cos_residential_aml, "/geo/cos_residential_aml.gpkg")
