# Diagnostic: identify BGRI units with zero matched residential building volume
# Standalone script - does NOT re-run any spatial intersection. Reuses the fragment
# checkpoints already written to disk by the completed full-AML run of
# 02c_dasymetric_population.R (chunk_NNN.Rds files - columns: id, BGRI2021, frag_volume),
# so this is cheap to run regardless of how long the original run took.
#
# Investigates the 8.78% BGRI-vs-dasymetric gap (2,870,208 vs 2,618,227, ~251,981 people)
# reported after the full-AML run - notably larger than the original local run's 0.64% gap.
# Two independent things can cause a BGRI unit's population to go undistributed:
#   (a) the block is legitimately non-residential per COS (park, industrial, agricultural),
#       so zero matched building volume is expected and correct - not a bug.
#   (b) the buildings-footprints fetch's AML boundary (municipalities_union.gpkg, used as
#       the bbox in 02b_buildings_volume_prep.R) doesn't fully cover BGRI's actual extent,
#       so blocks near that edge had zero buildings fetched for them in the first place -
#       a real coverage bug, not a land-use fact.
# This script checks both, plus geographic/municipality clustering, separately.

required_packages <- c("sf", "dplyr", "tidyr", "mapview")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) install.packages(missing_packages)

library(sf)
library(dplyr)
library(tidyr)
library(mapview)

CHECKPOINT_DIR <- "/base/daysimetric/checkpoints"
run_suffix <- "" # matches 02c_dasymetric_population.R's convention - "" for a full run,
# "_test_<municipio>" for a TEST_MODE run, if you ever need to point this at a test run instead.

# 1. Reload raw fragment checkpoints (id, BGRI2021, frag_volume) -------------------------
# These are UNNORMALIZED per-fragment volumes, exactly as 02c_dasymetric_population.R wrote
# them before its own BGRI-level normalization step - summing frag_volume by BGRI2021 here
# reproduces that script's own total_bgri_frag_volume without redoing any geometry work.
chunk_dir <- paste0("data", CHECKPOINT_DIR)
chunk_files <- list.files(chunk_dir, pattern = sprintf("^chunk%s_[0-9]+\\.Rds$", run_suffix), full.names = TRUE)
message(sprintf("Found %d fragment checkpoint files in %s", length(chunk_files), chunk_dir))
stopifnot(length(chunk_files) > 0) # if this trips, run_suffix or CHECKPOINT_DIR doesn't match what's on disk

all_fragments <- bind_rows(lapply(chunk_files, readRDS))
message(sprintf("Total fragments loaded: %s", format(nrow(all_fragments), big.mark = ",")))

bgri_volume <- all_fragments |>
  group_by(BGRI2021) |>
  summarise(total_bgri_frag_volume = sum(frag_volume, na.rm = TRUE), .groups = "drop")

# 2. Load BGRI and join in matched volume -------------------------------------------------
# st_make_valid() right after load, same defensive pattern used throughout this project's
# other scripts - this data has repeatedly needed it before any spatial predicate.
census_bgri <- impt_read("/base/daysimetric/BGRI21_170.gpkg") |> st_make_valid()

# Schema check, same "don't assume, verify" principle used elsewhere in this project - only
# N_INDIVIDUOS and BGRI2021 have actually been confirmed to exist (in 02c's own stopifnot).
# Print full column list so the municipality-grouping column below can be picked correctly
# rather than guessed.
message("census_bgri columns: ", paste(names(census_bgri), collapse = ", "))
stopifnot("N_INDIVIDUOS" %in% names(census_bgri), "BGRI2021" %in% names(census_bgri))

# Common INE BGRI municipality-code column names - adjust this if the printed column list
# above shows something different.
mun_col_candidates <- c("DTMN21", "DTMNFR21", "DICO", "DICOFRE")
mun_col <- intersect(mun_col_candidates, names(census_bgri))[1]
if (is.na(mun_col)) {
  message("WARNING: none of the expected municipality-code columns (", paste(mun_col_candidates, collapse = ", "), ") were found - geographic clustering below will be skipped. Check the column list above and adjust mun_col_candidates.")
}

census_with_volume <- census_bgri |>
  left_join(bgri_volume, by = "BGRI2021") |>
  mutate(total_bgri_frag_volume = replace_na(total_bgri_frag_volume, 0))

# 3. Identify zero-matched-volume units, report count and combined population -------------
zero_volume_units <- census_with_volume |> filter(total_bgri_frag_volume == 0)

message("--- Zero-matched-volume BGRI units ---")
message(sprintf(
  "%s of %s BGRI units (%.1f%%) have zero matched residential building volume",
  format(nrow(zero_volume_units), big.mark = ","), format(nrow(census_with_volume), big.mark = ","),
  100 * nrow(zero_volume_units) / nrow(census_with_volume)
))
message(sprintf(
  "Combined population of these units: %s (compare directly to the ~251,981 gap)",
  format(sum(zero_volume_units$N_INDIVIDUOS, na.rm = TRUE), big.mark = ",")
))

# 4. Geographic clustering by municipality ------------------------------------------------
if (!is.na(mun_col)) {
  message(sprintf("--- Zero-volume units by municipality (%s) ---", mun_col))
  zero_by_mun <- zero_volume_units |>
    st_drop_geometry() |>
    group_by(.data[[mun_col]]) |>
    summarise(n_units = n(), pop = sum(N_INDIVIDUOS, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(pop))
  print(zero_by_mun, n = Inf)
}

# 5. Land-use pattern check: do these units actually overlap residential COS at all? -------
# Distinguishes "legitimately non-residential" (near-zero overlap with cos_residential_aml -
# not a bug) from "should have matched buildings but didn't" (meaningful residential-COS
# overlap present, yet zero volume matched - points at the buildings-coverage hypothesis
# checked in step 6, not a land-use fact).
cos_residential_aml <- impt_read("/base/daysimetric/cos_residential_aml.gpkg") |> st_make_valid()
zero_volume_aligned <- st_transform(zero_volume_units, st_crs(cos_residential_aml)) |> st_make_valid()
overlaps_residential <- lengths(st_intersects(zero_volume_aligned, cos_residential_aml)) > 0

message("--- Land-use check on zero-volume units ---")
message(sprintf(
  "%d of %d zero-volume units (%.1f%%) DO overlap residential-zoned COS land - these should have matched a building and didn't (points at a coverage/fetch problem, not a land-use fact)",
  sum(overlaps_residential), length(overlaps_residential), 100 * mean(overlaps_residential)
))
message(sprintf(
  "Population in that overlapping-but-unmatched subset: %s",
  format(sum(zero_volume_units$N_INDIVIDUOS[overlaps_residential], na.rm = TRUE), big.mark = ",")
))

# 6. Bounding-box / extent comparison: municipalities_union.gpkg vs BGRI's actual extent ---
aml_boundary <- impt_read("/geo/municipalities_union.gpkg") |> st_make_valid()
bgri_in_aml_crs <- st_transform(census_bgri, st_crs(aml_boundary)) |> st_make_valid()

message("--- Extent comparison: municipalities_union.gpkg vs BGRI ---")
message("municipalities_union.gpkg bbox: ", paste(names(st_bbox(aml_boundary)), round(st_bbox(aml_boundary), 4), sep = "=", collapse = ", "))
message("BGRI bbox (reprojected to match): ", paste(names(st_bbox(bgri_in_aml_crs)), round(st_bbox(bgri_in_aml_crs), 4), sep = "=", collapse = ", "))

# Direct test: any BGRI unit that doesn't intersect the AML boundary AT ALL would have had
# zero buildings fetched for it in 02b_buildings_volume_prep.R (which queries by this exact
# boundary's bbox) - this is the sharpest possible test of the coverage hypothesis.
bgri_outside_aml <- bgri_in_aml_crs[lengths(st_intersects(bgri_in_aml_crs, aml_boundary)) == 0, ]
message(sprintf(
  "%d BGRI units fall entirely outside municipalities_union.gpkg (%s total population) - these could NEVER have gotten a matched building regardless of anything else in the pipeline",
  nrow(bgri_outside_aml), format(sum(bgri_outside_aml$N_INDIVIDUOS, na.rm = TRUE), big.mark = ",")
))

# 7. Export the zero-volume units spatially for visual inspection -------------------------
mapview(census_with_volume |> filter(total_bgri_frag_volume == 0), zcol = "N_INDIVIDUOS") +
  mapview(cos_residential_aml, alpha.regions = 0.2, col.regions = "green") +
  mapview(aml_boundary, alpha.regions = 0, color = "red", lwd = 2)

impt_write(zero_volume_units, "/base/daysimetric/diagnostic_zero_volume_bgri.gpkg")
