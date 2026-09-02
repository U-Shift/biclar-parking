# 04_building_attributes.R
# Aggregate building characteristics onto the team H3 grid (the SAME grid as
# 03_dasymetric_combined.R). No new geometry is created.
#
# TWO INDEPENDENT COMPUTATIONS, JOINED BY `id` AT THE END
# ------------------------------------------------------
#
# SECTION 1 - BGRI building-characteristic counts (dasymetric disaggregation)
#   Reuses 03_dasymetric_combined.R's ON-DISK checkpoints - it does NOT redo any
#   st_intersection. It rebuilds 03's exact weight hierarchy
#   (building volume > COS-area x class weight > bare BGRI-area overlap) and its
#   per-BGRI proportional split, then applies that split to a DIFFERENT set of
#   BGRI columns (building counts, not N_INDIVIDUOS).
#
#     buildings_1_2_floors            <- N_EDIFICIOS_1_OU_2_PISOS
#     buildings_3plus_floors          <- N_EDIFICIOS_3_OU_MAIS_PISOS
#     buildings_with_parking          <- N_RHABITUAL_COM_ESTACIONAMENTO
#     buildings_pre1945               <- N_EDIFICIOS_CONSTR_ANTES_1945
#     buildings_post1945              <- N_EDIFICIOS_CLASSICOS - N_EDIFICIOS_CONSTR_ANTES_1945
#     buildings_wheelchair_accessible <- N_RHABITUAL_ACESSIVEL_CADEIRAS_RODAS
#     buildings_exclusive_residential <- N_EDIFICIOS_EXCLUSIV_RESID   (helper, kept in output)
#     buildings_total                 <- N_EDIFICIOS_CLASSICOS        (helper, kept in output)
#     pct_residential                 <- 100 * buildings_exclusive_residential / buildings_total
#                                        computed PER GRID CELL, AFTER aggregation
#
# SECTION 2 - Global Building Atlas per-building stats (direct spatial join)
#   Exact per-building values (volume_m3, est_floors) - no weights, no disaggregation.
#   Each building is assigned to ONE grid cell by CENTROID-IN-CELL (st_within of the
#   building centroid against the grid). Buildings whose footprint straddles a cell
#   boundary are counted and reported but still credited wholly to their centroid cell.
#
#     building_count            <- number of buildings whose centroid falls in the cell
#     total_building_volume_m3  <- sum(volume_m3)
#     mean_estimated_floors     <- mean(est_floors)   (plain mean, not volume-weighted)
#
# COMBINE
#   Left-join Section 1 and Section 2 onto the FULL grid by `id`. Cells with no
#   buildings still appear: count columns -> 0, mean_estimated_floors / pct_residential -> NA.
#
# OUTPUT
#   data/base/daysimetric/grid_building_attributes[_test_<municipio>].gpkg + .csv
#   one row per grid `id`.
#
## VALIDATION RESULTS (full AML run, TEST_MODE = FALSE)
# ------------------------------------------------------
#   Section 1 - perfectly conserved. Every BGRI-derived column matches its raw
#   BGRI total exactly (0.00% difference, all 8 attributes):
#     buildings_1_2_floors, buildings_3plus_floors, buildings_with_parking,
#     buildings_pre1945, buildings_post1945, buildings_wheelchair_accessible,
#     buildings_exclusive_residential, buildings_total.
#   25,914 / 25,915 BGRI blocks (100.00%) are represented in the fragments;
#   the one exception overlaps no grid cell at all.
#
#   Section 2 - of 633,231 buildings:
#     541,764 (85.6%)  placed in a grid cell
#      91,467 (14.4%)  centroid falls outside the grid entirely - see LIMITATIONS
#      48,295 (7.6%)   footprint spans >1 cell, credited wholly to centroid cell
#
# LIMITATIONS
# ------------------------------------------------------
#   - The ~14.4% of buildings outside the grid is NOT a coverage bug. Confirmed
#     via bounding-box comparison: the buildings dataset's extent is fully
#     contained within the grid's extent. The real cause is that 03's grid was
#     filtered to cells overlapping at least one BGRI block, so it deliberately
#     excludes non-residential land (industrial areas, ports, large
#     non-residential zones, etc.) where buildings genuinely exist but no
#     residential population is counted.
#   - The centroid-in-cell rule (Section 2) exactly conserves building_count
#     and total_building_volume_m3 but means the ~7.6% of buildings that
#     straddle a cell boundary are not split proportionally - each is credited
#     wholly to one cell. A majority-overlap-by-area rule would avoid this but
#     requires a full polygon x polygon intersection over 633k buildings,
#     which was judged not worth the cost for this attribute set.
#
# DEPENDENCIES
#   03_dasymetric_combined.R must have been run to completion with the SAME
#   TEST_MODE setting as this script, so its checkpoints exist at
#   checkpoints/dasymetric_combined/. This script reads those; it never writes there.
#
# CONVENTIONS (identical to 03_dasymetric_combined.R)
#   - plain sf::st_read() / sf::st_write() on paths relative to ./data/ (bind-mounted
#     from the server). No impt_read/impt_write.
#   - TEST_MODE / TEST_MUNICIPIO / run_suffix so test and full runs never collide.
#   - local (non-mounted) checkpoint dir, absolute path via file.path(getwd(), ...).
#   - stopifnot on every expected column before proceeding.

# 0. Setup -------------------------------------------------------------------

required_packages <- c("sf", "dplyr", "tidyr", "tibble", "future", "furrr")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) install.packages(missing_packages)

library(sf)
library(dplyr)
library(tidyr)
library(tibble)
library(future)
library(furrr)

# Run from the repo root so ./data/ resolves against the mounted folder.
stopifnot(
  "Run from the repo root: setwd('/path/to/biclar-parking')" =
    file.exists("data") || dir.exists("data")
)

# --- Configuration flags (mirror 03_dasymetric_combined.R) ---

N_WORKERS <- 8
plan(multisession, workers = N_WORKERS)

target_crs <- 3763 # EPSG:3763 - Portugal metric CRS

# Must match the TEST_MODE setting used for the 03 run whose checkpoints we reuse.
TEST_MODE <- FALSE
TEST_MUNICIPIO <- "Oeiras"

# Building rows processed per parallel chunk in Section 2 (same size as 03).
CHUNK_SIZE <- 50000

run_suffix <- if (TEST_MODE) paste0("_test_", tolower(TEST_MUNICIPIO)) else ""

# This script's OWN checkpoint dir (Section 2 assignment chunks).
CHECKPOINT_DIR <- file.path(getwd(), "checkpoints", "building_attributes")

# 03's checkpoint dir - READ ONLY from here.
CP03_DIR <- file.path(getwd(), "checkpoints", "dasymetric_combined")

message(sprintf(
  "[config] TEST_MODE=%s  run_suffix='%s'\n[config] my checkpoints : %s\n[config] 03 checkpoints : %s",
  TEST_MODE, run_suffix, CHECKPOINT_DIR, CP03_DIR
))

if (!dir.create(CHECKPOINT_DIR, recursive = TRUE, showWarnings = FALSE)) {
  if (!dir.exists(CHECKPOINT_DIR)) {
    stop(sprintf("Cannot create checkpoint directory:\n  %s\nCheck write permissions.", CHECKPOINT_DIR))
  }
  message(sprintf("[checkpoints] Resuming from existing directory: %s", CHECKPOINT_DIR))
} else {
  message(sprintf("[checkpoints] Created: %s", CHECKPOINT_DIR))
}

read_checkpoint <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  tryCatch(readRDS(path), error = function(e) NULL)
}
write_checkpoint <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(object, path)
}

# Fail loudly if any required column is missing, and show what IS available.
require_cols <- function(x, cols, what) {
  missing <- setdiff(cols, names(x))
  if (length(missing) > 0) {
    stop(sprintf(
      "[schema] %s is missing required column(s): %s\n[schema] Available columns:\n  %s",
      what, paste(missing, collapse = ", "), paste(names(x), collapse = ", ")
    ))
  }
  invisible(TRUE)
}


# 1. Locate and validate 03's checkpoints ----------------------------------

message("[s1] Locating 03_dasymetric_combined.R checkpoints...")
stopifnot(
  "03's checkpoint directory not found - run 03_dasymetric_combined.R first." =
    dir.exists(CP03_DIR)
)

vol_frag_pattern <- sprintf("^vol_fragments%s_[0-9]+\\.Rds$", run_suffix)
vol_frag_files <- list.files(CP03_DIR, pattern = vol_frag_pattern, full.names = TRUE)
ckpt_cos  <- file.path(CP03_DIR, sprintf("cos_bgri_frags%s.Rds", run_suffix))
ckpt_bgri <- file.path(CP03_DIR, sprintf("bgri_area_frags%s.Rds", run_suffix))

if (length(vol_frag_files) == 0 || !file.exists(ckpt_cos) || !file.exists(ckpt_bgri)) {
  stop(sprintf(paste0(
    "[s1] Missing 03 checkpoints for run_suffix '%s'.\n",
    "     vol_fragments%s_*.Rds found : %d\n",
    "     %s : %s\n",
    "     %s : %s\n",
    "     Run 03_dasymetric_combined.R to completion with TEST_MODE=%s first."),
    run_suffix, run_suffix, length(vol_frag_files),
    basename(ckpt_cos), file.exists(ckpt_cos),
    basename(ckpt_bgri), file.exists(ckpt_bgri),
    TEST_MODE
  ))
}
message(sprintf(
  "[s1] Found: %d vol_fragments chunk files, cos_bgri_frags, bgri_area_frags",
  length(vol_frag_files)
))


# 2. Load inputs ----------------------------------------------------------

message("[load] Reading BGRI census blocks...")
census_bgri <- sf::st_read("data/base/daysimetric/BGRI21_170.gpkg", quiet = TRUE)

# Extended schema check - every BGRI column this script derives an attribute from.
bgri_required <- c(
  "BGRI2021",
  "N_EDIFICIOS_CLASSICOS",
  "N_EDIFICIOS_CONSTR_ANTES_1945",
  "N_EDIFICIOS_1_OU_2_PISOS",
  "N_EDIFICIOS_3_OU_MAIS_PISOS",
  "N_RHABITUAL_COM_ESTACIONAMENTO",
  "N_RHABITUAL_ACESSIVEL_CADEIRAS_RODAS",
  "N_EDIFICIOS_EXCLUSIV_RESID"
)
require_cols(census_bgri, bgri_required, "BGRI21_170.gpkg")
message(sprintf("[load] BGRI: %d blocks", nrow(census_bgri)))

message("[load] Reading Global Building Atlas footprints...")
buildings <- sf::st_read(
  "data/base/daysimetric/lisbon_metro_buildings_footprints.geojson",
  quiet = TRUE
)
message(sprintf("[load] Buildings: %d footprints", nrow(buildings)))
message(sprintf("[load] Building columns: %s", paste(names(buildings), collapse = ", ")))
require_cols(buildings, c("volume_m3", "est_floors"), "lisbon_metro_buildings_footprints.geojson")

message("[grid] Loading H3 grid from data/geo/grid.gpkg...")
grid <- sf::st_read("data/geo/grid.gpkg", quiet = TRUE) |> st_make_valid()
if ("gid" %in% names(grid)) {
  grid <- grid |> rename(id = gid)
} else if (!"id" %in% names(grid)) {
  id_col <- setdiff(names(grid), attr(grid, "sf_column"))[1]
  warning(sprintf("Grid has no 'gid' or 'id' column - using '%s'. Check names(grid).", id_col))
  grid <- grid |> rename(id = !!id_col)
}
message(sprintf("[grid] Grid loaded: %d cells", nrow(grid)))

# Municipalities - only needed for TEST_MODE subsetting (same pattern as 03).
municipalities_sf <- sf::st_read("data/geo/municipalities.gpkg", quiet = TRUE) |> st_make_valid()
if (!"municipio" %in% names(municipalities_sf) && file.exists("useful_data/municipalities.csv")) {
  mun_lookup <- read.csv("useful_data/municipalities.csv", stringsAsFactors = FALSE)
  if (all(c("mid", "name") %in% names(mun_lookup))) {
    municipalities_sf <- municipalities_sf |>
      mutate(mid = as.character(mid)) |>
      left_join(
        mun_lookup |> mutate(mid = as.character(mid)) |> select(mid, municipio = name),
        by = "mid"
      )
  }
}

message("[prep] st_make_valid on BGRI and buildings...")
census_bgri <- st_make_valid(census_bgri)
buildings <- st_make_valid(buildings)


# 3. TEST MODE subsetting ------------------------------------------------

if (TEST_MODE) {
  message(sprintf("[TEST MODE] Subsetting to municipality: %s", TEST_MUNICIPIO))
  if (!"municipio" %in% names(municipalities_sf) || !TEST_MUNICIPIO %in% municipalities_sf$municipio) {
    avail <- if ("municipio" %in% names(municipalities_sf)) {
      paste(sort(unique(municipalities_sf$municipio)), collapse = ", ")
    } else {
      "(no 'municipio' column)"
    }
    stop(sprintf("TEST_MUNICIPIO '%s' not found. Available: %s", TEST_MUNICIPIO, avail))
  }
  test_boundary <- municipalities_sf |>
    filter(municipio == TEST_MUNICIPIO) |>
    st_make_valid()
  stopifnot(nrow(test_boundary) == 1)

  ref_crs <- st_crs(test_boundary)
  grid        <- st_transform(grid, ref_crs)[test_boundary, ]
  buildings   <- st_transform(buildings, ref_crs)[test_boundary, ]
  census_bgri <- st_transform(census_bgri, ref_crs)[test_boundary, ]
  message(sprintf(
    "[TEST MODE] Subset: %d grid cells, %d buildings, %d BGRI blocks",
    nrow(grid), nrow(buildings), nrow(census_bgri)
  ))
}


# 4. Reproject to EPSG:3763 --------------------------------------------------

message("[prep] Reprojecting to EPSG:3763...")
grid      <- st_transform(grid, target_crs) |> st_make_valid()
buildings <- st_transform(buildings, target_crs)
# census_bgri geometry is not used in this script (Section 1 works off 03's
# fragment checkpoints), only its attribute table - no reprojection needed.


# =========================================================================
# SECTION 1 - BGRI-derived attributes (reuse 03's weight hierarchy)
# =========================================================================

# 5. Rebuild 03's weight tables from checkpoints (NO st_intersection) ------

message("[s1] Rebuilding vol_weights from vol_fragments chunk checkpoints...")
vol_frags_raw <- vector("list", length(vol_frag_files))
for (i in seq_along(vol_frag_files)) {
  vol_frags_raw[[i]] <- readRDS(vol_frag_files[i])
  if (i %% 10 == 0 || i == length(vol_frag_files)) {
    message(sprintf("[s1]   read %d/%d vol_fragments files", i, length(vol_frag_files)))
  }
}
vol_frags_raw <- bind_rows(vol_frags_raw)

# Identical aggregation to 03 step 7b tail.
vol_weights <- vol_frags_raw |>
  group_by(id, BGRI2021) |>
  summarise(vol_weight = sum(frag_volume, na.rm = TRUE), .groups = "drop") |>
  filter(vol_weight > 0)
rm(vol_frags_raw)
gc()
message(sprintf("[s1] vol_weights: %d (id x BGRI) pairs with volume > 0", nrow(vol_weights)))

message("[s1] Reading cos_bgri_frags and bgri_area_frags checkpoints...")
cos_bgri_frags  <- readRDS(ckpt_cos)   # cols: id, BGRI2021, cos_weight_sum  (already filtered > 0)
bgri_area_frags <- readRDS(ckpt_bgri)  # cols: id, BGRI2021, bare_area        (already filtered > 0)
require_cols(cos_bgri_frags, c("id", "BGRI2021", "cos_weight_sum"), "cos_bgri_frags checkpoint")
require_cols(bgri_area_frags, c("id", "BGRI2021", "bare_area"), "bgri_area_frags checkpoint")
message(sprintf(
  "[s1] cos_bgri_frags: %d pairs   bgri_area_frags: %d pairs",
  nrow(cos_bgri_frags), nrow(bgri_area_frags)
))


# 6. Rebuild the hierarchical weight + per-BGRI fraction (03 steps 10-11) --

message("[s1] Applying hierarchical fallback (volume > cos_area > bare_area)...")
all_pairs <- bgri_area_frags |>
  left_join(vol_weights, by = c("id", "BGRI2021")) |>
  left_join(cos_bgri_frags, by = c("id", "BGRI2021")) |>
  mutate(
    weight_source = case_when(
      !is.na(vol_weight) & vol_weight > 0 ~ "building_volume",
      !is.na(cos_weight_sum) & cos_weight_sum > 0 ~ "cos_area",
      TRUE ~ "bare_area"
    ),
    weight = case_when(
      weight_source == "building_volume" ~ vol_weight,
      weight_source == "cos_area" ~ cos_weight_sum,
      TRUE ~ bare_area
    )
  ) |>
  filter(weight > 0)

weight_summary <- all_pairs |>
  group_by(weight_source) |>
  summarise(n_pairs = n(), .groups = "drop") |>
  mutate(pct = round(n_pairs / sum(n_pairs) * 100, 1))
message("[s1] Weight source breakdown (should match 03's):")
print(as.data.frame(weight_summary))

rm(vol_weights, cos_bgri_frags, bgri_area_frags)
gc()

# Per-BGRI proportional split - identical maths to 03 step 11 (pop_fraction).
all_pairs <- all_pairs |>
  group_by(BGRI2021) |>
  mutate(total_bgri_weight = sum(weight)) |>
  ungroup() |>
  filter(total_bgri_weight > 0) |>
  mutate(weight_fraction = weight / total_bgri_weight)

n_bgri_in_pairs <- dplyr::n_distinct(all_pairs$BGRI2021)
n_bgri_total    <- dplyr::n_distinct(census_bgri$BGRI2021)
message(sprintf(
  "[s1] BGRI blocks represented in the fragments: %d / %d (%.2f%%). The rest do not overlap any grid cell.",
  n_bgri_in_pairs, n_bgri_total, 100 * n_bgri_in_pairs / n_bgri_total
))


# 7. Derive BGRI attribute columns and disaggregate ----------------------

# distinct(BGRI2021): BGRI blocks are one row per code, but guard against dupes.
census_attr <- census_bgri |>
  st_drop_geometry() |>
  transmute(
    BGRI2021,
    buildings_1_2_floors            = as.numeric(N_EDIFICIOS_1_OU_2_PISOS),
    buildings_3plus_floors          = as.numeric(N_EDIFICIOS_3_OU_MAIS_PISOS),
    buildings_with_parking          = as.numeric(N_RHABITUAL_COM_ESTACIONAMENTO),
    buildings_pre1945               = as.numeric(N_EDIFICIOS_CONSTR_ANTES_1945),
    buildings_post1945              = as.numeric(N_EDIFICIOS_CLASSICOS) -
                                       as.numeric(N_EDIFICIOS_CONSTR_ANTES_1945),
    buildings_wheelchair_accessible = as.numeric(N_RHABITUAL_ACESSIVEL_CADEIRAS_RODAS),
    buildings_exclusive_residential = as.numeric(N_EDIFICIOS_EXCLUSIV_RESID),
    buildings_total                 = as.numeric(N_EDIFICIOS_CLASSICOS)
  ) |>
  distinct(BGRI2021, .keep_all = TRUE)

# N_EDIFICIOS_CONSTR_ANTES_1945 should never exceed N_EDIFICIOS_CLASSICOS; clamp + warn.
n_neg_post45 <- sum(census_attr$buildings_post1945 < 0, na.rm = TRUE)
if (n_neg_post45 > 0) {
  warning(sprintf(
    "[s1] %d BGRI blocks have N_EDIFICIOS_CONSTR_ANTES_1945 > N_EDIFICIOS_CLASSICOS; clamping buildings_post1945 to 0.",
    n_neg_post45
  ))
  census_attr$buildings_post1945 <- pmax(0, census_attr$buildings_post1945)
}

distribute_cols <- c(
  "buildings_1_2_floors", "buildings_3plus_floors", "buildings_with_parking",
  "buildings_pre1945", "buildings_post1945", "buildings_wheelchair_accessible",
  "buildings_exclusive_residential", "buildings_total"
)

# Raw BGRI-wide totals (for validation later), computed BEFORE disaggregation.
bgri_raw_totals <- sapply(distribute_cols, function(col) sum(census_attr[[col]], na.rm = TRUE))

message("[s1] Distributing BGRI building counts across H3 cells...")
s1_grid <- all_pairs |>
  select(id, BGRI2021, weight_fraction, weight_source) |>
  left_join(census_attr, by = "BGRI2021") |>
  mutate(across(all_of(distribute_cols), ~ .x * weight_fraction)) |>
  group_by(id) |>
  summarise(
    across(all_of(distribute_cols), ~ sum(.x, na.rm = TRUE)),
    dominant_weight_source = names(which.max(table(weight_source))),
    .groups = "drop"
  )
message(sprintf("[s1] Section 1 done: %d grid cells with BGRI-derived attributes", nrow(s1_grid)))

rm(all_pairs, census_attr)
gc()


# =========================================================================
# SECTION 2 - Global Building Atlas attributes (centroid-in-cell join)
# =========================================================================

# 8. Assign each building to one grid cell by centroid --------------------
#
# RULE: centroid-in-cell (st_within of the building centroid against the grid).
# WHY : O(n) point-in-polygon rather than a polygon x polygon st_intersection over
#       every building; each building lands in exactly one cell; sum(volume_m3) and
#       building_count are conserved exactly. Buildings whose FOOTPRINT crosses a
#       cell boundary are counted separately (st_intersects predicate) and reported.

grid_slim <- grid |> select(id)

n_bldg <- nrow(buildings)
n_chunks <- if (TEST_MODE) 1L else ceiling(n_bldg / CHUNK_SIZE)
bldg_batches <- if (TEST_MODE) {
  list(buildings)
} else {
  lapply(seq_len(n_chunks), function(i) {
    buildings[((i - 1) * CHUNK_SIZE + 1):min(i * CHUNK_SIZE, n_bldg), ]
  })
}

message(sprintf(
  "[s2] Assigning %d buildings to grid cells (%d chunks, %d workers)...",
  n_bldg, n_chunks, N_WORKERS
))

run_gba_chunk <- function(i, bldg_batch) {
  library(sf)
  library(dplyr)
  ckpt <- sprintf("%s/gba_assign%s_%03d.Rds", CHECKPOINT_DIR, run_suffix, i)
  cached <- read_checkpoint(ckpt)
  if (!is.null(cached)) {
    message(sprintf("[s2 %d/%d] checkpoint found (%d rows)", i, n_chunks, nrow(cached)))
    return(cached)
  }
  t0 <- Sys.time()

  cent <- suppressWarnings(st_centroid(st_geometry(bldg_batch)))
  within_hits <- st_within(cent, grid_slim)
  cell_pos <- vapply(within_hits, function(x) if (length(x) >= 1) x[1] else NA_integer_, integer(1))

  footprint_hits <- st_intersects(bldg_batch, grid_slim)
  spans_multi <- lengths(footprint_hits) > 1

  out <- tibble(
    id          = grid_slim$id[cell_pos],
    volume_m3   = as.numeric(bldg_batch$volume_m3),
    est_floors  = as.numeric(bldg_batch$est_floors),
    spans_multi = spans_multi
  )
  write_checkpoint(out, ckpt)
  message(sprintf(
    "[s2 %d/%d] %d buildings -> %d placed, %d outside grid, %d span >1 cell (%.1fs)",
    i, n_chunks, nrow(bldg_batch), sum(!is.na(out$id)), sum(is.na(out$id)),
    sum(out$spans_multi), as.numeric(difftime(Sys.time(), t0, units = "secs"))
  ))
  out
}

gba_assigned <- bind_rows(future_map2(
  seq_len(n_chunks), bldg_batches, run_gba_chunk,
  .options = furrr_options(seed = NULL)
))
rm(bldg_batches)
gc()


# 9. Aggregate GBA stats per grid cell + report edge cases ----------------

n_outside <- sum(is.na(gba_assigned$id))
n_multi   <- sum(gba_assigned$spans_multi, na.rm = TRUE)
n_placed  <- nrow(gba_assigned) - n_outside
message(sprintf(
  "[s2] Buildings outside the grid extent (centroid matched no cell): %d (%.2f%%)",
  n_outside, 100 * n_outside / nrow(gba_assigned)
))
message(sprintf(
  "[s2] Buildings whose footprint spans >1 cell (credited wholly to centroid cell): %d (%.2f%%)",
  n_multi, 100 * n_multi / nrow(gba_assigned)
))

s2_grid <- gba_assigned |>
  filter(!is.na(id)) |>
  group_by(id) |>
  summarise(
    building_count           = n(),
    total_building_volume_m3 = sum(volume_m3, na.rm = TRUE),
    mean_estimated_floors    = mean(est_floors, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(mean_estimated_floors = ifelse(is.nan(mean_estimated_floors), NA_real_, mean_estimated_floors))

message(sprintf("[s2] Section 2 done: %d grid cells contain >=1 building", nrow(s2_grid)))
rm(gba_assigned)
gc()


# =========================================================================
# COMBINE
# =========================================================================

# 10. Join both sections onto the FULL grid by id ------------------------

plan(sequential) # release workers
message("[combine] Joining Section 1 + Section 2 onto the full grid...")

grid_out <- grid |>
  select(id) |>
  left_join(s1_grid, by = "id") |>
  left_join(s2_grid, by = "id") |>
  mutate(
    across(all_of(distribute_cols), ~ replace_na(.x, 0)),
    building_count           = replace_na(building_count, 0L),
    total_building_volume_m3 = replace_na(total_building_volume_m3, 0),
    # mean_estimated_floors: left as NA where the cell has no buildings.
    pct_residential = if_else(
      buildings_total > 0,
      round(buildings_exclusive_residential / buildings_total * 100, 2),
      NA_real_
    )
  ) |>
  # Round the disaggregated counts AFTER pct_residential is computed from them.
  mutate(
    across(all_of(distribute_cols), ~ round(.x, 2)),
    total_building_volume_m3 = round(total_building_volume_m3, 1),
    mean_estimated_floors    = round(mean_estimated_floors, 2)
  ) |>
  select(
    id,
    buildings_1_2_floors, buildings_3plus_floors,
    buildings_pre1945, buildings_post1945,
    buildings_with_parking, buildings_wheelchair_accessible,
    buildings_exclusive_residential, buildings_total, pct_residential,
    dominant_weight_source,
    building_count, total_building_volume_m3, mean_estimated_floors
  ) |>
  st_transform(4326)


# 11. Validation --------------------------------------------------------
# Section 1 columns only: grid-wide aggregated total vs raw BGRI-wide total.
# Same style and 2% threshold as 03_dasymetric_combined.R's population check.
# (Losses here have the same cause as 03's ~0.5%: BGRI blocks that overlap no grid cell.)

message("--- Section 1 attribute totals check (grid vs raw BGRI) ---")
val <- lapply(distribute_cols, function(col) {
  bgri_tot <- bgri_raw_totals[[col]]
  grid_tot <- sum(s1_grid[[col]], na.rm = TRUE)
  diff_pct <- if (bgri_tot != 0) 100 * (bgri_tot - grid_tot) / bgri_tot else NA_real_
  message(sprintf(
    "  %-34s BGRI %12s   grid %12s   diff %6.2f%%",
    col, format(round(bgri_tot), big.mark = ","),
    format(round(grid_tot), big.mark = ","), diff_pct
  ))
  tibble(column = col, bgri_total = bgri_tot, grid_total = grid_tot, diff_pct = diff_pct)
})
val <- bind_rows(val)

over <- val |> filter(!is.na(diff_pct) & abs(diff_pct) > 2)
if (nrow(over) > 0) {
  warning(sprintf(
    "Section 1: %d column(s) exceed the 2%% threshold: %s",
    nrow(over), paste(over$column, collapse = ", ")
  ))
}

message(sprintf(
  "[s2] (no disaggregation check for Section 2 - values are exact per-building; %d of %d buildings placed in a cell)",
  n_placed, n_bldg
))


# 12. Export -----------------------------------------------------------

out_dir <- "data/base/daysimetric"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_gpkg <- file.path(out_dir, sprintf("grid_building_attributes%s.gpkg", run_suffix))
out_csv  <- file.path(out_dir, sprintf("grid_building_attributes%s.csv", run_suffix))

sf::st_write(grid_out, out_gpkg, delete_dsn = TRUE)
write.csv(sf::st_drop_geometry(grid_out), out_csv, row.names = FALSE)

message(sprintf("[done] Exported: %s  (+ .csv)", out_gpkg))
message(sprintf(
  "[done] Grid cells: %d total, %d with buildings, %d with BGRI-derived attributes",
  nrow(grid_out), sum(grid_out$building_count > 0, na.rm = TRUE),
  sum(grid_out$buildings_total > 0, na.rm = TRUE)
))
