# 04_building_attributes.R
# Aggregate building and dwelling characteristics onto the team H3 grid
# All spatial operations and area calculations strictly use EPSG:3763 (Portugal metric CRS)
#
# Run with: $ Rscript code/buildings/04_building_attributes.R > code/buildings/04_building_attributes_$(date +%Y%m%d).log 2>&1
#
# TWO COMPLEMENTARY COMPUTATIONS, JOINED BY `id` AT THE END
# ---------------------------------------------------------
#
# SECTION 1 - INE Census 2021 BGRI attributes (empirical atlas-weighted disaggregation)
#   Disaggregates BGRI-level census indicators onto H3 cells using actual physical
#   building presence from the Global Building Atlas LOD1:
#   - Census Buildings (census_buildings_*): Disaggregated proportionally to physical
#     building footprint area (m2 in EPSG:3763) in each (H3 cell x BGRI block) fragment.
#   - Census Dwellings (census_dwellings_*): Disaggregated proportionally to physical
#     building volume (m3) in each (H3 cell x BGRI block) fragment.
#   - Fallback: For rare BGRI blocks with zero detected building footprints, weights
#     fall back to the geometric overlap area (m2 in EPSG:3763) between the cell and BGRI.
#
#     census_buildings_1_2_floors            <- N_EDIFICIOS_1_OU_2_PISOS
#     census_buildings_3plus_floors          <- N_EDIFICIOS_3_OU_MAIS_PISOS
#     census_buildings_pre1945               <- N_EDIFICIOS_CONSTR_ANTES_1945
#     census_buildings_post1945              <- N_EDIFICIOS_CLASSICOS - N_EDIFICIOS_CONSTR_ANTES_1945
#     census_buildings_exclusive_residential <- N_EDIFICIOS_EXCLUSIV_RESID
#     census_buildings_total                 <- N_EDIFICIOS_CLASSICOS
#     census_pct_residential                 <- 100 * exclusive_residential / total
#     census_dwellings_total                 <- N_ALOJAMENTOS_TOTAL (or N_ALOJAMENTOS)
#     census_dwellings_with_parking          <- N_RHABITUAL_COM_ESTACIONAMENTO
#     census_dwellings_wheelchair_accessible <- N_RHABITUAL_ACESSIVEL_CADEIRAS_RODAS
#     census_pct_dwellings_with_parking      <- 100 * with_parking / dwellings_total
#     census_pct_dwellings_wheelchair_accessible <- 100 * wheelchair_accessible / dwellings_total
#     census_weight_source                   <- "atlas_footprint" or "bare_area_fallback"
#     For docs on BGRI variables, refer to https://mapas.ine.pt/download/C2021_FSINTESE_VARIAVEIS.csv
#
# SECTION 2 - Global Building Atlas physical footprints (area-proportional allocation)
#   Direct physical building stats (volume_m3, footprint_m2, est_floors).
#   Buildings crossing grid cell boundaries are sliced proportionally by area (EPSG:3763):
#   each cell receives its exact share of volume, footprint area, and fractional building presence.
#
#     atlas_building_count                 <- sum of building presence (1.0 if inside, fractional if split)
#     atlas_total_volume_m3                <- sum of volume proportional to area in cell
#     atlas_total_footprint_m2             <- sum of footprint area inside cell (m2)
#     atlas_total_floors                   <- sum of estimated floors (proportional to building presence)
#     atlas_mean_estimated_floors          <- arithmetic mean of estimated floors
#     atlas_weighted_mean_estimated_floors <- footprint-area-weighted mean of estimated floors
#     atlas_straddling_building_count      <- count of fragments from buildings spanning >1 cell
#
# COMBINE
#   Left-join Section 1 and Section 2 onto the FULL grid by `id`. Cells with no
#   buildings still appear: count/volume columns -> 0, floor averages / pct -> NA.
#
# OUTPUT
#   data/base/daysimetric/grid_building_attributes[_test_<municipio>].gpkg + .csv
#   one row per grid `id` (exported in EPSG:4326 for web/GIS compatibility).

# 0. Setup -------------------------------------------------------------------

required_packages <- c("sf", "dplyr", "tidyr", "tibble", "future", "furrr")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) install.packages(missing_packages)

library(sf)
sf_use_s2(FALSE)
library(dplyr)
library(tidyr)
library(tibble)
library(future)
library(furrr)

# Run from repo root so relative paths resolve against ./data/
stopifnot(
  "Run from the repo root: setwd('/path/to/biclar-parking')" =
    file.exists("data") || dir.exists("data")
)

# --- Configuration flags ---

N_WORKERS <- 8
plan(multisession, workers = N_WORKERS)

target_crs <- 3763 # EPSG:3763 - Portugal metric CRS (meters)

TEST_MODE <- FALSE
TEST_MUNICIPIO <- "Oeiras"

# Building rows processed per parallel chunk
CHUNK_SIZE <- 50000

run_suffix <- if (TEST_MODE) paste0("_test_", tolower(TEST_MUNICIPIO)) else ""

message(sprintf(
  "[config] TEST_MODE=%s  run_suffix='%s'\n[config] target CRS: EPSG:%d\n[config] parallel workers: %d  chunk size: %d",
  TEST_MODE, run_suffix, target_crs, N_WORKERS, CHUNK_SIZE
))

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


# 1. Load inputs -------------------------------------------------------------

message("[load] Reading BGRI census blocks...")
census_bgri <- sf::st_read("data/base/daysimetric/BGRI21_170.gpkg", quiet = TRUE)

# Detect dwelling count column
aloj_col <- intersect(c("N_ALOJAMENTOS_TOTAL", "N_ALOJAMENTOS"), names(census_bgri))[1]
if (is.na(aloj_col)) {
  stop(sprintf(
    "[schema] BGRI21_170.gpkg is missing dwelling column (expected 'N_ALOJAMENTOS_TOTAL' or 'N_ALOJAMENTOS'). Available columns:\n  %s",
    paste(names(census_bgri), collapse = ", ")
  ))
}
message(sprintf("[load] Using dwelling count column: %s", aloj_col))

bgri_required <- c(
  "BGRI2021",
  "N_EDIFICIOS_CLASSICOS",
  "N_EDIFICIOS_CONSTR_ANTES_1945",
  "N_EDIFICIOS_1_OU_2_PISOS",
  "N_EDIFICIOS_3_OU_MAIS_PISOS",
  "N_RHABITUAL_COM_ESTACIONAMENTO",
  "N_RHABITUAL_ACESSIVEL_CADEIRAS_RODAS",
  "N_EDIFICIOS_EXCLUSIV_RESID",
  aloj_col
)
require_cols(census_bgri, bgri_required, "BGRI21_170.gpkg")
message(sprintf("[load] BGRI: %d blocks", nrow(census_bgri)))

message("[load] Reading Global Building Atlas footprints...")
buildings <- sf::st_read(
  "data/base/daysimetric/lisbon_metro_buildings_footprints.geojson",
  quiet = TRUE
)
message(sprintf("[load] Buildings: %d footprints", nrow(buildings)))
require_cols(buildings, c("volume_m3", "est_floors"), "lisbon_metro_buildings_footprints.geojson")

# Keep only needed columns and drop dataset-specific identifiers like 'id'
# to prevent column collisions with grid$id during spatial intersections
buildings <- buildings |>
  select(any_of(c("volume_m3", "est_floors", "footprint_m2", "height")))

message("[grid] Loading H3 grid from data/geo/grid.gpkg...")
grid <- sf::st_read("data/geo/grid.gpkg", quiet = TRUE) |> st_make_valid()
if ("gid" %in% names(grid)) {
  grid <- grid |> rename(id = gid)
} else if (!"id" %in% names(grid)) {
  id_col <- setdiff(names(grid), attr(grid, "sf_column"))[1]
  warning(sprintf("Grid has no 'gid' or 'id' column - using '%s'. Check names(grid).", id_col))
  grid <- grid |> rename(id = !!id_col)
}
grid$id <- as.integer(grid$id)
message(sprintf("[grid] Grid loaded: %d cells", nrow(grid)))

# Municipalities - needed only for TEST_MODE subsetting
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


# 2. Reproject to EPSG:3763 (Metric CRS for Portugal) -------------------------

message("[prep] Reprojecting all layers to EPSG:3763...")
grid <- st_transform(grid, target_crs) |> st_make_valid()
census_bgri <- st_transform(census_bgri, target_crs) |> st_make_valid()
buildings <- st_transform(buildings, target_crs) |> st_make_valid()

# Ensure footprint_m2 is accurately computed in metric units (m2 in EPSG:3763)
message("[prep] Computing footprint_m2 in EPSG:3763...")
buildings$footprint_m2 <- as.numeric(st_area(buildings))

if ("height" %in% names(buildings)) {
  message("[prep] Recomputing volume_m3 from metric footprint and height...")
  buildings$volume_m3 <- ifelse(
    !is.na(buildings$height) & buildings$height > 0,
    round(buildings$footprint_m2 * buildings$height, 1),
    buildings$volume_m3
  )
}


# 3. TEST MODE subsetting ----------------------------------------------------

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
    st_transform(target_crs) |>
    st_make_valid()
  stopifnot(nrow(test_boundary) == 1)

  grid <- grid[test_boundary, ]
  buildings <- buildings[test_boundary, ]
  census_bgri <- census_bgri[test_boundary, ]
  message(sprintf(
    "[TEST MODE] Subset: %d grid cells, %d buildings, %d BGRI blocks",
    nrow(grid), nrow(buildings), nrow(census_bgri)
  ))
}


# =============================================================================
# UNIFIED SPATIAL ALLOCATION (Building Atlas x H3 Grid x Census BGRI)
# =============================================================================

# 4. Proportional allocation and BGRI association ----------------------------
#
# For each building:
#   - Intersect with H3 grid (grid_slim). Straddling buildings (>1 cell) are sliced
#     by cell boundaries; their volume, footprint, and count are prorated proportionally.
#   - Match each fragment to its containing census block (BGRI2021) via interior point.
#
# This single unified pass produces all fragments required for BOTH Section 1 and Section 2.

grid_slim <- grid |> select(id)
bgri_slim <- census_bgri |> select(BGRI2021)

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
  "[alloc] Processing %d buildings across %d chunks (%d workers, EPSG:3763)...",
  n_bldg, n_chunks, N_WORKERS
))

run_unified_bldg_chunk <- function(i, bldg_batch) {
  library(sf)
  sf_use_s2(FALSE)
  library(dplyr)
  library(tibble)

  t0 <- Sys.time()

  empty_frags_tibble <- function() {
    tibble(
      id                  = integer(0),
      BGRI2021            = character(0),
      frag_volume_m3      = numeric(0),
      frag_footprint_m2   = numeric(0),
      frag_building_count = numeric(0),
      est_floors          = numeric(0),
      spans_multi         = logical(0)
    )
  }

  # 1. Spatial intersection against H3 grid
  hits_grid <- st_intersects(bldg_batch, grid_slim)
  n_hits_grid <- lengths(hits_grid)

  # A. Buildings touching exactly 1 cell (no geometry clipping required)
  idx_single <- which(n_hits_grid == 1)
  if (length(idx_single) > 0) {
    bldgs_single <- bldg_batch[idx_single, ]
    cell_ids_single <- grid_slim$id[unlist(hits_grid[idx_single])]

    # Find containing BGRI block via representative interior point
    cents_single <- suppressWarnings(st_point_on_surface(st_geometry(bldgs_single)))
    hits_bgri_single <- st_intersects(cents_single, bgri_slim)
    bgri_pos_single <- vapply(
      hits_bgri_single,
      function(x) if (length(x) >= 1) x[1] else NA_integer_,
      integer(1)
    )

    # For tiny border edge cases where point falls on border, fallback to nearest BGRI
    na_bgri <- which(is.na(bgri_pos_single))
    if (length(na_bgri) > 0) {
      bgri_pos_single[na_bgri] <- st_nearest_feature(cents_single[na_bgri], bgri_slim)
    }

    out_single <- tibble(
      id                  = as.integer(cell_ids_single),
      BGRI2021            = as.character(bgri_slim$BGRI2021[bgri_pos_single]),
      frag_volume_m3      = as.numeric(bldgs_single$volume_m3),
      frag_footprint_m2   = as.numeric(bldgs_single$footprint_m2),
      frag_building_count = 1.0,
      est_floors          = as.numeric(bldgs_single$est_floors),
      spans_multi         = FALSE
    )
  } else {
    out_single <- empty_frags_tibble()
  }

  # B. Buildings spanning multiple cells (slice proportionally in EPSG:3763)
  idx_multi <- which(n_hits_grid > 1)
  if (length(idx_multi) > 0) {
    bldgs_multi <- bldg_batch[idx_multi, ]
    cand_cells <- sort(unique(unlist(hits_grid[idx_multi])))
    grid_sub <- grid_slim[cand_cells, ]
    frags <- suppressWarnings(st_intersection(bldgs_multi, grid_sub))

    if (nrow(frags) > 0) {
      frags$frag_area <- as.numeric(st_area(frags)) # metric m2 in EPSG:3763
      # Discard degenerate zero-area intersections (e.g. touches on boundary edge or point)
      frags <- frags[!st_is_empty(frags) & frags$frag_area > 0.001, ]
    }

    if (nrow(frags) > 0) {
      frags$orig_footprint <- pmax(as.numeric(frags$footprint_m2), frags$frag_area, 1e-6)
      frags$area_ratio <- pmin(1.0, frags$frag_area / frags$orig_footprint)

      # Find containing BGRI block for each fragment
      cents_multi <- suppressWarnings(st_point_on_surface(st_geometry(frags)))
      hits_bgri_multi <- st_intersects(cents_multi, bgri_slim)
      bgri_pos_multi <- vapply(
        hits_bgri_multi,
        function(x) if (length(x) >= 1) x[1] else NA_integer_,
        integer(1)
      )
      na_bgri_m <- which(is.na(bgri_pos_multi))
      if (length(na_bgri_m) > 0) {
        bgri_pos_multi[na_bgri_m] <- st_nearest_feature(cents_multi[na_bgri_m], bgri_slim)
      }

      out_multi <- tibble(
        id                  = as.integer(frags$id),
        BGRI2021            = as.character(bgri_slim$BGRI2021[bgri_pos_multi]),
        frag_volume_m3      = as.numeric(frags$volume_m3) * frags$area_ratio,
        frag_footprint_m2   = as.numeric(frags$frag_area),
        frag_building_count = 1.0 * frags$area_ratio,
        est_floors          = as.numeric(frags$est_floors),
        spans_multi         = TRUE
      )
    } else {
      out_multi <- empty_frags_tibble()
    }
  } else {
    out_multi <- empty_frags_tibble()
  }

  out <- bind_rows(out_single, out_multi)

  n_placed <- length(idx_single) + length(idx_multi)
  n_out <- sum(n_hits_grid == 0)
  message(sprintf(
    "[alloc %d/%d] %d buildings -> %d placed (%d multi-cell), %d outside grid (%.1fs)",
    i, n_chunks, nrow(bldg_batch), n_placed, length(idx_multi), n_out,
    as.numeric(difftime(Sys.time(), t0, units = "secs"))
  ))
  out
}

all_fragments <- bind_rows(future_map2(
  seq_len(n_chunks), bldg_batches, run_unified_bldg_chunk,
  .options = furrr_options(seed = NULL)
))
rm(bldg_batches)
gc()

total_assigned_count <- sum(all_fragments$frag_building_count, na.rm = TRUE)
total_assigned_vol <- sum(all_fragments$frag_volume_m3, na.rm = TRUE)
total_assigned_area <- sum(all_fragments$frag_footprint_m2, na.rm = TRUE)

message(sprintf(
  "[alloc] Unified fragments ready: %s fragments, representing %s buildings in AML (%.1f%% of dataset)",
  format(nrow(all_fragments), big.mark = ","),
  format(round(total_assigned_count), big.mark = ","),
  100 * total_assigned_count / n_bldg
))


# =============================================================================
# SECTION 2 - Global Building Atlas attributes per H3 cell
# =============================================================================

# 5. Aggregate GBA stats per grid cell ----------------------------------------

message("[s2] Aggregating Building Atlas metrics per grid cell...")

s2_grid <- all_fragments |>
  filter(!is.na(id)) |>
  group_by(id) |>
  summarise(
    atlas_building_count = sum(frag_building_count, na.rm = TRUE),
    atlas_total_volume_m3 = sum(frag_volume_m3, na.rm = TRUE),
    atlas_total_footprint_m2 = sum(frag_footprint_m2, na.rm = TRUE),
    atlas_total_floors = sum(est_floors * frag_building_count, na.rm = TRUE),
    atlas_mean_estimated_floors = mean(est_floors, na.rm = TRUE),
    atlas_weighted_mean_estimated_floors = sum(est_floors * frag_footprint_m2, na.rm = TRUE) /
      pmax(sum(frag_footprint_m2, na.rm = TRUE), 1e-6),
    atlas_straddling_building_count = sum(spans_multi, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    atlas_mean_estimated_floors = ifelse(
      is.nan(atlas_mean_estimated_floors) | is.na(atlas_mean_estimated_floors),
      NA_real_, atlas_mean_estimated_floors
    ),
    atlas_weighted_mean_estimated_floors = ifelse(
      is.nan(atlas_weighted_mean_estimated_floors) | is.na(atlas_weighted_mean_estimated_floors) | atlas_total_footprint_m2 == 0,
      NA_real_, atlas_weighted_mean_estimated_floors
    )
  )

message(sprintf("[s2] Section 2 done: %d grid cells contain >=1 building", nrow(s2_grid)))


# =============================================================================
# SECTION 1 - INE Census BGRI attributes (empirical atlas-based disaggregation)
# =============================================================================

# 6. Build empirical disaggregation weights from Atlas fragments --------------
#
# Weights per (id x BGRI2021):
#   - Buildings: weight_footprint = sum(frag_footprint_m2)
#   - Dwellings: weight_volume    = sum(frag_volume_m3)
# Fallback for BGRIs with 0 Atlas footprints: geometric overlap area (m2 in EPSG:3763).

message("[s1] Building empirical weights from Global Building Atlas footprints...")

frag_weights <- all_fragments |>
  filter(!is.na(id) & !is.na(BGRI2021)) |>
  group_by(id, BGRI2021) |>
  summarise(
    weight_footprint = sum(frag_footprint_m2, na.rm = TRUE),
    weight_volume = sum(frag_volume_m3, na.rm = TRUE),
    .groups = "drop"
  )
rm(all_fragments)
gc()

# Detect any BGRI blocks with zero Atlas buildings; compute geometric area fallback
all_bgri_codes <- unique(census_bgri$BGRI2021)
bgri_with_atlas <- unique(frag_weights$BGRI2021)
missing_bgris <- setdiff(all_bgri_codes, bgri_with_atlas)

if (length(missing_bgris) > 0) {
  message(sprintf(
    "[s1] %d / %d BGRI blocks have zero Atlas building footprints. Computing geometric area fallback...",
    length(missing_bgris), length(all_bgri_codes)
  ))
  bgri_missing_sf <- census_bgri |>
    filter(BGRI2021 %in% missing_bgris) |>
    select(BGRI2021)

  fallback_frags <- suppressWarnings(st_intersection(grid_slim, bgri_missing_sf))
  if (nrow(fallback_frags) > 0) {
    fallback_frags$bare_area <- as.numeric(st_area(fallback_frags)) # EPSG:3763 metric area
    fallback_weights <- fallback_frags |>
      st_drop_geometry() |>
      filter(bare_area > 0) |>
      transmute(
        id,
        BGRI2021,
        weight_footprint = bare_area,
        weight_volume    = bare_area,
        weight_source    = "bare_area_fallback"
      )
  } else {
    fallback_weights <- tibble()
  }
} else {
  fallback_weights <- tibble()
}

all_weights <- frag_weights |>
  mutate(weight_source = "atlas_footprint") |>
  bind_rows(fallback_weights)

# Calculate proportional split fractions per BGRI
bldg_splits <- all_weights |>
  group_by(BGRI2021) |>
  mutate(total_bgri_fp = sum(weight_footprint)) |>
  ungroup() |>
  filter(total_bgri_fp > 0) |>
  mutate(frac_building = weight_footprint / total_bgri_fp)

dwell_splits <- all_weights |>
  group_by(BGRI2021) |>
  mutate(total_bgri_vol = sum(weight_volume)) |>
  ungroup() |>
  filter(total_bgri_vol > 0) |>
  mutate(frac_dwelling = weight_volume / total_bgri_vol)

all_splits <- bldg_splits |>
  select(id, BGRI2021, frac_building, weight_source, weight_footprint) |>
  left_join(
    dwell_splits |> select(id, BGRI2021, frac_dwelling),
    by = c("id", "BGRI2021")
  ) |>
  mutate(frac_dwelling = coalesce(frac_dwelling, frac_building))


# 7. Disaggregate BGRI census indicators across H3 cells ----------------------

census_attr <- census_bgri |>
  st_drop_geometry() |>
  transmute(
    BGRI2021,
    census_buildings_1_2_floors = as.numeric(N_EDIFICIOS_1_OU_2_PISOS),
    census_buildings_3plus_floors = as.numeric(N_EDIFICIOS_3_OU_MAIS_PISOS),
    census_buildings_pre1945 = as.numeric(N_EDIFICIOS_CONSTR_ANTES_1945),
    census_buildings_post1945 = pmax(0, as.numeric(N_EDIFICIOS_CLASSICOS) -
      as.numeric(N_EDIFICIOS_CONSTR_ANTES_1945)),
    census_buildings_exclusive_residential = as.numeric(N_EDIFICIOS_EXCLUSIV_RESID),
    census_buildings_total = as.numeric(N_EDIFICIOS_CLASSICOS),
    census_dwellings_total = as.numeric(.data[[aloj_col]]),
    census_dwellings_with_parking = as.numeric(N_RHABITUAL_COM_ESTACIONAMENTO),
    census_dwellings_wheelchair_accessible = as.numeric(N_RHABITUAL_ACESSIVEL_CADEIRAS_RODAS)
  ) |>
  distinct(BGRI2021, .keep_all = TRUE)

distribute_cols <- c(
  "census_buildings_1_2_floors", "census_buildings_3plus_floors",
  "census_buildings_pre1945", "census_buildings_post1945",
  "census_buildings_exclusive_residential", "census_buildings_total",
  "census_dwellings_total",
  "census_dwellings_with_parking", "census_dwellings_wheelchair_accessible"
)

# Raw BGRI totals for validation
bgri_raw_totals <- sapply(distribute_cols, function(col) sum(census_attr[[col]], na.rm = TRUE))

message("[s1] Distributing BGRI building and dwelling counts across H3 cells...")

s1_grid <- all_splits |>
  left_join(census_attr, by = "BGRI2021") |>
  mutate(
    census_buildings_1_2_floors            = census_buildings_1_2_floors * frac_building,
    census_buildings_3plus_floors          = census_buildings_3plus_floors * frac_building,
    census_buildings_pre1945               = census_buildings_pre1945 * frac_building,
    census_buildings_post1945              = census_buildings_post1945 * frac_building,
    census_buildings_exclusive_residential = census_buildings_exclusive_residential * frac_building,
    census_buildings_total                 = census_buildings_total * frac_building,
    census_dwellings_total                 = census_dwellings_total * frac_dwelling,
    census_dwellings_with_parking          = census_dwellings_with_parking * frac_dwelling,
    census_dwellings_wheelchair_accessible = census_dwellings_wheelchair_accessible * frac_dwelling
  ) |>
  group_by(id) |>
  summarise(
    across(all_of(distribute_cols), ~ sum(.x, na.rm = TRUE)),
    census_weight_source = weight_source[which.max(weight_footprint)],
    .groups = "drop"
  )

message(sprintf("[s1] Section 1 done: %d grid cells with BGRI-derived attributes", nrow(s1_grid)))

rm(all_splits, census_attr, bldg_splits, dwell_splits, all_weights)
gc()


# =============================================================================
# COMBINE & EXPORT
# =============================================================================

# 8. Join both sections onto the full grid ------------------------------------

plan(sequential) # release parallel workers
message("[combine] Joining Section 1 + Section 2 onto the full grid...")

grid_out <- grid |>
  select(id) |>
  left_join(s1_grid, by = "id") |>
  left_join(s2_grid, by = "id") |>
  mutate(
    across(all_of(distribute_cols), ~ replace_na(.x, 0)),
    atlas_building_count = replace_na(atlas_building_count, 0),
    atlas_total_volume_m3 = replace_na(atlas_total_volume_m3, 0),
    atlas_total_footprint_m2 = replace_na(atlas_total_footprint_m2, 0),
    atlas_total_floors = replace_na(atlas_total_floors, 0),
    atlas_straddling_building_count = replace_na(atlas_straddling_building_count, 0L),
    census_pct_residential = if_else(
      census_buildings_total > 0,
      round(pmin(100, census_buildings_exclusive_residential / census_buildings_total * 100), 2),
      NA_real_
    ),
    census_pct_dwellings_with_parking = if_else(
      census_dwellings_total > 0,
      round(pmin(100, census_dwellings_with_parking / census_dwellings_total * 100), 2),
      NA_real_
    ),
    census_pct_dwellings_wheelchair_accessible = if_else(
      census_dwellings_total > 0,
      round(pmin(100, census_dwellings_wheelchair_accessible / census_dwellings_total * 100), 2),
      NA_real_
    )
  ) |>
  mutate(
    across(all_of(distribute_cols), ~ round(.x, 2)),
    atlas_building_count                 = round(atlas_building_count, 2),
    atlas_total_volume_m3                = round(atlas_total_volume_m3, 1),
    atlas_total_footprint_m2             = round(atlas_total_footprint_m2, 1),
    atlas_total_floors                   = round(atlas_total_floors, 1),
    atlas_mean_estimated_floors          = round(atlas_mean_estimated_floors, 2),
    atlas_weighted_mean_estimated_floors = round(atlas_weighted_mean_estimated_floors, 2)
  ) |>
  select(
    id,
    # Census classical buildings
    census_buildings_1_2_floors, census_buildings_3plus_floors,
    census_buildings_pre1945, census_buildings_post1945,
    census_buildings_exclusive_residential, census_buildings_total,
    census_pct_residential,
    # Census dwellings
    census_dwellings_total,
    census_dwellings_with_parking, census_dwellings_wheelchair_accessible,
    census_pct_dwellings_with_parking, census_pct_dwellings_wheelchair_accessible,
    # Census weight source
    census_weight_source,
    # Global Building Atlas physical footprints
    atlas_building_count, atlas_total_volume_m3, atlas_total_footprint_m2,
    atlas_total_floors,
    atlas_mean_estimated_floors, atlas_weighted_mean_estimated_floors,
    atlas_straddling_building_count
  ) |>
  st_transform(4326) # standard WGS84 for GIS and web export


# 9. Validation --------------------------------------------------------------

message("--- Section 1 attribute totals check (grid vs raw BGRI) ---")
val <- lapply(distribute_cols, function(col) {
  bgri_tot <- bgri_raw_totals[[col]]
  grid_tot <- sum(s1_grid[[col]], na.rm = TRUE)
  diff_pct <- if (bgri_tot != 0) 100 * (bgri_tot - grid_tot) / bgri_tot else NA_real_
  message(sprintf(
    "  %-38s BGRI %12s   grid %12s   diff %6.2f%%",
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
  "[s2] Section 2 conservation: %s buildings, %s m3 volume, %s m2 footprint allocated to grid.",
  format(round(total_assigned_count), big.mark = ","),
  format(round(total_assigned_vol), big.mark = ","),
  format(round(total_assigned_area), big.mark = ",")
))


# 10. Export -----------------------------------------------------------------

out_dir <- "data/base/daysimetric"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_gpkg <- file.path(out_dir, sprintf("grid_building_attributes%s.gpkg", run_suffix))
out_csv <- file.path(out_dir, sprintf("grid_building_attributes%s.csv", run_suffix))

sf::st_write(grid_out, out_gpkg, delete_dsn = TRUE)
write.csv(sf::st_drop_geometry(grid_out), out_csv, row.names = FALSE)

message(sprintf("[done] Exported: %s  (+ .csv)", out_gpkg))
message(sprintf(
  "[done] Grid cells: %d total, %d with buildings, %d with BGRI-derived attributes",
  nrow(grid_out), sum(grid_out$atlas_building_count > 0, na.rm = TRUE),
  sum(grid_out$census_buildings_total > 0, na.rm = TRUE)
))
