# Dasymetric residential population map - COS class weight + building volume combined
#
# PURPOSE
# -------
# This script is a synthesis of two earlier dasymetric approaches:
#
#   1. 02_census_grid_with_cos.R (IMPT-data project)
#      Weights each grid × BGRI fragment by COS land-use class × area:
#      vertical apartments = 3×, horizontal = 1×, discontinuous = 0.6×, sparse = 0.3×.
#      Pro: < 1% population loss.
#      Con: ignores building height — a tall apartment block and a small house in the
#           same COS class get the same per-area weight.
#
#   2. 02c_dasymetric_population.R (this folder)
#      Weights each fragment by residential building volume (footprint × height),
#      sourced from the Global Building Atlas, clipped to residential COS zones.
#      Pro: captures density variation within a COS class.
#      Con: ~8.8% population loss where the building atlas has gaps (Amadora,
#           Vila Franca de Xira) — blocks with zero matched volume are dropped.
#
# STRATEGY: hierarchical fallback
# --------------------------------
# For every (H3 cell, BGRI block) pair the weight follows this priority:
#
#   Priority 1 — building volume > 0      → use volume weight   (best proxy)
#   Priority 2 — COS residential area > 0 → use COS-area × class weight (good proxy)
#   Priority 3 — neither                  → use raw BGRI-area overlap (last resort)
#
# Nothing is ever dropped: every BGRI block's population lands somewhere.
# Target: < 1% total population loss vs. raw BGRI census total.
#
# OUTPUT
# ------
# data/base/dasymetric/grid_dasymetric_combined[_test_<municipio>].gpkg + .csv
# One row per H3 r9 cell with total population and all demographic breakdowns
# (age groups, sex, households, buildings) — same schema as 02_census_grid_with_cos.R,
# not just total_pop like 02c.
#
# DATA ACCESS
# -----------
# This project stores no data in the repo. Data lives on the team server at
# /data/biclar-parking/ and is accessed by mounting it locally with sshfs:
#
#   mkdir data   # inside the repo root (already ignored by .gitignore)
#   sshfs username@ushift-alfa:/data/biclar-parking ./data
#
# All paths below are plain relative paths from the repo root, e.g.
# "data/base/daysimetric/BGRI21_170.gpkg". No impt_read/impt_write wrapper needed.
#
# INPUTS (all under data/, mounted from server)
# ----
#   data/geo/grid.gpkg                                         — H3 r9 grid (gid col)
#   data/geo/municipalities.gpkg                               — municipality boundaries
#   data/base/daysimetric/BGRI21_170.gpkg                      — 2021 census blocks
#   data/base/daysimetric/cos_residential_aml.gpkg             — residential COS (from 01c)
#   data/base/daysimetric/lisbon_metro_buildings_footprints.geojson  — buildings (from 02b)
#
# DEPENDENCIES
# ------------
# Run 01c_cos_residential_prep.R and 02b_buildings_volume_prep.R before this script.
# Those scripts still use impt_read/impt_write internally — that's fine, this script
# only needs their output files, which it reads directly.
#
# PARALLELISM & CHECKPOINTS
# -------------------------
# The two expensive intersection steps (clip buildings to COS; intersect grid × buildings
# × BGRI) are chunked and run sequentially with per-chunk checkpoints
# written to data/base/dasymetric/checkpoints_combined/. An interrupted run resumes by
# re-running from the top: cheap steps redo, chunked steps reload their checkpoints.
#
# TEST_MODE
# ---------
# Set TEST_MODE <- TRUE to run against one municipality only (Oeiras by default) for
# fast sanity-checking. Compare sum(grid_final$population) against the subset BGRI total.

# 0. Setup ---------------------------------------------------------------

required_packages <- c("sf", "dplyr", "tidyr", "tibble", "future", "furrr", "mapview")
missing_packages  <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) install.packages(missing_packages)

library(sf)
library(dplyr)
library(tidyr)
library(tibble)
library(future)
library(furrr)
library(mapview)

# Run this script from the repo root (biclar-parking/) so all relative paths
# below resolve correctly against the mounted ./data/ folder.
stopifnot(
  "Run from the repo root: setwd('/path/to/biclar-parking')" =
    file.exists("data") || dir.exists("data")
)

# --- Configuration flags ---

# Parallelism — safe to use because checkpoints are written to a LOCAL directory
# (checkpoints/dasymetric_combined/), not to the sshfs-mounted data/ folder.
# With 28 cores and ~107 GB free RAM, 8 workers is a conservative choice;
# raise to 12-14 if RAM stays below 80 GB during the first few chunks.
N_WORKERS <- 8
plan(multisession, workers = N_WORKERS)

target_crs <- 3763  # EPSG:3763 — Portugal metric CRS

# TEST MODE: restrict the whole run to one municipality for fast sanity-checking.
TEST_MODE      <- FALSE
TEST_MUNICIPIO <- "Oeiras"  # must match a 'municipio' name in useful_data/municipalities.csv

# Number of building rows processed per parallel chunk.
CHUNK_SIZE <- 50000

# Checkpoint directory — stored LOCALLY (not under data/) because the sshfs/bind-mounted
# data/ folder may be read-only from the client machine. Local checkpoints survive
# disconnection and can be safely deleted after a successful full run.
# Using an absolute path so PSOCK workers resolve it correctly regardless of their wd.
CHECKPOINT_DIR <- file.path(getwd(), "checkpoints", "dasymetric_combined")

# Unique suffix so TEST_MODE and full-run checkpoints never share a filename.
run_suffix <- if (TEST_MODE) paste0("_test_", tolower(TEST_MUNICIPIO)) else ""

# Create the checkpoint directory in the main process before any parallel dispatch.
# Stop with a clear message if it fails (e.g. permission denied) rather than
# letting workers fail silently.
if (!dir.create(CHECKPOINT_DIR, recursive = TRUE, showWarnings = TRUE)) {
  if (!dir.exists(CHECKPOINT_DIR)) {
    stop(sprintf(
      "Cannot create checkpoint directory:\n  %s\nCheck write permissions.",
      CHECKPOINT_DIR
    ))
  }
  # dir.create() returns FALSE if dir already exists — that's fine, just resume.
  message(sprintf("[checkpoints] Resuming from existing directory: %s", CHECKPOINT_DIR))
} else {
  message(sprintf("[checkpoints] Created: %s", CHECKPOINT_DIR))
}

# Helper: try to read a checkpointed .Rds; return NULL if it doesn't exist yet.
read_checkpoint <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) NULL)
}

# Helper: write a checkpointed .Rds.
# dir.create() here is belt-and-suspenders: the directory should already exist
# (created in the main process above) but this guards against edge cases where
# a worker runs with a slightly different environment.
write_checkpoint <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(object, path)
}


# 1. Load data -----------------------------------------------------------

message("[load] Reading BGRI census blocks...")
census_bgri <- sf::st_read("data/base/daysimetric/BGRI21_170.gpkg", quiet = TRUE)

# Schema sanity check — fail loudly rather than silently use wrong columns.
stopifnot(
  "N_INDIVIDUOS"              %in% names(census_bgri),
  "BGRI2021"                  %in% names(census_bgri),
  "N_EDIFICIOS_CLASSICOS"     %in% names(census_bgri),
  "N_EDIFICIOS_CONSTR_ANTES_1945" %in% names(census_bgri),
  "N_NUCLEOS_FAMILIARES"      %in% names(census_bgri),
  "N_INDIVIDUOS_0_14"         %in% names(census_bgri),
  "N_INDIVIDUOS_65_OU_MAIS"   %in% names(census_bgri),
  "N_INDIVIDUOS_M"            %in% names(census_bgri)
)
message(sprintf("[load] BGRI: %d blocks, total N_INDIVIDUOS = %s",
  nrow(census_bgri), format(sum(census_bgri$N_INDIVIDUOS, na.rm = TRUE), big.mark = ",")))

message("[load] Reading COS residential polygons...")
cos_residential_aml <- sf::st_read("data/base/daysimetric/cos_residential_aml.gpkg", quiet = TRUE)
message(sprintf("[load] COS residential: %d polygons", nrow(cos_residential_aml)))

message("[load] Reading Global Building Atlas footprints...")
buildings <- sf::st_read("data/base/daysimetric/lisbon_metro_buildings_footprints.geojson", quiet = TRUE)
message(sprintf("[load] Buildings: %d footprints", nrow(buildings)))

# Rename building attribute columns to avoid collisions with the grid's id/area columns.
buildings <- buildings |>
  rename(
    building_id           = id,
    building_footprint_m2 = footprint_m2,
    building_volume_m3    = volume_m3
  )

# Fix invalid geometries at load time.
message("[prep] st_make_valid on BGRI, COS, buildings...")
census_bgri         <- st_make_valid(census_bgri)
cos_residential_aml <- st_make_valid(cos_residential_aml)
buildings           <- st_make_valid(buildings)
message("[prep] done")


# 2. Load grid and municipalities -----------------------------------------
#
# The grid is identified by `gid` (per the project README: "the id for a grid
# cell is called gid"). Loaded directly — same line as 00_data_load.R.
# Municipality names come from useful_data/municipalities.csv (mid, name) joined
# onto the spatial layer, same pattern as 00_data_load.R's approach.

message("[grid] Loading H3 r9 grid from data/geo/grid.gpkg...")
grid <- sf::st_read("data/geo/grid.gpkg", quiet = TRUE) |> st_make_valid()
# Standardise to an `id` column so all downstream joins/group_bys are consistent.
# Per README the column is called `gid` — rename defensively in case it differs.
if ("gid" %in% names(grid)) {
  grid <- grid |> rename(id = gid)
} else if (!"id" %in% names(grid)) {
  id_col <- setdiff(names(grid), attr(grid, "sf_column"))[1]
  warning(sprintf("Grid has no 'gid' or 'id' column — using '%s'. Check names(grid).", id_col))
  grid <- grid |> rename(id = !!id_col)
}
message(sprintf("[grid] Grid loaded: %d cells", nrow(grid)))

message("[grid] Loading municipalities from data/geo/municipalities.gpkg...")
municipalities_sf <- sf::st_read("data/geo/municipalities.gpkg", quiet = TRUE) |> st_make_valid()

# Attach human-readable names from useful_data/municipalities.csv (mid, name).
if (!"municipio" %in% names(municipalities_sf) && file.exists("useful_data/municipalities.csv")) {
  mun_lookup <- read.csv("useful_data/municipalities.csv", stringsAsFactors = FALSE)
  if (all(c("mid", "name") %in% names(mun_lookup))) {
    municipalities_sf <- municipalities_sf |>
      mutate(mid = as.character(mid)) |>
      left_join(mun_lookup |> mutate(mid = as.character(mid)) |> select(mid, municipio = name),
                by = "mid")
  }
}


# 3. TEST MODE subsetting ------------------------------------------------

if (TEST_MODE) {
  message(sprintf("[TEST MODE] Subsetting to municipality: %s", TEST_MUNICIPIO))

  if (!"municipio" %in% names(municipalities_sf) || !TEST_MUNICIPIO %in% municipalities_sf$municipio) {
    avail <- if ("municipio" %in% names(municipalities_sf))
      paste(sort(unique(municipalities_sf$municipio)), collapse = ", ") else "(no 'municipio' column)"
    stop(sprintf("TEST_MUNICIPIO '%s' not found. Available: %s", TEST_MUNICIPIO, avail))
  }

  test_boundary <- municipalities_sf |> filter(municipio == TEST_MUNICIPIO) |> st_make_valid()
  stopifnot(nrow(test_boundary) == 1)

  ref_crs <- st_crs(test_boundary)
  grid                <- st_transform(grid,                ref_crs)[test_boundary, ]
  buildings           <- st_transform(buildings,           ref_crs)[test_boundary, ]
  census_bgri         <- st_transform(census_bgri,         ref_crs)[test_boundary, ]
  cos_residential_aml <- st_transform(cos_residential_aml, ref_crs)[test_boundary, ]

  message(sprintf(
    "[TEST MODE] Subset: %d grid cells, %d buildings, %d BGRI blocks",
    nrow(grid), nrow(buildings), nrow(census_bgri)
  ))
}


# 4. Reproject to EPSG:3763 for metric calculations ----------------------

message("[prep] Reprojecting to EPSG:3763...")
buildings           <- st_transform(buildings,           target_crs)
census_bgri         <- st_transform(census_bgri,         target_crs)
grid                <- st_transform(grid,                target_crs) |> st_make_valid()
cos_residential_aml <- st_transform(cos_residential_aml, target_crs)


# 5. Prepare weight layers -----------------------------------------------

# --- WEIGHT B: COS class weights (same as 02_census_grid_with_cos.R) ---
cos_weighted <- cos_residential_aml |>
  rename(classification = COS23_n4_L) |>
  mutate(cos_weight = case_when(
    classification == "Áreas edificadas residenciais contínuas predominantemente verticais"   ~ 3.0,
    classification == "Áreas edificadas residenciais contínuas predominantemente horizontais" ~ 1.0,
    classification == "Áreas edificadas residenciais descontínuas"                            ~ 0.6,
    TRUE                                                                                      ~ 0.3
  )) |>
  select(classification, cos_weight)

# Dissolve COS into one mask for clipping buildings.
message("[prep] Dissolving residential COS into a single mask (may take a few minutes)...")
cos_residential_mask <- cos_residential_aml |>
  st_make_valid() |>
  st_union() |>
  st_make_valid()

if (!all(st_geometry_type(cos_residential_mask) %in% c("POLYGON", "MULTIPOLYGON"))) {
  message("[prep] Non-polygon geometry from st_union() — extracting polygonal parts")
  cos_residential_mask <- st_collection_extract(cos_residential_mask, "POLYGON") |> st_make_valid()
}
message(sprintf("[prep] COS mask ready: %d vertices",
  nrow(st_coordinates(cos_residential_mask))))

# --- BGRI slim version for intersections ---
# Split MULTIPOLYGON → POLYGON for predictable intersection behaviour.
census_simple <- census_bgri |>
  select(BGRI2021, N_INDIVIDUOS,
         N_EDIFICIOS_CLASSICOS, N_EDIFICIOS_CONSTR_ANTES_1945,
         N_NUCLEOS_FAMILIARES,
         N_INDIVIDUOS_0_14, N_INDIVIDUOS_15_24, N_INDIVIDUOS_25_64,
         N_INDIVIDUOS_65_OU_MAIS, N_INDIVIDUOS_M) |>
  st_cast("POLYGON") |>
  st_make_valid()


# 6. Filter grid to active cells (overlap BGRI) --------------------------

message("[grid] Filtering to cells that overlap at least one BGRI block...")
n_grid_raw    <- nrow(grid)
n_grid_chunks <- if (TEST_MODE) 1L else ceiling(n_grid_raw / CHUNK_SIZE)

grid_filter_batches <- if (TEST_MODE) {
  list(grid)
} else {
  lapply(seq_len(n_grid_chunks), function(i) {
    grid[((i - 1) * CHUNK_SIZE + 1):min(i * CHUNK_SIZE, n_grid_raw), ]
  })
}

run_grid_filter_chunk <- function(i, grid_batch) {
  library(sf)
  ckpt <- sprintf("%s/grid_active%s_%03d.Rds", CHECKPOINT_DIR, run_suffix, i)
  cached <- read_checkpoint(ckpt)
  if (!is.null(cached)) {
    message(sprintf("[grid filter %d/%d] checkpoint found", i, n_grid_chunks))
    return(cached)
  }
  t0 <- Sys.time()
  hits   <- st_intersects(grid_batch, census_bgri, sparse = TRUE)
  result <- grid_batch[lengths(hits) > 0, ]
  write_checkpoint(result, ckpt)
  message(sprintf("[grid filter %d/%d] %d -> %d active cells (%.1fs)",
    i, n_grid_chunks, nrow(grid_batch), nrow(result),
    as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  result
}
grid_active <- bind_rows(future_map2(
  seq_len(n_grid_chunks), grid_filter_batches,
  run_grid_filter_chunk,
  .options = furrr_options(seed = NULL)
))
rm(grid_filter_batches)
gc()
message(sprintf("[grid] Active cells: %d / %d", nrow(grid_active), n_grid_raw))


# 7. WEIGHT A: building-volume fragments (chunked + parallel) ------------
#
# Step 7a: clip buildings to residential COS zones.
# Step 7b: intersect grid × clipped buildings × BGRI → volume fragments.
# Each fragment's frag_volume = building_volume_m3 × (fragment_area / building_footprint_m2).
# BGRI-level normalisation happens once at the end across all chunks combined.

# -- 7a: residential clip --

n_bldg_raw           <- nrow(buildings)
n_residential_chunks <- if (TEST_MODE) 1L else ceiling(n_bldg_raw / CHUNK_SIZE)

residential_batches <- if (TEST_MODE) {
  list(buildings)
} else {
  lapply(seq_len(n_residential_chunks), function(i) {
    buildings[((i - 1) * CHUNK_SIZE + 1):min(i * CHUNK_SIZE, n_bldg_raw), ]
  })
}

message(sprintf("[bldg] Clipping %d buildings to COS mask (%d chunks, %d workers)...",
  n_bldg_raw, n_residential_chunks, N_WORKERS))
run_residential_chunk <- function(i, buildings_batch) {
  library(sf)
  ckpt <- sprintf("%s/residential%s_%03d.Rds", CHECKPOINT_DIR, run_suffix, i)
  cached <- read_checkpoint(ckpt)
  if (!is.null(cached)) {
    message(sprintf("[bldg clip %d/%d] checkpoint found (%d buildings)",
      i, n_residential_chunks, nrow(cached)))
    return(cached)
  }
  t0 <- Sys.time()
  result <- st_intersection(buildings_batch, cos_residential_mask)
  write_checkpoint(result, ckpt)
  message(sprintf("[bldg clip %d/%d] %d -> %d residential fragments (%.1fs)",
    i, n_residential_chunks, nrow(buildings_batch), nrow(result),
    as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  result
}
buildings_residential <- bind_rows(future_map2(
  seq_len(n_residential_chunks), residential_batches,
  run_residential_chunk,
  .options = furrr_options(seed = NULL)
))
rm(residential_batches, buildings, cos_residential_mask)
gc()
message(sprintf("[bldg] Residential buildings after COS clip: %d", nrow(buildings_residential)))

# -- 7b: fragment intersection --

empty_vol_frags <- tibble(
  id          = grid_active$id[0],
  BGRI2021    = census_simple$BGRI2021[0],
  frag_volume = numeric(0)
)

n_bldg_res <- nrow(buildings_residential)
n_chunks   <- if (TEST_MODE) 1L else ceiling(n_bldg_res / CHUNK_SIZE)

fragment_batches <- if (TEST_MODE) {
  list(buildings_residential)
} else {
  lapply(seq_len(n_chunks), function(i) {
    buildings_residential[((i - 1) * CHUNK_SIZE + 1):min(i * CHUNK_SIZE, n_bldg_res), ]
  })
}

message(sprintf("[frags] Volume fragments: %d chunks across %d workers...", n_chunks, N_WORKERS))
t_wall <- Sys.time()
run_fragment_chunk <- function(i, buildings_batch) {
  library(sf)
  library(dplyr)
  ckpt <- sprintf("%s/vol_fragments%s_%03d.Rds", CHECKPOINT_DIR, run_suffix, i)
  cached <- read_checkpoint(ckpt)
  if (!is.null(cached)) {
    message(sprintf("[frags %d/%d] checkpoint found", i, n_chunks))
    return(cached)
  }
  t0 <- Sys.time()
  grid_bldg <- st_intersection(grid_active, buildings_batch)
  result <- if (nrow(grid_bldg) == 0) {
    empty_vol_frags
  } else {
    frags <- st_intersection(grid_bldg, census_simple)
    if (nrow(frags) == 0) {
      empty_vol_frags
    } else {
      frags$fragment_area <- as.numeric(st_area(frags))
      frags |>
        st_drop_geometry() |>
        mutate(frag_volume = building_volume_m3 * (fragment_area / building_footprint_m2)) |>
        select(id, BGRI2021, frag_volume)
    }
  }
  write_checkpoint(result, ckpt)
  message(sprintf("[frags %d/%d] %d buildings -> %d fragments (%.1fs)",
    i, n_chunks, nrow(buildings_batch), nrow(result),
    as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  result
}
vol_frags_raw <- bind_rows(future_map2(
  seq_len(n_chunks), fragment_batches,
  run_fragment_chunk,
  .options = furrr_options(seed = NULL)
))
message(sprintf("[frags] Done in %.1f min",
  as.numeric(difftime(Sys.time(), t_wall, units = "mins"))))

# Aggregate to (id × BGRI2021) — unnormalised; normalisation happens at step 10.
vol_weights <- vol_frags_raw |>
  group_by(id, BGRI2021) |>
  summarise(vol_weight = sum(frag_volume, na.rm = TRUE), .groups = "drop") |>
  filter(vol_weight > 0)

message(sprintf("[frags] (id × BGRI) pairs with building volume > 0: %d", nrow(vol_weights)))
rm(vol_frags_raw, fragment_batches, buildings_residential, empty_vol_frags)
gc()


# 8. WEIGHT B: COS-area fragments ----------------------------------------
# Not chunked — lighter than the building intersection.

message("[cos] Intersecting grid × COS weighted × BGRI...")
t0 <- Sys.time()
grid_cos <- st_intersection(grid_active, cos_weighted)
cos_bgri_frags <- st_intersection(grid_cos, census_simple) |>
  mutate(frag_area = as.numeric(st_area(.))) |>
  st_drop_geometry() |>
  mutate(cos_area_score = frag_area * cos_weight) |>
  group_by(id, BGRI2021) |>
  summarise(cos_weight_sum = sum(cos_area_score, na.rm = TRUE), .groups = "drop") |>
  filter(cos_weight_sum > 0)

message(sprintf("[cos] COS weight pairs: %d — %.1f min",
  nrow(cos_bgri_frags), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
rm(grid_cos, cos_weighted)
gc()


# 9. WEIGHT C: bare BGRI-area fallback -----------------------------------
# Ensures no BGRI block is ever silently dropped (last resort weight).

message("[fallback] Computing bare BGRI-area fallback fragments...")
t0 <- Sys.time()
bgri_area_frags <- st_intersection(grid_active, census_simple) |>
  mutate(bare_area = as.numeric(st_area(.))) |>
  st_drop_geometry() |>
  select(id, BGRI2021, bare_area) |>
  filter(bare_area > 0)

message(sprintf("[fallback] Bare-area pairs: %d — %.1f min",
  nrow(bgri_area_frags), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
rm(grid_active)
gc()


# 10. Combine weights with hierarchical fallback -------------------------

message("[combine] Applying hierarchical fallback...")

all_pairs <- bgri_area_frags |>
  left_join(vol_weights,    by = c("id", "BGRI2021")) |>
  left_join(cos_bgri_frags, by = c("id", "BGRI2021")) |>
  mutate(
    weight_source = case_when(
      !is.na(vol_weight)     & vol_weight    > 0 ~ "building_volume",
      !is.na(cos_weight_sum) & cos_weight_sum > 0 ~ "cos_area",
      TRUE                                        ~ "bare_area"
    ),
    weight = case_when(
      weight_source == "building_volume" ~ vol_weight,
      weight_source == "cos_area"        ~ cos_weight_sum,
      TRUE                               ~ bare_area
    )
  ) |>
  filter(weight > 0)

# Diagnostic: how many pairs came from each source.
weight_summary <- all_pairs |>
  group_by(weight_source) |>
  summarise(n_pairs = n(), .groups = "drop") |>
  mutate(pct = round(n_pairs / sum(n_pairs) * 100, 1))
message("[combine] Weight source breakdown:")
print(as.data.frame(weight_summary))

rm(vol_weights, cos_bgri_frags, bgri_area_frags)
gc()


# 11. Distribute population across cells ---------------------------------

message("[pop] Distributing BGRI population across H3 cells...")

demo_cols <- census_simple |>
  st_drop_geometry() |>
  select(BGRI2021, N_INDIVIDUOS, N_EDIFICIOS_CLASSICOS, N_EDIFICIOS_CONSTR_ANTES_1945,
         N_NUCLEOS_FAMILIARES, N_INDIVIDUOS_0_14, N_INDIVIDUOS_15_24,
         N_INDIVIDUOS_25_64, N_INDIVIDUOS_65_OU_MAIS, N_INDIVIDUOS_M)

fragments_with_pop <- all_pairs |>
  group_by(BGRI2021) |>
  mutate(total_bgri_weight = sum(weight)) |>
  ungroup() |>
  filter(total_bgri_weight > 0) |>
  mutate(pop_fraction = weight / total_bgri_weight) |>
  left_join(demo_cols, by = "BGRI2021") |>
  mutate(
    fragment_pop             = N_INDIVIDUOS                  * pop_fraction,
    fragment_buildings       = N_EDIFICIOS_CLASSICOS         * pop_fraction,
    fragment_buildings_pre45 = N_EDIFICIOS_CONSTR_ANTES_1945 * pop_fraction,
    fragment_households      = N_NUCLEOS_FAMILIARES          * pop_fraction,
    fragment_youth           = N_INDIVIDUOS_0_14             * pop_fraction,
    fragment_adults          = (N_INDIVIDUOS_15_24 + N_INDIVIDUOS_25_64) * pop_fraction,
    fragment_elderly         = N_INDIVIDUOS_65_OU_MAIS       * pop_fraction,
    fragment_women           = N_INDIVIDUOS_M                * pop_fraction
  )

h3_pop_table <- fragments_with_pop |>
  group_by(id) |>
  summarise(
    population        = sum(fragment_pop,             na.rm = TRUE),
    buildings         = sum(fragment_buildings,        na.rm = TRUE),
    buildings_pre1945 = sum(fragment_buildings_pre45,  na.rm = TRUE),
    households        = sum(fragment_households,       na.rm = TRUE),
    youth             = sum(fragment_youth,            na.rm = TRUE),
    adults            = sum(fragment_adults,           na.rm = TRUE),
    elderly           = sum(fragment_elderly,          na.rm = TRUE),
    women             = sum(fragment_women,            na.rm = TRUE),
    weight_source     = first(weight_source),
    .groups = "drop"
  )

rm(fragments_with_pop, all_pairs, demo_cols, census_simple)
gc()


# 12. Join population table to H3 grid geometry --------------------------

plan(sequential)  # release parallel workers
message("[output] Joining population table to H3 grid geometry...")

grid_final <- grid |>
  left_join(h3_pop_table, by = "id") |>
  mutate(
    across(c(population, buildings, buildings_pre1945, households,
             youth, adults, elderly, women), ~ replace_na(.x, 0)),
    across(c(population, buildings, buildings_pre1945, youth, adults, elderly, women), ceiling),
    households = if_else(households > 0 & households < 1, 1, households),
    area_km2              = as.numeric(st_area(.)) / 1e6,
    population_density    = population / area_km2,
    youth_ratio           = if_else(population > 0, round(youth   / population * 100, 2), 0),
    elderly_ratio         = if_else(population > 0, round(elderly / population * 100, 2), 0),
    youth_dependency      = if_else(adults > 0,     round(youth   / adults     * 100, 2), 0),
    elderly_dependency    = if_else(adults > 0,     round(elderly / adults     * 100, 2), 0),
    women_percentage      = if_else(population > 0, round(women   / population * 100, 2), 0),
    pp_hh_avg             = if_else(households >= 3, round(population / households, 2), NA_real_),
    buildings_pre1945_pct = if_else(buildings > 0,
                              round(buildings_pre1945 / buildings * 100, 2), 0)
  ) |>
  mutate(
    youth_dependency   = if_else(is.nan(youth_dependency)   | is.infinite(youth_dependency),   0,   youth_dependency),
    elderly_dependency = if_else(is.nan(elderly_dependency) | is.infinite(elderly_dependency), 100, elderly_dependency),
    pp_hh_avg          = if_else(pp_hh_avg > 50, NA_real_, pp_hh_avg)
  ) |>
  st_transform(4326)


# 13. Validation ---------------------------------------------------------

bgri_total <- sum(census_bgri$N_INDIVIDUOS, na.rm = TRUE)
grid_total <- sum(grid_final$population,    na.rm = TRUE)
diff_abs   <- bgri_total - grid_total
diff_pct   <- 100 * diff_abs / bgri_total

message("--- Population totals check ---")
message(sprintf("BGRI census total:     %s", format(bgri_total,        big.mark = ",")))
message(sprintf("Grid population total: %s", format(round(grid_total), big.mark = ",")))
message(sprintf("Difference (BGRI-grid): %s  (%.2f%%)",
  format(round(diff_abs), big.mark = ","), diff_pct))

if (diff_pct > 2) warning(sprintf(
  "Population loss (%.2f%%) exceeds 2%% — check weight_source breakdown and coverage.",
  diff_pct
))

if (interactive()) {
  mapview(grid_final |> filter(population > 0), zcol = "population")
  mapview(grid_final |> filter(population > 0), zcol = "weight_source")
}


# 14. Export -------------------------------------------------------------

out_dir <- "data/base/daysimetric"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_gpkg <- file.path(out_dir, sprintf("grid_dasymetric_combined%s.gpkg", run_suffix))
out_csv  <- file.path(out_dir, sprintf("grid_dasymetric_combined%s.csv",  run_suffix))

sf::st_write(grid_final, out_gpkg, delete_dsn = TRUE)
write.csv(sf::st_drop_geometry(grid_final), out_csv, row.names = FALSE)

message(sprintf("[done] Exported: %s  (+.csv)", out_gpkg))
message(sprintf("[done] Cells with population > 0: %d", sum(grid_final$population > 0, na.rm = TRUE)))
