# =============================================================================
# Dasymetric residential population mapping (Parameterized Workflow)
#
# Run with: Rscript 03_dasymetric_combined.R --mode=cos_only
#
# Modes:
#   - "combined":      Building volume inside residential COS -> COS area fallback -> BGRI area fallback (default)
#   - "cos_only":      Residential COS land-use area weights only (no fallback)
#   - "all_buildings":  Total building volume across all buildings only (no fallback)
#
# Procedure:
#   1. Setup & Load: Read H3 r9 grid, municipalities, BGRI 2021 census, and conditionally
#      load residential COS and/or building footprints based on DASYMETRIC_MODE.
#   2. Weight Preparation:
#      - If "combined": Clip buildings to residential COS mask, compute fragment volume.
#      - If "all_buildings": Compute fragment volume across all buildings (no COS clip).
#      - If "cos_only": Compute weighted COS area.
#      - Bare BGRI area: Intersect grid x BGRI for raw area fallback (only in "combined" mode).
#   3. Weight Assignment: Combine or assign weights according to active DASYMETRIC_MODE.
#   4. Disaggregation: Prorate BGRI census counts across H3 cells.
#   5. Grid Join & Export: Aggregate to H3 cells, calculate demographic indicators,
#      validate against BGRI census totals, and export GPKG and CSV.
#
# Inputs (under data/, mounted from server):
#   - data/geo/grid.gpkg
#   - data/geo/municipalities.gpkg
#   - data/base/daysimetric/BGRI21_170.gpkg
#   - data/base/daysimetric/cos_residential_aml.gpkg (used in combined & cos_only)
#   - data/base/daysimetric/lisbon_metro_buildings_footprints.geojson (used in combined & all_buildings)
#
# Outputs:
#   - data/base/daysimetric/grid_dasymetric_<mode>[_test_<municipio>].gpkg / .csv
# =============================================================================

# 0. Setup -------------------------------------------------------------------

required_packages <- c("sf", "dplyr", "tidyr", "tibble", "future", "furrr", "mapview")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) install.packages(missing_packages)

library(sf)
library(dplyr)
library(tidyr)
library(tibble)
library(future)
library(furrr)
library(mapview)

# Run from repo root so relative paths resolve against ./data/
stopifnot(
  "Run from the repo root: setwd('/path/to/biclar-parking')" =
    file.exists("data") || dir.exists("data")
)

# --- Configuration flags ---

# DASYMETRIC_MODE: "combined" | "cos_only" | "all_buildings"
DASYMETRIC_MODE <- "combined"
stopifnot(DASYMETRIC_MODE %in% c("combined", "cos_only", "all_buildings"))

N_WORKERS <- 4
if (DASYMETRIC_MODE != "cos_only") {
  plan(multisession, workers = N_WORKERS)
}

target_crs <- 3763 # EPSG:3763 — Portugal metric CRS

# TEST_MODE: restrict to a single municipality for rapid testing
TEST_MODE <- FALSE
TEST_MUNICIPIO <- "Oeiras" # must match a 'municipio' name in useful_data/municipalities.csv

# Number of building rows processed per parallel chunk
CHUNK_SIZE <- 50000

# Suffix for test runs
run_suffix <- if (TEST_MODE) paste0("_test_", tolower(TEST_MUNICIPIO)) else ""


# 1. Load data ---------------------------------------------------------------

message("[load] Reading BGRI census blocks...")
census_bgri <- sf::st_read("data/base/daysimetric/BGRI21_170.gpkg", quiet = TRUE)

stopifnot(
  "N_INDIVIDUOS" %in% names(census_bgri),
  "BGRI2021" %in% names(census_bgri),
  "N_EDIFICIOS_CLASSICOS" %in% names(census_bgri),
  "N_EDIFICIOS_CONSTR_ANTES_1945" %in% names(census_bgri),
  "N_NUCLEOS_FAMILIARES" %in% names(census_bgri),
  "N_INDIVIDUOS_0_14" %in% names(census_bgri),
  "N_INDIVIDUOS_65_OU_MAIS" %in% names(census_bgri),
  "N_INDIVIDUOS_M" %in% names(census_bgri)
)
message(sprintf(
  "[load] BGRI: %d blocks, total N_INDIVIDUOS = %s",
  nrow(census_bgri), format(sum(census_bgri$N_INDIVIDUOS, na.rm = TRUE), big.mark = ",")
))
census_bgri <- st_make_valid(census_bgri)

# Load COS if needed by active mode
if (DASYMETRIC_MODE %in% c("combined", "cos_only")) {
  message("[load] Reading COS residential polygons...")
  cos_residential_aml <- sf::st_read("data/base/daysimetric/cos_residential_aml.gpkg", quiet = TRUE) |> st_make_valid()
  message(sprintf("[load] COS residential: %d polygons", nrow(cos_residential_aml)))
}

# Load buildings if needed by active mode
if (DASYMETRIC_MODE %in% c("combined", "all_buildings")) {
  message("[load] Reading Global Building Atlas footprints...")
  buildings <- sf::st_read("data/base/daysimetric/lisbon_metro_buildings_footprints.geojson", quiet = TRUE)
  message(sprintf("[load] Buildings: %d footprints", nrow(buildings)))

  buildings <- buildings |>
    rename(
      building_id           = id,
      building_footprint_m2 = footprint_m2,
      building_volume_m3    = volume_m3
    ) |>
    st_make_valid()
}
message("[prep] Data loaded and validated")


# 2. Load grid and municipalities -------------------------------------------

message("[grid] Loading H3 r9 grid from data/geo/grid.gpkg...")
grid <- sf::st_read("data/geo/grid.gpkg", quiet = TRUE) |> st_make_valid()

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

if (!"municipio" %in% names(municipalities_sf) && file.exists("useful_data/municipalities.csv")) {
  mun_lookup <- read.csv("useful_data/municipalities.csv", stringsAsFactors = FALSE)
  if (all(c("mid", "name") %in% names(mun_lookup))) {
    municipalities_sf <- municipalities_sf |>
      mutate(mid = as.character(mid)) |>
      left_join(mun_lookup |> mutate(mid = as.character(mid)) |> select(mid, municipio = name),
        by = "mid"
      )
  }
}


# 3. TEST MODE subsetting ---------------------------------------------------

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
  grid <- st_transform(grid, ref_crs)[test_boundary, ]
  census_bgri <- st_transform(census_bgri, ref_crs)[test_boundary, ]
  if (DASYMETRIC_MODE %in% c("combined", "cos_only")) {
    cos_residential_aml <- st_transform(cos_residential_aml, ref_crs)[test_boundary, ]
  }
  if (DASYMETRIC_MODE %in% c("combined", "all_buildings")) {
    buildings <- st_transform(buildings, ref_crs)[test_boundary, ]
  }

  message(sprintf(
    "[TEST MODE] Subset: %d grid cells, %d BGRI blocks",
    nrow(grid), nrow(census_bgri)
  ))
}


# 4. Reproject to EPSG:3763 for metric calculations ------------------------

message("[prep] Reprojecting to EPSG:3763...")
census_bgri <- st_transform(census_bgri, target_crs)
grid <- st_transform(grid, target_crs) |> st_make_valid()
if (DASYMETRIC_MODE %in% c("combined", "cos_only")) {
  cos_residential_aml <- st_transform(cos_residential_aml, target_crs)
}
if (DASYMETRIC_MODE %in% c("combined", "all_buildings")) {
  buildings <- st_transform(buildings, target_crs)
}


# 5. Prepare weight layers --------------------------------------------------

if (DASYMETRIC_MODE %in% c("combined", "cos_only")) {
  # COS class weights
  cos_weighted <- cos_residential_aml |>
    rename(classification = COS23_n4_L) |>
    mutate(cos_weight = case_when(
      classification == "Áreas edificadas residenciais contínuas predominantemente verticais" ~ 3.0,
      classification == "Áreas edificadas residenciais contínuas predominantemente horizontais" ~ 1.0,
      classification == "Áreas edificadas residenciais descontínuas" ~ 0.6,
      TRUE ~ 0.3
    )) |>
    select(classification, cos_weight)
}

if (DASYMETRIC_MODE == "combined") {
  # Dissolve COS into a single mask for clipping buildings
  message("[prep] Dissolving residential COS into a single mask (may take a few minutes)...")
  cos_residential_mask <- cos_residential_aml |>
    st_make_valid() |>
    st_union() |>
    st_make_valid()

  if (!all(st_geometry_type(cos_residential_mask) %in% c("POLYGON", "MULTIPOLYGON"))) {
    message("[prep] Non-polygon geometry from st_union() — extracting polygonal parts")
    cos_residential_mask <- st_collection_extract(cos_residential_mask, "POLYGON") |> st_make_valid()
  }
  message(sprintf(
    "[prep] COS mask ready: %d vertices",
    nrow(st_coordinates(cos_residential_mask))
  ))
}

# BGRI slim version for intersections
census_simple <- census_bgri |>
  select(
    BGRI2021, N_INDIVIDUOS,
    N_EDIFICIOS_CLASSICOS, N_EDIFICIOS_CONSTR_ANTES_1945,
    N_NUCLEOS_FAMILIARES,
    N_INDIVIDUOS_0_14, N_INDIVIDUOS_15_24, N_INDIVIDUOS_25_64,
    N_INDIVIDUOS_65_OU_MAIS, N_INDIVIDUOS_M
  ) |>
  st_cast("POLYGON") |>
  st_make_valid()


# 6. Filter grid to active cells (overlap BGRI) -----------------------------

message("[grid] Filtering to cells that overlap at least one BGRI block...")
n_grid_raw <- nrow(grid)
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
  t0 <- Sys.time()
  hits <- st_intersects(grid_batch, census_bgri, sparse = TRUE)
  result <- grid_batch[lengths(hits) > 0, ]
  message(sprintf(
    "[grid filter %d/%d] %d -> %d active cells (%.1fs)",
    i, n_grid_chunks, nrow(grid_batch), nrow(result),
    as.numeric(difftime(Sys.time(), t0, units = "secs"))
  ))
  result
}

if (DASYMETRIC_MODE != "cos_only") {
  grid_active <- bind_rows(future_map2(
    seq_len(n_grid_chunks), grid_filter_batches,
    run_grid_filter_chunk,
    .options = furrr_options(seed = NULL)
  ))
} else {
  grid_active <- bind_rows(lapply(seq_len(n_grid_chunks), function(i) {
    run_grid_filter_chunk(i, grid_filter_batches[[i]])
  }))
}
rm(grid_filter_batches)
gc()
message(sprintf("[grid] Active cells: %d / %d", nrow(grid_active), n_grid_raw))


# 7. Priority 1: Building-volume fragments -----------------------------------

if (DASYMETRIC_MODE %in% c("combined", "all_buildings")) {
  message(sprintf("[bldg] Processing building-volume weights (mode: %s)...", DASYMETRIC_MODE))

  if (DASYMETRIC_MODE == "combined") {
    # -- 7a: Residential clip --
    n_bldg_raw <- nrow(buildings)
    n_residential_chunks <- if (TEST_MODE) 1L else ceiling(n_bldg_raw / CHUNK_SIZE)

    residential_batches <- if (TEST_MODE) {
      list(buildings)
    } else {
      lapply(seq_len(n_residential_chunks), function(i) {
        buildings[((i - 1) * CHUNK_SIZE + 1):min(i * CHUNK_SIZE, n_bldg_raw), ]
      })
    }

    message(sprintf(
      "[bldg] Clipping %d buildings to COS mask (%d chunks, %d workers)...",
      n_bldg_raw, n_residential_chunks, N_WORKERS
    ))
    run_residential_chunk <- function(i, buildings_batch) {
      library(sf)
      t0 <- Sys.time()
      result <- st_intersection(buildings_batch, cos_residential_mask)
      message(sprintf(
        "[bldg clip %d/%d] %d -> %d residential fragments (%.1fs)",
        i, n_residential_chunks, nrow(buildings_batch), nrow(result),
        as.numeric(difftime(Sys.time(), t0, units = "secs"))
      ))
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
  } else {
    # All buildings mode: bypass COS clipping
    buildings_residential <- buildings
    rm(buildings)
    gc()
  }

  # -- 7b: Fragment intersection --
  empty_vol_frags <- tibble(
    id          = grid_active$id[0],
    BGRI2021    = census_simple$BGRI2021[0],
    frag_volume = numeric(0)
  )

  n_bldg_res <- nrow(buildings_residential)
  n_chunks <- if (TEST_MODE) 1L else ceiling(n_bldg_res / CHUNK_SIZE)

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
    message(sprintf(
      "[frags %d/%d] %d buildings -> %d fragments (%.1fs)",
      i, n_chunks, nrow(buildings_batch), nrow(result),
      as.numeric(difftime(Sys.time(), t0, units = "secs"))
    ))
    result
  }
  vol_frags_raw <- bind_rows(future_map2(
    seq_len(n_chunks), fragment_batches,
    run_fragment_chunk,
    .options = furrr_options(seed = NULL)
  ))
  message(sprintf(
    "[frags] Done in %.1f min",
    as.numeric(difftime(Sys.time(), t_wall, units = "mins"))
  ))

  # Aggregate volume per (id x BGRI2021)
  vol_weights <- vol_frags_raw |>
    group_by(id, BGRI2021) |>
    summarise(vol_weight = sum(frag_volume, na.rm = TRUE), .groups = "drop") |>
    filter(vol_weight > 0)

  message(sprintf("[frags] (id × BGRI) pairs with building volume > 0: %d", nrow(vol_weights)))
  rm(vol_frags_raw, fragment_batches, buildings_residential, empty_vol_frags)
  gc()
} else {
  vol_weights <- NULL
}


# 8. COS land-use area weights -----------------------------------------------

if (DASYMETRIC_MODE %in% c("combined", "cos_only")) {
  message("[cos] Intersecting grid × COS weighted × BGRI...")
  t0 <- Sys.time()
  grid_cos <- st_intersection(grid_active, cos_weighted)
  cos_bgri_frags_sf <- st_intersection(grid_cos, census_simple)
  cos_bgri_frags_sf$frag_area <- as.numeric(st_area(cos_bgri_frags_sf))
  cos_bgri_frags <- cos_bgri_frags_sf |>
    st_drop_geometry() |>
    mutate(cos_area_score = frag_area * cos_weight) |>
    group_by(id, BGRI2021) |>
    summarise(cos_weight_sum = sum(cos_area_score, na.rm = TRUE), .groups = "drop") |>
    filter(cos_weight_sum > 0)
  message(sprintf(
    "[cos] COS weight pairs: %d — %.1f min",
    nrow(cos_bgri_frags), as.numeric(difftime(Sys.time(), t0, units = "mins"))
  ))
  rm(grid_cos, cos_bgri_frags_sf, cos_weighted)
  gc()
} else {
  cos_bgri_frags <- NULL
}


# 9. Bare BGRI area fallback (combined mode only) ----------------------------

if (DASYMETRIC_MODE == "combined") {
  message("[fallback] Computing bare BGRI-area fallback fragments...")
  t0 <- Sys.time()
  bgri_area_frags_sf <- st_intersection(grid_active, census_simple)
  bgri_area_frags_sf$bare_area <- as.numeric(st_area(bgri_area_frags_sf))
  bgri_area_frags <- bgri_area_frags_sf |>
    st_drop_geometry() |>
    select(id, BGRI2021, bare_area) |>
    filter(bare_area > 0)
  message(sprintf(
    "[fallback] Bare-area pairs: %d — %.1f min",
    nrow(bgri_area_frags), as.numeric(difftime(Sys.time(), t0, units = "mins"))
  ))
  rm(bgri_area_frags_sf)
} else {
  bgri_area_frags <- NULL
}
rm(grid_active)
gc()


# 10. Combine / prepare weights ---------------------------------------------

message(sprintf("[weights] Preparing weights (mode: %s)...", DASYMETRIC_MODE))

if (DASYMETRIC_MODE == "combined") {
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
    )
} else if (DASYMETRIC_MODE == "cos_only") {
  # COS-only mode: strictly residential COS weights without fallback
  all_pairs <- cos_bgri_frags |>
    mutate(
      weight_source = "cos_area",
      weight        = cos_weight_sum
    )
} else if (DASYMETRIC_MODE == "all_buildings") {
  # All-buildings mode: strictly building volume without fallback
  all_pairs <- vol_weights |>
    mutate(
      weight_source = "building_volume",
      weight        = vol_weight
    )
}

all_pairs <- all_pairs |> filter(weight > 0)

weight_summary <- all_pairs |>
  group_by(weight_source) |>
  summarise(n_pairs = n(), .groups = "drop") |>
  mutate(pct = round(n_pairs / sum(n_pairs) * 100, 1))
message("[weights] Weight source breakdown:")
print(as.data.frame(weight_summary))

if (!is.null(vol_weights)) rm(vol_weights)
if (!is.null(cos_bgri_frags)) rm(cos_bgri_frags)
if (!is.null(bgri_area_frags)) rm(bgri_area_frags)
gc()


# 11. Distribute population across cells ------------------------------------

message("[pop] Distributing BGRI population across H3 cells...")

demo_cols <- census_bgri |>
  st_drop_geometry() |>
  select(
    BGRI2021, N_INDIVIDUOS, N_EDIFICIOS_CLASSICOS, N_EDIFICIOS_CONSTR_ANTES_1945,
    N_NUCLEOS_FAMILIARES, N_INDIVIDUOS_0_14, N_INDIVIDUOS_15_24,
    N_INDIVIDUOS_25_64, N_INDIVIDUOS_65_OU_MAIS, N_INDIVIDUOS_M
  ) |>
  distinct(BGRI2021, .keep_all = TRUE)

fragments_with_pop <- all_pairs |>
  group_by(BGRI2021) |>
  mutate(total_bgri_weight = sum(weight)) |>
  ungroup() |>
  filter(total_bgri_weight > 0) |>
  mutate(pop_fraction = weight / total_bgri_weight) |>
  left_join(demo_cols, by = "BGRI2021") |>
  mutate(
    fragment_pop             = N_INDIVIDUOS * pop_fraction,
    fragment_buildings       = N_EDIFICIOS_CLASSICOS * pop_fraction,
    fragment_buildings_pre45 = N_EDIFICIOS_CONSTR_ANTES_1945 * pop_fraction,
    fragment_households      = N_NUCLEOS_FAMILIARES * pop_fraction,
    fragment_youth           = N_INDIVIDUOS_0_14 * pop_fraction,
    fragment_adults          = (N_INDIVIDUOS_15_24 + N_INDIVIDUOS_25_64) * pop_fraction,
    fragment_elderly         = N_INDIVIDUOS_65_OU_MAIS * pop_fraction,
    fragment_women           = N_INDIVIDUOS_M * pop_fraction
  )

h3_pop_table <- fragments_with_pop |>
  group_by(id) |>
  summarise(
    population_raw = sum(fragment_pop, na.rm = TRUE),
    population = sum(fragment_pop, na.rm = TRUE),
    buildings = sum(fragment_buildings, na.rm = TRUE),
    buildings_pre1945 = sum(fragment_buildings_pre45, na.rm = TRUE),
    households = sum(fragment_households, na.rm = TRUE),
    youth = sum(fragment_youth, na.rm = TRUE),
    adults = sum(fragment_adults, na.rm = TRUE),
    elderly = sum(fragment_elderly, na.rm = TRUE),
    women = sum(fragment_women, na.rm = TRUE),
    # --- Debugging weights and sources ---
    weight_source = first(weight_source),
    weight_sources = paste(sort(unique(weight_source)), collapse = "+"),
    n_frags = n(),
    n_bgri = n_distinct(BGRI2021),
    weight_total = sum(weight, na.rm = TRUE),
    weight_vol_sum = sum(weight[weight_source == "building_volume"], na.rm = TRUE),
    weight_cos_sum = sum(weight[weight_source == "cos_area"], na.rm = TRUE),
    weight_area_sum = sum(weight[weight_source == "bare_area"], na.rm = TRUE),
    .groups = "drop"
  )

rm(fragments_with_pop, all_pairs, demo_cols, census_simple)
gc()


# 12. Join population table to H3 grid geometry -----------------------------

if (DASYMETRIC_MODE != "cos_only") {
  plan(sequential) # release parallel workers
}
message("[output] Joining population table to H3 grid geometry...")

grid$area_km2 <- as.numeric(st_area(grid)) / 1e6

grid_final <- grid |>
  left_join(h3_pop_table, by = "id") |>
  mutate(
    across(c(
      population, population_raw, buildings, buildings_pre1945, households,
      youth, adults, elderly, women, n_frags, n_bgri,
      weight_total, weight_vol_sum, weight_cos_sum, weight_area_sum
    ), ~ replace_na(.x, 0)),
    weight_sources = replace_na(weight_sources, "none"),
    across(c(population, buildings, buildings_pre1945, youth, adults, elderly, women), ceiling),
    households = if_else(households > 0 & households < 1, 1, households),
    population_density = population / area_km2,
    youth_ratio = if_else(population > 0, round(youth / population * 100, 2), 0),
    elderly_ratio = if_else(population > 0, round(elderly / population * 100, 2), 0),
    youth_dependency = if_else(adults > 0, round(youth / adults * 100, 2), 0),
    elderly_dependency = if_else(adults > 0, round(elderly / adults * 100, 2), 0),
    women_percentage = if_else(population > 0, round(women / population * 100, 2), 0),
    pp_hh_avg = if_else(households >= 3, round(population / households, 2), NA_real_),
    buildings_pre1945_percentage = if_else(buildings > 0, round(buildings_pre1945 / buildings * 100, 2), 0)
  ) |>
  mutate(
    youth_dependency   = if_else(is.nan(youth_dependency) | is.infinite(youth_dependency), 0, youth_dependency),
    elderly_dependency = if_else(is.nan(elderly_dependency) | is.infinite(elderly_dependency), 100, elderly_dependency),
    pp_hh_avg          = if_else(pp_hh_avg > 50, NA_real_, pp_hh_avg)
  ) |>
  st_transform(4326)


# 13. Validation ---------------------------------------------------------

# grid_final <- sf::st_read(sprintf("data/base/daysimetric/grid_dasymetric_combined%s.gpkg", run_suffix))
# census_bgri <- sf::st_read("data/base/daysimetric/BGRI21_170.gpkg")

bgri_total <- sum(census_bgri$N_INDIVIDUOS, na.rm = TRUE)
grid_total <- sum(grid_final$population, na.rm = TRUE)
diff_abs <- bgri_total - grid_total
diff_pct <- 100 * diff_abs / bgri_total

message("--- Population totals check ---")
message(sprintf("BGRI census total:     %s", format(bgri_total, big.mark = ",")))
message(sprintf("Grid population total: %s", format(round(grid_total), big.mark = ",")))
message(sprintf(
  "Difference (BGRI-grid): %s  (%.2f%%)",
  format(round(diff_abs), big.mark = ","), diff_pct
))

if (diff_pct > 2) {
  warning(sprintf(
    "Population loss (%.2f%%) exceeds 2%% — check weight_source breakdown and coverage.",
    diff_pct
  ))
}

if (interactive()) {
  mapview(grid_final |> filter(population > 0), zcol = "population")
  mapview(grid_final |> filter(population > 0), zcol = "weight_source")
}


# 14. Export ----------------------------------------------------------------

out_dir <- "data/base/daysimetric"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_base <- paste0("grid_dasymetric_", DASYMETRIC_MODE, run_suffix)
out_gpkg <- file.path(out_dir, paste0(out_base, ".gpkg"))
out_csv <- file.path(out_dir, paste0(out_base, ".csv"))

sf::st_write(grid_final, out_gpkg, delete_dsn = TRUE)
write.csv(sf::st_drop_geometry(grid_final), out_csv, row.names = FALSE)

message(sprintf("[done] Exported: %s  (+.csv)", out_gpkg))
message(sprintf("[done] Cells with population > 0: %d", sum(grid_final$population > 0, na.rm = TRUE)))
