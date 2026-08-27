# Dasymetric residential population map - building-volume-weighted
# Adapted from the flat COS-class-weight logic in 02_census_grid_with_cos.R (lines 39-193,
# removed from this project during cleanup - see git history), replacing
# "frag_area * class_weight" with building volume (height * footprint_m2), clipped to
# residential COS zones, prorated by each fragment's share of the original building's
# footprint area.
#
# The problem this script solves, in plain terms: BGRI (Base Geográfica de Referenciação de
# Informação) is INE's official Portuguese census geography - it reports population counts
# per census block, but those blocks are large, irregularly shaped administrative polygons,
# not the H3 hexagon grid this project needs population on. "Dasymetric mapping" is the
# technique of redistributing a coarse count (a BGRI block's total population) onto a finer
# grid using some other, spatially-detailed variable as a proxy for where within that block
# people actually are - here, that proxy is residential building volume: a BGRI block's
# population is split across its constituent H3 cells in proportion to how much residential
# building volume sits in each cell, rather than assuming population is spread evenly across
# the whole block's area (which would badly misrepresent e.g. a block that's mostly park with
# a small dense apartment cluster in one corner).
#
# Script flow, top to bottom: (1) load the four input layers (buildings, residential COS
# mask, H3 grid, BGRI census); (2) clip buildings to residential-zoned land only; (3)
# reproject everything to a metric CRS; (4) intersect grid cells against buildings, then
# against BGRI blocks, to produce "fragments" - see the comment above process_fragment_chunk
# below for exactly what a fragment is; (5) normalize each fragment's share of its parent
# BGRI block's building volume; (6) distribute that block's population across its fragments
# accordingly and sum up to the grid-cell level; (7) export. Steps 2 and 4 are the
# computationally expensive ones (hundreds of thousands of buildings/grid cells/census
# blocks all being spatially intersected against each other), which is why most of this
# script's complexity is chunking/parallelizing/checkpointing those two steps rather than the
# underlying dasymetric logic itself, which is comparatively simple.
#
# IMPORTANT - unverified assumption, please check before trusting results:
# This script assumes BGRI21_170.gpkg has columns "N_INDIVIDUOS" (population) and
# "BGRI2021" (unique census-block id). These names come from their consistent use in
# 01_data_prep.R, 02_census_grid_with_cos.R, and 04_mobility_transit_population.R - but
# none of us have actually opened the raw file's attribute table to confirm this directly.
# A names(census_bgri) sanity check is included below (step 0) - stop and adjust the field
# names throughout this script if it doesn't show what's expected.
#
# SERVER VARIANT - adapted to run on the shared team server (ushift@alfa), replacing this
# project's own locally-generated grid with the team's canonical one:
# - grid/parishes/municipalities are now loaded via source("code/00_data_load.R") instead of
#   this project's own 01_data_prep_grid_only.R output - that script is no longer a
#   dependency of this one at all. The loaded `grid` object's own id column is called
#   `h3_address` (it also carries a separate `gid`, per the server's naming convention - see
#   00_data_load.R's README) - renamed to `id` immediately after loading so none of the
#   downstream logic (which joins/groups by `id` everywhere) needs to change.
# - This server grid is a DIFFERENT, COARSER resolution than the r10 grid this script and
#   its TEST_MODE validation were built against - 25,380 cells here vs ~182,000 before. See
#   the loud warning at the TEST_MODE flag below: the ~1% Oeiras validation from the old
#   grid does NOT carry over to this one and must be re-run before trusting a full run.
# - BGRI, buildings-footprints, and cos_residential_aml now come from data/base/daysimetric/
#   instead of data/original/, data/pois/, and data/geo/ respectively - matching where
#   01c_cos_residential_prep.R and 02b_buildings_volume_prep.R now write their outputs on
#   this server, and where this script's own outputs (checkpoints, final population layer)
#   are written too.

required_packages <- c("sf", "dplyr", "tidyr", "tibble", "future", "furrr", "mapview")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) install.packages(missing_packages)

library(sf)
library(dplyr)
library(tidyr)
library(tibble)
library(future)
library(furrr)
library(mapview) # loaded by code/00_data_load.R when sourced below - listed explicitly
# here too so the install-guard above catches a missing install before that source() call
# fails partway through.

# PSOCK cluster via future's "multisession" plan - "multicore" (fork-based) isn't available
# on Windows, so multisession is the correct choice here. Dropped from 6 to 2 workers on the
# original 16GB Windows machine after an out-of-memory crash ("cannot allocate vector of
# size 11.7 GB") - each worker holds a full copy of cos_residential_mask/grid_active/
# census_simple once exported, so N concurrent workers meant N copies of those layers in
# memory at once, on top of the main process's own copy.
#
# Briefly bumped to 4 on this shared server (8 cores, ~23GB available), reverted back to 2
# after a full-AML run silently died (no error, no checkpoints) right at the first
# future_map2() dispatch in the script - the point where N fresh worker processes get spun
# up and cos_residential_mask/the first batch get shipped to each, i.e. exactly where memory
# pressure would spike hardest. A hard OS-level kill (e.g. the OOM-killer) wouldn't leave an
# R-level error to diagnose, and this server doesn't allow checking dmesg/journalctl to
# confirm it either way - so rather than guess at some value between 2 and 4, reverting to
# 2 is the safe choice: it's the one value already confirmed to run reliably, just on a
# machine with less RAM than this one, meaning it should have at least as much headroom here.
# future_map2() below only ever runs as many chunks concurrently as N_WORKERS allows - it
# does NOT dispatch all chunks at once regardless of worker count, so this number alone is
# the "process N at a time" behavior, no separate batching mechanism needed. Released back
# to sequential at the end of the script.
N_WORKERS <- 2
plan(multisession, workers = N_WORKERS)

target_crs <- 3763 # Portugal metric CRS, matching 02_census_grid_with_cos.R convention

# --- TEST MODE -----------------------------------------------------------
# Set TEST_MODE <- TRUE to sanity-check total_pop sums roughly match BGRI population totals
# fast, on a single municipality, before committing to a full run. While TRUE,
# grid/buildings/BGRI/COS are all subset to TEST_MUNICIPIO first.
#
# REVALIDATION RECORD: the server's canonical grid (25,380 cells, loaded via
# source("code/00_data_load.R")) is a different, coarser resolution than this project's own
# r10 H3 grid (~182,000 cells) the original ~1% Oeiras validation was run against - a
# different cell size changes how buildings/BGRI blocks get cut into fragments, so that old
# validation could not be assumed to carry over automatically. Re-run and confirmed: Oeiras
# on this server grid also comes out to ~1% total_pop-vs-BGRI agreement, same order of
# accuracy as the original r10 validation. TEST_MODE restored to FALSE below accordingly -
# this is no longer a pending action.
TEST_MODE <- FALSE
TEST_MUNICIPIO <- "Oeiras"

# Full-run chunking: only used when TEST_MODE is FALSE. Buildings are processed in
# sequential batches (by row position, not by any spatial partition) so a very long run
# reports progress incrementally instead of going silent for however many hours the full
# AML intersection takes. See the "FULL RUN" section below for why chunking by building
# row-index, rather than by municipality/grid cell, avoids double-counting fragments near
# municipality borders.
CHUNK_SIZE <- 50000

# Each chunk's raw fragment output is checkpointed to disk as it completes, and skipped
# (reloaded instead of recomputed) if its checkpoint file already exists - so an interrupted
# run can resume from wherever it left off, rather than restarting from chunk 1. Resuming
# means re-running this whole script from the top: steps 1-3 (load/clean/reproject/COS-clip)
# always redo, but those are far cheaper than the grid x building x BGRI intersection - only
# that per-chunk loop actually gets skipped for chunks with an existing checkpoint.
CHECKPOINT_DIR <- "/base/daysimetric/checkpoints"

# CRITICAL FIX: checkpoint filenames must be TEST_MODE-aware. A TEST_MODE=TRUE run (e.g.
# Oeiras, ~469 grid cells) and a full-AML run (25,380 grid cells) can both produce exactly
# ONE chunk under CHUNK_SIZE=50000 - without a distinguishing suffix, both write/read the
# exact same filename (e.g. "grid_active_chunk_001.Rds"), so a full run can silently reuse a
# stale checkpoint computed against the wrong (TEST_MODE) grid instead of erroring or
# recomputing - this actually happened: a full run reused last night's 469-cell Oeiras
# grid_active checkpoint, producing suspiciously tiny downstream fragment outputs with no
# error at all. `run_suffix` is used in every checkpoint filename below (residential_chunk,
# grid_active_chunk, chunk) AND in the final export filename - one shared definition, used
# everywhere a filename needs to distinguish a TEST_MODE run from a full run.
run_suffix <- if (TEST_MODE) paste0("_test_", tolower(TEST_MUNICIPIO)) else ""

# tryCatch-based existence check, using impt_read itself (consistent with the rest of the
# pipeline's convention of never calling base file.exists()/readRDS() directly) - an error
# on read just means no checkpoint exists yet. Defined up here since it's now reused by both
# the residential-clip chunking below and the grid x buildings x BGRI chunking further down.
read_checkpoint <- function(path) tryCatch(impt_read(path), error = function(e) NULL)

# 1. Load ---------------------------------------------------------------

# SERVER VARIANT: grid/parishes/municipalities come from the team's canonical loader, not
# this project's own 01_data_prep_grid_only.R. source() runs that file's top-level code in
# the current environment, which defines `grid`, `parishes`, `municipalities`,
# `conversion_dicofre`, and `conversion_dicofre_weight` as global variables here - only
# `grid` (and `municipalities`, used below in TEST_MODE) are actually used by this script;
# the rest are simply along for the ride. Per that script's own header comment, it expects
# to be sourced with the R working directory at the repo root (it uses plain relative paths
# like "data/geo/grid.gpkg", not impt_read() - same working-directory assumption impt_read
# makes, just without the /data/ prefix indirection, so nothing extra to configure here).
source("code/00_data_load.R")

# The loaded grid's own cell-id column is called h3_address (it also carries `gid`, unused
# here) - renamed to `id` so every downstream join/group_by in this script (all written
# against a column literally named `id`) works unchanged.
grid <- grid |> rename(id = h3_address)

buildings <- impt_read("/base/daysimetric/lisbon_metro_buildings_footprints.geojson")
cos_residential_aml <- impt_read("/base/daysimetric/cos_residential_aml.gpkg")
census_bgri <- impt_read("/base/daysimetric/BGRI21_170.gpkg")

# PROGRESS INSTRUMENTATION - added after a full-AML run hung for 11+ hours with zero
# console output and zero checkpoint files between BGRI loading and the residential-clip
# loop. Every step between here and that loop now reports start/finish + elapsed time, so
# the next attempt shows exactly which step is slow/stuck instead of going dark. Leading
# suspects: st_make_valid() on the full 633k-building set (run twice - once here, once
# again after reprojection below) and st_union() dissolving the residential COS layer into
# one polygon, further down - both are single-threaded GEOS operations with no built-in
# progress reporting that can legitimately take a very long time on large polygon counts.

# Fix invalid geometries right at load time, before any spatial predicate (bracket
# subsetting, st_intersects, st_intersection) touches them. This is what surfaced the
# "Loop 0 is not valid: Edge 5 is degenerate (duplicate vertex)" error during TEST MODE
# subsetting - it can equally occur later in the real intersection steps for any of these
# layers, not just municipios, so all four get fixed here defensively.
message(sprintf("[prep] st_make_valid(buildings) starting (%d rows) - a prime suspect for the 11-hour hang", nrow(buildings)))
step_start <- Sys.time()
buildings <- st_make_valid(buildings)
message(sprintf("[prep] st_make_valid(buildings) done in %.1f min", as.numeric(difftime(Sys.time(), step_start, units = "mins"))))

message(sprintf("[prep] st_make_valid(cos_residential_aml) starting (%d rows)", nrow(cos_residential_aml)))
step_start <- Sys.time()
cos_residential_aml <- st_make_valid(cos_residential_aml)
message(sprintf("[prep] st_make_valid(cos_residential_aml) done in %.1f min", as.numeric(difftime(Sys.time(), step_start, units = "mins"))))

message(sprintf("[prep] st_make_valid(grid) starting (%d rows)", nrow(grid)))
step_start <- Sys.time()
grid <- st_make_valid(grid)
message(sprintf("[prep] st_make_valid(grid) done in %.1f min", as.numeric(difftime(Sys.time(), step_start, units = "mins"))))

message(sprintf("[prep] st_make_valid(census_bgri) starting (%d rows)", nrow(census_bgri)))
step_start <- Sys.time()
census_bgri <- st_make_valid(census_bgri)
message(sprintf("[prep] st_make_valid(census_bgri) done in %.1f min", as.numeric(difftime(Sys.time(), step_start, units = "mins"))))

# Step 0 - schema sanity check (see IMPORTANT note above)
names(census_bgri)
stopifnot(
  "N_INDIVIDUOS" %in% names(census_bgri),
  "BGRI2021" %in% names(census_bgri)
)

if (TEST_MODE) {
  # SERVER VARIANT: `municipalities` now comes from source("code/00_data_load.R") above,
  # not this project's own municipios_2024.gpkg (produced by 01_data_prep_grid_only.R,
  # which is no longer a dependency of this script at all). Confirmed: this layer only
  # carries mid/area_km2/geom - no human-readable name column, unlike this project's own
  # CAOP-derived layer which had one called "municipio". Names live separately, in
  # useful_data/municipalities.csv (mid, name, nid, area_km2) - joined in below by `mid`.
  # Still fails loudly (rather than silently producing an all-NA municipio column) if that
  # lookup table is ever missing, restructured, or doesn't actually contain TEST_MUNICIPIO -
  # same principle as the COS schema check in 01c_cos_residential_prep.R.
  if (!"municipio" %in% names(municipalities)) {
    municipio_lookup <- impt_read("municipalities.csv", root = "useful_data")

    if (!all(c("mid", "name") %in% names(municipio_lookup))) {
      stop(sprintf(
        paste(
          "useful_data/municipalities.csv is missing 'mid' and/or 'name'",
          "(columns found: %s). This is the lookup table `municipalities` (from",
          "00_data_load.R) relies on for human-readable names, since that layer itself",
          "only carries mid/area_km2/geom. Update this block if the lookup table's",
          "schema has changed.",
          sep = " "
        ),
        paste(names(municipio_lookup), collapse = ", ")
      ))
    }

    # Coerce both join keys to character defensively - an integer/character mismatch
    # between the sf layer's `mid` and the CSV's `mid` would otherwise fail the join
    # (or silently match nothing) without necessarily raising an obvious error.
    municipalities <- municipalities |>
      mutate(mid = as.character(mid)) |>
      left_join(
        municipio_lookup |> mutate(mid = as.character(mid)) |> select(mid, municipio = name),
        by = "mid"
      )

    if (!TEST_MUNICIPIO %in% municipalities$municipio) {
      stop(sprintf(
        paste(
          "TEST_MUNICIPIO '%s' not found among municipality names after joining",
          "useful_data/municipalities.csv onto `municipalities` (available names: %s).",
          "Check for a naming mismatch (accents, capitalization) before proceeding.",
          sep = " "
        ),
        TEST_MUNICIPIO, paste(sort(unique(municipalities$municipio)), collapse = ", ")
      ))
    }
  }
  municipios <- municipalities |> st_make_valid()
  test_boundary <- municipios |> filter(municipio == TEST_MUNICIPIO)
  stopifnot(nrow(test_boundary) == 1) # confirm TEST_MUNICIPIO matched exactly one polygon

  # CRS check - print each layer's CRS before subsetting, since st_intersects (used by the
  # bracket subsetting below) requires all inputs to share the same CRS. This is what
  # surfaced the "st_crs(x) == st_crs(y) is not TRUE" error - one of these five almost
  # certainly differs from the rest (BGRI is INE data, likely a different native CRS than
  # the CAOP-derived 4326 layers; nobody has verified this directly, same caveat as the
  # N_INDIVIDUOS/BGRI2021 field-name assumption above).
  message("CRS check before TEST MODE subsetting:")
  message("test_boundary (municipios): ", st_crs(test_boundary)$input)
  message("grid: ", st_crs(grid)$input)
  message("buildings: ", st_crs(buildings)$input)
  message("census_bgri: ", st_crs(census_bgri)$input)
  message("cos_residential_aml: ", st_crs(cos_residential_aml)$input)

  # Align every layer to test_boundary's CRS before subsetting, regardless of which one(s)
  # turned out to be mismatched above.
  grid <- st_transform(grid, st_crs(test_boundary))
  buildings <- st_transform(buildings, st_crs(test_boundary))
  census_bgri <- st_transform(census_bgri, st_crs(test_boundary))
  cos_residential_aml <- st_transform(cos_residential_aml, st_crs(test_boundary))

  # Bracket subsetting (st_intersects-based) on each layer - keeps whole features that touch
  # the municipality boundary, not clipped exactly to it. This means grid cells/buildings
  # just outside Oeiras but straddling its edge are still included, and census_bgri's total
  # population below reflects this same subsetting rule - so the sanity-check comparison
  # stays apples-to-apples even though it isn't a precise municipio-boundary clip.
  grid <- grid[test_boundary, ]
  buildings <- buildings[test_boundary, ]
  census_bgri <- census_bgri[test_boundary, ]
  cos_residential_aml <- cos_residential_aml[test_boundary, ]

  message(sprintf(
    "TEST MODE: subset to %s - %d grid cells, %d buildings, %d BGRI units",
    TEST_MUNICIPIO, nrow(grid), nrow(buildings), nrow(census_bgri)
  ))
}

# Avoid column collisions later: grid uses "id" for H3 cell id, so rename the buildings'
# own id/area/volume fields before any intersection.
buildings <- buildings |>
  rename(
    building_id = id,
    building_footprint_m2 = footprint_m2,
    building_volume_m3 = volume_m3
  )

# 3. Reproject everything to EPSG:3763 for the spatial math ------------------
# All four layers are currently in EPSG:4326 (plain lat/lon degrees), which is the standard
# storage format across this project's /geo/ files - but degrees aren't a metric unit, so
# area/distance calculations and spatial intersections against it are either wrong or slow.
# EPSG:3763 is a Portugal-specific projected (metric) CRS, so from here on all the area
# math (fragment_area below, footprint proration, etc.) is done in real meters. Everything
# gets converted back to 4326 before being written to disk at the very end, matching how
# every other geo output in this project is stored.

# Geometry validity was already fixed once, right after load above - a CRS reprojection is
# a coordinate-wise transform and doesn't reintroduce topology problems (self-intersections,
# duplicate vertices, etc.), so re-running st_make_valid() here on buildings/census_bgri was
# pure redundant work on the largest objects in the pipeline (633k buildings). REMOVED - not
# just for efficiency: the load-time pass is also the one TEST_MODE actually depends on, since
# TEST_MODE's bracket subsetting (further up, `buildings[test_boundary, ]` etc.) runs BEFORE
# this reprojection block - dropping the load-time pass instead and keeping only this one
# would have silently reintroduced the original "Loop 0 is not valid" bug for TEST_MODE runs.

message(sprintf("[prep] st_transform(buildings) starting (%d rows)", nrow(buildings)))
step_start <- Sys.time()
buildings <- st_transform(buildings, target_crs)
message(sprintf("[prep] st_transform(buildings) done in %.1f min", as.numeric(difftime(Sys.time(), step_start, units = "mins"))))

message(sprintf("[prep] st_transform(grid) starting (%d rows)", nrow(grid)))
step_start <- Sys.time()
grid <- st_transform(grid, target_crs)
message(sprintf("[prep] st_transform(grid) done in %.1f min", as.numeric(difftime(Sys.time(), step_start, units = "mins"))))

message(sprintf("[prep] st_transform(census_bgri) starting (%d rows)", nrow(census_bgri)))
step_start <- Sys.time()
census_bgri <- st_transform(census_bgri, target_crs)
message(sprintf("[prep] st_transform(census_bgri) done in %.1f min", as.numeric(difftime(Sys.time(), step_start, units = "mins"))))

# Residential COS zones, dissolved into a single geometry mask (no class weighting anymore,
# only used as a residential/non-residential spatial filter, so per-class attributes aren't
# needed - this also keeps the intersection below fast, since buildings_residential ends up
# with no extra COS columns to carry through).
message(sprintf("[prep] cos_residential_mask: transform + st_make_valid starting (%d rows)", nrow(cos_residential_aml)))
step_start <- Sys.time()
cos_residential_mask <- cos_residential_aml |>
  st_transform(target_crs) |>
  st_make_valid()
message(sprintf("[prep] cos_residential_mask: transform + st_make_valid done in %.1f min", as.numeric(difftime(Sys.time(), step_start, units = "mins"))))

message("[prep] cos_residential_mask: st_union() starting - dissolving the whole residential COS layer into one polygon, a prime suspect for the 11-hour hang")
step_start <- Sys.time()
cos_residential_mask <- cos_residential_mask |> st_union()
message(sprintf(
  "[prep] cos_residential_mask: st_union() done in %.1f min - result geometry type: %s",
  as.numeric(difftime(Sys.time(), step_start, units = "mins")),
  paste(unique(st_geometry_type(cos_residential_mask)), collapse = ", ")
))

# ROOT CAUSE FOUND: st_union() here was producing a GEOMETRYCOLLECTION, not a clean
# POLYGON/MULTIPOLYGON - this traces back to COS_GLPS_LAND.gpkg itself loading with mixed/
# ambiguous geometry (see the same fix applied at the source, right after loading that file,
# in 01c_cos_residential_prep.R). st_intersection() against a GEOMETRYCOLLECTION is known to
# be slower/less predictable in GEOS than against clean polygons - this is what was actually
# behind the residential-clip loop running far slower here than it did locally, not merely a
# "heavier COS data" difference. Extracting polygonal parts again here too (even though
# 01c should now prevent it upstream) since this script has no guarantee it's being run
# against a cos_residential_aml.gpkg regenerated with that fix - cheap insurance either way.
if (!all(st_geometry_type(cos_residential_mask) %in% c("POLYGON", "MULTIPOLYGON"))) {
  message("[prep] cos_residential_mask: non-polygon geometry after st_union() - extracting polygonal parts via st_collection_extract('POLYGON')")
  cos_residential_mask <- st_collection_extract(cos_residential_mask, "POLYGON")
}

message("[prep] cos_residential_mask: final st_make_valid() starting")
step_start <- Sys.time()
cos_residential_mask <- cos_residential_mask |> st_make_valid()
message(sprintf("[prep] cos_residential_mask: final st_make_valid() done in %.1f min - prep sequence complete, entering residential-clip loop next", as.numeric(difftime(Sys.time(), step_start, units = "mins"))))

# DIAGNOSTIC - complexity check on cos_residential_mask, to compare against whatever ran
# locally. st_union() collapses it to a single feature (nrow will always be 1 after this
# point), so the number of rows says nothing about complexity - vertex count and object
# size are what actually matter for how expensive st_intersection() against it will be.
# st_coordinates() returns one row per vertex across every ring/part of the geometry, so
# nrow() on that is the total vertex count - cheap relative to the operations already timed
# above, but on a genuinely enormous geometry this itself could take a moment.
# CONFIRMED: st_coordinates() does not work on a GEOMETRYCOLLECTION (this diagnostic would
# have errored before reaching here, on the original bug). No fix needed here specifically -
# resolves on its own now that cos_residential_mask is guaranteed POLYGON/MULTIPOLYGON by
# the st_collection_extract() step above, which runs before this point.
message(sprintf(
  "[prep] cos_residential_mask complexity: %d vertices, %s in memory",
  nrow(st_coordinates(cos_residential_mask)),
  format(object.size(cos_residential_mask), units = "auto")
))

# 2. Clip buildings to residential zones only --------------------------------
# Chunked and parallelized across N_WORKERS PSOCK workers - observed to run over an hour
# with zero progress output on the full 633k-building set when run as one single-threaded
# st_intersection() call. This step never touches BGRI, so there's no cross-chunk
# normalization dependency like the later loop has - each chunk's result is complete and
# independent, safe to compute in any order/in parallel, with no extra caveats.
#
# Buildings are pre-sliced into batches HERE, in the main session (cheap, non-spatial row
# indexing), and each batch is passed as an explicit argument to future_map2() - NOT left as
# a global variable for the worker function to close over. This matters: if `buildings`
# (633k rows) were referenced as a global instead, future would serialize and ship the ENTIRE
# table to every one of the 6 workers, regardless of which slice that worker actually needs.
# Passing pre-sliced batches means each worker only ever receives its own chunk.
# cos_residential_mask, CHECKPOINT_DIR, read_checkpoint, and impt_write ARE still exported as
# globals (auto-detected by future) - that's fine, they're small/cheap to copy per worker.

n_buildings_raw <- nrow(buildings)
n_residential_chunks <- if (TEST_MODE) 1 else ceiling(n_buildings_raw / CHUNK_SIZE)

if (TEST_MODE) {
  residential_batches <- list(buildings)
} else {
  residential_batches <- lapply(seq_len(n_residential_chunks), function(i) {
    row_start <- (i - 1) * CHUNK_SIZE + 1
    row_end <- min(i * CHUNK_SIZE, n_buildings_raw)
    buildings[row_start:row_end, ]
  })
}

# DIAGNOSTIC - added after a full-AML run silently returned to the console prompt right at
# this loop, with no error and zero checkpoints written. Confirms n_residential_chunks and
# residential_batches actually contain what's expected before dispatch - if n_buildings_raw
# were ever 0 (e.g. buildings failed to load/got emptied upstream), ceiling(0/CHUNK_SIZE) is
# 0, seq_len(0) is integer(0), and residential_batches would silently be an empty list -
# future_map2() over zero elements returns an empty list with no error and no iterations,
# which would exactly match "no checkpoints, no error, execution just continues/ends".
message(sprintf(
  "[prep] n_buildings_raw = %d, CHUNK_SIZE = %d, n_residential_chunks = %d, length(residential_batches) = %d",
  n_buildings_raw, CHUNK_SIZE, n_residential_chunks, length(residential_batches)
))
if (length(residential_batches) > 0) {
  message(sprintf(
    "[prep] residential_batches row counts: %s",
    paste(vapply(residential_batches, nrow, integer(1)), collapse = ", ")
  ))
} else {
  message("[prep] WARNING: residential_batches is empty - the loop below will do nothing.")
}

# One call to this function = one chunk's worth of work, run inside a worker process. Given
# a chunk index `i` (for naming its checkpoint file) and that chunk's slice of buildings, it
# either reloads a previously-completed result or computes fresh, always returning a small
# list (the result itself, plus bookkeeping used only for the progress messages printed
# after future_map2() below returns - a worker can't message() back to the console live).
process_residential_chunk <- function(i, buildings_batch) {
  library(sf)
  library(dplyr)

  checkpoint_path <- sprintf("%s/residential_chunk%s_%03d.Rds", CHECKPOINT_DIR, run_suffix, i)
  checkpoint <- read_checkpoint(checkpoint_path)

  if (!is.null(checkpoint)) {
    return(list(chunk = checkpoint, n_input = nrow(buildings_batch), elapsed_sec = 0, from_checkpoint = TRUE))
  }

  # DIAGNOSTIC - message() alone is NOT enough here: a worker's message() output only gets
  # relayed back to the main session's console when the future actually resolves (i.e. when
  # future_map2() finishes collecting it) - if you interrupt/kill the process before that,
  # whatever a worker "said" is lost with it, never having reached the console at all. So
  # this also writes directly to a log file - real disk I/O from this worker process,
  # independent of future's result-capture pipeline - meaning a "starting" line is safely on
  # disk the instant it's written, even if the process is killed a second later. After an
  # interrupt, check this chunk's log file: a "starting" line with no matching "done" line
  # pinpoints that this specific chunk's st_intersection() call is the one still running.
  log_path <- sprintf("data%s/residential_chunk%s_%03d_log.txt", CHECKPOINT_DIR, run_suffix, i)
  log_dir <- dirname(log_path)
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
  log_line <- function(msg) cat(sprintf("[%s] %s\n", format(Sys.time()), msg), file = log_path, append = TRUE)

  message(sprintf("[chunk %d] st_intersection() starting (%d buildings vs cos_residential_mask)", i, nrow(buildings_batch)))
  log_line(sprintf("chunk %d: st_intersection() starting (%d buildings vs cos_residential_mask)", i, nrow(buildings_batch)))

  batch_start_time <- Sys.time()
  # st_intersection naturally returns a zero-row (not NULL) sf object when nothing matches,
  # so no NULL-sentinel handling is needed here the way the fragments loop below needs one.
  result <- st_intersection(buildings_batch, cos_residential_mask)
  intersection_elapsed <- as.numeric(difftime(Sys.time(), batch_start_time, units = "mins"))

  message(sprintf("[chunk %d] st_intersection() done in %.1f min (%d result rows)", i, intersection_elapsed, nrow(result)))
  log_line(sprintf("chunk %d: st_intersection() done in %.1f min (%d result rows)", i, intersection_elapsed, nrow(result)))

  impt_write(result, checkpoint_path)

  out <- list(
    chunk = result,
    n_input = nrow(buildings_batch),
    elapsed_sec = as.numeric(difftime(Sys.time(), batch_start_time, units = "secs")),
    from_checkpoint = FALSE
  )

  # With N_WORKERS well below the total chunk count, each worker still processes several
  # chunks sequentially within this stage (not just once) - gc() here is a defensive addition
  # beyond what was asked, to keep a worker's own memory from creeping up across its several
  # assigned chunks, not only between the two stages (that part is the explicit gc() calls
  # after each future_map2() below, which run in the main process instead).
  rm(buildings_batch, result)
  gc()

  out
}

# DIAGNOSTIC - the tryCatch below surfaces any R-level error a worker throws (e.g. a bad
# geometry in st_intersection, a checkpoint write failure) with an explicit message instead
# of it getting lost in future's own error-wrapping. IMPORTANT LIMITATION: this can only
# catch genuine R errors - it cannot catch or explain a hard process kill (e.g. the OS's
# OOM-killer sending SIGKILL if spinning up N_WORKERS fresh worker processes and shipping
# cos_residential_mask/each batch to them pushes memory over the edge right at this point,
# which is a very plausible cause for "silently returns to the shell prompt, no R error at
# all" specifically - this was in fact seen once already with N_WORKERS=4, before it was
# reverted to 2). The "[prep] dispatching..." message below at least confirms the
# process was still alive immediately before the call - if that prints but "returned" never
# does on the next attempt, that's evidence for a kill during this call, not an R error.
message(sprintf(
  "[prep] dispatching %d residential-clip chunk(s) to %d workers now...",
  n_residential_chunks, N_WORKERS
))
residential_wall_start <- Sys.time()
residential_results <- tryCatch(
  future_map2(
    seq_len(n_residential_chunks), residential_batches,
    process_residential_chunk
  ),
  error = function(e) {
    message("[prep] future_map2() raised an error during the residential-clip stage:")
    message(conditionMessage(e))
    stop(e) # re-throw - halt here with a clear message rather than silently continuing
  }
)
message("[prep] future_map2() for residential-clip returned successfully")
residential_wall_elapsed <- round(difftime(Sys.time(), residential_wall_start, units = "mins"), 1)

# Reported in order after the parallel batch completes - can't stream progress live across
# separate worker processes the way a single-threaded loop's message() calls could.
for (i in seq_along(residential_results)) {
  r <- residential_results[[i]]
  if (isTRUE(r$from_checkpoint)) {
    message(sprintf(
      "[residential clip %d/%d] checkpoint found, skipping (%d buildings)",
      i, n_residential_chunks, nrow(r$chunk)
    ))
  } else {
    message(sprintf(
      "[residential clip %d/%d] %d buildings -> %d residential fragments, %.1fs",
      i, n_residential_chunks, r$n_input, nrow(r$chunk), r$elapsed_sec
    ))
  }
}
message(sprintf("Residential clip: %s min wall time across %d workers", residential_wall_elapsed, N_WORKERS))

buildings_residential <- bind_rows(lapply(residential_results, `[[`, "chunk"))

# Explicit gc() between stages, as requested - drop everything from stage 1 that the
# fragment stage below doesn't need (raw buildings, per-chunk batches/results, the COS mask)
# before forcing garbage collection in the main process, rather than letting it accumulate
# through the second future_map2() call too.
rm(residential_results, residential_batches, buildings, cos_residential_aml, cos_residential_mask)
gc()

# Filter grid to cells that actually intersect BGRI ---------------------------
# Purpose: the H3 grid covers the entire AML boundary, including areas with no census data
# at all relevance (sea, large uninhabited stretches) - restricting to only the cells that
# overlap at least one BGRI polygon avoids wasting the expensive building/BGRI intersection
# work later on cells that could never end up with any population anyway.
# Chunked and checkpointed the same way as the residential-clip stage above. The actual
# out-of-memory cause here was `sparse = FALSE`, which builds one dense ~182k x 26k logical
# matrix (4.7+ billion cells) for the whole grid x BGRI comparison in one shot - chunking
# alone, with sparse = FALSE still inside each chunk, would just move the same OOM to a
# smaller (but still potentially huge) matrix depending on chunk size. Switched to
# sparse = TRUE (the default - returns per-row match-index lists instead of a dense matrix)
# together with chunking, since the dense-matrix approach doesn't scale regardless of how
# small the chunks are. No correctness risk from chunking here either - this is a pure
# spatial filter (keep cells that intersect ANY BGRI polygon), not an aggregation.

n_grid_raw <- nrow(grid)
n_grid_chunks <- if (TEST_MODE) 1 else ceiling(n_grid_raw / CHUNK_SIZE)

if (TEST_MODE) {
  grid_filter_batches <- list(grid)
} else {
  grid_filter_batches <- lapply(seq_len(n_grid_chunks), function(i) {
    row_start <- (i - 1) * CHUNK_SIZE + 1
    row_end <- min(i * CHUNK_SIZE, n_grid_raw)
    grid[row_start:row_end, ]
  })
}

# Same one-chunk-per-call worker pattern as process_residential_chunk above: given a slice
# of grid cells, keep only the ones that intersect at least one BGRI polygon.
process_grid_filter_chunk <- function(i, grid_batch) {
  library(sf)

  checkpoint_path <- sprintf("%s/grid_active_chunk%s_%03d.Rds", CHECKPOINT_DIR, run_suffix, i)
  checkpoint <- read_checkpoint(checkpoint_path)

  if (!is.null(checkpoint)) {
    return(list(chunk = checkpoint, n_input = nrow(grid_batch), elapsed_sec = 0, from_checkpoint = TRUE))
  }

  batch_start_time <- Sys.time()
  matches <- st_intersects(grid_batch, census_bgri, sparse = TRUE)
  result <- grid_batch[lengths(matches) > 0, ]
  impt_write(result, checkpoint_path)

  out <- list(
    chunk = result,
    n_input = nrow(grid_batch),
    elapsed_sec = as.numeric(difftime(Sys.time(), batch_start_time, units = "secs")),
    from_checkpoint = FALSE
  )

  rm(grid_batch, matches, result)
  gc()

  out
}

grid_filter_wall_start <- Sys.time()
grid_filter_results <- future_map2(
  seq_len(n_grid_chunks), grid_filter_batches,
  process_grid_filter_chunk
)
grid_filter_wall_elapsed <- round(difftime(Sys.time(), grid_filter_wall_start, units = "mins"), 1)

for (i in seq_along(grid_filter_results)) {
  r <- grid_filter_results[[i]]
  if (isTRUE(r$from_checkpoint)) {
    message(sprintf(
      "[grid filter %d/%d] checkpoint found, skipping (%d active cells)",
      i, n_grid_chunks, nrow(r$chunk)
    ))
  } else {
    message(sprintf(
      "[grid filter %d/%d] %d cells -> %d active, %.1fs",
      i, n_grid_chunks, r$n_input, nrow(r$chunk), r$elapsed_sec
    ))
  }
}
message(sprintf("Grid-BGRI filter: %s min wall time across %d workers", grid_filter_wall_elapsed, N_WORKERS))

grid_active <- bind_rows(lapply(grid_filter_results, `[[`, "chunk"))

# census_bgri is NOT removed here - still needed for census_simple right below, and for the
# bgri_total sanity check at the very end of the script.
rm(grid_filter_results, grid_filter_batches)
gc()

# Keep only the BGRI columns we actually need before the expensive intersections below.
# st_cast("POLYGON") splits any BGRI feature stored as a MULTIPOLYGON (a single row
# representing several disconnected pieces of land, which happens for parishes/blocks with
# exclaves) into one row per individual polygon piece - st_intersection() further down
# expects single-part geometries to behave predictably, and this also means a
# multi-part BGRI block's population gets correctly attributed piece-by-piece rather than
# implicitly as one lump covering a shape that isn't actually contiguous.
census_simple <- census_bgri |>
  select(BGRI2021, N_INDIVIDUOS) |>
  st_cast("POLYGON") |>
  st_make_valid()

# 4-7. Grid x buildings x BGRI intersection, prorating, and BGRI-level normalization --------
#
# FULL RUN chunking + parallelization rationale: buildings are split into batches (by row
# position, CHUNK_SIZE each) and run across N_WORKERS PSOCK workers. Each batch produces raw,
# UNNORMALIZED fragments (id, BGRI2021, frag_volume) - the BGRI-level normalization
# (total_bgri_frag_volume, pop_fraction) is deliberately NOT done per batch, and instead
# happens once at the end across all batches combined (after future_map2() returns and
# results are merged). This matters because a single BGRI census block's total matched
# building volume must be summed across the WHOLE AML before dividing by it - if we
# normalized per-batch, any BGRI block whose buildings/cells happened to fall across two
# batches (possible if INE's BGRI boundaries and DGT's building/COS layers don't perfectly
# align at the pixel level, even where they're administratively the same unit) would get an
# incomplete, wrong denominator. Splitting by building row-index (not by municipality or
# grid cell) avoids any partition/double-counting risk entirely - each building is trivially
# in exactly one batch, so this holds regardless of running sequentially or in parallel.
#
# As with the residential-clip stage above, buildings_residential is pre-sliced into batches
# in the main session and passed as an explicit future_map2() argument - not left as a global
# for workers to close over - so each worker only receives its own slice, not the full table.
# grid_active/census_simple/empty_fragments ARE exported as globals (each worker needs the
# whole of these, same tradeoff as cos_residential_mask in the stage above).
#
# TEST_MODE keeps a single-batch, non-chunked run (already validated on Oeiras), since a
# one-municipality subset is small and fast enough that parallelizing adds nothing.

n_buildings <- nrow(buildings_residential)
n_chunks <- if (TEST_MODE) 1 else ceiling(n_buildings / CHUNK_SIZE)

# Zero-row sentinel for "no fragments in this chunk" - deliberately NOT a literal NULL:
# `all_fragments[[i]] <- NULL` would delete that list slot and shift later elements down
# (a real R gotcha). Column types are taken from the real data (zero-row slices) so
# bind_rows() later doesn't hit a type mismatch against non-empty chunks.
empty_fragments <- tibble(
  id = grid_active$id[0],
  BGRI2021 = census_simple$BGRI2021[0],
  frag_volume = numeric(0)
)

if (TEST_MODE) {
  fragment_batches <- list(buildings_residential)
} else {
  fragment_batches <- lapply(seq_len(n_chunks), function(i) {
    row_start <- (i - 1) * CHUNK_SIZE + 1
    row_end <- min(i * CHUNK_SIZE, n_buildings)
    buildings_residential[row_start:row_end, ]
  })
}

# This is the heart of the dasymetric logic. Same one-chunk-per-call worker pattern as the
# two functions above, but doing two intersections in sequence rather than one:
#   1. st_intersection(grid_active, buildings_batch) - chops each building's footprint at
#      every H3 cell boundary it crosses, so a building straddling two cells becomes two
#      separate geometry pieces, one per cell.
#   2. st_intersection(..., census_simple) - chops each of THOSE pieces again at every BGRI
#      block boundary it crosses.
# A "fragment" (the term used throughout this script and its surrounding comments) is what
# survives both cuts: a piece of one specific building that sits in exactly one H3 cell AND
# one BGRI block simultaneously. fragment_area is that fragment's own area (which is smaller
# than the whole building's footprint whenever a building got cut by either intersection),
# and frag_volume scales the building's total volume down to just this fragment's share,
# based on what fraction of the building's total footprint area this fragment represents
# (see the comment further below, at the fragments_with_pop step, for what happens to
# frag_volume next).
process_fragment_chunk <- function(i, buildings_batch) {
  library(sf)
  library(dplyr)

  checkpoint_path <- sprintf("%s/chunk%s_%03d.Rds", CHECKPOINT_DIR, run_suffix, i)
  checkpoint <- read_checkpoint(checkpoint_path)

  if (!is.null(checkpoint)) {
    return(list(chunk = checkpoint, n_input = nrow(buildings_batch), elapsed_sec = 0, from_checkpoint = TRUE))
  }

  batch_start_time <- Sys.time()
  grid_buildings_batch <- st_intersection(grid_active, buildings_batch)

  if (nrow(grid_buildings_batch) == 0) {
    result <- empty_fragments
  } else {
    fragments_batch <- st_intersection(grid_buildings_batch, census_simple)

    if (nrow(fragments_batch) == 0) {
      result <- empty_fragments
    } else {
      # fragment_area/building_footprint_m2 is a fraction between 0 and 1 (1 only if this
      # fragment IS the whole, uncut building) - multiplying building_volume_m3 by that
      # fraction gives this fragment's proportional share of the building's total volume.
      fragments_batch$fragment_area <- as.numeric(st_area(fragments_batch))
      result <- fragments_batch |>
        st_drop_geometry() |>
        mutate(frag_volume = building_volume_m3 * (fragment_area / building_footprint_m2)) |>
        select(id, BGRI2021, frag_volume)
    }
  }

  impt_write(result, checkpoint_path)

  out <- list(
    chunk = result,
    n_input = nrow(buildings_batch),
    elapsed_sec = as.numeric(difftime(Sys.time(), batch_start_time, units = "secs")),
    from_checkpoint = FALSE
  )

  # Same defensive per-chunk gc() as the residential-clip worker above - each worker
  # processes several chunks sequentially within this stage.
  rm(buildings_batch, grid_buildings_batch, result)
  gc()

  out
}

fragment_wall_start <- Sys.time()
fragment_results <- future_map2(
  seq_len(n_chunks), fragment_batches,
  process_fragment_chunk
)
fragment_wall_elapsed <- round(difftime(Sys.time(), fragment_wall_start, units = "mins"), 1)

for (i in seq_along(fragment_results)) {
  r <- fragment_results[[i]]
  if (isTRUE(r$from_checkpoint)) {
    message(sprintf(
      "[chunk %d/%d] checkpoint found, skipping (%d fragments)",
      i, n_chunks, nrow(r$chunk)
    ))
  } else {
    message(sprintf(
      "[chunk %d/%d] %d buildings -> %d fragments, %.1fs",
      i, n_chunks, r$n_input, nrow(r$chunk), r$elapsed_sec
    ))
  }
}
message(sprintf("Fragment intersection: %s min wall time across %d workers", fragment_wall_elapsed, N_WORKERS))

all_fragments_combined <- bind_rows(lapply(fragment_results, `[[`, "chunk"))

# Explicit gc() after this stage too, as requested - drop the large intermediate objects
# this stage no longer needs before continuing. NOTE: census_simple is NOT removed here -
# it's still needed for the left_join() a few lines below (in the normalization step).
rm(fragment_results, fragment_batches, buildings_residential, grid_active, empty_fragments)
gc()

# 6. Prorating already applied per-fragment above (frag_volume); combined here across chunks.
# 7. GLOBAL BGRI normalization - across ALL chunks combined, not per-chunk (see note above).
#
# This is where population actually gets distributed. For every BGRI block, its fragments'
# frag_volume values are summed (total_bgri_frag_volume) - this is "how much residential
# building volume did we find in this census block, in total, across every H3 cell it
# touches". pop_fraction is then each individual fragment's share of that block-wide total,
# and fragment_pop = N_INDIVIDUOS (the block's real, known census population) x pop_fraction
# - i.e. each fragment gets a slice of its block's population exactly proportional to its
# slice of that block's building volume. Blocks with zero matched residential volume
# (filter(total_bgri_frag_volume > 0)) are dropped rather than divided by zero - their
# population isn't distributed anywhere, which is part of why the totals check further below
# won't exactly match the raw BGRI sum.
fragments_with_pop <- all_fragments_combined |>
  group_by(BGRI2021) |>
  mutate(total_bgri_frag_volume = sum(frag_volume, na.rm = TRUE)) |>
  ungroup() |>
  filter(total_bgri_frag_volume > 0) |> # ignore BGRI units with no residential building volume
  left_join(st_drop_geometry(census_simple), by = "BGRI2021") |>
  mutate(
    pop_fraction = frag_volume / total_bgri_frag_volume,
    fragment_pop = N_INDIVIDUOS * pop_fraction
  )

# 8. Aggregate to grid cell
# A single H3 cell can hold fragments from many different buildings and even from more than
# one BGRI block (if the cell sits on a block boundary) - this collapses all of a cell's
# fragment_pop values down to the one total_pop number that ends up in the final map.
grid_population <- fragments_with_pop |>
  group_by(id) |>
  summarise(total_pop = sum(fragment_pop, na.rm = TRUE), .groups = "drop")

# Both parallel stages are done - release the PSOCK worker processes back to sequential.
plan(sequential)

# Rejoin to clean H3 hexagon geometries (not the fragment geometries) and back to 4326.
# Deliberately joins onto the FULL `grid` (every H3 cell in the AML boundary), not
# `grid_active` (which was already removed by rm() above anyway) - so cells that never
# matched any BGRI polygon still appear in the final output with total_pop = 0, rather than
# being silently absent from the map altogether.
grid_final <- grid |>
  select(id) |>
  left_join(grid_population, by = "id") |>
  mutate(total_pop = replace_na(total_pop, 0)) |>
  st_transform(4326)

# Check - compare to raw BGRI total, same sanity check pattern as 02_census_grid_with_cos.R
# Using message()/explicit print() rather than bare expressions, since this script may be
# run non-interactively (source()), where bare expressions don't auto-print.
bgri_total <- sum(census_bgri$N_INDIVIDUOS, na.rm = TRUE)
grid_total <- sum(grid_final$total_pop, na.rm = TRUE)
diff_total <- bgri_total - grid_total

message("--- Population totals check ---")
message(sprintf("BGRI census total (census_bgri$N_INDIVIDUOS):  %s", format(bgri_total, big.mark = ",")))
message(sprintf("Dasymetric grid total (grid_final$total_pop):  %s", format(round(grid_total), big.mark = ",")))
message(sprintf("Difference (BGRI - grid):                      %s (%.2f%%)", format(round(diff_total), big.mark = ","), 100 * diff_total / bgri_total))
# difference reflects: non-residential-COS building volume (excluded by design), BGRI blocks
# with buildings but zero matched volume, and normal edge/rounding loss from intersections

# 9. Export ---------------------------------------------------------------
# Test-mode output goes to a distinct path so it can never be confused with, or accidentally
# overwrite, a full-AML run's output. SERVER VARIANT: written under /base/daysimetric/ (not
# /landuse/) - matching where every other derived output in this workflow now lives.
# Reuses `run_suffix` (defined once, near CHECKPOINT_DIR) rather than redefining the same
# TEST_MODE/TEST_MUNICIPIO logic a second time here - this was previously its own separate
# `output_suffix` variable with identical logic, which is exactly the kind of duplication
# that let the checkpoint filenames drift out of sync with this one in the first place.

impt_write(grid_final, sprintf("/base/daysimetric/grid_dasymetric_population%s.gpkg", run_suffix))
impt_write(grid_final |> st_drop_geometry(), sprintf("/base/daysimetric/grid_dasymetric_population%s.csv", run_suffix))
