# Changes to `code/daysimetric-map/` — handoff notes

## Summary

The scripts in `code/daysimetric-map/` were using `impt_read()`/`impt_write()` from
`00a_impt_data_handle_external.R`, a helper borrowed from the IMPT-data project.
That wrapper is not needed here: this project's data access convention is plain
`sf::st_read()` / `sf::st_write()` on paths relative to `./data/`, which is
bind-mounted (server) or sshfs-mounted (locally). All scripts in the folder have
been updated to use that convention consistently.

---

## Data access — no change to the convention, just made explicit

Per the project README, all data lives at `/data/biclar-parking/` on `ushift@alfa`
and is accessed by mounting it as `./data/` inside the repo:

**On the server:**
```bash
cd ~/Repos/biclar-parking
mount --bind /data/biclar-parking ./data
```

**Locally (via Tailscale + sshfs):**
```bash
cd ~/Repos/biclar-parking
mkdir -p data
sshfs username@ushift-alfa:/data/biclar-parking ./data
```

Once mounted, all scripts read and write using plain relative paths, e.g.:

```r
# Before (IMPT wrapper — removed)
x <- impt_read("/base/daysimetric/BGRI21_170.gpkg")
impt_write(x, "/base/daysimetric/output.gpkg")

# After (plain R — what all scripts now use)
x <- sf::st_read("data/base/daysimetric/BGRI21_170.gpkg", quiet = TRUE)
sf::st_write(x, "data/base/daysimetric/output.gpkg", delete_dsn = TRUE)
```

---

## Files removed

| File | Reason |
|---|---|
| `code/daysimetric-map/00a_impt_data_handle_external.R` | IMPT-project helper — not needed in this repo |
| `code/daysimetric-map/01_data_prep_grid_only.R` | Grid is already available via `data/geo/grid.gpkg` (loaded in `00_data_load.R`); instructions for generating it are in `code/01_aggregation_levels.R` |

---

## Files updated

### `01c_cos_residential_prep.R`

- Added a **server-only / documentation** banner at the top — its outputs
  (`data/base/daysimetric/cos_residential_aml.gpkg` and `clipping_boundary.geojson`)
  already exist; no need to re-run unless source data changes.
- Replaced all `impt_read()` / `impt_write()` with plain equivalents:

```r
# reads
caop_glps       <- sf::st_read("data/geo/CAOP_GLPS.gpkg",       quiet = TRUE)
cos_raw         <- sf::st_read("data/geo/COS_GLPS_LAND.gpkg",   quiet = TRUE)

# write
sf::st_write(cos_residential_aml,
             "data/base/daysimetric/cos_residential_aml.gpkg",
             delete_dsn = TRUE)
```

### `02b_buildings_volume_prep.R`

- Added a **server-only / documentation** banner — its output
  (`data/base/daysimetric/lisbon_metro_buildings_footprints.geojson`) already exists.
- Replaced `impt_read()` / `impt_write()`:

```r
# read
aml_boundary <- sf::st_read("data/geo/municipalities_union.gpkg", quiet = TRUE)

# write
sf::st_write(aml_buildings_footprints,
             "data/base/daysimetric/lisbon_metro_buildings_footprints.geojson",
             delete_dsn = TRUE)
```

---

## New file: `03_dasymetric_combined.R`

A new dasymetric script that combines both the COS-class-area approach
(from the IMPT project's `02_census_grid_with_cos.R`) and the building-volume approach
(from `02c_dasymetric_population.R`) using a **hierarchical fallback**:

1. Where building volume > 0 → use building volume as weight *(best proxy)*
2. Where building volume = 0 but COS residential area > 0 → fall back to COS area × class weight
3. Where neither → use raw BGRI-area overlap *(last resort — nothing is dropped)*

This eliminates the ~8.8% population loss in `02c` caused by atlas coverage gaps,
while still capturing building density variation where the atlas is complete.

Output columns match `02_census_grid_with_cos.R` (population, age groups, sex,
households, buildings) — not just `total_pop` like `02c`.

Uses plain R I/O throughout — no `impt_*` anywhere.

Output is registered in `code/99_results_load.R`:
```r
grid_dasymetric     <- sf::st_read("data/base/daysimetric/grid_dasymetric_combined.gpkg", quiet = TRUE)
grid_dasymetric_csv <- read.csv("data/base/daysimetric/grid_dasymetric_combined.csv")
```

---

## Parallelism and Checkpoint Management in `03_dasymetric_combined.R`

### Multi-Core Parallel Processing (`future` + `furrr`)

`03_dasymetric_combined.R` leverages `future` / `furrr` multisession workers
(`plan(multisession, workers = N_WORKERS)`) to parallelise the heavy spatial intersection steps:
1. Grid filtering across BGRI blocks.
2. Clipping building footprints to the residential COS mask (chunked by 50k buildings).
3. Computing building-volume fragments across H3 grid cells and BGRI blocks.

`N_WORKERS` defaults to `8` (adjustable according to available CPU cores and RAM).
Calls use `.options = furrr_options(seed = NULL)` to avoid false-positive RNG warnings from `furrr`.

### Local Checkpoints (Fixing SSHFS Permissions)

When working locally, the mounted `./data/` directory over SSHFS can be read-only or restricted, causing parallel workers to fail with *Permission denied* if attempting to create subfolders inside `data/base/daysimetric/`.

To resolve this:
- All intermediate `.Rds` chunk checkpoints are saved locally to `checkpoints/dasymetric_combined/` (inside the local repository root, excluded via `.gitignore`).
- The directory is created by the main session before parallel dispatch using an absolute path.
- If a run is cancelled or interrupted, re-running the script will detect the existing chunk `.Rds` files and skip already-processed chunks immediately.
- Once the final output is produced, the local `checkpoints/` directory can be safely removed if desired.

---

## Run order (dasymetric pipeline)

> Steps 1 and 2 only need to be re-run if the source files change.
> Their outputs already exist on the server.

```
1. code/daysimetric-map/01c_cos_residential_prep.R   → data/base/daysimetric/cos_residential_aml.gpkg
2. code/daysimetric-map/02b_buildings_volume_prep.R  → data/base/daysimetric/lisbon_metro_buildings_footprints.geojson
3. code/daysimetric-map/03_dasymetric_combined.R     → data/base/daysimetric/grid_dasymetric_combined.gpkg / .csv
```

To run step 3 locally (after mounting `./data/`):

```r
# Set working directory to the repo root, then:
source("code/daysimetric-map/03_dasymetric_combined.R")
```

For a quick sanity check on a single municipality before a full AML run,
set `TEST_MODE <- TRUE` at the top of the script.
