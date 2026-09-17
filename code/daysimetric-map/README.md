# Dasymetric Population Map

Disaggregates BGRI 2021 census population to the team's shared H3 resolution 9 grid (`data/geo/grid.gpkg`) across the Lisbon Metropolitan Area (AML).

## Disaggregation Modes

The main pipeline in `03_dasymetric_combined.R` supports three configurable modes via the `DASYMETRIC_MODE` flag:

1. **`"combined"` (Default):**
   - **Methodology**: Hierarchical fallback weighting across each (H3 cell × BGRI block) fragment:
     1. *Building Volume (Priority 1 — best proxy)*: Where Global Building Atlas footprints fall inside residential COS parcels, fragment weight equals building volume (`footprint_m2` × `height`).
     2. *COS Land-Use Area (Priority 2 — fallback for atlas gaps)*: Where building volume is unavailable, weights fall back to residential COS area multiplied by class weights (vertical: 3.0×, horizontal: 1.0×, discontinuous: 0.6×, sparse: 0.3×).
     3. *BGRI Area (Priority 3 — last resort)*: Where neither is present, weights fall back to raw overlapping census block area to ensure no population is lost.

2. **`"cos_only"` (Night Model):**
   - **Methodology**: Uses strictly residential COS land-use class weights ($3.0\times, 1.0\times, 0.6\times, 0.3\times$). No fallback to bare area.
   - **Purpose**: Fast residential home-base / night model based purely on land-use classification.

3. **`"all_buildings"` (Dynamics Model):**
   - **Methodology**: Uses total building volume (`volume_m3`) across all buildings without clipping to residential COS. No fallback to bare area.
   - **Purpose**: Captures total physical built mass and urban activity dynamics across all built structures.


## Pipeline Scripts

| Script | Purpose | Status |
|---|---|---|
| `01c_cos_residential_prep.R` | Prepares dissolved AML boundary and residential COS polygons (`cos_residential_aml.gpkg`). | Pre-computed on server |
| `02b_buildings_volume_prep.R` | Queries Global Building Atlas LOD1 via DuckDB S3 HTTPFS and calculates building footprint volume (`lisbon_metro_buildings_footprints.geojson`). | Pre-computed on server |
| `03_dasymetric_combined.R` | **Main pipeline:** Parameterized dasymetric disaggregation supporting all three modes (`"combined"`, `"cos_only"`, `"all_buildings"`). | Run as needed |
| `diagnose_zero_volume_bgri.R` | Standalone diagnostic script to inspect BGRI blocks with zero building volume. | Diagnostic only |

## Running the Pipeline

To run the pipeline:

```r
# From the repository root:
source("code/daysimetric-map/03_dasymetric_combined.R")
```

OR 

```bash 
$ Rscript code/daysimetric-map/03_dasymetric_combined.R
```

### Configuration & Options (at top of `03_dasymetric_combined.R`)
- **`DASYMETRIC_MODE`**: Set to `"combined"` (default), `"cos_only"`, or `"all_buildings"`.
- **`TEST_MODE <- TRUE`**: Restricts processing to a single municipality (default: `"Oeiras"`) for quick verification.
- **`N_WORKERS`**: Number of parallel workers for `furrr` (default: 4).
- **Checkpoints**: Spatial intersections are chunked and cached locally under `checkpoints/dasymetric_<mode>/` (ignored by git). Interrupted runs resume automatically.

## Output

Outputs are saved under `data/base/daysimetric/`:
- `grid_dasymetric_<mode>[_test_<municipio>].gpkg` (spatial polygons)
- `grid_dasymetric_<mode>[_test_<municipio>].csv` (tabular attributes)

*(e.g., `grid_dasymetric_combined.gpkg`, `grid_dasymetric_cos_only.gpkg`, `grid_dasymetric_all_buildings.gpkg`)*

Attributes per H3 grid cell include:
- `population`, `population_density`, `households`, `buildings`, `buildings_pre1945`
- Demographics: `youth` (0–14), `adults` (15–64), `elderly` (65+), `women`
- Derived indicators: `youth_ratio`, `elderly_ratio`, `youth_dependency`, `elderly_dependency`, `women_percentage`, `pp_hh_avg`
- `weight_source`: identifies whether `building_volume`, `cos_area`, or `bare_area` was used for each cell.
