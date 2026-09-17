# Dasymetric Population Map

Disaggregates BGRI 2021 census population to the team's shared H3 resolution 9 grid (`data/geo/grid.gpkg`) across the Lisbon Metropolitan Area (AML).

## Methodology

The pipeline uses **hierarchical fallback weighting** for each (H3 cell × BGRI block) fragment:

1. **Building Volume (Priority 1 — best proxy):** Where Global Building Atlas footprints fall inside residential COS parcels, fragment weight equals building volume (footprint area × height).
2. **COS Land-Use Area (Priority 2 — fallback for atlas gaps):** Where building volume is unavailable, weights fall back to residential COS area multiplied by class weights (vertical: 3.0×, horizontal: 1.0×, discontinuous: 0.6×, sparse: 0.3×).
3. **BGRI Area (Priority 3 — last-resort fallback):** Where neither is present, weights fall back to raw overlapping census block area to ensure no population is dropped.

## Pipeline Scripts

| Script | Purpose | Status |
|---|---|---|
| `01c_cos_residential_prep.R` | Prepares dissolved AML boundary and residential COS polygons (`cos_residential_aml.gpkg`). | Pre-computed on server |
| `02b_buildings_volume_prep.R` | Queries Global Building Atlas LOD1 via DuckDB S3 HTTPFS and calculates building footprint volume (`lisbon_metro_buildings_footprints.geojson`). | Pre-computed on server |
| `03_dasymetric_combined.R` | **Main pipeline:** Runs parallel intersections, applies hierarchical weighting, and distributes population and demographic attributes onto H3 grid cells. | Run as needed |
| `diagnose_zero_volume_bgri.R` | Standalone diagnostic script to inspect BGRI blocks with zero building volume. | Diagnostic only |

## Running the Pipeline

To run the main dasymetric pipeline:

```r
# From the repository root:
source("code/daysimetric-map/03_dasymetric_combined.R")
```

### Configuration & Options
- **`TEST_MODE <- TRUE`**: In `03_dasymetric_combined.R`, restricts processing to a single municipality (default: `"Oeiras"`) for quick verification.
- **`N_WORKERS`**: Number of parallel workers for `furrr` (default: 4).
- **Checkpoints**: Spatial intersections are chunked and cached locally under `checkpoints/dasymetric_combined/` (ignored by git). Interrupted runs resume automatically.

## Output

Outputs are saved under `data/base/daysimetric/`:
- `grid_dasymetric_combined.gpkg` (spatial polygons)
- `grid_dasymetric_combined.csv` (tabular attributes)

Attributes per H3 grid cell include:
- `population`, `population_density`, `households`, `buildings`, `buildings_pre1945`
- Demographics: `youth` (0–14), `adults` (15–64), `elderly` (65+), `women`
- Derived indicators: `youth_ratio`, `elderly_ratio`, `youth_dependency`, `elderly_dependency`, `women_percentage`, `pp_hh_avg`
- `weight_source`: identifies whether `building_volume`, `cos_area`, or `bare_area` was used for each cell.
