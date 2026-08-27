# Daysimetric Map - Dasymetric Residential Population Pipeline

This directory contains the pipeline that builds a building-volume-weighted dasymetric
residential population map: BGRI (2021 census) population counts, reported at the level of
large, irregularly-shaped census blocks, are redistributed onto the team's shared H3 grid in
proportion to how much residential building volume sits in each cell - rather than assuming
population is spread evenly across each census block's area.

The redistribution weight is residential building volume (footprint area x height), computed
from the Global Building Atlas and clipped to COS-classified residential land - so a block
that's mostly park with one dense apartment cluster gets its population concentrated there,
not smeared across the whole block.

## Setup

Source the data-handling helper once at the start of every session, before running anything
else below:

```r
source("code/00a_impt_data_handle_external.R")
```

This defines `impt_read()`/`impt_write()`, which every script here uses instead of base R's
read/write functions to resolve logical paths (e.g. `/base/daysimetric/foo.gpkg`) against the
actual server data layout.

## Script run order

1. **`01c_cos_residential_prep.R`** - filters COS (land-use classification) to residential
   categories and clips to the AML study area, producing the residential-zone mask the
   dasymetric weighting is based on.
2. **`02b_buildings_volume_prep.R`** - fetches building footprint polygons and heights for
   the whole AML from the Global Building Atlas, computing each building's volume
   (footprint area x height).
3. **`02c_dasymetric_population.R`** - the actual dasymetric disaggregation: clips buildings
   to residential COS zones, intersects against the grid and BGRI census blocks, and
   redistributes each block's population across its H3 cells in proportion to matched
   building volume. Internally sources `code/00_data_load.R` (the team's canonical
   grid/parishes/municipalities loader) to get the H3 grid this project's own output is
   built on - **this pipeline does not generate its own grid**.

Each script can be run independently once its own inputs exist - 01c and 02b don't depend on
each other and can run in either order, but both must run before 02c.

### Other files in this folder

- **`00a_impt_data_handle_external.R`** - the data-handling helper, see Setup above.
- **`02_jobs_imob.R`** - **not part of this pipeline.** Jittered job-location output for a
  separate future work item (activity/demand modeling), kept here for later use. Has its own
  unrelated data dependencies (IMOB survey, OSM road network) not covered by anything above.
- **`diagnose_zero_volume_bgri.R`** - standalone diagnostic script for investigating BGRI
  census blocks with zero matched residential building volume (see Known limitations below).
  Not part of a normal run - reuses `02c_dasymetric_population.R`'s fragment checkpoints
  rather than recomputing anything.

## Data locations

Everything this pipeline reads or writes lives under `data/base/daysimetric/` - including
BGRI census data (`BGRI21_170.gpkg`), all outputs of 01c and 02b
(`cos_residential_aml.gpkg`, `clipping_boundary.geojson`,
`lisbon_metro_buildings_footprints.geojson`), the residential-clip/grid-filter/fragment
checkpoints, and the final population layer.

The exceptions are shared team layers that live at `data/geo/` instead, since they're
maintained independently of this pipeline: `CAOP_GLPS.gpkg` and `COS_GLPS_LAND.gpkg` (raw
inputs to 01c), `municipalities_union.gpkg` (the AML boundary 02b uses to bound its buildings
fetch), and the grid/parishes/municipalities geometries loaded via `code/00_data_load.R`.

## Output

`data/base/daysimetric/grid_dasymetric_population.gpkg` and the equivalent `.csv` (attributes
only, no geometry) - one row per H3 grid cell, with a `total_pop` column giving that cell's
share of the AML's residential population.

## Validation

`02c_dasymetric_population.R` has a `TEST_MODE` flag that restricts the whole pipeline to one
municipality for fast sanity-checking before committing to a full AML run. Validated against
Oeiras' real 2021 census population: **~1% agreement** between the dasymetric total and the
real figure.

## Known limitations

A full-AML run currently undercounts population by **~8.8%** relative to the real BGRI census
total (2,870,208 vs. ~2,618,227). This is well above the ~1% Oeiras validation figure, and
traced to gaps in building-footprint coverage - some BGRI census blocks, concentrated in
**Amadora** and **Vila Franca de Xira**, have zero matched residential building volume even
where they sit in residential-zoned COS land, so their population currently isn't
distributed anywhere in the output. See `diagnose_zero_volume_bgri.R` for the investigation.
The buildings-fetch boundary has been checked and ruled out as the cause -
`municipalities_union.gpkg` and BGRI's actual extent are near-identical. The leading
explanation is a precision/boundary-alignment difference between `COS_GLPS_LAND.gpkg` and
the raw DGT COS file used in an earlier local-only version of this pipeline, not yet
confirmed at the individual-building level.
