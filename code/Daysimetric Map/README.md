# IMPT Data Processing Pipeline Flow

This directory contains the R scripts used for processing data, generating indicators, and aggregating the final scores for the Índice Multimodal de Pobreza nos Transportes (IMPT).

The scripts are naturally organized in a sequential data processing pipeline. To clarify their precedents and dependencies, their filenames are prefixed with a number. Below is the general flow and what each script achieves.

## General Flow & Suggested Numbering

### Phase 0: Initialization and Pre-processing

These scripts set up the environment and prepare base spatial or demographic datasets.

- **`00a_impt_data_handle.R`**: Helper script that sets up method to access and manage data.
- **`00b_data_load.R`**: Helper script that sets up useful global variables that are used across the entire pipeline.
- **`01_data_prep.R`**: Core geometry preparation script. It handles administrative boundaries, road network downloading and filtering, processes OSM point of interest (POI) layers, parses GTFS transit routes, sets up the origins and destinations (via jittering), and prepares the building footprints.

### Phase 2: Socio-Demographics and Land Use

These scripts rely on base geometries to extract and calculate land use and census indicators. They can be executed in any order relative to each other.

- **`02_pois_at_scales.R`**: Computes point of interest (POI) densities and availabilities scaled at different geographic units.
- **`02_jobs_imob.R`**: Calculates job and employment data mobility patterns, relying on the IMOB (inquérito à mobilidade).
- **`02_land_use_export.R`**: Spatially aggregates housing, income, population, density, and demographic statistics to the grid, freguesia, and municipality levels.
- **`02_census_modalShare.R`**: Processes and cleans basic modal share data from Census 2021 responses.
- **`02_veh_ownership.R`**: Extracts and aggregates vehicle ownership averages at different geographic scales.

### Phase 3: Routing and Time/Distance Matrices

- **`03_ttm_gridh3.R`**: Uses the `r5r` package (Rapid Realistic Routing on Real-world networks) engine configured earlier to compute the travel travel time matrices (TTM) and basic distances for multiple transport modes (car, PT, walk, bike) across origins to destinations.

### Phase 4: Dimension Calculations

These scripts parse outputs from Phase 2 and 3 to compute the exact indicators for the 4 core IMPT dimensions (Accessibility, Mobility, Affordability, Safety).

- **`04_access_opportunities.R`**: Calculates general physical accessibility metrics by analyzing the ease of reaching POIs and opportunities.
- **`04_accessibility_gap.R`**: Evaluates accessibility gaps between distinct spatial zones along commuting constraints.
- **`04_mobility.R`**: Calculates infrastructural mobility statistics like pedestrian infrastructure ratios and the integration of generic shared mobility networks.
- **`04_mobility_bike_infrastructure.R`**: Classifies cycling infrastructure (e.g., segregated paths vs shared lanes) and computes cycling quality ratios.
- **`04_mobility_commuting.R`**: Focuses processing travel time arrays, trip generations, and modal availability primarily targeting peak commuting logic.
- **`04_isochrones_PTstops.R`**: Generates walking isochrones (catchment areas) around public transit stops using the r5r engine, differentiated by transport mode.
- **`04_mobility_transit_population.R`**: Estimates the population served by public transit using a dasymetric intersection of COS residential buildings and the computed isochrones.
- **`04_mobility_transit.R`**: Aggregates complex public transit statistics (e.g., route frequencies and headways).
- **`04_mobility_costs_time.R`**: Values and calculates generalized time costs dedicated across variable modes.
- **`04_mobility_costs_money_car.R`**: Computes out-of-pocket financial expenditures (fuel, maintenance, etc.) expected for private commuting networks.
- **`04_mobility_costs_money_pt_cost_structure.r`**: Specific helpers mapping local fare policies for the entire transit grid.
- **`04_mobility_costs_money_pt.R`**: Evaluates monetary affordability strictly relating to PT use (e.g. single-fare vs overall transit passes).
- **`04_affordability.R`**: Pre-calculates raw and weighted affordability metrics by freguesia.
- **`04_safety.R`**: Computes road safety vulnerabilities by intersecting traffic crashes spatially, and grouping them against area-based population scales.

### Phase 5: Result Aggregation and Normalization

- **`05_IMPTcalculator.R`**: The core scoring engine. It loads the normalized dimension tables, scales and inverts them to percentages [0-100], calculates variance via Principal Component Analysis (PCA) and calculates weights through entropy scoring. It merges the output arrays to build the final IMPT general and sub-modal scores.

### Phase 6: Output Pipelines & Dashboarding

These scripts compile the calculated results strictly for system interaction and visualization tasks.

- **`06a_results_load.R`**: Utility script to load and verify all the generated variables/results files from Phase 4 explicitly into memory.
- **`06b_dashboard_export.R`**: The final integration point. It takes raw table indices joining them heavily with the visual geometries (grid cells, polygons). Formats columns precisely for the dashboard environment (web) and uses `piggyback` to deploy the unified `GeoJSON` blocks straight to the live environment tracking platform (`IMPT-web`).
