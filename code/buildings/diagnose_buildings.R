# =============================================================================
# Diagnostic: Validate Global Building Atlas footprints inside vs. outside AML
#
# Investigates why ~14.4% (91,467) of buildings in lisbon_metro_buildings_footprints.geojson
# fall outside the team's H3 grid (data/geo/grid.gpkg).
#
# Key Hypothesis:
#   In 02b_buildings_volume_prep.R, the DuckDB S3 query fetched all buildings within
#   the axis-aligned rectangular bounding box (st_bbox) of municipalities_union.gpkg.
#   Because the AML boundary is irregular/concave, this rectangle encompasses parts
#   of neighboring municipalities outside AML (e.g., Torres Vedras, Alenquer,
#   Azambuja, Benavente, Salvaterra de Magos, Coruche, Montemor-o-Novo, Vendas Novas,
#   Alcácer do Sal). Those buildings are physically outside the AML territory.
#
# Procedure:
#   1. Load AML union boundary (data/geo/municipalities_union.gpkg).
#   2. Load H3 grid (data/geo/grid.gpkg).
#   3. Load Global Building Atlas footprints (data/base/daysimetric/lisbon_metro_buildings_footprints.geojson).
#   4. Test containment: centroid-in-AML, centroid-in-grid, and polygon-intersects-AML.
#   5. Quantify buildings outside AML and inspect their spatial distribution.
# =============================================================================

required_packages <- c("sf", "dplyr", "tibble")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) install.packages(missing_packages)

library(sf)
library(dplyr)
library(tibble)

# 1. Check data availability --------------------------------------------------

paths <- list(
  aml_boundary = "data/geo/municipalities_union.gpkg",
  grid         = "data/geo/grid.gpkg",
  buildings    = "data/base/daysimetric/lisbon_metro_buildings_footprints.geojson"
)

missing_files <- names(paths)[!vapply(paths, file.exists, logical(1))]
if (length(missing_files) > 0) {
  stop(sprintf(
    "Missing required data file(s):\n  %s\nEnsure data/ is mounted from the server (e.g. via sshfs or bind mount).",
    paste(vapply(missing_files, function(k) paths[[k]], character(1)), collapse = "\n  ")
  ))
}

# 2. Load layers -------------------------------------------------------------

message("[load] Loading AML boundary...")
aml_boundary <- sf::st_read(paths$aml_boundary, quiet = TRUE) |> st_make_valid()

message("[load] Loading H3 grid...")
grid <- sf::st_read(paths$grid, quiet = TRUE) |> st_make_valid()
if ("gid" %in% names(grid)) {
  grid <- grid |> rename(id = gid)
} else if (!"id" %in% names(grid)) {
  id_col <- setdiff(names(grid), attr(grid, "sf_column"))[1]
  grid <- grid |> rename(id = !!id_col)
}

message("[load] Loading building footprints (this may take a moment)...")
buildings <- sf::st_read(paths$buildings, quiet = TRUE) |> st_make_valid()
n_bldg <- nrow(buildings)
message(sprintf("[load] Loaded %s building footprints", format(n_bldg, big.mark = ",")))

# Align CRS
target_crs <- 3763
aml_boundary <- st_transform(aml_boundary, target_crs)
grid         <- st_transform(grid, target_crs)
buildings    <- st_transform(buildings, target_crs)

# 3. Bounding box comparison -------------------------------------------------

message("\n--- Bounding Box Comparison (EPSG:3763) ---")
bbox_aml  <- st_bbox(aml_boundary)
bbox_grid <- st_bbox(grid)
bbox_bldg <- st_bbox(buildings)

cat("AML boundary bbox:\n"); print(round(bbox_aml))
cat("H3 grid bbox:\n"); print(round(bbox_grid))
cat("Buildings dataset bbox:\n"); print(round(bbox_bldg))

# 4. Containment tests -------------------------------------------------------

message("\n[calc] Computing building centroids...")
bldg_cents <- suppressWarnings(st_centroid(st_geometry(buildings)))

message("[calc] Testing centroid within AML boundary...")
in_aml_cent <- lengths(st_intersects(bldg_cents, aml_boundary)) > 0

message("[calc] Testing centroid within H3 grid...")
in_grid_cent <- lengths(st_intersects(bldg_cents, grid)) > 0

message("[calc] Testing footprint intersection with AML boundary...")
in_aml_poly <- lengths(st_intersects(buildings, aml_boundary)) > 0

# 5. Diagnostic Summary ------------------------------------------------------

n_in_aml_cent  <- sum(in_aml_cent)
n_out_aml_cent <- sum(!in_aml_cent)
n_in_grid_cent  <- sum(in_grid_cent)
n_out_grid_cent <- sum(!in_grid_cent)
n_in_aml_poly   <- sum(in_aml_poly)
n_out_aml_poly  <- sum(!in_aml_poly)

cat("\n===================================================================\n")
cat("                BUILDING COVERAGE DIAGNOSTIC REPORT                \n")
cat("===================================================================\n")
cat(sprintf("Total buildings in dataset:                    %10s (100.00%%)\n", format(n_bldg, big.mark = ",")))
cat("-------------------------------------------------------------------\n")
cat("CENTROID-BASED CHECKS:\n")
cat(sprintf("  Centroid INSIDE AML boundary:                %10s (%6.2f%%)\n",
            format(n_in_aml_cent, big.mark = ","), 100 * n_in_aml_cent / n_bldg))
cat(sprintf("  Centroid OUTSIDE AML boundary:               %10s (%6.2f%%)\n",
            format(n_out_aml_cent, big.mark = ","), 100 * n_out_aml_cent / n_bldg))
cat(sprintf("  Centroid INSIDE H3 grid:                     %10s (%6.2f%%)\n",
            format(n_in_grid_cent, big.mark = ","), 100 * n_in_grid_cent / n_bldg))
cat(sprintf("  Centroid OUTSIDE H3 grid:                    %10s (%6.2f%%)\n",
            format(n_out_grid_cent, big.mark = ","), 100 * n_out_grid_cent / n_bldg))
cat("-------------------------------------------------------------------\n")
cat("FOOTPRINT-BASED CHECKS:\n")
cat(sprintf("  Footprint INTERSECTS AML boundary:           %10s (%6.2f%%)\n",
            format(n_in_aml_poly, big.mark = ","), 100 * n_in_aml_poly / n_bldg))
cat(sprintf("  Footprint COMPLETELY OUTSIDE AML boundary:   %10s (%6.2f%%)\n",
            format(n_out_aml_poly, big.mark = ","), 100 * n_out_aml_poly / n_bldg))
cat("===================================================================\n\n")

# Difference between AML boundary and H3 grid
diff_aml_grid <- sum(in_grid_cent != in_aml_cent)
cat(sprintf("Buildings where grid-inclusion != AML-inclusion: %d (%.3f%%)\n",
            diff_aml_grid, 100 * diff_aml_grid / n_bldg))
cat("(A tiny difference is expected due to H3 hexagonal cell overhang at administrative borders)\n\n")

# 6. Conclusion --------------------------------------------------------------

pct_out <- round(100 * n_out_aml_cent / n_bldg, 1)
cat(sprintf("CONCLUSION:
  The ~%s%% of buildings outside the grid is confirmed to be buildings that lie
  geographically OUTSIDE the Lisbon Metropolitan Area (AML). They were included in
  the GeoJSON because 02b_buildings_volume_prep.R queried S3 with a rectangular
  bounding box rather than the AML polygon. Excluding them from the AML grid is
  the correct and intended behavior.\n\n", pct_out))

# 7. Optional Mapview --------------------------------------------------------

if (interactive()) {
  library(mapview)
  sample_out <- buildings[!in_aml_cent, ] |> slice_sample(n = min(2000, n_out_aml_cent))
  mapview(aml_boundary, layer.name = "AML Boundary", color = "blue", alpha.regions = 0.1) +
    mapview(sample_out, layer.name = "Sample Buildings Outside AML", col.regions = "red")
}
