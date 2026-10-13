source("code/00_data_load.R")
source("code/00_helpers.R")
source("code/99_results_load.R")


# SSH authentication ------------------------------------------------------
# When updated, data should be uploaded to the IST serve through SSH, to serve the web dashboard
# install.packages("ssh")
library(ssh)
session <- ssh_connect("ist1108284@sigma.ist.utl.pt")
print(session)

# Aggregate results ------------------------------------------------------

## Grid ------------------------------------------------------
grid_dashboard <- grid |>
  # Metadata
  left_join(grid_meta) |>
  # Raw data 
  # > Dasymetric map 
  left_join(
    grid_dasymetric |> select(
      gid, 
      # Raw
      population, adults, youth, elderly, women, households,
      buildings, buildings_pre1945,
      # Composed
      ratio_youth = youth_ratio, ratio_elderly = elderly_ratio,
      dependency_youth = youth_dependency, dependency_elderly = elderly_dependency,
      pp_hh_avg,
      women_percentage,
      buildings_pre1945_percentage
    ) |> rename_with(~ paste0("dasymetric_", .), -gid)
  )  |>
  # Models
  # > Minimum ratios
  left_join(
    model_mr |>
      select(
        gid,
        model_mr_ratio_residential_min = residential_min,
        model_mr_ratio_residential_max = residential_max,
        model_mr_parking_residential_min = residential_parking_min,
        model_mr_parking_residential_max = residential_parking_max
      ),
    by = "gid"
  )

# View(grid_dashboard)
str(grid_dashboard |> st_drop_geometry())
names(grid_dashboard)


## Parish ------------------------------------------------------
parishes_dashboard <- parishes |>
  mutate(pid = as.integer(pid)) |>
  left_join(parishes_meta)

parishes_dashboard <- aggregate_at_layer(
  grid_dashboard,
  parishes_dashboard,
  layer_id = "pid",
  col_to_agg = c(
    # Raw data 
    # > Dasymetric map 
    dasymetric_population = "sum", dasymetric_adults = "sum", dasymetric_youth = "sum", dasymetric_elderly = "sum", dasymetric_women = "sum", dasymetric_households = "sum",
    dasymetric_buildings = "sum", dasymetric_buildings_pre1945 = "sum",
    # Models
    # > Minimum ratios
    model_mr_ratio_residential_min = "min",
    model_mr_ratio_residential_max = "max",
    model_mr_parking_residential_min = "sum",
    model_mr_parking_residential_max = "sum"
  )
) |> mutate( # TODO! Isolate in a separate method for municipalities
    # Composed
    dasymetric_ratio_youth = if_else(dasymetric_population > 0, round(dasymetric_youth / dasymetric_population * 100, 2), 0),
    dasymetric_ratio_elderly = if_else(dasymetric_population > 0, round(dasymetric_elderly / dasymetric_population * 100, 2), 0),
    dasymetric_dependency_youth = if_else(dasymetric_adults > 0, round(dasymetric_youth / dasymetric_adults * 100, 2), 0), 
    dasymetric_dependency_elderly = if_else(dasymetric_adults > 0, round(dasymetric_elderly / dasymetric_adults * 100, 2), 0), 
    dasymetric_pp_hh_avg = if_else(dasymetric_households >= 3, round(dasymetric_population / dasymetric_households, 2), NA_real_),
    dasymetric_women_percentage = if_else(dasymetric_population > 0, round(dasymetric_women / dasymetric_population * 100, 2), 0),
    dasymetric_buildings_pre1945_percentage = if_else(dasymetric_buildings > 0, round(dasymetric_buildings_pre1945 / dasymetric_buildings * 100, 2), 0)
)

# View(parishes_dashboard)
str(parishes_dashboard |> st_drop_geometry())
names(parishes_dashboard)

# Normalize data ----------------------------------------------------------

# Rename "União das Freguesias de" to "UF" at freguesias_aggregated
parishes_dashboard <- parishes_dashboard |>
  mutate(name = gsub("União das freguesias de ", "UF ", name)) |>
  mutate(name = gsub("União das freguesias do ", "UF ", name))

# Round all numeric values to 2 digits (except ID columns like gid, pid, mid, id, *_id)
grid_dashboard <- grid_dashboard |>
  mutate(across(where(is.numeric) & !matches("(^|_)(id|gid|pid|mid|nid)$|^id$", ignore.case = TRUE), ~ round(., 2)))
parishes_dashboard <- parishes_dashboard |>
  mutate(across(where(is.numeric) & !matches("(^|_)(id|gid|pid|mid|nid)$|^id$", ignore.case = TRUE), ~ round(., 2)))


# Store output ------------------------------------------------------
output_dir <- "data/dashboard_data"

st_write(grid_dashboard, file.path(output_dir, "grid_dashboard.geojson"), delete_ds = TRUE)
st_write(parishes_dashboard, file.path(output_dir, "parishes_dashboard.geojson"), delete_ds = TRUE)

write.csv(grid_dashboard |> st_drop_geometry(), file.path(output_dir, "grid_dashboard.csv"), row.names = FALSE)
write.csv(parishes_dashboard |> st_drop_geometry(), file.path(output_dir, "parishes_dashboard.csv"), row.names = FALSE)

# Upload to IST server for dashboard  ----------------------------------------------------
files <- c(
  "grid_dashboard.geojson"#, "parishes_dashboard.geojson",
  # "grid_dashboard.csv", "parishes_dashboard.csv"
)
for (f in files) {
  scp_upload(session, paste(output_dir, f, sep = "/"), to = paste("/afs/.ist.utl.pt/groups/ushift/web/content/biclar", f, sep = "/"))
}
# scp_upload(session, paste(output_location, "champions.json", sep = "/"), to = paste("/afs/.ist.utl.pt/groups/ushift/web/content/biclar", "champions.json", sep = "/"))
