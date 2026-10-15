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
      population,
      population_adults = adults, population_youth = youth, population_elderly = elderly, population_women = women, households,
      buildings, buildings_pre1945,
      # Composed
      ratio_youth = youth_ratio, ratio_elderly = elderly_ratio,
      dependency_youth = youth_dependency, dependency_elderly = elderly_dependency,
      pp_hh_avg,
      women_percentage,
      buildings_pre1945_percentage
    ) |> rename_with(~ paste0("dasymetric_", .), -gid)
  ) |>
  # > Thefts
  left_join(
    grid_thefts |> rename_with(~ stringr::str_replace(., "n_thefts", "thefts_n"))
  ) |>
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


## Geographical aggregations ------------------------------------------------------
col_to_agg <- c(
  # Raw data
  # > Dasymetric map
  dasymetric_population = "sum", dasymetric_population_adults = "sum", dasymetric_population_youth = "sum", dasymetric_population_elderly = "sum", dasymetric_population_women = "sum", dasymetric_households = "sum",
  dasymetric_buildings = "sum", dasymetric_buildings_pre1945 = "sum",
  # > Thefts
  thefts_n = "sum", thefts_n_gnr = "sum", thefts_n_psp = "sum",
  # Models
  # > Minimum ratios
  model_mr_ratio_residential_min = "min",
  model_mr_ratio_residential_max = "max",
  model_mr_parking_residential_min = "sum",
  model_mr_parking_residential_max = "sum"
)

cols_mutate <- function(aggregated_layer) {
  return(aggregated_layer |> mutate(
    # Composed
    dasymetric_ratio_youth = if_else(dasymetric_population > 0, round(dasymetric_population_youth / dasymetric_population * 100, 2), 0),
    dasymetric_ratio_elderly = if_else(dasymetric_population > 0, round(dasymetric_population_elderly / dasymetric_population * 100, 2), 0),
    dasymetric_dependency_youth = if_else(dasymetric_population_adults > 0, round(dasymetric_population_youth / dasymetric_population_adults * 100, 2), 0),
    dasymetric_dependency_elderly = if_else(dasymetric_population_adults > 0, round(dasymetric_population_elderly / dasymetric_population_adults * 100, 2), 0),
    dasymetric_pp_hh_avg = if_else(dasymetric_households >= 3, round(dasymetric_population / dasymetric_households, 2), NA_real_),
    dasymetric_women_percentage = if_else(dasymetric_population > 0, round(dasymetric_population_women / dasymetric_population * 100, 2), 0),
    dasymetric_buildings_pre1945_percentage = if_else(dasymetric_buildings > 0, round(dasymetric_buildings_pre1945 / dasymetric_buildings * 100, 2), 0)
  ))
}

### Parishes ------------------------------------------------------
parishes_dashboard <- parishes |>
  mutate(pid = as.integer(pid)) |>
  left_join(parishes_meta)
parishes_dashboard <- aggregate_at_layer(
  grid_dashboard,
  parishes_dashboard,
  layer_id = "pid",
  col_to_agg
) |> cols_mutate()

# View(parishes_dashboard)
str(parishes_dashboard |> st_drop_geometry())
names(parishes_dashboard)

### Municipalities ------------------------------------------------------
municipalities_dashboard <- municipalities |>
  mutate(mid = as.integer(mid)) |>
  left_join(municipalities_meta)
municipalities_dashboard <- aggregate_at_layer(grid_dashboard, municipalities_dashboard, layer_id = "mid", col_to_agg) |>
  cols_mutate()

# View(municipalities_dashboard)
str(municipalities_dashboard |> st_drop_geometry())
names(municipalities_dashboard)


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
municipalities_dashboard <- municipalities_dashboard |>
  mutate(across(where(is.numeric) & !matches("(^|_)(id|gid|pid|mid|nid)$|^id$", ignore.case = TRUE), ~ round(., 2)))

# Define relational ids (group_id for parent)
grid_dashboard <- grid_dashboard |> mutate(
  region_id = nid,
  group_id = pid
)
parishes_dashboard <- parishes_dashboard |> mutate(
  region_id = nid,
  group_id = mid
)
municipalities_dashboard <- municipalities_dashboard |> mutate(
  region_id = nid
)

# Store output ------------------------------------------------------
output_dir <- "data/dashboard_data"

st_write(grid_dashboard, file.path(output_dir, "grid_dashboard.geojson"), delete_dsn = TRUE)
st_write(parishes_dashboard, file.path(output_dir, "parishes_dashboard.geojson"), delete_dsn = TRUE)
st_write(municipalities_dashboard, file.path(output_dir, "municipalities_dashboard.geojson"), delete_dsn = TRUE)

write.csv(grid_dashboard |> st_drop_geometry(), file.path(output_dir, "grid_dashboard.csv"), row.names = FALSE)
write.csv(parishes_dashboard |> st_drop_geometry(), file.path(output_dir, "parishes_dashboard.csv"), row.names = FALSE)
write.csv(municipalities_dashboard |> st_drop_geometry(), file.path(output_dir, "municipalities_dashboard.csv"), row.names = FALSE)
# grid_dashboard = read.csv(file.path(output_dir, "grid_dashboard.csv"))

# Upload to IST server for dashboard  ----------------------------------------------------
files <- c(
  "municipalities_dashboard.geojson", "parishes_dashboard.geojson", "grid_dashboard.geojson",
  "municipalities_dashboard.csv", "parishes_dashboard.csv", "grid_dashboard.csv"
)
for (f in files) {
  scp_upload(session, paste(output_dir, f, sep = "/"), to = paste("/afs/.ist.utl.pt/groups/ushift/web/content/biclar", f, sep = "/"))
}
# scp_upload(session, paste(output_location, "champions.json", sep = "/"), to = paste("/afs/.ist.utl.pt/groups/ushift/web/content/biclar", "champions.json", sep = "/"))

str(parishes_dashboard)
