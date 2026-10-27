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
      # Raw continuous (for parish/municipality aggregation)
      any_of(c(
        "population_raw",
        population_adults_raw = "adults_raw",
        population_youth_raw = "youth_raw",
        population_elderly_raw = "elderly_raw",
        population_women_raw = "women_raw",
        "households_raw",
        "buildings_raw",
        buildings_pre1945_raw = "buildings_pre45_raw"
      )),
      # Discrete integer totals (for grid display)
      population,
      population_adults = adults, population_youth = youth, population_elderly = elderly, population_women = women,
      households,
      buildings, buildings_pre1945,
      # Composed
      population_youth_percentage = youth_ratio, population_elderly_percentage = elderly_ratio, population_women_percentage = women_percentage,
      dependency_ratio_youth = youth_dependency, dependency_ratio_elderly = elderly_dependency,
      pp_hh_avg,
      buildings_pre1945_percentage
    ) |> rename_with(~ paste0("dasymetric_", .), -gid)
  ) |>
  # > Infrastructure
  left_join(
    cycleways_grid |> st_drop_geometry() |> select(
      gid,
      cycleways_length = total_cycleway_length,
      cycleways_length_protected = protected_cycleway_length,
      cycleways_length_active = active_cycleway_length,
      cycleways_length_advisory = advisory_cycleway_length
    )
  ) |>
  left_join(
    parking_grid |> st_drop_geometry() |> select(
      gid,
      parking_n_points = n_points,
      parking_n_capacity = capacity
    )
  ) |>
  # > Thefts
  left_join(
    grid_thefts |> rename_with(~ stringr::str_replace(., "n_thefts", "thefts_n"))
  ) |>
  # > Ticketing
  left_join(
    grid_ticketing_global |> rename_with(~ paste0("ticketing_", .), -gid)
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
  # > Dasymetric map (aggregate raw continuous values to eliminate cumulative ceiling bias)
  dasymetric_population_raw = "sum",
  dasymetric_population_adults_raw = "sum",
  dasymetric_population_youth_raw = "sum",
  dasymetric_population_elderly_raw = "sum",
  dasymetric_population_women_raw = "sum",
  dasymetric_households_raw = "sum",
  dasymetric_buildings_raw = "sum",
  dasymetric_buildings_pre1945_raw = "sum",
  # > Infrastructure
  cycleways_length = "sum",
  cycleways_length_protected = "sum",
  cycleways_length_active = "sum",
  cycleways_length_advisory = "sum",
  parking_n_points = "sum",
  parking_n_capacity = "sum",
  # > Thefts
  thefts_n = "sum", thefts_n_gnr = "sum", thefts_n_psp = "sum",
  # > Ticketing
  ticketing_n = "sum",
  ticketing_n_bus = "sum",
  ticketing_n_train = "sum",
  ticketing_n_metro = "sum",
  ticketing_n_ltr = "sum",
  ticketing_n_boat = "sum",
  # Models
  # > Minimum ratios
  model_mr_ratio_residential_min = "min",
  model_mr_ratio_residential_max = "max",
  model_mr_parking_residential_min = "sum",
  model_mr_parking_residential_max = "sum"
)

cols_mutate <- function(aggregated_layer) {
  aggregated_layer <- aggregated_layer |>
    mutate(
      # Discrete totals rounded/ceiled once at the aggregated layer
      dasymetric_population = ceiling(dasymetric_population_raw),
      dasymetric_buildings = ceiling(dasymetric_buildings_raw),
      dasymetric_buildings_pre1945 = pmin(dasymetric_buildings, ceiling(dasymetric_buildings_pre1945_raw)),
      dasymetric_households = if_else(dasymetric_population > 0 & dasymetric_households_raw > 0, 
                                      pmax(1, pmin(dasymetric_population, round(dasymetric_households_raw))), 0),

      # Indicators computed from raw continuous sums
      dasymetric_population_youth_percentage = if_else(dasymetric_population_raw > 0, round(dasymetric_population_youth_raw / dasymetric_population_raw * 100, 2), 0),
      dasymetric_population_elderly_percentage = if_else(dasymetric_population_raw > 0, round(dasymetric_population_elderly_raw / dasymetric_population_raw * 100, 2), 0),
      dasymetric_population_women_percentage = if_else(dasymetric_population_raw > 0, round(dasymetric_population_women_raw / dasymetric_population_raw * 100, 2), 0),
      dasymetric_dependency_ratio_youth = if_else(dasymetric_population_adults_raw > 0, round(dasymetric_population_youth_raw / dasymetric_population_adults_raw * 100, 2), 0),
      dasymetric_dependency_ratio_elderly = if_else(dasymetric_population_adults_raw > 0, round(dasymetric_population_elderly_raw / dasymetric_population_adults_raw * 100, 2), 0),
      dasymetric_pp_hh_avg = if_else(dasymetric_households >= 3, round(dasymetric_population / dasymetric_households, 2), NA_real_),
      dasymetric_buildings_pre1945_percentage = if_else(dasymetric_buildings_raw > 0, round(dasymetric_buildings_pre1945_raw / dasymetric_buildings_raw * 100, 2), 0),

      # Consistent integer demographic counts at the aggregated layer
      raw_age_sum = dasymetric_population_youth_raw + dasymetric_population_adults_raw + dasymetric_population_elderly_raw,
      has_ages = raw_age_sum > 0 & dasymetric_population > 0,
      py = if_else(has_ages, dasymetric_population_youth_raw / raw_age_sum, 0),
      pa = if_else(has_ages, dasymetric_population_adults_raw / raw_age_sum, 1),
      pe = if_else(has_ages, dasymetric_population_elderly_raw / raw_age_sum, 0),

      fy = floor(dasymetric_population * py),
      fa = floor(dasymetric_population * pa),
      fe = floor(dasymetric_population * pe),

      rem_age = dasymetric_population - (fy + fa + fe),
      ry = (dasymetric_population * py) - fy,
      ra = (dasymetric_population * pa) - fa,
      re = (dasymetric_population * pe) - fe,

      add_y = (rem_age >= 1 & ry >= ra & ry >= re) | (rem_age == 2 & (ry >= pmin(ra, re))),
      rem_after_y = rem_age - as.integer(add_y),
      add_a = (rem_after_y >= 1 & ra >= re) | (rem_after_y == 2),
      rem_after_a = rem_after_y - as.integer(add_a),
      add_e = rem_after_a >= 1,

      dasymetric_population_youth = if_else(dasymetric_population > 0, fy + as.integer(add_y), 0),
      dasymetric_population_adults = if_else(dasymetric_population > 0, fa + as.integer(add_a), 0),
      dasymetric_population_elderly = if_else(dasymetric_population > 0, fe + as.integer(add_e), 0),
      dasymetric_population_women = if_else(dasymetric_population > 0, pmin(dasymetric_population, round(dasymetric_population * if_else(dasymetric_population_raw > 0, dasymetric_population_women_raw / dasymetric_population_raw, 0))), 0)
    ) |>
    select(-c(
      dasymetric_population_raw, dasymetric_population_adults_raw, dasymetric_population_youth_raw,
      dasymetric_population_elderly_raw, dasymetric_population_women_raw, dasymetric_households_raw,
      dasymetric_buildings_raw, dasymetric_buildings_pre1945_raw,
      raw_age_sum, has_ages, py, pa, pe, fy, fa, fe,
      rem_age, ry, ra, re, add_y, rem_after_y, add_a, rem_after_a, add_e
    ))
  return(aggregated_layer)
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
  select(-any_of(c(
    "dasymetric_population_raw", "dasymetric_population_adults_raw", "dasymetric_population_youth_raw",
    "dasymetric_population_elderly_raw", "dasymetric_population_women_raw", "dasymetric_households_raw",
    "dasymetric_buildings_raw", "dasymetric_buildings_pre1945_raw"
  ))) |>
  mutate(across(where(is.numeric) & !matches("(^|_)(id|gid|pid|mid|nid)$|^id$", ignore.case = TRUE), ~ round(., 2)))
parishes_dashboard <- parishes_dashboard |>
  mutate(across(where(is.numeric) & !matches("(^|_)(id|gid|pid|mid|nid)$|^id$", ignore.case = TRUE), ~ round(., 2)))
municipalities_dashboard <- municipalities_dashboard |>
  mutate(across(where(is.numeric) & !matches("(^|_)(id|gid|pid|mid|nid)$|^id$", ignore.case = TRUE), ~ round(., 2)))

# Define relational ids (group_id for parent)
grid_dashboard <- grid_dashboard |> mutate(
  id = gid,
  region_id = nid,
  group_id = pid
)
parishes_dashboard <- parishes_dashboard |> mutate(
  id = pid,
  region_id = nid,
  group_id = mid
)
municipalities_dashboard <- municipalities_dashboard |> mutate(
  id = mid,
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

st_write(
  municipalities |> mutate(mid = as.integer(mid)) |> left_join(municipalities_meta) |>
    mutate(region_id = nid),
  file.path(output_dir, "municipios_limits.geojson"),
  delete_dsn = TRUE
)

# Extra geo layers ------------------------------------------------------
str(cycleways |> st_drop_geometry())
# mapview(municipalities, col.regions = "#00ccff", alpha.regions = 0.3) + mapview(cycleways |> filter(is.na(municipality)), color = "red")
st_write(
  cycleways |>
    st_transform(4326) |>
    left_join(municipalities_meta, by = c("municipality" = "name")) |>
    select(osm_id, name, cycle_segregation, region_id = nid),
  file.path(output_dir, "layer_cycleways.geojson"),
  delete_dsn = TRUE
)

str(parking |> st_drop_geometry())
# mapview(municipalities, col.regions = "#00ccff", alpha.regions = 0.3) + mapview(parking |> filter(is.na(municipality)), color = "red")
st_write(
  parking |>
    filter(!is.na(municipality)) |>
    left_join(municipalities_meta, by = c("municipality" = "name")) |>
    select(ID, capacity, region_id = nid) |>
    st_transform(4326),
  file.path(output_dir, "layer_parking.geojson"),
  delete_dsn = TRUE
)

# Upload to IST server for dashboard  ----------------------------------------------------
files <- c(
  "municipalities_dashboard.geojson", "parishes_dashboard.geojson", "grid_dashboard.geojson",
  "municipalities_dashboard.csv", "parishes_dashboard.csv", "grid_dashboard.csv",
  "municipios_limits.geojson",
  "layer_cycleways.geojson", "layer_parking.geojson"
)
for (f in files) {
  scp_upload(session, paste(output_dir, f, sep = "/"), to = paste("/afs/.ist.utl.pt/groups/ushift/web/content/biclar", f, sep = "/"))
}
# scp_upload(session, paste(output_location, "champions.json", sep = "/"), to = paste("/afs/.ist.utl.pt/groups/ushift/web/content/biclar", "champions.json", sep = "/"))

str(parishes_dashboard)
