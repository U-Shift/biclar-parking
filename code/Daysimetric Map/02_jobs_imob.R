# Jittered job locations based on IMOB
# Purpose     Export ODs for jobs trips (IMOB 2017) with building weight
# Scale       Points
# Issue       https://github.com/U-Shift/IMPT-data/issues/1


library(tidyverse)
library(sf)
# re-install odjitter in cargo (v0.2.1) to consider weitghs for buildings, in terminal:
# cargo install --git https://github.com/itsleeds/odjitter
# remotes::install_github("itsleeds/odjitter", subdir = "r")
library(odjitter)
library(stplanr)


# load imob data
# IMOB = readRDS("data/IMOB_trips.Rds")
IMOB <- impt_read("/trips/IMOB_trips.Rds")
names(IMOB)

table(IMOB$D0500_Dsg)


IMOB <- IMOB |>
  mutate(trip_purpose = case_when(
    # --- Work ---
    D0500_Dsg == "Ir para o trabalho" ~ "Commute to Work",
    D0500_Dsg == "Tratar de assuntos profissionais" ~ "Work (Other)",

    # --- Education ---
    D0500_Dsg == "Ir para a escola ou atividades escolares" ~ "Education",

    # --- Return home ---
    D0500_Dsg == "Regressar a casa" ~ "Return Home",

    # --- Escort trips ---
    D0500_Dsg == "Levar/buscar/acompanhar familiares ou amigos (crianças à escola, etc)" ~ "Escort",

    # --- Leisure ---
    D0500_Dsg %in% c(
      "Assistir a eventos desportivos ou culturais (cinema, teatro, concerto, futebol, etc.)",
      "Ir a restaurante, café, bar, discoteca, etc.",
      "Outras atividades de lazer, entretenimento ou turismo",
      "Praticar atividades ao ar livre (desporto ou lazer) ou em ginásio ou pavilhão",
      "Fazer percurso pedonal (início e fim no mesmo local), jogging, passear o cão, etc. (com pelo menos 200 metros)",
      "Realizar atividade em grupo ou em contexto coletivo (em associações, comícios, igrejas, voluntariado, ...)",
      "Visitar familiares ou amigos"
    ) ~ "Leisure",

    # --- Utilitarian / personal maintenance ---
    D0500_Dsg %in% c(
      "Fazer compras (supermercado, mercearia, utilidades, etc)",
      "Ir a consulta, tratamentos, exames médicos e similares",
      "Tratar de assuntos pessoais (ir ao banco, lavandaria, cabeleireiro, levar ou buscar coisas pessoais, etc)"
    ) ~ "Utilitarian",

    # --- Other ---
    D0500_Dsg == "Outra atividade" ~ "Other",
    TRUE ~ "Other"
  ))

table(IMOB$trip_purpose)
# Commute to Work       Education          Escort         Leisure           Other     Return Home     Utilitarian    Work (Other)
# 19409            7262           10032           13314             896           48500           10180            1531

prop.table(table(IMOB$trip_purpose))
# Commute to Work       Education          Escort         Leisure           Other     Return Home     Utilitarian    Work (Other)
# 0.174660739     0.065350419     0.090277528     0.119812102     0.008063065     0.436449372     0.091609373     0.013777402

table(IMOB$modo)

## groupped by OD, mode, trip purpose

OD_all <- IMOB |>
  mutate(modo = case_when(
    modo == "Motorcycle" ~ "Car",
    TRUE ~ modo
  )) |>
  group_by(
    Origin_dicofre16, Destination_dicofre16,
    modo, # is mode necessary??
    trip_purpose
  ) |>
  summarise(
    trips = sum(PESOFIN, na.rm = TRUE), # expanded number of trips
    n_obs = n(), # number of survey records (optional but useful)
    .groups = "drop"
  )

sum(OD_all$trips) # 5266475
sum(IMOB$PESOFIN) # 5266475

## Convert to new freguesias 21/24
OD_all_new <- OD_all |>
  left_join(conversion_dicofre_weight, by = c("Origin_dicofre16" = "dtmnfr16")) |>
  mutate(trips = trips * weight) |>
  rename(Origin_dicofre24 = "dtmnfr24") |>
  select(-Origin_dicofre16, -weight) |>
  left_join(conversion_dicofre_weight, by = c("Destination_dicofre16" = "dtmnfr16")) |>
  mutate(trips = trips * weight) |>
  rename(Destination_dicofre24 = "dtmnfr24") |>
  select(-Destination_dicofre16, -weight) |>
  group_by(Origin_dicofre24, Destination_dicofre24, modo, trip_purpose) |>
  summarise(
    trips = sum(trips, na.rm = TRUE),
    .groups = "drop"
  )

sum(OD_all_new$trips) # 5266475


# jobs --------------------------------------------------------------------

OD_jobs <- OD_all_new |>
  filter(trip_purpose == "Commute to Work") |>
  select(-trip_purpose) |>
  group_by(Origin_dicofre24, Destination_dicofre24) |> # hide this if consider by mode
  summarise(
    trips = sum(trips),
    .groups = "drop_last"
  ) |>
  ungroup()
sum(OD_jobs$trips) # 839142
summary(OD_jobs$trips) # median 30 or 43

# jitter in freguesias - choose road network or builgings, not both!

# with road network
road_network <- impt_read("/geo/IMPT_Road_network.gpkg")

od_jobs_jittered <- odjitter::jitter(
  od = OD_jobs,
  zones = freguesias,
  subpoints = road_network,
  disaggregation_key = "trips",
  disaggregation_threshold = 50,
  rng_seed = 42
)


# with buildings, weighted by construction area (ABC)
buildings <- impt_read("/pois/lisbon_metro_buildings_height.geojson")
buildings <- buildings |>
  mutate(volume = height * footprint_m2) |> # volume
  rename(weight = volume)


od_jobs_jittered_buildings <- odjitter::jitter(
  od = OD_jobs,
  zones = freguesias,
  subpoints_origins = buildings,
  subpoints_destinations = buildings,
  weight_key_destinations = "weight",
  disaggregation_key = "trips",
  disaggregation_threshold = 50,
  rng_seed = 42 # deterministic randomization
)
od_jobs_jittered <- od_jobs_jittered_buildings


# add an id to the jittered pairs, so we can join later
od_jobs_jittered_id <- od_jobs_jittered
od_jobs_jittered_id$id <- 1:nrow(od_jobs_jittered_id)
sf::impt_write(od_jobs_jittered_id, "/trips/od_jobs_jt50_buildings.gpkg")


# with stplanr
od_jobs_jittered_points <- line2df(od_jobs_jittered)
od_jobs_jittered_OR <- od_jobs_jittered_points |>
  select(L1, fx, fy) |> # from
  rename(
    id = L1,
    lon = fx,
    lat = fy
  )
od_jobs_jittered_DE <- od_jobs_jittered_points |>
  select(L1, tx, ty) |> # to
  rename(
    id = L1,
    lon = tx,
    lat = ty
  )

# as sf
od_jobs_jittered_OR_geo <- st_as_sf(od_jobs_jittered_OR,
  coords = c("lon", "lat"),
  crs = 4326
)
od_jobs_jittered_DE_geo <- st_as_sf(od_jobs_jittered_DE,
  coords = c("lon", "lat"),
  crs = 4326
)

sf::impt_write(od_jobs_jittered_OR_geo, "/trips/od_jobs_jt50_buildings_OR.gpkg")
sf::impt_write(od_jobs_jittered_DE_geo, "/trips/od_jobs_jt50_buildings_DE.gpkg")

## POIS

pois_jobs <- od_jobs_jittered_DE_geo |>
  left_join(od_jobs_jittered_id |> st_drop_geometry() |> select(id, trips))

pois_jobs_check <- pois_jobs |>
  group_by(geometry) |>
  summarize(trips = sum(trips))

mapview::mapview(pois_jobs_check, zcol = "trips")

# st_write(pois_jobs, "data/pois/pois_jobs_imob_jt50.gpkg", delete_dsn = TRUE)
# impt_write(pois_jobs, "/pois/pois_jobs_imob_jt50_buildings.gpkg")


## Save OD mode and trip purpose for other ttm statistics?
saveRDS(OD_all_new, "data/IMOB_od_freg_mode_purpose.Rds")


## Modal share from IMOB - EXPORT -----------------------------------
# file from 01_data_prep.R

# --- Load raw OD data ---
od_raw <- impt_read("/trips/TRIPSmode_freguesias_2024.Rds") |>
  mutate(freg_id = as.character(Origin_dicofre24))

# --- FREGUESIA LEVEL ---
modal_share_freg <- od_raw |>
  group_by(freg_id) |>
  summarise(
    trips_total = round(sum(Total, na.rm = TRUE)),
    share_car = round(sum(Car, na.rm = TRUE) / trips_total * 100, 2),
    share_pt = round(sum(PTransit, na.rm = TRUE) / trips_total * 100, 2),
    share_walk = round(sum(Walk, na.rm = TRUE) / trips_total * 100, 2),
    share_bike = round(sum(Bike, na.rm = TRUE) / trips_total * 100, 2),
    share_active = round((sum(Walk, na.rm = TRUE) + sum(Bike, na.rm = TRUE)) / trips_total * 100, 2),
    .groups = "drop"
  )
# sum(modal_share_freg$trips_total) #5299854

impt_write(modal_share_freg, "/trips/imob_modal_share_freg.csv")

# --- MUNICIPALITY LEVEL ---
# Aggregate raw trips directly to avoid double-weighting
modal_share_mun <- od_raw |>
  left_join(grid_freg_mun |> select(freg_id, mun_id) |> distinct(), by = "freg_id") |>
  group_by(mun_id) |>
  summarise(
    trips_total = round(sum(Total, na.rm = TRUE)),
    share_car = round(sum(Car, na.rm = TRUE) / trips_total * 100, 2),
    share_pt = round(sum(PTransit, na.rm = TRUE) / trips_total * 100, 2),
    share_walk = round(sum(Walk, na.rm = TRUE) / trips_total * 100, 2),
    share_bike = round(sum(Bike, na.rm = TRUE) / trips_total * 100, 2),
    share_active = round((sum(Walk, na.rm = TRUE) + sum(Bike, na.rm = TRUE)) / trips_total * 100, 2),
    .groups = "drop"
  )
# sum(modal_share_mun$trips_total) #5299853

impt_write(modal_share_mun, "/trips/imob_modal_share_mun.csv")

# --- GRID LEVEL ---
# IMOB OD is at freguesia resolution; freg-level shares assigned to grid cells
# via grid_freg_mun lookup (generalised assumption).
modal_share_grid <- grid_freg_mun |>
  select(grid_id, freg_id) |>
  left_join(modal_share_freg |> select(-trips_total), by = "freg_id") |>
  select(-freg_id)

impt_write(modal_share_grid, "/trips/imob_modal_share_grid.csv")
