# Script to aggregate cycling infrastructure data. Steps:
# 1. Get municipalities with names.
# 2. Analyse PMUS database (deprecated)
# 3.1. CRITICAL STEP: Load and standardize OSM data (occasionaly run "bikelanes_00_osm_export.R" to update fetching).
# 3.2. CRITICAL STEP: Aggregate OSM data based on grid cell by cycleway length (UNFINISHED)
# 4. Load and standardize updated data from available municipalities (for now, Cascais, Lisboa, Oeiras, Seixal, Sintra).
# 5. For the municipalities whose updated data is not available, get data from Biclar (2022).
# 6. Aggregate data from all municipalities.
# 7. Function to compare different sources of cycleway data and calculate the overlap between one another.
# 8. Applying the function in 7. to compare municipality and OSM data.

library(dplyr)
library(sf)
library(mapview)

# 1. Municipalities with names ----------------------------------------------
parishes <- read_sf("/data/biclar-parking/geo/CAOP_GLPS_LAND_UNIQUE_dtmnfr.gpkg")
municipalities <- parishes |>
  group_by(municipio) |>
  summarize(
    dtmn = first(substr(dtmnfr, 1, 4)),
    nuts3_cod = first(nuts3_cod),
    nuts3 = first(nuts2),
    area_ha = sum(area_ha),
    geom = st_union(geom)
  ) |>
  st_transform(3763)

# 2. TML PMUS Database -------------------------------------------------------
## 2.1. Bike network (existant + planned) ------------------------------------------------------------
# aml_cyclelanes_existant_open <- st_read("https://geoportal.tmlmobilidade.pt/ogc-api/collections/rede_ciclavel_rede_ciclavel_existente/items?f=json&limit=1000") |>
#   bind_rows(st_read("https://geoportal.tmlmobilidade.pt/ogc-api/collections/rede_ciclavel_rede_ciclavel_existente/items?f=json&limit=1000&offset=1000"))
# nrow(aml_cyclelanes_existant_open) # 1199
# summary(aml_cyclelanes_existant_open)

aml_cyclelanes_existant <- st_read("/data/IMPT/BaseDados_PMMUS/10-ModosAtivos/Fig10_5_RedeCiclavel_Existente-Prevista.gpkg", layer = "Fig5_RedeCiclavelExistente")
nrow(aml_cyclelanes_existant) # 1199
summary(aml_cyclelanes_existant)
sum(aml_cyclelanes_existant$Length / 1000) # 440
table(aml_cyclelanes_existant$Tipologia)
# Dedicada            Faixa ciclavel              Não-dedicada Pista ciclavel (ciclovia)
# 672                         2                       463                         2
table(aml_cyclelanes_existant$Municipio_)
# Alcochete              Almada             Amadora            Barreiro             Cascais              Lisboa              Loures
# 3                  40                  19                  19                  29                 798                  45
# Mafra               Moita             Montijo            Odivelas              Oeiras             Palmela              Seixal
# 8                   6                  14                   9                  19                  14                  96
# Sesimbra             Setúbal              Sintra Vila Franca de Xira
# 9                  29                   7                  35

# aml_cyclelanes_planned_open <- st_read("https://geoportal.tmlmobilidade.pt/ogc-api/collections/rede_ciclavel_rede_ciclavel_prevista/items?f=json&limit=1000") |>
#   bind_rows(st_read("https://geoportal.tmlmobilidade.pt/ogc-api/collections/rede_ciclavel_rede_ciclavel_prevista/items?f=json&limit=1000&offset=1000")) |>
#   bind_rows(st_read("https://geoportal.tmlmobilidade.pt/ogc-api/collections/rede_ciclavel_rede_ciclavel_prevista/items?f=json&limit=1000&offset=2000")) |>
#   bind_rows(st_read("https://geoportal.tmlmobilidade.pt/ogc-api/collections/rede_ciclavel_rede_ciclavel_prevista/items?f=json&limit=1000&offset=3000"))
# nrow(aml_cyclelanes_planned_open) # 3215
# summary(aml_cyclelanes_planned_open)

aml_cyclelanes_planned <- st_read("/data/IMPT/BaseDados_PMMUS/10-ModosAtivos/Fig10_5_RedeCiclavel_Existente-Prevista.gpkg", layer = "Fig5_RedeCiclavelPrevista")
nrow(aml_cyclelanes_planned) # 3215
summary(aml_cyclelanes_planned)
sum(aml_cyclelanes_planned$Lenght / 1000, na.rm=TRUE) # 1592
table(aml_cyclelanes_planned$Estado)
# Com projeto de execução           Em construção           Estudo prévio     Intenção de traçado                Proposta
# 95                       9                     814                     887                    1033
table(aml_cyclelanes_planned$Municipio_)
# Alcochete              Almada             Amadora            Barreiro             Cascais              Lisboa              Loures
# 41                 183                  31                 112                 140                 779                  62
# Mafra               Moita             Montijo            Odivelas              Oeiras             Palmela              Seixal
# 1                 122                  24                 234                 102                  24                 991
# Sesimbra             Setúbal              Sintra Vila Franca de Xira
# 110                  77                  13                 168

#mapview(aml_cyclelanes_existant, color="darkgreen") + mapview(aml_cyclelanes_planned, color="orange")

# 3.1. Retrieve OSM cycleway data -------------------------------------------------------
list_osm_cycleways = list.files(path = "/data/biclar-parking/raw/cycling_network/osm", pattern = "\\.gpkg$", full.names=TRUE)
aml_osm_cycleways <- st_read(dplyr::last(list_osm_cycleways)) |> st_transform(3763)
nrow(aml_osm_cycleways)
summary(aml_osm_cycleways)
aml_osm_cycleways <- st_join(aml_osm_cycleways, municipalities["municipio"], join=st_within)
aml_osm_cycleways <- aml_osm_cycleways |> rename(municipality = municipio)
# mapview(aml_osm_cycleways)

# osm_protected_cycleways <- aml_osm_cycleways[aml_osm_cycleways$cycle_segregation != "Advisory lane",]
# mapview(osm_protected_cycleways)

# write_sf(aml_osm_cycleways, "/data/biclar-parking/base/existing_cycleways/aml_osm_cycleways.gpkg")

# 3.2. Aggregate OSM data by grid cell -------------------------------------------------------
sf::sf_use_s2(FALSE) #remove spherical geometry (this was causing the next code to take too much time to run)
osm_cycleways_split <- st_transform(aml_osm_cycleways, 4326)
osm_cycleways_split <- st_intersection(osm_cycleways_split, grid)
osm_cycleways_split$length <- st_length(osm_cycleways_split)

length_per_cell <- osm_cycleways_split |>
  st_drop_geometry() |>
  group_by(gid) |>
  summarise(
    protected_cycleway_length = sum(length[cycle_segregation == "Cycle track or lane"], na.rm = TRUE),
    active_cycleway_length = sum(length[cycle_segregation == "Protected Active"], na.rm = TRUE),
    advisory_cycleway_length = sum(length[cycle_segregation == "Advisory lane"], na.rm = TRUE),
    total_cycleway_length = sum(length, na.rm = TRUE),
    .groups="drop"      # maintain df structure
  )

osm_cycleways_grid <- grid |>
  left_join(length_per_cell,by ="gid") |>
  mutate(protected_cycleway_length = ifelse(is.na(protected_cycleway_length), 0, protected_cycleway_length)) |>
  mutate(active_cycleway_length = ifelse(is.na(active_cycleway_length), 0, active_cycleway_length)) |>
  mutate(advisory_cycleway_length = ifelse(is.na(advisory_cycleway_length), 0, advisory_cycleway_length)) |>
  mutate(total_cycleway_length = ifelse(is.na(total_cycleway_length), 0, total_cycleway_length))

#write_sf(osm_cycleways_grid, "/data/biclar-parking/base/existing_cycleways/osm_cycleways_grid.gpkg")

# 4. Updated Municipality Cycleway Data -------------------------------------------------------
## 4.1. Get cycleway data for every available municipality ----------------------
list_mun_cycleways = list.files(path = "/data/biclar-parking/raw/cycling_network/municipalities", full.names=TRUE)

# aml_mun_cycleways <- list()
# for (file in list_mun_cycleways) {
#   mun_name <- gsub("\\..*$", "", basename(file))
#   mun <- st_read(file) |> mutate(municipality = mun_name)
#   mun <- st_as_sf(mun)
#   aml_mun_cycleways[[file]] <- mun
# }

# Alcochete_cycleways
# Almada_cycleways
# Amadora_cycleways
# Barreiro_cycleways
Cascais_cycleways <- st_read(list_mun_cycleways[2]) |> mutate(municipality = gsub("/data/biclar-parking/raw/cycling_network/municipalities/|\\..*$", "", list_mun_cycleways[2]))
Lisboa_cycleways <- st_read(list_mun_cycleways[3]) |> mutate(municipality = gsub("/data/biclar-parking/raw/cycling_network/municipalities/|\\..*$", "", list_mun_cycleways[3]))
# Loures_cycleways
# Mafra_cycleways
# Moita_cycleways
# Montijo_cycleways
# Odivelas_cycleways
Oeiras_cycleways <- st_read(list_mun_cycleways[4]) |> mutate(municipality = gsub("/data/biclar-parking/raw/cycling_network/municipalities/|\\..*$", "", list_mun_cycleways[4]))
# Palmela_cycleways
Seixal_cycleways <- st_read(list_mun_cycleways[5]) |> mutate(municipality = gsub("/data/biclar-parking/raw/cycling_network/municipalities/|\\..*$", "", list_mun_cycleways[5]))
# Sesimbra_cycleways
# Setúbal_cycleways
Sintra_cycleways <- st_read(list_mun_cycleways[6]) |> mutate(municipality = gsub("/data/biclar-parking/raw/cycling_network/municipalities/|\\..*$", "", list_mun_cycleways[6]))
# VFX_cycleways


## 4.2. Standardize cycleway data ---------------------------------------------
### CRS (Standardize as 3763) --------------------------------------------
Cascais_cycleways <- Cascais_cycleways |> st_transform(3763)
Lisboa_cycleways <- Lisboa_cycleways |> st_transform(3763)
Oeiras_cycleways <- Oeiras_cycleways |> st_transform(3763)
Seixal_cycleways <- Seixal_cycleways |> st_transform(3763)
Sintra_cycleways <- Sintra_cycleways |> st_transform(3763)

### Remove empty geometries --------------------------------------------
Cascais_cycleways <- Cascais_cycleways[!st_is_empty(Cascais_cycleways), ]
Lisboa_cycleways <- Lisboa_cycleways[!st_is_empty(Lisboa_cycleways), ]
Oeiras_cycleways <- Oeiras_cycleways[!st_is_empty(Oeiras_cycleways), ]
Seixal_cycleways <- Seixal_cycleways[!st_is_empty(Seixal_cycleways), ]
Sintra_cycleways <- Sintra_cycleways[!st_is_empty(Sintra_cycleways), ]

### Remove Z/M geometry parameter --------------------------------------------
Cascais_cycleways <- st_zm(Cascais_cycleways)
Lisboa_cycleways <- st_zm(Lisboa_cycleways)
Oeiras_cycleways <- st_zm(Oeiras_cycleways)
Seixal_cycleways <- st_zm(Seixal_cycleways)
Sintra_cycleways <- st_zm(Sintra_cycleways)

### Remove non-existent (planned or under construction) network ---------------------------------------------------------------
Oeiras_cycleways <- subset(Oeiras_cycleways, Oeiras_cycleways$Fase=="Existente") # Only existing cycleways
Seixal_cycleways <- Seixal_cycleways[!is.na(Seixal_cycleways$Data_Inau), ] # Inauguration date has to exist

### Standardize fields ------------------------------------------------------------------
# Keep fields "name", "mun_class", "direction", "geom", "municipality"
Cascais_cycleways <- Cascais_cycleways |>
  select(tipo, designacao, geometry, municipality) |>
  rename(type_original = tipo, name = designacao)

Lisboa_cycleways <- Lisboa_cycleways |>
  select(DESIGNACAO, TIPOLOGIA, SHAPE, municipality) |>
  rename(name = DESIGNACAO, type_original = TIPOLOGIA, geometry = SHAPE)

Oeiras_cycleways <- Oeiras_cycleways |> # Oeiras has no type_original...
  select(Designação, Fase, geometry, municipality) |>
  rename(name = Designação, type_original = Fase)

Seixal_cycleways <- Seixal_cycleways |>
  select(SEGMENTO_I, TIPOLOGIA, geom, municipality) |>
  rename(name = SEGMENTO_I, type_original = TIPOLOGIA, geometry = geom)

Sintra_cycleways <- Sintra_cycleways |>
  select(tipologia, observacoe, geom, municipality) |>
  rename(name = observacoe, type_original = tipologia, geometry = geom)


### Remove rows with type_original = NA -------------------------
# (there were some cycleways whose geometry made no sense and didn't correspond to reality)
Cascais_cycleways <- Cascais_cycleways[!is.na(Cascais_cycleways$type_original), ]
Lisboa_cycleways <- Lisboa_cycleways[!is.na(Lisboa_cycleways$type_original), ]
Oeiras_cycleways <- Oeiras_cycleways[!is.na(Oeiras_cycleways$type_original), ]
Seixal_cycleways <- Seixal_cycleways[!is.na(Seixal_cycleways$type_original), ]
Sintra_cycleways <- Sintra_cycleways[!is.na(Sintra_cycleways$type_original), ]


## 4.3. Standardize cycle_segregation -------------------------------------------
Cascais_cycleways <- Cascais_cycleways |>
    mutate(cycle_segregation = recode(type_original,
                                      "Corredor ciclável" = "Cycle track or lane", ### "Corredor ciclável" means uni-directional cycle lanes along a each side of a road, segregated only with paint.
                                      "Pista ciclável" = "Cycle track or lane", ### "Pista ciclável" means a bi-directional cycle lane, somewhat segregated from traffic (by a curb or bollards).
                                      "Via banalizada" = "Mixed traffic",
                                      "Trilhos ciclaveis" = "Off-road track"
                                      )
      )
Lisboa_cycleways <- Lisboa_cycleways |>
  mutate(cycle_segregation = recode(type_original,
                                    "30+Bici" = "Advisory lane",
                                    "Zona de Coexistência" = "Advisory lane",
                                    "Pista Ciclável Unidirecional" = "Cycle track or lane",
                                    "Pista Ciclável Bidirecional" = "Cycle track or lane",
                                    "Faixa Ciclável" = "Cycle track or lane",
                                    "Contrassentido" = "Cycle track or lane",
                                    "Trilho" = "Off-road track",
                                    "Percurso Ciclopedonal" = "Protected Active"
                                    )
  )
Oeiras_cycleways <- Oeiras_cycleways |>
  mutate(cycle_segregation = recode(type_original,
                                    "Existente" = "Cycle track or lane",
                                    )
  )
Seixal_cycleways <- Seixal_cycleways |>
  mutate(cycle_segregation = recode(type_original,
                                    "Misto com automóveis (rede viária)" = "Advisory lane",
                                    "Faixa ciclável" = "Cycle track or lane", ### "Corredor ciclável" means uni-directional cycle lanes along a each side of a road, segregated either with paint or a curb.
                                    "Pista Ciclável" = "Cycle track or lane", ### "Pista ciclável" means a cycle lane (either uni or bi-directional), somewhat segregated from traffic.
                                    "Misto com peões" = "Protected Active"
                                    )
  )
Sintra_cycleways <- Sintra_cycleways |>
  mutate(cycle_segregation = recode(type_original,
                                    "Pista ciclável" = "Cycle track or lane", # Sintra does not distinguish their cycle tracks. "Pista ciclável" only means a corridor intended for cyclists (even if there are parts shared with cars or with no signage)
                                    )
  )


# 5. Get Biclar data (2022), and join to the municipalities we already have -------------------------------------------------------------
mun_remove <- sub("\\.[^.]*$", "", list.files(path = "/data/biclar-parking/raw/cycling_network/municipalities", full.names=FALSE))
aml_biclar_cycleways <- st_read("/data/biclar-parking/raw/cycling_network/municipalities/_RedeExistente2022.gpkg")
aml_biclar_cycleways <- aml_biclar_cycleways |>
  select (Municipio, ID, Tipologia, geom) |>
  rename (name = ID, type_original = Tipologia, municipality = Municipio, geometry = geom)
aml_biclar_cycleways <- aml_biclar_cycleways |> filter(!municipality %in% mun_remove)
aml_biclar_cycleways <- aml_biclar_cycleways |>
  mutate(cycle_segregation = recode(type_original,
                                    "Não-dedicada" = "Advisory lane",
                                    "Dedicada" = "Cycle track or lane"
                                    ) # NOTE: The original classification is too broad, "Dedicada" also includes cycleways shared with pedestrians
  )


# 6. Join all municipality data -----------------------------------------------------
aml_municipality_cycleways <- rbind(aml_biclar_cycleways, Cascais_cycleways, Lisboa_cycleways, Oeiras_cycleways, Seixal_cycleways, Sintra_cycleways)
aml_municipality_cycleways$length = st_length(aml_municipality_cycleways) |> units::drop_units() |> round()

# write_sf(aml_municipality_cycleways, "/data/biclar-parking/base/existing_cycleways/aml_municipality_cycleways.gpkg")
# mapview(aml_osm_cycleways, color="red") + mapview(aml_municipality_cycleways, color="darkblue") + mapview(aml_cyclelanes_existant, color="darkgreen")
# mapview(aml_municipality_cycleways, color="red",alpha=1,lwd=7) + mapview(aml_osm_cycleways, color="yellow", alpha=0.5, lwd=7)
mapview(aml_osm_cycleways, zcol="cycle_segregation")

# 7. Function to compare sources -----------------------------------------------------
# Function to calculate percentage of overlap between
# "cycleways1", the object whose data you want to update or see what's missing - for our purposes, it will usually be OSM
# and "cycleways2", the object with the "most reliable" data - in our case, updated municipality + 2022 Biclar data
cycleway_overlap <- function (cycleways1, cycleways2, buffer_distance = 10){
  union_buffer <- st_buffer(cycleways1, buffer_distance) |> st_union()
  multiple_buffer <- st_buffer(cycleways2, buffer_distance)

  overlap_percentages <- c()
  for (i in 1:nrow(multiple_buffer)){
    single_buffer <- multiple_buffer[i,]
    intersection <- st_intersection(single_buffer, union_buffer) # Overlap between buffers

    # Calculate overlap area
    if (nrow(intersection) > 0 && !is.null(intersection)) {
      overlap_area <- sum(st_area(intersection))
    } else {
      overlap_area <- 0
    }
    buffer_area <- st_area(single_buffer)
    overlap_percentages <- c(overlap_percentages,as.numeric(overlap_area / buffer_area)*100)
  }
  return(overlap_percentages)
}


# 8. Cycleways in municipalities but not in OSM -------------------------
# aml_municipality_cycleways$osm_overlap <- cycleway_overlap(aml_osm_cycleways, aml_municipality_cycleways)
# osm_lacking_cycleways <- subset(aml_municipality_cycleways, osm_overlap <= 80)
# mapview(osm_lacking_cycleways, color="red") + mapview(aml_osm_cycleways, color="darkblue")
