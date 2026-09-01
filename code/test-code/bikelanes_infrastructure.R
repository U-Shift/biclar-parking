library(dplyr)
library(sf)
library(mapview)

# TML PMUS Database -------------------------------------------------------

## Bike network (existant + planned) ------------------------------------------------------------

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

mapview(aml_cyclelanes_existant, color="darkgreen") + mapview(aml_cyclelanes_planned, color="orange")

# OSM cycleway data -------------------------------------------------------
osm_cyclelanes <- st_read("/data/IMPT/mobility/cycle_network_class.gpkg") |> st_transform(4326)
nrow(osm_cyclelanes)
summary(osm_cyclelanes)
#mapview(osm_cyclelanes)

#osm_protected_cyclelanes <- osm_cyclelanes[osm_cyclelanes$cycle_segregation != "Advisory lane",]
#mapview(osm_protected_cyclelanes)


# Municipality Data -------------------------------------------------------
parishes <- read_sf("/data/biclar-parking/geo/CAOP_GLPS_LAND_UNIQUE_dtmnfr.gpkg")

## Aggregate by municipality -----------------------------------------------
municipalities <- parishes |>
  group_by(municipio) |>
  summarize(
    dtmn = first(substr(dtmnfr, 1, 4)),
    nuts3_cod = first(nuts3_cod),
    nuts3 = first(nuts2),
    area_ha = sum(area_ha),
    geom = st_union(geom)
  )

## Get cycleway data for every available municipality ----------------------
list_cycleways = list.files(path = "/data/biclar-parking/raw/cycling_network", full.names=TRUE)

# aml_mun_cycleways <- list()
# for (file in list_cycleways) {
#   mun_name <- gsub("\\..*$", "", basename(file))
#   mun <- st_read(file) |> mutate(municipality = mun_name)
#   mun <- st_as_sf(mun)
#   aml_mun_cycleways[[file]] <- mun
# }

# Alcochete_cycleways
# Almada_cycleways
# Amadora_cycleways
# Barreiro_cycleways
Cascais_cycleways <- st_read(list_cycleways[1]) |> mutate(municipality = gsub("/data/biclar-parking/raw/cycling_network/|\\..*$", "", list_cycleways[1]))
Lisboa_cycleways <- st_read(list_cycleways[2]) |> mutate(municipality = gsub("/data/biclar-parking/raw/cycling_network/|\\..*$", "", list_cycleways[2]))
# Loures_cycleways
# Mafra_cycleways
# Moita_cycleways
# Montijo_cycleways
# Odivelas_cycleways
# Oeiras_cycleways
# Palmela_cycleways
Seixal_cycleways <- st_read(list_cycleways[3]) |> mutate(municipality = gsub("/data/biclar-parking/raw/cycling_network/|\\..*$", "", list_cycleways[3]))
# Sesimbra_cycleways
# Setúbal_cycleways
# Sintra_cycleways
# VFX_cycleways


## Standardize cycleway data ---------------------------------------------
### CRS (Standardize as 4326) --------------------------------------------
Cascais_cycleways <- Cascais_cycleways |> st_transform(4326)
Lisboa_cycleways <- Lisboa_cycleways |> st_transform(4326)
Seixal_cycleways <- Seixal_cycleways |> st_transform(4326)

### Only existing network ---------------------------------------------------------------
Seixal_cycleways <- Seixal_cycleways[!is.na(Seixal_cycleways$Data_Inau), ] # Inauguration date has to exist


### Fields ------------------------------------------------------------------
# Keep fields "name", "cycle_segregation", "direction", "geom", "municipality"
Cascais_cycleways <- Cascais_cycleways |>
  select(tipo, designacao, geometry, municipality) |>
  rename(cycle_segregation = tipo, name = designacao, geom = geometry)

Lisboa_cycleways <- Lisboa_cycleways |>
  select(DESIGNACAO, TIPOLOGIA, SHAPE, municipality) |>
  rename(name = DESIGNACAO, cycle_segregation = TIPOLOGIA, geom = SHAPE)

Seixal_cycleways <- Seixal_cycleways |>
  select(SEGMENTO_I, TIPOLOGIA, geom, municipality) |>
  rename(name = SEGMENTO_I, cycle_segregation = TIPOLOGIA)

# Standardize cycle_segregation
Cascais_cycleways <- Cascais_cycleways |>
    mutate(cycle_segregation = recode(cycle_segregation,
                                      "Corredor ciclável" = "Cycle track or lane",
                                      "Pista ciclável" = "Cycle track or lane",
                                      "Via banalizada" = "Advisory lane",
                                      "Trilhos ciclaveis" = "Protected Active"
                                      )
      )

Lisboa_cycleways <- Lisboa_cycleways |>
  mutate(cycle_segregation = recode(cycle_segregation,
                                    "30+Bici" = "Advisory lane",
                                    "Zona de Coexistência" = "Advisory lane",
                                    "Pista Ciclável Unidirecional" = "Cycle track or lane",
                                    "Pista Ciclável Bidirecional" = "Cycle track or lane",
                                    "Faixa Ciclável" = "Cycle track or lane",
                                    "Contrassentido" = "Cycle track or lane",
                                    "Trilho" = "Protected Active",
                                    "Percurso Ciclopedonal" = "Protected Active"
                                    )
  )

Seixal_cycleways <- Seixal_cycleways |>
  mutate(cycle_segregation = recode(cycle_segregation,
                                    "Misto com automóveis (rede viária)" = "Advisory lane",
                                    "Faixa ciclável" = "Cycle track or lane",
                                    "Pista Ciclável" = "Cycle track or lane",
                                    "Misto com peões" = "Protected Active"
                                    )
  )


## Join Municipalities -----------------------------------------------------
aml_municipality_cycleways <- rbind(Cascais_cycleways, Lisboa_cycleways, Seixal_cycleways)
mapview(aml_municipality_cycleways)

### SEIXAL não está a aparecer depois de fazer bind, e não encontro o erro.
### não só aparece se for corrido isoladamente, como aparece no qgis se exportar o ficheiro

write_sf(aml_municipality_cycleways, "/data/biclar-parking/base/existing_cycleways/aml_municipality_cycleways.gpkg")
