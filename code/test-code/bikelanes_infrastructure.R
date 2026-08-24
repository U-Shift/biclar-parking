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

# OSM cycleway data
osm_cyclelanes <- st_read("/data/IMPT/mobility/cycle_network_class.gpkg") |> st_transform(4326)
nrow(osm_cyclelanes)
summary(osm_cyclelanes)
#mapview(osm_cyclelanes)

osm_protected_cyclelanes <- osm_cyclelanes[osm_cyclelanes$cycle_segregation != "Advisory lane",]
mapview(osm_protected_cyclelanes)

