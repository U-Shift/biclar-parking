library(dplyr)
library(sf)
library(mapview)
library(units)

# TML PMUS Database -------------------------------------------------------

## Bike parking spaces (Only Lisbon!) -------------------------------------------------------

lisbon_parking_points <- st_read("https://geoportal.tmlmobilidade.pt/ogc-api/collections/cml_ciclovia_estacionamento/items?f=json&limit=1000") |>
  bind_rows(st_read("https://geoportal.tmlmobilidade.pt/ogc-api/collections/cml_ciclovia_estacionamento/items?f=json&limit=1000&offset=1000")) |>
  bind_rows(st_read("https://geoportal.tmlmobilidade.pt/ogc-api/collections/cml_ciclovia_estacionamento/items?f=json&limit=1000&offset=2000"))
nrow(lisbon_parking_points) # 2209
summary(lisbon_parking_points)

summary(lisbon_parking_points$CAPACIDADE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     NAs
# 2.000   6.000   9.000   8.621  10.000  66.000      14

mapview(st_zm(lisbon_parking_points), zcol="CAPACIDADE")

## Shared mobility parking spaces -------------------------------------------------------
# aml_shared_mobility_open <- st_read("https://geoportal.tmlmobilidade.pt/ogc-api/collections/modos_partilhados_pontos_partilha_aml/items?f=json&limit=1000") |>
#   bind_rows(st_read("https://geoportal.tmlmobilidade.pt/ogc-api/collections/modos_partilhados_pontos_partilha_aml/items?f=json&limit=1000&offset=1000")) |>
#   bind_rows(st_read("https://geoportal.tmlmobilidade.pt/ogc-api/collections/modos_partilhados_pontos_partilha_aml/items?f=json&limit=1000&offset=2000"))
# nrow(aml_shared_mobility_open) # 2907
# summary(aml_shared_mobility_open)

aml_shared_mobility <- st_read("/data/IMPT/BaseDados_PMMUS/11-ModosPartilhados/11 - ModosPartilhados.gpkg", layer = "Pontos-partilha_amL") |>
  st_transform(4326)
nrow(aml_shared_mobility) # 2907
summary(aml_shared_mobility)
mapview(st_zm(aml_shared_mobility), zcol="N_Lugares")
mapview(st_zm(aml_shared_mobility), zcol="Tipologia")
mapview(st_zm(aml_shared_mobility), zcol="Modo")

summary(aml_shared_mobility$N_Lugares)
summary(aml_shared_mobility$Tipologia)

table(aml_shared_mobility$Concelho)
# Alcochete    Almada   Cascais    Lisboa   Montijo    Oeiras   Palmela
# 2       129       148      2039        42       206       341
table(aml_shared_mobility$Tipologia)
# Doca   Doca virtual   Doca Virtual Estacionamento
# 164            381           1311           1051
table(aml_shared_mobility$N_Lugares)
# 10  11  12  13  17  19  20   3   4   5   6   8   9
# 381   7   6  28   2   1   2   5   8  22   6  12 359
table(aml_shared_mobility$Modo)
# Bicicleta  Bicicleta e Trotinet Bicicleta e trotineta Bicicleta e Trotineta        Bicicleta GIRA             Motociclo             Trotineta
# 13                   129                   343                   837                   151                  1051                    42
# Trotinetas
# 341

## Bike parking at interfaces (with demand) -------------------------------------------------------

# Demand data, from PMMUS. Details, from Relatório Técnico da Caracterização e Diagnóstico (Pag 321)

# Durante os trabalhos de campo realizados, foi feito o levantamento da ocupaçǎo dos parques
# de estacionamento de 102 interfaces de transportes, correspondendo a 67% do total de
# interfaces. Na totalidade destas interfaces, em média observou-se 69% de ocupaçao.
# Observando a procura por escaloes de ocupaçāo (Quadro 7.9), verifica-se que 26% destes
# parques estao a menos de metade da capacidade. Exatamente 3 parques estao com metade
# dos lugares ocupados e 51% dos parques estao com ocupaçǎo acima da metade sem estarem
# completos. Há 20 parques que se encontravam totalmente cheios.

# Dos parques com muito baixa ocupaçǎo, inferior a 10%, há 2 que sǎo gratuitos. Um associa-se
# à interface de Foros da Amora, a qual tem também associado um parque pago com 95% de
# ocupaçāo, o que pode indicar haver insegurança por parte dos passageiros em estacionar
# aqui, ou simplesmente por estar um pouco mais afastado. O outro é um dos dois parques
# associados à interface de Mira Sintra – Meleças, onde ambos sǎo gratuitos e tēm baixa
# procura.
# Os parques totalmente lotados sāo na sua maioria gratuitos, destacando-se très pagos,
# nomeadamente o parque associado à estaçǎo da Amadora, onde nǎo existe mais nenhum,
# tendo este uma capacidade relativamente baixa (l41 lugares). Outro é um dos cinco parques
# associados à interface de Algés. O terceiro é o da estaçāo de Caxias que é o único parque
# associado e com pouca capacidade (66 lugares).
# Refere-se que o parque de estacionamento de Castanheira do Ribatejo, também totalmente
# lotado, lida com o fenómeno de vinda de utilizadores de fora da amL e que utilizam apenas o
# passe navegante®. Esta questǎo pōe-se também no acesso pedonal deficitário à interface,
# inclusive vindo do território próximo do limite da amL.
# Na figura seguinte apresenta-se a comparaçāo entre a oferta e procura nestes l02 parques
# levantados.

# Junto a algumas interfaces identificou-se estacionamento ilegal. É de destacar especialmente
# as interfaces do Terminal da Várzea em Setúbal e da Portela de Sintra em Sintra, onde este
# estacionamento ilegal ocorre devido à lotaçǎo máxima dos parques associados que sǎo
# gratuitos.
# Já na interface da Coina, no Seixal, verifica-se uma quantidade excessiva de estacionamento
# ilegal mesmo estando os dois parques abaixo da metade de ocupaçao (43% e 23%). De
# salientar que estes parques sǎo tarifados.

aml_bike_parking_interfaces <- st_read("/data/IMPT/BaseDados_PMMUS/10-ModosAtivos/Fig10_8_Estacionamento_Bicicletas_Interfaces.gpkg", layer = "Interfaces-amL_W2G_02_EstacBici_capacidade") |>
  mutate(
    Oferta = as.integer(Oferta),
    PROCURA = as.integer(PROCURA),
    occupation = round(PROCURA/Oferta*100, digits=2)
  )
nrow(aml_bike_parking_interfaces) # 263
summary(aml_bike_parking_interfaces)
mapview(st_zm(aml_bike_parking_interfaces), zcol="X_capacidad")
mapview(st_zm(aml_bike_parking_interfaces), zcol="Oferta")
mapview(st_zm(aml_bike_parking_interfaces), zcol="PROCURA")
mapview(st_zm(aml_bike_parking_interfaces |> filter(!is.na(occupation))), zcol="occupation")
mapview(st_zm(aml_bike_parking_interfaces), zcol="X_conforto")

table(aml_bike_parking_interfaces$tipo)
# Estação de Metro  Estação Ferroviária Interface intermodal          Paragem TCR  Terminal Rodoviário
# 47                   27                   66                  106                   12
summary(aml_bike_parking_interfaces$Oferta)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     NAs
# 21.0   149.5   343.0   504.7   626.5  2667.0     200
summary(aml_bike_parking_interfaces$PROCURA)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     NAs
# 0.00   80.75  202.50  268.72  333.00 1618.00     199
summary(aml_bike_parking_interfaces$occupation)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     NAs
# 0.00   45.25   70.00   67.95   96.64  100.09     200
table(aml_bike_parking_interfaces$tipo)
# Estação de Metro  Estação Ferroviária Interface intermodal          Paragem TCR  Terminal Rodoviário
# 47                   27                   66                  106                   12
table(aml_bike_parking_interfaces$Município)
# Alcochete              Almada             Amadora            Barreiro             Cascais              Lisboa              Loures
# 4                  25                  14                   8                  14                  55                  22
# Mafra               Moita             Montijo            Odivelas              Oeiras             Palmela              Seixal
# 17                   6                   7                   8                  12                  10                  11
# Sesimbra             Setúbal              Sintra Vila Franca de Xira  Vila Franca e Xira
# 4                  13                  21                  10                   2


# OSM ---------------------------------------------------------------------

# https://wiki.openstreetmap.org/wiki/Tag:amenity%3Dbicycle_parking

# Create overpassturbo for nodes[amenity="bicycle_parking"]
library(osmdata)
aml = sf::st_read("https://github.com/U-Shift/MQAT/raw/refs/heads/main/geo/MUNICIPIOSgeo.gpkg", quiet = TRUE)
aml_bbox <- st_bbox(aml)
mapview(aml) + mapview(aml_bbox)

q <- opq(aml_bbox) |>
  add_osm_feature(key="amenity", value="bicycle_parking")

set_overpass_url("https://overpass-api.de/api/interpreter")
osm_parking <- osmdata_sf(q)
summary(osm_parking)
osm_parking_points <- osm_parking$osm_points |> mutate(capacity = as.integer(capacity))
summary(osm_parking_points)
nrow(osm_parking_points)
osm_parking_pygons <- osm_parking$osm_polygons |> mutate(capacity = as.integer(capacity))
summary(osm_parking_pygons)
nrow(osm_parking_pygons)
mapview(osm_parking_points, zcol="capacity") + mapview(osm_parking_pygons, zcol="capacity")


# MUBI: Cidade Ciclável ---------------------------------------------------
# https://cidadeciclavel.mubi.pt/
# Data scraped from their API, using Bruno (github.com/U-Shift/MobilityDataPT/commit/4bfc4fc8c3bf84f09da69f7f3ea4595ffd5baf25)

## Existing infrastructure ---------------------------------------------------
mubi_parking_points <- jsonlite::fromJSON("/data/biclar-parking/raw/mubi_cidadeciclavel/bicicletarios.json") |>
  as_tibble() |>
  mutate(
    lat = as.numeric(lat),
    lng = as.numeric(lng),
    slots = as.integer(slots),
    reviews = as.integer(reviews),
    average = as.numeric(average),
    views = as.integer(views)
  ) |>
  st_as_sf(coords = c("lng", "lat"), crs=4326)
nrow(mubi_parking_points) # 3774
# Filter by those inside aml
mubi_parking_points <- st_join(mubi_parking_points, aml |> select(Concelho), join = st_within) |> filter(!is.na(Concelho))
nrow(mubi_parking_points) # 2465

mapview(mubi_parking_points |> select(id, text, description, slots), zcol="slots", layer.name="Parking slots")
mapview(mubi_parking_points |> select(id, text, description, reviews), zcol="reviews", layer.name="Nr of reviews")
mapview(mubi_parking_points |> select(id, text, description, average), zcol="average", layer.name="Average evaluation")

summary(mubi_parking_points$slots)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     NAs
# 0.000   4.000   8.000   8.196  10.000  64.000     259
summary(mubi_parking_points$classification)
table(mubi_parking_points$classification)
# biciparque    hotspot    regular
# 8        334       1394
table(mubi_parking_points$structureType)
# deroda      grade     grelha          m      other      trave uinvertido
# 241         14         33         66         67          7        692
table(mubi_parking_points$Concelho)
# Almada             Amadora            Barreiro             Cascais              Lisboa              Loures               Mafra
# 120                   9                  11                  33                2050                  17                   3
# Moita             Montijo            Odivelas              Oeiras             Palmela              Seixal             Setúbal
# 14                   8                  17                  52                   9                  17                  49
# Sintra Vila Franca de Xira
# 40                  16



## Infrastructure requests ---------------------------------------------------
mubi_parking_requests <- jsonlite::fromJSON("/data/biclar-parking/raw/mubi_cidadeciclavel/pedidos.json") |>
  as_tibble() |>
  mutate(
    lat = as.numeric(lat),
    lng = as.numeric(lng),
    views = as.integer(views),
    support = as.integer(support)
  ) |>
  filter(!is.na(lat) & !is.na(lng)) |>
  st_as_sf(coords = c("lng", "lat"), crs=4326)
nrow(mubi_parking_requests) # 947
mubi_parking_requests <- st_join(mubi_parking_requests, aml |> select(Concelho), join = st_within) |> filter(!is.na(Concelho))
nrow(mubi_parking_requests) # 721

mapview(mubi_parking_requests |> select(id, text, description, support) |> filter(support>0), zcol="support", layer.name="Nr of supports")
summary(mubi_parking_requests$support)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.000   1.000   2.000   2.368   3.000  35.000


# Aggregation of OSM and MUBi data ---------------------------------------

## Consolidation of OSM data (turning polygons into points)
osm_parking_centroids <- st_centroid(osm_parking_pygons)
osm_parking_beta <- bind_rows(osm_parking_points, osm_parking_centroids)
osm_parking_beta <- osm_parking_beta[!is.na(osm_parking_beta$amenity),] # removing vertices from polygon objects (which were in "osm_parking_points" without any data)
osm_parking_beta <- osm_parking_beta |> select(osm_id, bicycle_parking, capacity, covered, geometry)


#osm_parking_beta_filter <- osm_parking_beta |> filter(!is.na(capacity))
#mapview(osm_parking_beta, zcol="capacity")
#mapview(osm_parking_beta_filter, zcol="capacity")

# Fields for aggregating OSM and MUBi data
# OSM -> osm_id, bicycle_parking (tipo de estrutura), capacity, covered, geometry; adicionar municipio
# MUBi -> id, structureType, isCovered, slots, Concelho, geometry
# Final -> data_source, ID, structure_type, capacity, is_covered, municipality, geometry

# Add municípios
osm_parking_final <- st_join(osm_parking_beta, aml, join = st_within)

# Standardizing column names
osm_parking_final <- osm_parking_final |> rename(
  ID = osm_id,
  municipality = Concelho,
  structure_type = bicycle_parking,
  is_covered = covered
)
osm_parking_final <- osm_parking_final |> mutate(ID = as.integer(ID)) #id as integer and not string
osm_parking_final <- osm_parking_final |> mutate(is_covered = ifelse(is_covered == "yes", TRUE, FALSE)) #is_covered as boolean and not string
mapview(osm_parking_final, zcol="capacity")

mubi_parking_final <- mubi_parking_points |> select(id, structureType, isCovered, slots, geometry, Concelho)
mubi_parking_final <- mubi_parking_final |> rename(
  ID = id,
  municipality = Concelho,
  structure_type = structureType,
  is_covered = isCovered,
  capacity = slots
)
mubi_parking_final <- mubi_parking_final |> mutate(ID = as.integer(ID)) #id as integer and not string
mapview(mubi_parking_final, zcol="capacity")

total_parking_beta <- bind_rows(osm_parking_final=osm_parking_final, mubi_parking_final=mubi_parking_final, .id="data_source")
mapview(total_parking_beta, zcol="capacity")


## Function to remove duplicates -------------------------------------------
# Methodology:
# Euclidean buffer of radius "distance_threshold", created for each parking location
# If any point other than the original exists inside this buffer, the points with one of the following characteristics are removed:
# OR no information on the points' capacity (i.e. "capacity == NA")
# OR lower or equal capacity than the studied point
# given that both OSM and MUBi databases had a lot of duplicate points (even within themselves)
# it makes sense to keep only one parking location inside each buffer, without much regard to the point's source


remove_duplicates <- function(points, distance_threshold, capacity_column) {
  removal_list <- logical(nrow(points))
  distance_threshold <- units::set_units(distance_threshold, "m")

  for (i in 1:nrow(points)) {
    if (removal_list[i]) next
    distances <- st_distance(points[i, ], points)
    distances_n <- as.numeric(distances)
    nearby_points <- which(distances<distance_threshold & distances>units::set_units(0,"m"))

    if (length(nearby_points)>0) {
        nearby_capacities <- st_drop_geometry(points[nearby_points, ])[capacity_column]
        na_points <- nearby_points[is.na(nearby_capacities)] #points with NA capacity inside buffer
        lower_capacity <- nearby_points[which(nearby_capacities <= points[[capacity_column]][i] & !is.na(nearby_capacities))] #points with lower capacity than the evaluated one
        points_to_remove <- c(na_points, lower_capacity)
        removal_list[points_to_remove] <- TRUE
    }
  }
  return(points[!removal_list, ])
}

total_parking_final <- remove_duplicates(total_parking_beta, distance_threshold=25, capacity_column="capacity")
mapview(total_parking_final, zcol="capacity")

### Benchmarks for different distance thresholds --------------------------
# distance_threshold        nrow(total_parking_final)     difference from previous
#         5m                          3043                           -
#         10m                         2804                          239
#         15m                         2653                          151
#         20m                         2549                          104
#         25m                         2482                          67
#         30m                         2425                          57
#         35m                         2350                          75
#         40m                         2291                          59
#         45m                         2232                          59
#         50m                         2175                          57
#         55m                         2110                          65
#         60m                         2035                          75
#         65m                         1960                          75
#         70m                         1912                          48
#         75m                         1869                          43
#         100m                        1639

#write_sf(osm_parking_final, "/data/biclar-parking/base/existing_parking/aml_osm_parking.gpkg")
#write_sf(mubi_parking_final, "/data/biclar-parking/base/existing_parking/aml_mubi_parking.gpkg")
write_sf(total_parking_final, "/data/biclar-parking/base/existing_parking/aml_total_parking.gpkg")


## Function to compare different data sources (i.e. points that are lacking in one) --------
lacking_points <- function(base_source, check_source, distance_threshold) { #distance_threshold should be the same as in the function "remove duplicates"
  removal_list <- logical(nrow(base_source))
  distance_threshold <- units::set_units(distance_threshold, "m")
  points_to_remove <- c()

  for (i in 1:nrow(base_source)) {
    if (removal_list[i]) next
    distances <- st_distance(base_source[i, ], check_source)
    distances_n <- as.numeric(distances)
    nearby_points <- which(distances<distance_threshold & distances>units::set_units(0,"m"))

    if (length(nearby_points)>0) {
      points_to_remove <- c(points_to_remove, i)
      removal_list[points_to_remove] <- TRUE
    }
  }
  return(base_source[!removal_list, ])
}

osm_lacking_points <- lacking_points(remove_duplicates(mubi_parking_final, distance_threshold=25, capacity_column="capacity"), osm_parking_final, distance_threshold=25)
mapview(osm_parking_final, col.regions="darkblue") + mapview(osm_lacking_points, col.regions="red")




## Reviews of existing infrastructure  ---------------------------------------------------
# To be analysed, stored at /data/biclar-parking/raw/mubi_cidadeciclavel/reviews.json


