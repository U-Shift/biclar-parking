library(dplyr)
library(sf)
library(mapview)

# TML PMUS Database -------------------------------------------------------

## Shared mobility parking spaces -------------------------------------------------------
aml_shared_mobility <- st_read("/data/IMPT/BaseDados_PMMUS/11-ModosPartilhados/11 - ModosPartilhados.gpkg", layer = "Pontos-partilha_amL") |>
  st_transform(4326)
nrow(aml_shared_mobility) # 2907
summary(aml_shared_mobility)
mapview(st_zm(aml_shared_mobility), zcol="N_Lugares")
mapview(st_zm(aml_shared_mobility), zcol="Tipologia")
mapview(st_zm(aml_shared_mobility), zcol="Modo")

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
osm_parking_pygons <- osm_parking$osm_polygons |> mutate(capacity = as.integer(capacity))
summary(osm_parking_pygons)

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


## Reviews of existing infrastructure  ---------------------------------------------------
# To be analysed, stored at /data/biclar-parking/raw/mubi_cidadeciclavel/reviews.json


