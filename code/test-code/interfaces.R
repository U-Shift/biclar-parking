library(dplyr)
library(sf)
library(mapview)

# From AML Open Data: https://geoportal.tmlmobilidade.pt/ogc-api/collections/interfaces_interfaces_hierarquia_aml
interfaces_hirerachy <- st_read("https://geoportal.tmlmobilidade.pt/ogc-api/collections/interfaces_interfaces_hierarquia_aml/items?f=json&limit=2000")
nrow(interfaces_hirerachy) # 267
summary(interfaces_hirerachy)

table(interfaces_hirerachy$tipo)
# Estação de Metro  Estação Ferroviária Interface intermodal          Paragem TCR  Terminal Rodoviário
# 47                   27                   66                  106                   12
table(interfaces_hirerachy$hierarq)
# 1   2   3   4   5
# 113  68  62  21   3

mapview(interfaces_hirerachy, zcol="tipo")
mapview(interfaces_hirerachy |> mutate(hierarq = as.character(hierarq)), zcol="hierarq")
