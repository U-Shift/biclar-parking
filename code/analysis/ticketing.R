library(sf)
library(dplyr)
library(mapview)

# Load TML interfaces --------------------------------------------------
interfaces <- st_read("https://geoportal.tmlmobilidade.pt/ogc-api/collections/interfaces_interfaces_hierarquia_aml/items?f=json&limit=1000&offset=0") |>
    mutate(
        Interface = trimws(tolower(gsub("[()]", "", Interface))),
        # Split interface by "+", creating list of rows
        interface_list = strsplit(Interface, "\\+"),
        # For each element, trim leading/trailing whitespace
        interface_list = lapply(interface_list, function(x) trimws(x))
    )

View(interfaces |> st_drop_geometry())
nrow(interfaces) # 267

mapview(interfaces, zcol = "tipo")

# > Extend data with flags on modes covered
table(unlist(interfaces$interface_list))

# (interfaces[1, ])$interface_list

interfaces <- interfaces |> mutate(
    # Modes / Facilities
    airplane = grepl("avião", Interface),
    boat = grepl("barco", Interface),
    bike_sharing = sapply(interface_list, function(x) any(grepl("bike-sharing", x))),
    train = grepl("comboio", Interface),
    tram = grepl("elétrico", Interface),
    car_parking = sapply(interface_list, function(x) any(x == "estacionamento" | x == "p&r")),
    bike_parking = sapply(interface_list, function(x) any(grepl("estacionamento.*bicicletas", x))),
    dropoff_pickup = sapply(interface_list, function(x) any(grepl("tomada e largada|tom e larg", x))),
    metro = grepl("metro|metros", Interface),
    park_and_ride = grepl("p&r", Interface),
    bus_stop = grepl("paragem.*tcr", Interface),
    bus_terminal = grepl("terminal.*tcr", Interface),
    bus_tcr = grepl("tcr", Interface),
    taxi = grepl("táxis", Interface)
)
View(interfaces |> st_drop_geometry())

# mapview(interfaces |> select(-interface_list), zcol="bus_tcr")
# mapview(interfaces |> select(-interface_list), zcol="metro")
# mapview(interfaces |> select(-interface_list), zcol="bike_parking")
# mapview(interfaces |> select(-interface_list), zcol="dropoff_pickup")
# mapview(interfaces |> select(-interface_list), zcol="tram")
# mapview(interfaces |> select(-interface_list), zcol="boat")

# Load ticketing data --------------------------------------------------
# TODO!
