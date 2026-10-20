library(sf)
library(dplyr)
library(mapview)

source("code/00_data_load.R")
source("code/00_helpers.R")

# Parameters --------------------------------------------------
INTERFACE_BUFFER_M <- 150
OUTPUT_FOLDER <- "data/analysis/ticketing"

# Load TML interfaces --------------------------------------------------
interfaces <- st_read("https://geoportal.tmlmobilidade.pt/ogc-api/collections/interfaces_interfaces_hierarquia_aml/items?f=json&limit=1000&offset=0") |>
    mutate(
        Interface = trimws(tolower(gsub("[()]", "", Interface))),
        # Split interface by "+", creating list of rows
        interface_list = strsplit(Interface, "\\+"),
        # For each element, trim leading/trailing whitespace
        interface_list = lapply(interface_list, function(x) trimws(x))
    ) |>
    mutate(cod_interf = row_number())

View(interfaces |> st_drop_geometry())
nrow(interfaces) # 267

mapview(interfaces |> select(cod_interf, designacao, hierarq, tipo, municipio), zcol = "tipo")

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
base_folder <- "data/raw/TML_validacoes/06-05-2026 Todos Operadores"
ticketing_sources <- list.files(base_folder, pattern = "\\.zip$", full.names = TRUE)

validation_types <- c("Entry Transaction")

ticketing_raw <- data.frame()
operators <- data.frame(
    id = numeric(),
    name = character()
)

# For each zip, load CSVs inside to ticketing_raw
for (z in ticketing_sources) {
    message(sprintf("\nScrapping %s...", z))
    # List CSV files inside the zip
    files_in_zip <- unzip(z, list = TRUE)$Name
    csv_files <- files_in_zip[grepl("\\.csv$", files_in_zip, ignore.case = TRUE)]

    ticketing_zip <- data.frame()
    for (csv_file in csv_files) {
        # Read the CSV file directly from the zip using a connection
        con <- unz(z, csv_file)
        lines <- readLines(con, encoding = "latin1")
        utf8_lines <- iconv(lines, from = "latin1", to = "UTF-8")
        temp_df <- read.csv(text = utf8_lines, sep = ";", dec = ",", stringsAsFactors = FALSE)
        # Bind to the main data frame
        ticketing_zip <- bind_rows(ticketing_zip, temp_df)
    }
    # Log relevant parameters
    message("Unique entry-exits: ", paste(unique(ticketing_zip$ENTRY_EXIT), collapse = ", "))
    message("Names: ", paste(names(ticketing_zip), collapse = ", "))

    ticketing_raw <- bind_rows(ticketing_raw, ticketing_zip |>
        filter(ENTRY_EXIT %in% validation_types) |>
        select(X_COORDINATE, Y_COORDINATE, Codigo_Entidade, DataHora) |>
        rename(lat = X_COORDINATE, lon = Y_COORDINATE, operator_id = Codigo_Entidade) |>
        mutate(
            # Convert DataHora (06/05/2026 00:07) to timestamp
            timestamp = as.POSIXct(DataHora, format = "%d/%m/%Y %H:%M")
        ) |>
        select(-DataHora))

    operators <- bind_rows(operators, data.frame(
        id = unique(ticketing_zip$Codigo_Entidade),
        name = unique(ticketing_zip$Nome_Entidade)
    ))
}

nrow(ticketing_raw) # 2124810
table((ticketing_raw |> left_join(operators, by = c("operator_id" = "id")))$name)
ticketing_sf <- ticketing_raw |>
    filter(!is.na(lat), !is.na(lon)) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    mutate(id = row_number())
nrow(ticketing_sf) # 2101672 (-23138 Total, -18750 Carris, -3806 CP)
table((ticketing_sf |> left_join(operators, by = c("operator_id" = "id")))$name)
table(ticketing_sf$operator_id)

mapview(
    ticketing_sf |> sample_n(100) |> left_join(operators, by = c("operator_id" = "id")),
    zcol = "name",
    layer.name = "Entry validations"
)

operators$mode <- NA_character_
operators[operators$name == "Barraqueiro Transportes", ]$mode <- "bus"
operators[operators$name == "Carris", ]$mode <- "bus"
operators[operators$name == "Cascais Próxima", ]$mode <- "bus"
operators[operators$name == "Área 1", ]$mode <- "bus"
operators[operators$name == "Área 2", ]$mode <- "bus"
operators[operators$name == "Área 3", ]$mode <- "bus"
operators[operators$name == "Área 4", ]$mode <- "bus"
operators[operators$name == "Transportes Colectivos do Barreiro", ]$mode <- "bus"
operators[operators$name == "CP", ]$mode <- "train"
operators[operators$name == "Fertagus", ]$mode <- "train"
operators[operators$name == "Metropolitano", ]$mode <- "metro"
operators[operators$name == "Metro Transportes do Sul", ]$mode <- "LTR"
operators[operators$name == "Transtejo", ]$mode <- "boat"
operators

ticketing_sf <- ticketing_sf |> left_join(operators |> select(id, mode), by = c("operator_id" = "id"))
table(ticketing_sf$mode)

# Overlap ticketing and interfaces --------------------------------------------------

interfaces_buffer <- interfaces |>
    select(-interface_list) |>
    st_transform(3763)

# Generate Voronoi cells to split overlaps fairly between nearest points
points_union <- st_union(interfaces_buffer)
voronoi_geom <- st_voronoi(points_union)
voronoi_sf <- st_collection_extract(voronoi_geom, "POLYGON") |> st_as_sf()

# Match each interface point to its corresponding Voronoi cell
cell_indices <- st_within(interfaces_buffer, voronoi_sf) |> as.integer()
voronoi_polys <- st_geometry(voronoi_sf)[cell_indices]

# Buffer the interfaces by INTERFACE_BUFFER_M meters
buffers <- st_buffer(interfaces_buffer, INTERFACE_BUFFER_M)

# Intersect each buffer with its corresponding Voronoi cell (element-wise)
clean_geoms <- Map(st_intersection, st_geometry(buffers), voronoi_polys)

# Assign the clipped geometries back and transform to 4326
st_geometry(interfaces_buffer) <- st_sfc(clean_geoms, crs = 3763)
interfaces_buffer <- interfaces_buffer |> st_transform(4326)

mapview(interfaces_buffer)

ticketing_sf_joined <- st_join(
    ticketing_sf |> select(id),
    interfaces_buffer |> select(cod_interf),
    join = st_within
)

interface_ticketing <- ticketing_sf_joined |>
    st_drop_geometry() |>
    filter(!is.na(cod_interf)) |>
    group_by(cod_interf) |>
    summarise(n = n())

interface_ticketing_sf <- interfaces |>
    select(cod_interf) |>
    left_join(interface_ticketing) |>
    mutate(n = ifelse(!is.na(n), n, 0))

mapview(
    interface_ticketing_sf,
    zcol = "n",
    layer.name = sprintf("Number of validations within %dm", INTERFACE_BUFFER_M)
)

# Overlap ticketing and grid --------------------------------------------------

# > All tickets
ticketing_in_grid <- st_join(
    ticketing_sf |> select(id) |> rename(tid = id),
    grid |> select(gid),
    join = st_within
)
ticketing_in_grid <- ticketing_in_grid |> left_join(
    ticketing_sf |> st_drop_geometry() |> select(id, mode),
    by = c("tid" = "id")
)
ticketing_in_grid |> sample_n(10)
mapview(ticketing_in_grid |> filter(is.na(gid)))

grid_ticketing_global <- ticketing_in_grid |>
    st_drop_geometry() |>
    filter(!is.na(gid)) |>
    group_by(gid) |>
    summarise(
        n = n(),
        n_bus = sum(mode == "bus", na.rm = TRUE),
        n_train = sum(mode == "train", na.rm = TRUE),
        n_metro = sum(mode == "metro", na.rm = TRUE),
        n_ltr = sum(tolower(mode) == "ltr", na.rm = TRUE),
        n_boat = sum(mode == "boat", na.rm = TRUE)
    )

sum(grid_ticketing_global$n)
mapview(grid |> left_join(grid_ticketing_global), zcol = "n")
# mapview(grid |> left_join(grid_ticketing_global) |> filter(n_bus>0), zcol = "n_bus")
# mapview(grid |> left_join(grid_ticketing_global) |> filter(n_train>0), zcol = "n_train")
# mapview(grid |> left_join(grid_ticketing_global) |> filter(n_metro>0), zcol = "n_metro")
# mapview(grid |> left_join(grid_ticketing_global) |> filter(n_ltr>0), zcol = "n_ltr")
# mapview(grid |> left_join(grid_ticketing_global) |> filter(n_boat>0), zcol = "n_boat")

write.csv(
    grid_ticketing_global,
    sprintf("%s/grid_ticketing_global.csv", OUTPUT_FOLDER),
    row.names = FALSE
)

# > Tickets at interfaces

interfaces_in_grid <- st_join(
    interfaces |> select(cod_interf),
    grid |> select(gid),
    join = st_within
)

grid_ticketing_interfaces <- interfaces_in_grid |>
    st_drop_geometry() |>
    filter(!is.na(cod_interf)) |>
    left_join(interface_ticketing_sf |> st_drop_geometry()) |>
    group_by(gid) |>
    summarise(n = sum(n))

sum(grid_ticketing_interfaces$n)
mapview(grid |> left_join(grid_ticketing_interfaces), zcol = "n")

write.csv(
    grid_ticketing_interfaces,
    sprintf("%s/grid_ticketing_interfaces.csv", OUTPUT_FOLDER),
    row.names = FALSE
)
