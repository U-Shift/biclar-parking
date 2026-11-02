library(sf)
library(dplyr)
library(mapview)

source("code/00_data_load.R")
source("code/00_helpers.R")

# Parameters --------------------------------------------------
OUTPUT_FOLDER <- "data/analysis/ticketing"

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
        select(X_COORDINATE, Y_COORDINATE, Codigo_Entidade, Nome_Paragem, DataHora) |>
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

# Attach operator names to raw validations
ticketing_raw <- ticketing_raw |>
    left_join(operators |> distinct(), by = c("operator_id" = "id"))

nrow(ticketing_raw) # 2124810
table(ticketing_raw$name)

ticketing_missing_coordinates <- ticketing_raw |> filter(is.na(lat), is.na(lon))
nrow(ticketing_missing_coordinates) # 23138
nrow(ticketing_missing_coordinates) / nrow(ticketing_raw) # 1.08%
nrow(ticketing_raw |> filter(!is.na(lat), !is.na(lon))) / nrow(ticketing_raw) # 98.91%
table(ticketing_missing_coordinates$name)
# Carris                       CP Metro Transportes do Sul                  
# 18750                     3806                      582

# Recover missing coordinates from GTFS stops.txt -------------------------
gtfs_dir <- "data/analysis/ticketing/gtfs"
dir.create(gtfs_dir, recursive = TRUE, showWarnings = FALSE)

gtfs_feeds <- list(
    "Carris" = list(
        url = "https://gateway.carris.pt/gateway/gtfs/api/v2.8/GTFS",
        local = file.path(gtfs_dir, "Carris.zip")
    ),
    "CP" = list(
        url = "https://publico.cp.pt/gtfs/gtfs.zip",
        local = file.path(gtfs_dir, "CP.zip")
    ),
    "Metro Transportes do Sul" = list(
        url = "https://mts.pt/imt/MTS-20240129.zip",
        local = file.path(gtfs_dir, "MTS.zip")
    )
)

read_gtfs_stops <- function(feed_info, op_name) {
    zip_path <- feed_info$local
    if (!file.exists(zip_path)) {
        tryCatch({
            download.file(feed_info$url, zip_path, mode = "wb", quiet = TRUE)
        }, error = function(e) {
            warning(sprintf("Could not download GTFS for %s from %s: %s", op_name, feed_info$url, conditionMessage(e)))
        })
    }
    if (file.exists(zip_path)) {
        con <- unz(zip_path, "stops.txt")
        stops <- read.csv(con, stringsAsFactors = FALSE)
        stops |>
            select(stop_name, stop_lat, stop_lon) |>
            group_by(stop_name) |>
            slice(1) |>
            ungroup() |>
            mutate(operator_name = op_name)
    } else {
        data.frame(stop_name = character(), stop_lat = numeric(), stop_lon = numeric(), operator_name = character())
    }
}

gtfs_stops_all <- bind_rows(lapply(names(gtfs_feeds), function(op_name) {
    read_gtfs_stops(gtfs_feeds[[op_name]], op_name)
}))

# Match missing coordinates by operator name and Nome_Paragem (first match)
ticketing_raw <- ticketing_raw |>
    left_join(
        gtfs_stops_all,
        by = c("name" = "operator_name", "Nome_Paragem" = "stop_name")
    ) |>
    mutate(
        lat = coalesce(lat, stop_lat),
        lon = coalesce(lon, stop_lon)
    ) |>
    select(-stop_lat, -stop_lon)

# Identify validations that remain without coordinates
ticketing_missing_coordinates_gtfs_fix_still_missing <- ticketing_raw |>
    filter(is.na(lat), is.na(lon))
nrow(ticketing_missing_coordinates_gtfs_fix_still_missing) # 4959
table(ticketing_missing_coordinates_gtfs_fix_still_missing$name)
#     Carris                       CP Metro Transportes do Sul 
# 571                     3806                      582 

unmatched_stops_summary <- ticketing_missing_coordinates |>
    group_by(name, Nome_Paragem) |>
    summarise(validations = n(), .groups = "drop") |>
    arrange(name, desc(validations))
print(unmatched_stops_summary, n = 50)

ticketing_sf <- ticketing_raw |>
    filter(!is.na(lat), !is.na(lon)) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    mutate(id = row_number())
nrow(ticketing_sf) # 2119851
nrow(ticketing_sf) / nrow(ticketing_raw) # 99.766%
table(ticketing_sf$name)
table(ticketing_sf$operator_id)

mapview(
    ticketing_sf |> sample_n(100),
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

sum(grid_ticketing_global$n) # 2117346
testthat::expect_equal(sum(grid_ticketing_global$n), nrow(ticketing_sf) - nrow(ticketing_in_grid |> filter(is.na(gid))))
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
