# Adapted from https://github.com/U-Shift/MQAT/blob/main/code/dataprep/schools_lx.R

library(sf)
library(tidyverse)
library(osmdata)
library(logr) # install.packages("logr")

source("code/00_data_load.R")

# If missing rJava dependency when running read_xlsx, run the commands below:
# $ sudo apt update & sudo apt install default-jdk
# $ sudo R CMD javareconf
# $ find /usr/lib/jvm -name "libjvm.so" # Replace output in the next command
# > dyn.load('/usr/lib/jvm/java-21-openjdk-amd64/lib/server/libjvm.so', local = FALSE)

# 1. Get number of students for each school
students_n_file <- tempfile(fileext = ".xlsx")
download.file(url = "https://www.dgeec.medu.pt/api/ficheiros/6a3e81a11a35c6289d788ae6", destfile = students_n_file, mode = "wb")
students_n <- readxl::read_xlsx(students_n_file)
names(students_n) <- gsub(" ", "_", names(students_n)) # Replace spaces in col names by "_" to make easier to use variables

# View(students_n)
names(students_n)
table(students_n$ANO_LETIVO)
table(students_n$NATUREZA)
table(students_n$TIPOLOGIA)
table(students_n$NÍVEL_DE__ENSINO)

length(unique(students_n$CÓDIGO_DGEEC_ESCOLA)) # 7648
unique(students_n$MUNICÍPIO)

# Group by total students
# Each "agrupamento" (CÓDIGO_DGEEC_AGRUPAMENTO) has multiple schools
# Each school (ENTIDADE, CÓDIGO_DGEEC_ESCOLA) has multiple rows, one per NÍVEL_DE__ENSINO, ANO_DE_ESCOLARIDADE, SEXO (Homens, Mulheres)
# Students number is given by NÚMERO_DE_ALUNOS_MATRICULADOS

schools_students_n <- students_n |>
    filter(
        DISTRITO %in% c("Lisboa", "Setúbal"),
        MUNICÍPIO %in% municipalities_meta$name,
        !is.na(TIPOLOGIA),
        !is.na(ESCOLA)
    ) |>
    group_by(
        # Location
        MUNICÍPIO,
        # School identifier
        CÓDIGO_DGEEC_AGRUPAMENTO, CÓDIGO_DGEEC_ESCOLA, ESCOLA,
        # School category
        TIPOLOGIA, NATUREZA
        # Grade
        # NÍVEL_DE__ENSINO, ANO_DE_ESCOLARIDADE
    ) |>
    summarise(
        N_ESTUDANTES = sum(NÚMERO_DE_ALUNOS_MATRICULADOS),
        # List NÍVEL_DE__ENSINO, ANO_DE_ESCOLARIDADE
        NIVEIS = paste(unique(NÍVEL_DE__ENSINO), collapse = ", "),
        ANOS_DE_ESCOLARIDADE = paste(unique(ANO_DE_ESCOLARIDADE), collapse = ", ")
    ) |>
    mutate(
        # Remove quotes from text on ESCOLA
        ESCOLA = gsub('"', "", ESCOLA)
    )
# View(schools_students_n)
unique(schools_students_n$MUNICÍPIO)
length(unique(schools_students_n$MUNICÍPIO))
testthat::expect_equal(sort(unique(schools_students_n$MUNICÍPIO)), sort(municipalities_meta$name))

# 2. Get school locations
# No matching ID, match using NOME, CONCELHO (= ESCOLA, MUNICÍPIO)
schools_locations <- read.csv("data/raw/schools_universities/RedeEscolar.csv") |>
    filter(
        DISTRITO %in% c("Lisboa", "Setúbal"),
        CONCELHO %in% municipalities_meta$name,
        !is.na(x)
    ) |>
    sf::st_as_sf(coords = c("x", "y"), crs = 3763) |>
    sf::st_filter(municipalities |> st_transform(3763))
# View(schools_locations)

unique(schools_locations$CONCELHO)
length(unique(schools_locations$CONCELHO))
testthat::expect_equal(sort(unique(schools_locations$CONCELHO)), sort(municipalities_meta$name))

nrow(schools_students_n) # 1837
nrow(schools_locations) # 1936
schools_locations_students <- schools_locations |>
    left_join(schools_students_n, by = c("NOME" = "ESCOLA", "CONCELHO" = "MUNICÍPIO")) |>
    filter(!is.na(N_ESTUDANTES)) |>
    select(DISTRITO, CONCELHO, NOME, TIPOLOGIA, N_ESTUDANTES, NIVEIS)

nrow(schools_locations_students) # 1629
nrow(schools_locations_students) / nrow(schools_students_n) # 88.67%

mapview::mapview(schools_locations_students, zcol = "N_ESTUDANTES")

missing_students_locations <- schools_students_n |>
    left_join(schools_locations, by = c("ESCOLA" = "NOME", "MUNICÍPIO" = "CONCELHO")) |>
    filter(sf::st_is_empty(geometry))
nrow(missing_students_locations) # 208
table(missing_students_locations$NATUREZA)
table(missing_students_locations$TIPOLOGIA)
table(missing_students_locations$NIVEIS)

# 3. Geocode missing schools using OSM (Nominatim) with ArcGIS fallback
query_osm_school <- function(escola, municipio) {
    Sys.sleep(1) # Respect Nominatim 1 req/sec rate limit
    q <- paste(escola, municipio, "Portugal", sep = ", ")

    res <- tryCatch(
        suppressWarnings(osmdata::getbb(q, format_out = "data.frame")),
        error = function(e) NULL
    )

    if (is.null(res) || !is.data.frame(res) || nrow(res) == 0) {
        return(NULL)
    }

    # Filter to school amenities to avoid false positives (e.g. bus stops or street names)
    if ("class" %in% names(res) && any(res$class == "amenity")) {
        res <- res |> filter(class == "amenity", type %in% c("school", "college", "kindergarten"))
    }

    # If multiple matches exist, prefer 'node' over 'way' / 'relation' when it resolves unambiguously
    if (nrow(res) > 1 && "osm_type" %in% names(res) && any(res$osm_type == "node")) {
        res_nodes <- res |> filter(osm_type == "node")
        if (nrow(res_nodes) == 1) {
            res <- res_nodes
        }
    }

    # Only keep if there is a single unambiguous result
    if (nrow(res) == 1) {
        logr::log_info(sprintf("OSM match found for school %s in %s", escola, municipio))
        return(tibble(
            lat = as.numeric(res$lat[1]),
            lon = as.numeric(res$lon[1])
        ))
    }
    NULL
}

query_arcgis_school <- function(escola, municipio) {
    Sys.sleep(0.2)
    q <- paste(escola, municipio, "Portugal", sep = ", ")
    url <- paste0(
        "https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/findAddressCandidates?SingleLine=",
        URLencode(q),
        "&f=json&maxLocations=1&outFields=Addr_type,Type,Score,Match_addr"
    )

    res <- tryCatch(
        jsonlite::fromJSON(url),
        error = function(e) NULL
    )

    if (!is.null(res$candidates) && is.data.frame(res$candidates) && nrow(res$candidates) > 0) {
        cand <- res$candidates[1, ]
        addr_type <- if ("attributes" %in% names(cand) && "Addr_type" %in% names(cand$attributes)) {
            cand$attributes$Addr_type[1]
        } else if ("attributes.Addr_type" %in% names(cand)) {
            cand$attributes.Addr_type[1]
        } else {
            ""
        }
        score <- cand$score[1]

        # Only accept confident POI or street address matches (excludes broad municipality/locality matches)
        if (score >= 80 && addr_type %in% c("POI", "PointAddress", "StreetAddress")) {
            logr::log_info(sprintf("ArcGIS match found for school %s in %s (score: %.1f, type: %s)",
                                   escola, municipio, score, addr_type))
            return(tibble(
                lat = as.numeric(cand$location$y[1]),
                lon = as.numeric(cand$location$x[1])
            ))
        }
    }
    NULL
}

geocode_school <- function(escola, municipio) {
    # 1. Try OSM first
    res <- query_osm_school(escola, municipio)
    if (!is.null(res)) return(res)

    # 2. Fallback to ArcGIS
    res_arcgis <- query_arcgis_school(escola, municipio)
    if (!is.null(res_arcgis)) return(res_arcgis)

    logr::log_warning(sprintf("No match found (OSM & ArcGIS) for school %s in %s", escola, municipio))
    NULL
}

setubal_munis <- c("Alcochete", "Almada", "Barreiro", "Moita", "Montijo", "Palmela", "Seixal", "Sesimbra", "Setúbal")

logr::log_open()
missing_geocoded <- missing_students_locations |>
    st_drop_geometry() |>
    mutate(
        geocoded_coord = purrr::map2(ESCOLA, MUNICÍPIO, geocode_school, .progress = "Geocoding (OSM + ArcGIS)")
    ) |>
    tidyr::unnest(geocoded_coord) |>
    filter(!is.na(lat), !is.na(lon)) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    st_transform(3763) |>
    transmute(
        DISTRITO = ifelse(MUNICÍPIO %in% setubal_munis, "Setúbal", "Lisboa"),
        CONCELHO = MUNICÍPIO,
        NOME = ESCOLA,
        TIPOLOGIA = TIPOLOGIA,
        NIVEIS = NIVEIS,
        N_ESTUDANTES = N_ESTUDANTES
    )
logr::log_close()
nrow(missing_geocoded) # 153
nrow(missing_geocoded) / nrow(missing_students_locations) # 73.56%

# 4. Manually add schools not found in OSM
manual_schools <- tribble(
    ~ESCOLA, ~MUNICÍPIO, ~lat, ~lon,
    "Centro de Apoio à Família Crescer para Integrar", "Cascais", 38.705033, -9.353837,
    "Jardim de Infância A Palmeira", "Seixal", 38.628089, -9.122673,
    "Jardim de Infância do Posto Assistência Social da Malveira", "Mafra", 38.933525, -9.257217
)

manual_geocoded <- missing_students_locations |>
    st_drop_geometry() |>
    anti_join(missing_geocoded, by = c("ESCOLA" = "NOME", "MUNICÍPIO" = "CONCELHO")) |>
    inner_join(manual_schools, by = c("ESCOLA", "MUNICÍPIO")) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    st_transform(3763) |>
    transmute(
        DISTRITO = ifelse(MUNICÍPIO %in% setubal_munis, "Setúbal", "Lisboa"),
        CONCELHO = MUNICÍPIO,
        NOME = ESCOLA,
        TIPOLOGIA = TIPOLOGIA,
        NIVEIS = NIVEIS,
        N_ESTUDANTES = N_ESTUDANTES
    )

# Add recovered schools to schools_locations_students
schools_locations_students_final <- bind_rows(
    schools_locations_students,
    missing_geocoded,
    manual_geocoded
) |> mutate(
    # School type, for minimum rations
    secondary_school = case_when(
        grepl("secundário", NIVEIS) ~ TRUE,
        TRUE ~ FALSE
    ),
    primary_school = case_when(
        grepl("secundário", NIVEIS) ~ FALSE,
        grepl("básico", NIVEIS) ~ TRUE,
        TRUE ~ FALSE
    ),
    childcare = case_when(
        grepl("secundário", NIVEIS) ~ FALSE,
        grepl("básico", NIVEIS) ~ FALSE,
        grepl("pré-escolar", NIVEIS) ~ TRUE,
        TRUE ~ FALSE
    )
)
testthat::expect_equal(schools_locations_students_final |> filter(primary_school & secondary_school & childcare) |> nrow(), 0)
testthat::expect_equal(schools_locations_students_final |> filter(primary_school | secondary_school | childcare) |> nrow(), nrow(schools_locations_students_final))

nrow(schools_locations_students_final) # 1785
nrow(schools_locations_students_final) - nrow(schools_students_n) # 52
nrow(schools_locations_students_final) / nrow(schools_students_n) # 97.16%
# View(schools_locations_students_final |> st_transform(4326))

mapview::mapview(schools_locations_students_final, zcol = "N_ESTUDANTES") +
    mapview::mapview(schools_locations_students_final |> filter(secondary_school), zcol = "N_ESTUDANTES", hide = TRUE) +
    mapview::mapview(schools_locations_students_final |> filter(primary_school), zcol = "N_ESTUDANTES", hide = TRUE) + 
  mapview::mapview(schools_locations_students_final |> filter(childcare), zcol = "N_ESTUDANTES", hide = TRUE)


sf::st_write(schools_locations_students_final, "data/processed/poi/schools/schools_students.gpkg", delete_dsn = TRUE)

# Aggregate at grid
grid_schools <- grid |>
    st_transform(4326) |>
    st_join(schools_locations_students_final |> st_transform(4326), join = st_intersects) |>
    group_by(gid) |>
    summarise(
        n_students = sum(N_ESTUDANTES, na.rm = TRUE),
        n_schools = n_distinct(NOME, na.rm = TRUE),
        n_students_childcare = sum(N_ESTUDANTES * childcare, na.rm = TRUE),
        n_students_primary_school = sum(N_ESTUDANTES * primary_school, na.rm = TRUE),
        n_students_secondary_school = sum(N_ESTUDANTES * secondary_school, na.rm = TRUE),
        n_schools_childcare = sum(childcare, na.rm = TRUE),
        n_schools_primary_school = sum(primary_school, na.rm = TRUE),
        n_schools_secondary_school = sum(secondary_school, na.rm = TRUE)
    )

mapview::mapview(grid_schools |> filter(n_schools != 0), zcol = "n_students") +
    mapview::mapview(grid_schools |> filter(n_schools != 0), zcol = "n_schools", hide = TRUE) +
    mapview::mapview(grid_schools |> filter(n_schools != 0), zcol = "n_schools_primary_school", hide = TRUE) +
    mapview::mapview(grid_schools |> filter(n_schools != 0), zcol = "n_schools_secondary_school", hide = TRUE) + 
    mapview::mapview(grid_schools |> filter(n_schools != 0), zcol = "n_schools_childcare", hide = TRUE)

sf::st_write(grid_schools, "data/processed/poi/schools/grid_schools_students.gpkg", delete_dsn = TRUE)
write.csv(grid_schools |> st_drop_geometry(), "data/processed/poi/schools/grid_schools_students.csv")
