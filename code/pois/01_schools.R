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
#
# To use Google Maps Geocoding as a fallback (3rd resource):
# 1. Go to Google Cloud Console (https://console.cloud.google.com/) and create/select a project.
# 2. Enable billing (Google provides a recurring $200 monthly credit, covering ~40,000 requests/month).
# 3. Enable the "Geocoding API" under APIs & Services > Library.
# 4. Create an API key under APIs & Services > Credentials (optionally restrict it to "Geocoding API").
# 5. Add the key to your environment: run `usethis::edit_r_environ()` or create ~/.Renviron or .Renviron:
#    GOOGLE_MAPS_KEY="AIzaSy..."
# 6. Restart your R session so Sys.getenv("GOOGLE_MAPS_KEY") picks it up.

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

# 3. Geocode missing schools using OSM (Nominatim) with ArcGIS and Google Maps fallback
source("code/pois/00_geocode_helpers.R")

setubal_munis <- c("Alcochete", "Almada", "Barreiro", "Moita", "Montijo", "Palmela", "Seixal", "Sesimbra", "Setúbal")

logr::log_open()
missing_geocoded <- missing_students_locations |>
    st_drop_geometry() |>
    mutate(
        geocoded_coord = purrr::map2(ESCOLA, MUNICÍPIO, geocode_school, .progress = "Geocoding (OSM + ArcGIS + Google)")
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
nrow(missing_geocoded) # 197
nrow(missing_geocoded) / nrow(missing_students_locations) # 94.71%

# 4. Manually add schools not found in OSM
manual_schools <- tribble(
    ~ESCOLA, ~MUNICÍPIO, ~lat, ~lon,
    "Centro de Apoio à Família Crescer para Integrar", "Cascais", 38.705033, -9.353837,
    "Jardim de Infância A Palmeira", "Seixal", 38.628089, -9.122673,
    "Jardim de Infância do Posto Assistência Social da Malveira", "Mafra", 38.933525, -9.257217,
    "C. Comunitário Promoção Soc. Feijó e Laranjeiro", "Almada", 38.655155, -9.160754,
    "Oficina da Phantasia", "Almada", 38.642740, -9.229696,
    "Casa da Infância de Santa Quitéria - Fundação Dom Pedro Iv", "Lisboa", 38.717319, -9.158061,
    "Centro Infantil O Patronato (Centro Assist.Paroq. da Amora)", "Seixal", 38.628587, -9.115810,
    "C. Paroq. B. Estar Social de Arrentela - Jardim Infantil O Picapau", "Seixal", 38.638248, -9.096375,
    "Jardim de Infância da Unidade Integrada de Atendimento à Distância", "Sesimbra", 38.471107, -9.094717,
    "Os Pirilampos (Associação Central Assist. de Setúbal)", "Setúbal", 38.527288, -8.882930,
    "Jardim de Infância da Associação de Actividades de Tempos Livres de Bolonha", "Vila Franca de Xira", 38.859700, -9.076168
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
) 

nrow(schools_locations_students_final) # 1837
# View(schools_locations_students_final |> group_by(CONCELHO, NOME) |> summarise(n = n())) # Find duplicates

nrow(schools_locations_students_final) - nrow(schools_students_n) # 0
nrow(schools_locations_students_final) / nrow(schools_students_n) # 100%
# View(schools_locations_students_final |> st_transform(4326))


# 5. Associate locations to schools disaggregated by level 
schools_locations_students_by_level_original <- students_n |>
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
        TIPOLOGIA, NATUREZA,
        # Grade
        NÍVEL_DE__ENSINO
    ) |> summarise(
        N_ESTUDANTES = sum(NÚMERO_DE_ALUNOS_MATRICULADOS)
    ) |>
    mutate(
        # Remove quotes from text on ESCOLA
        ESCOLA = gsub('"', "", ESCOLA)
    )

nrow(schools_locations_students_by_level_original) # 2724

schools_locations_students_by_level = schools_locations_students_by_level_original|>
    select(CONCELHO = MUNICÍPIO, NOME = ESCOLA, NIVEL = NÍVEL_DE__ENSINO, N_ESTUDANTES) |>
    left_join(schools_locations_students_final |> select(CONCELHO, NOME, geometry)) |>
    filter(!sf::st_is_empty(geometry)) |>
    sf::st_as_sf() |> 
    mutate(
        # School type, for minimum rations
        secondary_school = case_when(
            grepl("secundário", NIVEL) ~ TRUE,
            TRUE ~ FALSE
        ),
        primary_school = case_when(
            grepl("secundário", NIVEL) ~ FALSE,
            grepl("básico", NIVEL) ~ TRUE,
            TRUE ~ FALSE
        ),
        childcare = case_when(
            grepl("secundário", NIVEL) ~ FALSE,
            grepl("básico", NIVEL) ~ FALSE,
            grepl("pré-escolar", NIVEL) ~ TRUE,
            TRUE ~ FALSE
        )
    )

testthat::expect_equal(schools_locations_students_by_level |> filter(primary_school & secondary_school & childcare) |> nrow(), 0)
testthat::expect_equal(schools_locations_students_by_level |> filter(primary_school | secondary_school | childcare) |> nrow(), nrow(schools_locations_students_by_level))
testthat::expect_equal(sum(schools_locations_students_by_level$N_ESTUDANTES), sum((students_n |>
    filter(
        DISTRITO %in% c("Lisboa", "Setúbal"),
        MUNICÍPIO %in% municipalities_meta$name,
        !is.na(TIPOLOGIA),
        !is.na(ESCOLA)
    ))$NÚMERO_DE_ALUNOS_MATRICULADOS))

nrow(schools_locations_students_by_level) # 2724
nrow(schools_locations_students_by_level) / nrow(schools_locations_students_by_level_original) # 100%

mapview::mapview(schools_locations_students_by_level, zcol = "N_ESTUDANTES") +
    mapview::mapview(schools_locations_students_by_level |> filter(secondary_school), zcol = "N_ESTUDANTES", hide = TRUE) +
    mapview::mapview(schools_locations_students_by_level |> filter(primary_school), zcol = "N_ESTUDANTES", hide = TRUE) + 
    mapview::mapview(schools_locations_students_by_level |> filter(childcare), zcol = "N_ESTUDANTES", hide = TRUE)

sf::st_write(schools_locations_students_by_level, "data/processed/poi/schools/schools_students.gpkg", delete_dsn = TRUE)

# Aggregate at grid
grid_schools <- grid |>
    st_transform(4326) |>
    st_join(schools_locations_students_by_level |> st_transform(4326), join = st_intersects) |>
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
