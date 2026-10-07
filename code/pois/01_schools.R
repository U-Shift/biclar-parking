# Adapted from https://github.com/U-Shift/MQAT/blob/main/code/dataprep/schools_lx.R

library(sf)
library(tidyverse)

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

# Group by total students
# Each "agrupamento" (CÓDIGO_DGEEC_AGRUPAMENTO) has multiple schools
# Each school (ENTIDADE, CÓDIGO_DGEEC_ESCOLA) has multiple rows, one per NÍVEL_DE__ENSINO, ANO_DE_ESCOLARIDADE, SEXO (Homens, Mulheres)
# Students number is given by NÚMERO_DE_ALUNOS_MATRICULADOS

schools_students_n <- students_n |>
    filter(
        DISTRITO %in% c("Lisboa", "Setúbal"),
        NÍVEL_DE__ENSINO != "Educação pré-escolar"
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

# 2. Get school locations
# No matching ID, match using NOME, CONCELHO (= ESCOLA, MUNICÍPIO)
schools_locations <- read.csv("data/raw/escolas_gesedu/RedeEscolar.csv") |>
    filter(
        DISTRITO %in% c("Lisboa", "Setúbal"),
        !(CICLO %in% c("Pré-escolar", "Extra-escolar")),
        !is.na(x)
    ) |>
    sf::st_as_sf(coords = c("x", "y"), crs = 3763) |>
    sf::st_filter(municipalities |> st_transform(3763))
# View(schools_locations)

nrow(schools_students_n) # 1355
nrow(schools_locations) # 1310
schools_locations_students <- schools_locations |>
    left_join(schools_students_n, by = c("NOME" = "ESCOLA", "CONCELHO" = "MUNICÍPIO")) |>
    filter(!is.na(N_ESTUDANTES)) |>
    select(DISTRITO, CONCELHO, NOME, TIPOLOGIA, N_ESTUDANTES)

nrow(schools_locations_students) # 1120
nrow(schools_locations_students) / nrow(schools_locations) # 85.5%

mapview::mapview(schools_locations_students, zcol = "N_ESTUDANTES")

missing <- schools_locations |>
    left_join(schools_students_n, by = c("NOME" = "ESCOLA", "CONCELHO" = "MUNICÍPIO")) |>
    filter(is.na(N_ESTUDANTES))
table(missing$CONCELHO)
table(missing$CICLO)

sf::st_write(schools_locations, "data/base/pois/schools_students.gpkg")

# Aggregate at grid
grid_schools <- grid |>
    st_transform(4326) |>
    st_join(schools_locations_students |> st_transform(4326), join = st_intersects) |>
    group_by(gid) |>
    summarise(
        n_students = sum(N_ESTUDANTES, na.rm = TRUE),
        n_schools = n_distinct(NOME, na.rm = TRUE)
    )
mapview::mapview(grid_schools |> filter(n_schools!=0), zcol = "n_students")
mapview::mapview(grid_schools |> filter(n_schools!=0), zcol = "n_schools")

sf::st_write(grid_schools, "data/processed/poi/schools/grid_schools_students.gpkg")
write.csv(grid_schools |> st_drop_geometry(), "data/processed/poi/schools/grid_schools_students.csv")
