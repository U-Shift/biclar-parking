library(sf)
library(dplyr)
library(tidyverse)

source("code/00_data_load.R")
source("code/pois/00_geocode_helpers.R")

# 1. Get data for universities -----------------------------------------

univ_students_raw <- read.csv("data/raw/schools_universities/Universities.csv",
  skip = 2, check.names = FALSE,
  fileEncoding = "Latin1"
) # this way accents and special characters appear in the headers

nrow(univ_students_raw) # 173642
# View(univ_students_raw[0:100, ])
# View(univ_students_raw |> sample_n(100))
# names(univ_students_raw)


## 1.1 Aggregate number of students by faculty -----------
univ_students <- univ_students_raw |>
  filter(
    Distrito %in% c("Lisboa", "Setúbal")
  ) |>
  group_by(
    # Code of faculty
    `Código da unidade orgânica`
  ) |>
  summarise(
    `Código do estabelecimento de ensino` = first(`Código do estabelecimento de ensino`),
    UNIVERSIDADE = first(`Estabelecimento de ensino`),
    FACULDADE = first(`Unidade orgânica`),
    CONCELHO = first(Município),
    N_ESTUDANTES = sum(N)
  ) |>
  rename(
    COD_UNIVERSIDADE = `Código do estabelecimento de ensino`,
    COD_FACULDADE = `Código da unidade orgânica`
  )

nrow(univ_students) # 92
# View(univ_students)

## 1.2 Geocode faculty locations -------------------------

logr::log_open("poi_universities.log")
univ_students_geocoded <- univ_students |>
  mutate(
    geocoded_coord = purrr::map2(
      FACULDADE,
      CONCELHO,
      geocode_poi,
      .progress = "Geocoding faculties (OSM + ArcGIS + Google)"
    )
  ) |>
  tidyr::unnest(geocoded_coord, keep_empty = TRUE)
logr::log_close()

nrow(univ_students_geocoded) # 92
nrow(univ_students_geocoded |> filter(is.na(lat), is.na(lon))) # 3
# View(univ_students_geocoded |> filter(is.na(lat), is.na(lon)))

# Convert automated geocoded faculties to sf spatial points
univ_students_sf <- univ_students_geocoded |>
  filter(!is.na(lat), !is.na(lon)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(3763)


## 1.3 Manually add faculties not found by automated geocoding --------

manual_univ <- tribble(
  ~FACULDADE, ~CONCELHO, ~lat, ~lon,
  "Instituto Superior de Estudos Interculturais e Transdisciplinares de Almada", "Almada", 38.6664657, -9.176506,
  "Escola Superior de Saúde Atlântica", "Oeiras", 38.741990, -9.284957,
  "Instituto Politécnico Jean Piaget do Sul - Escola Superior de Tecnologia e Gestão Jean Piaget", "Almada", 38.6664657, -9.176506
)

manual_geocoded <- univ_students_geocoded |>
  filter(is.na(lat), is.na(lon)) |>
  select(-lat, -lon) |>
  inner_join(manual_univ, by = c("FACULDADE", "CONCELHO")) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(3763)

# Extend results with manual geocoded faculties
univ_students_sf <- bind_rows(
  univ_students_sf,
  manual_geocoded
)

nrow(univ_students_sf) # 92
nrow(univ_students_sf) / nrow(univ_students) # 100%
View(univ_students_sf)

dir.create("data/processed/poi/universities", recursive = TRUE, showWarnings = FALSE)
sf::st_write(univ_students_sf, "data/processed/poi/universities/universities_students.gpkg", delete_dsn = TRUE)


# 2. Aggregate at grid --------------------------------------------------

grid_universities <- grid |>
  st_transform(4326) |>
  st_join(univ_students_sf |> st_transform(4326), join = st_intersects) |>
  group_by(gid) |>
  summarise(
    n_students = sum(N_ESTUDANTES, na.rm = TRUE),
    n_faculties = n_distinct(FACULDADE, na.rm = TRUE),
    n_universities = n_distinct(UNIVERSIDADE, na.rm = TRUE)
  )

testthat::expect_equal(sum(grid_universities$n_students), sum(univ_students$N_ESTUDANTES))

mapview::mapview(grid_universities |> filter(n_faculties != 0), zcol = "n_students") +
  mapview::mapview(grid_universities |> filter(n_faculties != 0), zcol = "n_faculties", hide = TRUE)

sf::st_write(grid_universities, "data/processed/poi/universities/grid_universities_students.gpkg", delete_dsn = TRUE)
write.csv(grid_universities |> st_drop_geometry(), "data/processed/poi/universities/grid_universities_students.csv")



