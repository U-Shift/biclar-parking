# Introduction -----------------------------
# 02_schools_univ_agg.R
#
# NOTE: This script is different from "99_poi_extraction.R" due to the databases being neither CMet or OSM,
#       and due to the necessity of an extra attribute compared to the other POIs: number of students.
#
# Script purposes:
#   - Process data from code/pois/01_schools.R (regarding pre-schools, primary and secondary schools).
#   - Get data for universities, from DGEEC (https://www.dgeec.medu.pt/api/ficheiros/68f89bf9c82132f5733ead7a).
#   - This dataset includes the number of students enrolled in universities in the school year 2024/2025.
#   - Data for universities was turned into .csv and is available at data/raw/schools_universities/Universities.csv
#
# Code steps:

library(sf)
library(dplyr)
library(tidyverse)


# 1. Process school data -----------------------------------------------

# cm_facilities <- cm_facilities |>
#   mutate(category = case_when(
#     category == "schools" & str_detect(name, "Jardim de Infância") ~ "childcare",
#     category == "schools" & str_detect(name, "Jardim De Infância") ~ "childcare",
#     category == "schools" & str_detect(name, "Jardim Escola") ~ "childcare",
#     category == "schools" & str_detect(name, "Jardim Infantil") ~ "childcare",
#     category == "schools" & str_detect(name, "Centro Infantil") ~ "childcare",
#     category == "schools" & str_detect(name, "Creche") ~ "childcare",
#     category == "schools" & str_detect(name, "Universidade") ~ "college_university",
#     category == "schools" & str_detect(name, "Instituto") ~ "college_university",
#     category == "schools" & str_detect(name, "Escola Superior") ~ "college_university",
#     TRUE ~ category
#   )) |>
#   mutate(subcategory = case_when(
#     category == "schools" & str_detect(name, "Escola Primária") ~ "primary",
#     category == "schools" & str_detect(name, "Escola Básica") ~ "primary",
#     category == "schools" & str_detect(name, "Colégio") ~ "primary",
#     category == "schools" & str_detect(name, "Externato") ~ "primary",
#     category == "schools" & str_detect(name, "Escola Secundária") ~ "secondary",
#     category == "schools" & str_detect(name, "Escola Profissional") ~ "secondary",
#     category == "schools" & str_detect(name, "Escola Artística") ~ "secondary",
#     TRUE ~ subcategory # schools whose names include none of these keywords will have subcategory ~ NA
#   )
# )


# 2. Get data for universities -----------------------------------------

univ_students_raw <- read.csv("data/raw/schools_universities/Universities.csv", skip=2, check.names = FALSE,
                          fileEncoding = "Latin1") # this way accents and special characters appear in the headers

#View(univ_students)
#names(univ_students)


## 2.1 Aggregate number of students by faculty, rename columns -----------

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



