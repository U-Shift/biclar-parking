

##   - School data (pre-school, basic and secondary) taken from "biclar-parking/code/pois/01_schools.R
##   - University data taken from DGEEC (https://www.dgeec.medu.pt/art/ensino-superior/bases-de-dados/todas/652ff89abd5c2b00958292d7) - inscritos no ano letivo 2024/2025


# 3. Process school data -----------------------------------------------

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


# 4. Get data for universities -----------------------------------------
