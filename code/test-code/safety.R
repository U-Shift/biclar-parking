library(dplyr)
library(sf)
library(mapview)

# TML PMUS Database -------------------------------------------------------

## Safety ------------------------------------------------------------------
# Adapted from https://github.com/U-Shift/IMPT-data/blob/main/code/04_safety.R
aml_safety <- st_read("/data/IMPT/BaseDados_PMMUS/13-SegurançaRodoviária/13-SegurançaRodoviária.gpkg", layer = "ANSR_AML_sinistralidade_02 - IG") |>
  mutate(
    vitimas_mortais30 = as.integer(X_VM.30d),
    feridos_graves30 = as.integer(X_FG.30d),
    feridos_ligeiros30 = as.integer(X_FL.30d),
    total_vitimas30 = vitimas_mortais30 + feridos_graves30 + feridos_ligeiros30,
    veh_motorizado = X_Veículo + X_Veícul_1 + X_Ciclomoto + X_Desconhec + X_Máquina + X_Motociclo + X_Motocic_1 + X_Quadricic + X_Triciclo + X_Veícul_2 + X_Veícul_3 + X_Veícul_4,
    veh_bicicleta = as.integer(X_Velocípe) + as.integer(X_Velocí_1),
    veh_peoes = as.integer(Total.Peõ),
    total_veh = veh_motorizado + veh_bicicleta + veh_peoes,
    localidades_dentro = case_when(Localizaç == "Dentro das localidades" ~ 1, TRUE ~ 0),
    # localidades_fora = case_when(Localizaç== "Fora das localidades" ~ 1, TRUE ~ 0),
    noite = case_when(grepl("Noite", Luminosida, ignore.case = TRUE) ~ 1, TRUE ~ 0)
  )

summary(aml_safety)
summary(aml_safety$veh_bicicleta)

mapview(aml_safety |> filter(veh_bicicleta>0), zcol="veh_bicicleta")
mapview(aml_safety |> filter(veh_bicicleta>0 & vitimas_mortais30>0), zcol="vitimas_mortais30")

