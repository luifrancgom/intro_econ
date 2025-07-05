library(tidyverse)
library(gt)
library(janitor)
library(scales)

# https://www.dane.gov.co/ > 
# ANDA | ARCHIVO NACIONAL DE DATOS > 
# Microdatos > 
# Economía > 
# Industria > 
# Encuesta Anual Manufacturera – EAM - 2022 > 
# Obtener Microdatos

# Import
manufac_tbl <- read_csv(file = "000_data/001_encuesta_anual_manufacturera_2022.csv") |>  
  clean_names()

# Tidy
manufac_tbl_clean <- manufac_tbl |> 
  select(
    # Identificador empresa
    nordemp, 
    # Identificador establecimiento
    nordest,
    # Clasificación Industrial Internacional 
    # Uniforme Rev. 4. A.C.
    ciiu4, 
    # Año EAM
    periodo,
    # Total personal promedio ocupado en el año - mujeres
    c4r4c9t,
    # Total personal promedio ocupado en el año - hombres
    c4r4c10t,
    # Total en cantidades de energía eléctrica consumida (Kwh)
    c5r1c4,
    # Producción bruta
    prodbr2,
    # Consumo intermedio
    consin2,
    # Valor Agregado: Producción bruta - consumo intermedio 
    valagri)

manufac_tbl |> 
  select(nordest,
         consin2,
         consmate,
         c3r14c1,
         c3r15c1,
         c3r16c1,
         c3r17c1,
         c3r13c1,
         c3r18c1,
         c3r21c1,
         c3r36c1,
         totalv,
         c3r23c1,
         c3r42c1,
         c3r19c3,
         c3r8c3) |>
  rowwise(nordest) |> 
  mutate(con_inter = sum(c_across(c3r14c1:c3r8c3))) |> 
  ungroup() |> 
  select(consin2, con_inter) |> 
  filter(consin2 != con_inter)

manufac_tbl |> 
  select(prodbr2, prodbind) |> 
  mutate(comp = prodbr2 >= prodbind)

