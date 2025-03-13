# Libraries ----
library(tidyverse)

# Import ----
## GEIH module Ocupados 
employed <- read_delim(file = "000_data/009_geih/ocupados.csv", 
                       delim = ";", 
                       col_types = cols(
                         P6880S1 = col_character(),
                         P3055S1 = col_double(),
                         P6780S1 = col_character(),
                         P7028S1 = col_character()
                       )
                      )

### Check possible problems
employed |> problems()

## GEIH module caracteristicas generales seguridad social en salud y educacion
social_security_health_education <- read_delim(file = "000_data/009_geih/caracteristicas_generales_seguridad_social_en_salud_y_educacion.csv") |>
  # Eliminate duplicate columns
  select(-c(MES, PER, HOGAR, REGIS, AREA, CLASE, FEX_C18, DPTO))

### Check possible problems
social_security_health_education |> problems()

# Code informality ----
## https://www.dane.gov.co >
## Mercado Laboral >
## Empleo informal y seguridad social >
## Anexos
### Código_SAS
### Código_STATA

# Merge data ----
employed_informality <- employed |> 
  left_join(y = social_security_health_education, 
            by = join_by(PERIODO, DIRECTORIO, SECUENCIA_P, ORDEN))

## Auxiliary variables
employed_informality <- employed_informality |> 
  mutate(
    ANIOS = PER - 1,
    # See Clasificación Única de Ocupaciones para Colombia (CUOC)
    OFICIO_C8_2D = str_sub(string = OFICIO_C8, 
                           start = 1, 
                           end = 2)
    )

# Build variables ----
employed_informality <- employed_informality |> 
  mutate(
    FORMAL = case_when(
      ## Informal sector
      P6430 == 3 ~ NA_real_,
      P6430 == 6 ~ 0,
      RAMA2D_R4 %in% c("84", "99") ~ 1,
      P6430 == 8 ~ 0,
      ## Asalariados
      P6430 == 2 ~ 1,
      P6430 %in% c(1, 7) & (P3045S1 == 1) ~ 1,
      P6430 %in% c(1, 7) & ((P3045S1 %in% c(2, 9)) & P3046 == 1) ~ 1,
      P6430 %in% c(1, 7) & ((P3045S1 %in% c(2, 9)) & P3046 == 2) ~ 0,
      P6430 %in% c(1, 7) & ((P3045S1 %in% c(2, 9)) & P3046 == 9) & (P3069 >= 4) ~ 1,
      P6430 %in% c(1, 7) & ((P3045S1 %in% c(2, 9)) & P3046 == 9) & (P3069 <= 3) ~ 0,
      ## Independientes
      ### Sin negocio
      P6430 %in% c(4, 5) & !(P6765 %in% 7) & P3065 == 1 ~ 1,
      P6430 %in% c(4, 5) & !(P6765 %in% 7) & P3065 %in% c(2, 9) & P3066 == 1 ~ 1,
      P6430 %in% c(4, 5) & !(P6765 %in% 7) & P3065 %in% c(2, 9) & P3066 == 2 ~ 0,
      P6430 %in% 5 & !(P6765 %in% 7) & P3065 %in% c(2, 9) & P3066 == 9 & P3069 >= 4 ~ 1,
      P6430 %in% 5 & !(P6765 %in% 7) & P3065 %in% c(2, 9) & P3066 == 9 & P3069 <= 3 ~ 0,
      P6430 %in% 4 & !(P6765 %in% 7) & P3065 %in% c(2, 9) & P3066 == 9 & (OFICIO_C8_2D >= "00" & OFICIO_C8_2D <= "20") ~ 1,
      P6430 %in% 4 & !(P6765 %in% 7) & P3065 %in% c(2, 9) & P3066 == 9 & (OFICIO_C8_2D >= "21") ~ 0,
      ## Con negocio
      ### Con registro mercantil
      P6430 %in% c(4, 5) & P6765 %in% 7 & P3067 == 1 & P3067S1 == 1 & P3067S2 >= ANIOS ~ 1,
      P6430 %in% c(4, 5) & P6765 %in% 7 & P3067 == 1 & P3067S1 == 1 & P3067S2 < ANIOS ~ 0,
      P6430 %in% c(4, 5) & P6765 %in% 7 & P3067 == 1 & P3067S1 == 2 & P6775 %in% 1 ~ 1,
      P6430 %in% c(4, 5) & P6765 %in% 7 & P3067 == 1 & P3067S1 == 2 & P6775 %in% 3 & (OFICIO_C8_2D >= "00" & OFICIO_C8_2D <= "20") ~ 1,
      P6430 %in% c(4, 5) & P6765 %in% 7 & P3067 == 1 & P3067S1 == 2 & P6775 %in% 3 & (OFICIO_C8_2D >= "21") ~ 0,
      P6430 %in% c(4, 5) & P6765 %in% 7 & P3067 == 1 & P3067S1 == 2 & P6775 == 2 ~ 0,
      P6430 %in% 4 & P6765 %in% 7 & P3067 == 1 & P3067S1 == 2 & P6775 == 9 & OFICIO_C8_2D >= "00" & OFICIO_C8_2D <= "20" ~ 1,
      P6430 %in% 4 & P6765 %in% 7 & P3067 == 1 & P3067S1 == 2 & P6775 == 9 & OFICIO_C8_2D >= "21" ~ 0,
      P6430 %in% 5 & P6765 %in% 7 & P3067 == 1 & P3067S1 == 2 & P6775 == 9 & P3069 >= 4 ~ 1,
      P6430 %in% 5 & P6765 %in% 7 & P3067 == 1 & P3067S1 == 2 & P6775 == 9 & P3069 <= 3 ~ 0,
      ### Sin registro mercantil
      P6430 %in% c(4, 5) & P6765 %in% 7 & P3067 == 2 & P6775 %in% 1 & P3068 == 1 ~ 1,
      P6430 %in% c(4, 5) & P6765 %in% 7 & P3067 == 2 & P6775 %in% 1 & P3068 == 2 ~ 0,
      P6430 %in% c(4, 5) & P6765 %in% 7 & P3067 == 2 & P6775 %in% 3 & (OFICIO_C8_2D >= "00" & OFICIO_C8_2D <= "20") ~ 1,
      P6430 %in% c(4, 5) & P6765 %in% 7 & P3067 == 2 & P6775 %in% 3 & (OFICIO_C8_2D >= "21") ~ 0,
      # Nota: Se agrega la opción 3 ya que esta reemplaza al 9 desde 2023
      P6430 %in% c(4, 5) & P6765 %in% 7 & P3067 == 2 & P6775 == 1 & P3068 %in% c(3, 9) ~ 0, 
      P6430 %in% c(4, 5) & P6765 %in% 7 & P3067 == 2 & P6775 == 2 ~ 0,
      P6430 %in% 5 & P6765 %in% 7 & P3067 == 2 & P6775 == 9 & P3069 >= 4 ~ 1,
      P6430 %in% 5 & P6765 %in% 7 & P3067 == 2 & P6775 == 9 & P3069 <= 3 ~ 0,
      P6430 %in% 4 & P6765 %in% 7 & P3067 == 2 & P6775 == 9 & (OFICIO_C8_2D >= "00" & OFICIO_C8_2D <= "20") ~ 1,
      P6430 %in% 4 & P6765 %in% 7 & P3067 == 2 & P6775 == 9 & (OFICIO_C8_2D >= "21") ~ 0,
      TRUE ~ NA_real_ 
    )
  )

## Ocupacion informal
### Salud
employed_informality <- employed_informality |> 
  mutate(
    SALUD = case_when(
      P6430 %in% c(1, 3, 7) & P6100 %in% c(1, 2) & P6110 %in% c(1, 2, 4) ~ 1,
      P6430 %in% c(1, 3, 7) & P6100 == 9 & P6450 == 2 ~ 1,
      P6430 %in% c(1, 3, 7) & P6110 == 9 & P6450 == 2 ~ 1,
      P6430 %in% c(1, 3, 7) ~ 0,
      TRUE ~ NA_real_ 
    )
  )

### Pension
employed_informality <- employed_informality |> 
  mutate(
    PENSION = case_when(
      P6430 %in% c(1, 3, 7) & P6920 == 3 ~ 1,
      P6430 %in% c(1, 3, 7) & P6920 == 1 & P6930 %in% c(1, 2, 3) & P6940 %in% c(1, 3) ~ 1,
      P6430 %in% c(1, 3, 7) ~ 0,
      TRUE ~ NA_real_ 
    )
  )

## Calculate EI (Informal employment)
employed_informality <- employed_informality |> 
  mutate(
    EI = case_when(
      P6430 %in% 2 ~ 1,
      P6430 %in% c(6, 8) ~ 0,
      P6430 %in% c(4, 5) ~ FORMAL, 
      P6430 %in% c(1, 3, 7) & SALUD == 1 & PENSION == 1 ~ 1,
      P6430 %in% c(1, 3, 7) ~ 0,
      RAMA2D_R4 %in% c("84", "99") & !(P6430 %in% c(6, 8)) ~ 1, 
      TRUE ~ NA_real_ 
    )
  )

# Check consistency ----
employed_informality |> glimpse()

# Check total employed
employed_informality |> 
  summarise(n = sum(FEX_C18))

# Check informality
## EI = 0; employed informal
## EI = 1; employed formal
employed_informality |> 
  group_by(EI) |> 
  summarise(n = sum(FEX_C18))

employed_informality |> 
  count(P6430)
