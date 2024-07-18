# Data origin ----
## https://www.dane.gov.co/ > 
## Estadísticas por tema >
## Mercado Laboral > 
## Acceso a microdatos Acceder >
## Microdatos > 
## Economía > 
## Mercado Laboral. >
## Gran Encuesta Integrada de Hogares - GEIH - 2024.

# Libraries ----
library(fs)
library(janitor)
library(tidyverse)

# Clean file names ----

## Folder path ----
folder_path <- "000_data/009_geih/"

## File list ----
file_paths <- dir_ls(path = folder_path)

### Names ----
old_names <- path_file(path = file_paths)
new_names <- old_names |> 
  path_ext_remove() |> 
  make_clean_names() |> 
  path_ext_set(ext = ".csv")

### New paths
new_file_paths <- path_dir(path = file_paths) |> 
  path(new_names)

## Rename files ----
file_paths |> 
  file_move(new_path = new_file_paths)

# Import files ----

## Modulo:
### I. Ocupados
#### The delim is ;
##### In case of poblems importing the data use
##### problems(ocupados) 
ocupados <- read_delim(file = "000_data/009_geih/ocupados.csv",
                       delim = ";",
                       col_types = cols(
                         P6430S1 = col_character(),
                         P6630S6A1 = col_integer(),
                         P6765S1 = col_character(),
                         P3055S1 = col_integer(),
                         P6780S1 = col_character(),
                         P6880S1 = col_character(),
                         P6915S1 = col_character(),
                         P7028S1 = col_character()), 
                       locale = locale(decimal_mark = "."))

ocupados_ingreso_laboral <- ocupados |> 
         # Mes
  select(MES,
         # Año
         PER,
         # Año, mes y semana de 
         # recolección de la encuesta 
         PERIODO,
         # identifica la vivienda
         DIRECTORIO,
         # identifica al hogar
         SECUENCIA_P,
         # identifica la persona
         ORDEN,
         # identifica la posición del hogar
         # dentro de la vivienda
         HOGAR,
         # Factor de expansión de personas para
         # el caso del módulo de ocupados
         FEX_C18,
         # Ingreso laboral
         INGLABO)

## Modulos:
### E. Caracteristicas Generales
### F. Seguridad Social en Salud
### G. Educación
#### The delim is ;
##### In case of poblems importing the data use
##### problems(caracteristicas_generales_seguridad_social_en_salud_y_educacion) 
caracteristicas_generales_seguridad_social_en_salud_y_educacion <- read_delim(file = "000_data/009_geih/caracteristicas_generales_seguridad_social_en_salud_y_educacion.csv",
                                                                              delim = ";",
                                                                              col_types = cols(
                                                                                P3147S10A1 = col_character()
                                                                                ),
                                                                              locale = locale(decimal_mark = "."))

nivel_educativo_caract_indiv <- caracteristicas_generales_seguridad_social_en_salud_y_educacion |>
         # Mes
  select(MES,
         # Año
         PER,
         # Año, mes y semana de 
         # recolección de la encuesta 
         PERIODO,
         # identifica la vivienda
         DIRECTORIO,
         # identifica al hogar
         SECUENCIA_P,
         # identifica la persona
         ORDEN,
         # identifica la posición del hogar
         # dentro de la vivienda
         HOGAR,
         # Sexo al nacer
         ## 1	Masculino
         ## 2	Femenino
         P3271,
         # Años cumplidos
         P6040,
         # Mayor nivel educativo alcanzado
         ## 1	Ninguno
         ## 2	Preescolar 
         ## 3	Básica primaria (1o - 5o)
         ## 4	Básica secundaria (6o - 9o)
         ## 5	Media académica (Bachillerato clásico)
         ## 6	Media técnica (Bachillerato técnico)
         ## 7	Normalista
         ## 8	Técnica profesional
         ## 9	Tecnológica 
         ## 10	Universitaria
         ## 11	Especialización 
         ## 12	Maestría 
         ## 13	Doctorado 
         ## 99	No sabe, no informa
         ### Para el módulo G. Educación
         ### sólo es contestado por personas
         ### de 3 años cumplidos o más
         #### Debido a lo anterior existen
         #### valores NA y corresponden a
         #### individuos con menos de 3 años
         #### cumplidos
         ##### Ver P6040 < 3 y is.na(P3042) 
         P3042)

# Merge data ----
ingreso_nivel_educativo <- ocupados_ingreso_laboral |> 
  left_join(y = nivel_educativo_caract_indiv, 
            by = join_by(MES, PER, PERIODO,
                         DIRECTORIO,
                         SECUENCIA_P,
                         ORDEN,
                         HOGAR))

## Check NA values ----
### Mayor nivel educativo alcanzado
ingreso_nivel_educativo |> filter(is.na(P3042))
### Ingreso laboral
ingreso_nivel_educativo |> filter(is.na(INGLABO))

## Wrangling data ----
### Salario mínimo mensual legal vigente
#### Periodo: 2024
smmlv <- 1300000

nivel_ingreso_levels <- c("Without information",
                          "[0, 0.5)",
                          "[0.5, 1)",
                          "[1, 1.5)",
                          "[1.5, 2)",
                          "[2, 2.5)",
                          "[2.5, 3)",
                          "[3, 3.5)",
                          "[3.5, 4)",
                          "[4, 4.5)",
                          "[4.5, more than 4.5)")

nivel_educativo_levels <- c("None",
                            "Preschool", 
                            "Primary school",
                            "Secondary school",
                            "Classical baccalaureate",
                            "Technical Baccalaureate",
                            "Normal school",
                            "Professional technique",
                            "Technological", 
                            "University",
                            "Specialization", 
                            "Master's Degree", 
                            "PhD", 
                            "Without information")

ingreso_nivel_educativo_categories <- ingreso_nivel_educativo |> 
  mutate(nivel_educativo = case_when(
    P3042 == 1	~ nivel_educativo_levels[1],
    P3042 == 2	~ nivel_educativo_levels[2], 
    P3042 == 3	~ nivel_educativo_levels[3],
    P3042 == 4	~ nivel_educativo_levels[4],
    P3042 == 5	~ nivel_educativo_levels[5],
    P3042 == 6	~ nivel_educativo_levels[6],
    P3042 == 7	~ nivel_educativo_levels[7],
    P3042 == 8	~ nivel_educativo_levels[8],
    P3042 == 9	~ nivel_educativo_levels[9],
    P3042 == 10	~ nivel_educativo_levels[10],
    P3042 == 11	~ nivel_educativo_levels[11], 
    P3042 == 12	~ nivel_educativo_levels[12], 
    P3042 == 13	~ nivel_educativo_levels[13], 
    P3042 == 99	~ nivel_educativo_levels[14], 
    .default = NA_character_)) |> 
  mutate(nivel_educativo = factor(x = nivel_educativo,
                                  levels = nivel_educativo_levels, 
                                  ordered = TRUE)) |>
  mutate(nivel_ingreso = case_when(
    is.na(INGLABO)       ~ nivel_ingreso_levels[1],
    INGLABO < 0.5*smmlv  ~ nivel_ingreso_levels[2],
    INGLABO >= 0.5*smmlv & INGLABO < 1*smmlv   ~ nivel_ingreso_levels[3],
    INGLABO >= 1*smmlv   & INGLABO < 1.5*smmlv ~ nivel_ingreso_levels[4],
    INGLABO >= 1.5*smmlv & INGLABO < 2*smmlv   ~ nivel_ingreso_levels[5],
    INGLABO >= 2*smmlv   & INGLABO < 2.5*smmlv ~ nivel_ingreso_levels[6],
    INGLABO >= 2.5*smmlv & INGLABO < 3*smmlv   ~ nivel_ingreso_levels[7],
    INGLABO >= 3*smmlv   & INGLABO < 3.5*smmlv ~ nivel_ingreso_levels[8],
    INGLABO >= 3.5*smmlv & INGLABO < 4*smmlv   ~ nivel_ingreso_levels[9],
    INGLABO >= 4*smmlv   & INGLABO < 4.5*smmlv ~ nivel_ingreso_levels[10],
    INGLABO >= 4.5*smmlv ~ nivel_ingreso_levels[11], 
    .default = NA_character_)) |> 
  mutate(nivel_ingreso = factor(x = nivel_ingreso,
                                levels = nivel_ingreso_levels, 
                                ordered = TRUE))
# Group data ----
group_ingreso_nivel_educativo_categories <- ingreso_nivel_educativo_categories |> 
  select(nivel_educativo, 
         nivel_ingreso,
         FEX_C18) |> 
  group_by(nivel_educativo, nivel_ingreso) |> 
  summarise(FEX_C18 = sum(FEX_C18), 
            .groups = "drop")

# Export data ----
group_ingreso_nivel_educativo_categories |> 
  write_rds(file = "000_data/009_income_educational_level_geih.rds")

levels_geih <- list(ingreso = nivel_ingreso_levels, 
                    educativo = nivel_educativo_levels)
levels_geih |> 
  write_rds(file = "000_data/009_levels.rds")

# Check data ----
read_rds(file = "000_data/009_income_educational_level_geih.rds")
read_rds(file = "000_data/009_levels.rds")
