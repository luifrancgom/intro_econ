library(tidyverse)
library(janitor)
library(sf)
library(readxl)

# Import data ----
## https://www.colombiaenmapas.gov.co/ >
## Temáticas >
## Límites > 
## Departamentos de Colombia >
## Formato de descarga: Shapefile
### Fecha: 2024-12-30
map_col_depto <- st_read(dsn = "000_data/004_departamentos_colombia_shp/Departamento.shp")

## https://www.dane.gov.co/ >
## Estadísticas por tema >
## Demografía y población >
## Proyecciones de población >
## Proyecciones y retroproyecciones de población departamental para el periodo 1985-2019 y 2020-2050 con base en el CNPV 2018 >
## Serie departamental de población por área, para el periodo 2020-2050
### Fecha: 2023-03-04
pop_col <- read_excel(path = "000_data/004_serie_departamental_de_poblacion_por_area_2020-2050.xlsx", 
                      sheet = 1, 
                      range = "A12:E2091")

## https://www.policia.gov.co/grupo-informacion-criminalidad >
## PORTAL ESTADÍSTICO >
## Estadística delictiva >
### Delito de Impacto: Homicidios, 
###                    Hurto a personas
###                    Hurtos a entidades comerciales
###                    Hurto a entidades financieras
###                    Secuestro
### Año: 2024
### Fecha: 2025-01-07
homicidio_intencional <- read_excel(path = "000_data/004_homicidio_intencional_2023.xlsx", 
                                    sheet = 1, 
                                    range = "A10:H11022")
hurto_personas <- read_excel(path = "000_data/004_hurto_a_personas_2024.xlsx", 
                             sheet = 1, 
                             range = "A10:H91840")
hurto_comerciales <- read_excel(path = "000_data/004_hurto_a_comercio_2024.xlsx", 
                             sheet = 1, 
                             range = "A10:F18959")
hurto_financieras <- read_excel(path = "000_data/004_hurto_entidades_financieras_2024.xlsx", 
                                sheet = 1, 
                                range = "A10:F62")
secuestro <- read_excel(path = "000_data/004_secuestro_2024.xlsx", 
                        sheet = 1, 
                        range = "A10:H248")

# Clean data ----
year <- 2024

# The Coordinate Reference System
# is MAGNA-SIRGAS 2018 / Origen-Nacional
## It is not EPSG:4326
st_crs(x = map_col_depto)

map_col_depto_tbl <- map_col_depto |>
  # Transform to WGS84 (EPSG:4326)
  ## Geographic coordinate system that uses latitude and 
  ## longitude to define locations on the Earth's surface
  ### The idea is to change the Coordinate Reference 
  ### System (crs)
  st_transform(crs = 4326) |> 
  as_tibble() |> 
  clean_names() |> 
  select(de_codigo, de_nombre, de_norma,
         geometry)

pop_col_tbl <- pop_col |> 
  clean_names() |> 
  set_names(nm = c("de_codigo",
                   "de_nombre",
                   "year",
                   "area_geografica",
                   "poblacion")) |> 
  filter(year == {{year}}) |> 
  filter(area_geografica == "Total") |> 
  mutate(
    de_codigo = case_when(
      # Bogota, D.C is not a department
      # of Colombia. Therefore we include
      # it as part of the department of 
      # Cundinamarca
      de_codigo == "11" ~ "25",
      .default = as.character(de_codigo)),
    de_nombre = case_when(
      # Change the name and incorporate
      # Bogota, D.C to Cundinamarca
      de_nombre == "Bogotá, D.C." ~ "Cundinamarca",
      .default = as.character(de_nombre))) |> 
  group_by(de_codigo, de_nombre, year) |> 
  summarise(poblacion = sum(poblacion), 
            .groups = "drop") |> 
  select(-de_nombre)

homicidio_intencional_tbl <- homicidio_intencional |> 
  clean_names() |> 
  select(codigo_dane, cantidad) |> 
  mutate(codigo_dane = str_sub(string = codigo_dane, 
                               start = 1L,
                               end = 2L)) |> 
  rename(de_codigo = codigo_dane,
         homicidio_cantidad = cantidad) |> 
  group_by(de_codigo) |> 
  summarize(homicidio_cantidad = sum(homicidio_cantidad))

hurto_personas_tbl <- hurto_personas |> 
  clean_names() |> 
  select(codigo_dane, cantidad) |> 
  mutate(codigo_dane = str_sub(string = codigo_dane, 
                               start = 1L,
                               end = 2L)) |> 
  rename(de_codigo = codigo_dane,
         hurto_personas_cantidad = cantidad) |> 
  group_by(de_codigo) |> 
  summarize(hurto_personas_cantidad = sum(hurto_personas_cantidad))

hurto_comerciales_tbl <- hurto_comerciales |> 
  clean_names() |> 
  select(codigo_dane, cantidad) |> 
  mutate(codigo_dane = str_sub(string = codigo_dane, 
                               start = 1L,
                               end = 2L)) |> 
  rename(de_codigo = codigo_dane,
         hurto_comerciales_cantidad = cantidad) |> 
  group_by(de_codigo) |> 
  summarize(hurto_comerciales_cantidad = sum(hurto_comerciales_cantidad))

hurto_financieras_tbl <- hurto_financieras |> 
  clean_names() |> 
  select(codigo_dane, cantidad) |> 
  mutate(codigo_dane = str_sub(string = codigo_dane, 
                               start = 1L,
                               end = 2L)) |> 
  rename(de_codigo = codigo_dane,
         hurto_financieras_cantidad = cantidad) |> 
  group_by(de_codigo) |> 
  summarize(hurto_financieras_cantidad = sum(hurto_financieras_cantidad))

secuestro_tbl <- secuestro |> 
  clean_names() |> 
  select(codigo_dane, cantidad) |> 
  mutate(codigo_dane = str_sub(string = codigo_dane, 
                               start = 1L,
                               end = 2L)) |> 
  rename(de_codigo = codigo_dane,
         secuestro_cantidad = cantidad) |> 
  group_by(de_codigo) |> 
  summarize(secuestro_cantidad = sum(secuestro_cantidad))

# Merge data ----
tibbles_lst <- list(map_col_depto_tbl,
                    pop_col_tbl,
                    homicidio_intencional_tbl,
                    hurto_personas_tbl,
                    hurto_comerciales_tbl,
                    hurto_financieras_tbl,
                    secuestro_tbl)

map_col_depto_merge_tbl <- reduce(.x = tibbles_lst, 
                                  .f = left_join,
                                  by = join_by(de_codigo)) |> 
  # Delete Area en Litigio Cauca - Huila
  ## de_codigo: 00
  ### We can not assign data to Cauca or
  ### Huila
  filter(de_codigo != "00")

# Prepare data ----
map_col_depto_merge_prepare_tbl <- map_col_depto_merge_tbl |> 
  ## In the case of NA values we can assume that
  ## they are 0. That is, the citizens don't
  ## report this type of crime
  mutate(across(.cols = homicidio_cantidad:secuestro_cantidad,
                .fns = \(x) replace_na(data = x, 0))) |> 
  ## Express data per 10000 inhabitants
  mutate(across(.cols = homicidio_cantidad:secuestro_cantidad,
                .fns = \(x) (x / poblacion)*100000)) |> 
  ## Rename variable with an appropiate label
  ## taking into account the everything is expressed as
  ## 10000 per inhabitants
  rename_with(.fn = \(x) str_replace(string = x,
                                     pattern = "cantidad",
                                     replace = "per"),
              .cols = homicidio_cantidad:secuestro_cantidad)

# Export data ----
map_col_depto_merge_prepare_tbl |> 
  write_rds(file = "000_data/004_crime_col_2024.rds")

# Checking data ----
read_rds(file = "000_data/004_crime_col_2024.rds")
