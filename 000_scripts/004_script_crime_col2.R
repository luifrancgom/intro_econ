library(tidyverse)
library(janitor)
library(sf)
library(readxl)

# Import data ----
## https://www.colombiaenmapas.gov.co/ >
## Filtrar - Entidad territorial: Colombia
## Temáticas >
## Límites >
## Departamentos de Colombia >
## Formato de descarga: Shapefile
### Fecha: 2025-09-30
map_col_depto <- st_read(
  dsn = "000_data/004_departamentos_colombia_shp/Departamento.shp"
)

## https://www.dane.gov.co/ >
## Estadísticas por tema >
## Demografía y población >
## Proyecciones de población >
## Proyecciones de población regional para el periodo 2018-2050 y proyecciones y retroproyecciones de población departamental para el periodo 1985-2017 y 2018-2050 con base en el CNPV 2018 >
## Serie departamental de población por área, para el periodo 2018-2050 >
### Fecha: 2025-08-08
pop_col <- read_excel(
  path = "000_data/004_serie_departamental_de_poblacion_por_area_2018-2050.xlsx",
  sheet = 3,
  range = "A8:E2946"
) |>
  # Column label are in A8 so there is row with
  # NA values
  slice(-1)

## https://www.mindefensa.gov.co/ >
## Cifras >
## Información Estadística >
## Información estadística desagregada
### Fecha: 2026-01-21

### Delitos contra la vida y la integridad personal
#### HOMICIDIO INTENCIONAL.xlsx
### Delitos contra el patrimonio economico
#### HURTO PERSONAS.xlsx
#### HURTO A COMERCIO.xlsx
#### HURTO ENTIDADES FINANCIERAS.xlsx
### Delitos contra la libertad individual y otras garantías consti
#### SECUESTRO.xlsx

homicidio_intencional <- read_excel(
  path = "000_data/004_homicidio_intencional.xlsx",
  sheet = 1,
  range = cell_cols("A:H")
)

hurto_personas <- read_excel(
  path = "000_data/004_hurto_personas.xlsx",
  sheet = 1,
  range = cell_cols("A:F")
)

hurto_comerciales <- read_excel(
  path = "000_data/004_hurto_comerciales.xlsx",
  sheet = 1,
  range = cell_cols("A:F")
)

hurto_financieras <- read_excel(
  path = "000_data/004_hurto_financieras.xlsx",
  sheet = 1,
  range = cell_cols("A:F")
)

secuestro <- read_excel(
  path = "000_data/004_secuestro.xlsx",
  sheet = 1,
  range = cell_cols("A:G")
)

# Clean data ----
year <- 2025

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
  select(de_codigo, de_nombre, de_norma, geometry)

pop_col_tbl <- pop_col |>
  clean_names() |>
  set_names(
    nm = c("de_codigo", "de_nombre", "year", "area_geografica", "poblacion")
  ) |>
  filter(year == {{ year }}) |>
  filter(area_geografica == "Total") |>
  mutate(
    de_codigo = case_when(
      # Bogota, D.C is not a department
      # of Colombia. Therefore we include
      # it as part of the department of
      # Cundinamarca
      de_codigo == "11" ~ "25",
      .default = as.character(de_codigo)
    ),
    de_nombre = case_when(
      # Change the name and incorporate
      # Bogota, D.C to Cundinamarca
      de_nombre == "Bogotá, D.C." ~ "Cundinamarca",
      .default = as.character(de_nombre)
    )
  ) |>
  group_by(de_codigo, de_nombre, year) |>
  summarise(poblacion = sum(poblacion), .groups = "drop") |>
  select(-de_nombre)

homicidio_intencional_tbl <- homicidio_intencional |>
  clean_names() |>
  mutate(fecha_hecho = ymd(fecha_hecho)) |>
  filter(
    year(fecha_hecho) ==
      {
        year
      }
  ) |>
  select(cod_depto, victimas) |>
  rename(de_codigo = cod_depto, homicidio_cantidad = victimas) |>
  group_by(de_codigo) |>
  summarize(homicidio_cantidad = sum(homicidio_cantidad))

hurto_personas_tbl <- hurto_personas |>
  clean_names() |>
  mutate(fecha_hecho = ymd(fecha_hecho)) |>
  filter(
    year(fecha_hecho) ==
      {
        year
      }
  ) |>
  select(cod_depto, cantidad) |>
  rename(de_codigo = cod_depto, hurto_personas_cantidad = cantidad) |>
  group_by(de_codigo) |>
  summarize(hurto_personas_cantidad = sum(hurto_personas_cantidad))

hurto_comerciales_tbl <- hurto_comerciales |>
  clean_names() |>
  mutate(fecha_hecho = ymd(fecha_hecho)) |>
  filter(
    year(fecha_hecho) ==
      {
        year
      }
  ) |>
  select(cod_depto, cantidad) |>
  rename(de_codigo = cod_depto, hurto_comerciales_cantidad = cantidad) |>
  group_by(de_codigo) |>
  summarize(hurto_comerciales_cantidad = sum(hurto_comerciales_cantidad))

hurto_financieras_tbl <- hurto_financieras |>
  clean_names() |>
  mutate(fecha_hecho = ymd(fecha_hecho)) |>
  filter(
    year(fecha_hecho) ==
      {
        year
      }
  ) |>
  select(cod_depto, cantidad) |>
  rename(de_codigo = cod_depto, hurto_financieras_cantidad = cantidad) |>
  group_by(de_codigo) |>
  summarize(hurto_financieras_cantidad = sum(hurto_financieras_cantidad))

secuestro_tbl <- secuestro |>
  clean_names() |>
  mutate(fecha_hecho = ymd(fecha_hecho)) |>
  filter(
    year(fecha_hecho) ==
      {
        year
      }
  ) |>
  select(cod_depto, cantidad) |>
  rename(de_codigo = cod_depto, secuestro_cantidad = cantidad) |>
  group_by(de_codigo) |>
  summarize(secuestro_cantidad = sum(secuestro_cantidad))

# Merge data ----
tibbles_lst <- list(
  map_col_depto_tbl,
  pop_col_tbl,
  homicidio_intencional_tbl,
  hurto_personas_tbl,
  hurto_comerciales_tbl,
  hurto_financieras_tbl,
  secuestro_tbl
)

map_col_depto_merge_tbl <- reduce(
  .x = tibbles_lst,
  .f = left_join,
  by = join_by(de_codigo)
) |>
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
  mutate(across(.cols = homicidio_cantidad:secuestro_cantidad, .fns = \(x) {
    replace_na(data = x, 0)
  })) |>
  ## Express data per 10000 inhabitants
  mutate(across(.cols = homicidio_cantidad:secuestro_cantidad, .fns = \(x) {
    (x / poblacion) * 100000
  })) |>
  ## Rename variable with an appropiate label
  ## taking into account the everything is expressed as
  ## 10000 per inhabitants
  rename_with(
    .fn = \(x) str_replace(string = x, pattern = "cantidad", replace = "per"),
    .cols = homicidio_cantidad:secuestro_cantidad
  )

# Export data ----
map_col_depto_merge_prepare_tbl |>
  write_rds(file = "000_data/004_crime_col_2025.rds")

# Checking data ----
read_rds(file = "000_data/004_crime_col_2025.rds")
