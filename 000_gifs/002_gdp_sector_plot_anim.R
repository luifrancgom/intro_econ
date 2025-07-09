# Libraries ----
library(tidyverse)
library(readxl)
library(treemapify)
library(scales)
library(gganimate)

# Palette ----
umng_palette <- c(
  "#043074",
  "#fdc600",
  "#0d5e30",
  "#ee2a24",
  "#fc6700",
  "#00b3f0",
  "#6e3c1a",
  "#f8941c",
  "#8e44ad",
  "#2c3e50",
  "#16a085",
  "#c0392b",
  "#e91e63"
)

# Data
# Estadísticas por tema > 
# Cuentas nacionales >
# Cuentas Nacionales Anuales >
# Agregados macroeconómicos - Retropolación base 2015 > 
# Anexo Retropolación Base 2015

# Import ----
gdp_sector_annual_current_1975_2013 <- read_excel("000_data/002_anexo_retropolacion_base_2015.xlsx",
                                                  sheet = 2,
                                                  range = "B13:AP26",
                                                  col_names = c("id", "sector", as.character(1975:2013)),
                                                  trim_ws = TRUE)

gdp_sector_annual_current_2014_2024 <- read_excel("000_data/002_agregados_macroeconomicos_cuentas_nal_anuales_2005_2023p_2024pr.xlsx",
                                                  sheet = 2,
                                                  range = "B14:Y114",
                                                  col_names = c("id", "id_25", "id_61", "sector", 
                                                                as.character(2005:2024)))

# Tidy ----
gdp_sector_annual_current_1975_2013_clean <- gdp_sector_annual_current_1975_2013 |>
  # Delete row related to Valor agregado bruto
  ## You don't need that part
  slice(-13) |> 
  mutate(id = replace(id, is.na(id), "Impuestos - Subsidios"),
         # There is a typo in the file: +S must be + S 
         id = replace(id, id == "R +S + T", "R + S + T")) |> 
  pivot_longer(cols = `1975`:`2013`, 
               names_to = "year",
               values_to = "value") |> 
  mutate(year = as.integer(year))

gdp_sector_annual_current_2014_2024_clean <- gdp_sector_annual_current_2014_2024 |> 
  mutate(id = replace(id, 
                      sector == "Impuestos menos subvenciones sobre los productos", 
                      "Impuestos - Subsidios")) |> 
  filter(!is.na(id)) |> 
  select(-c("id_25", "id_61")) |> 
  pivot_longer(cols = `2005`:`2024`, 
               names_to = "year",
               values_to = "value") |> 
  mutate(year = as.integer(year)) |> 
  filter(year %in% 2014:2024)

gdp_sector_annual_current_1975_2024_clean <- gdp_sector_annual_current_1975_2013_clean |>
  bind_rows(gdp_sector_annual_current_2014_2024_clean) |> 
  group_by(year) |> 
  mutate(pib = sum(value)) |> 
  ungroup() |> 
  mutate(pct_value = value / pib,
         text_id_pct_value = str_glue("{id}: {percent(pct_value, accuracy = 0.01)}"))

# Helpers
last_update_gdp_sector_annual_current <- ymd("2025-06-27")

# Visualization ----
gdp_sector_plot_static <- gdp_sector_annual_current_1975_2024_clean |> 
  ggplot(aes(area = pct_value, 
             fill = id, 
             label = text_id_pct_value)) +
  geom_treemap(color = "black",
               show.legend = FALSE) + 
  geom_treemap_text(place = "centre") +
  scale_fill_manual(values = umng_palette) +
  labs(title = "Year: {frame_time}", 
       caption = str_glue("Source: DANE - Cuentas Nacionales Anuales - Retropolación Base 2015 - PIB enfoque de la producción y del gasto a precios corrientes
                          Last update: {last_update_gdp_sector_annual_current}")) +
  theme(
    panel.border = element_rect(
      fill = NA,
      color = "black"
    ),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    legend.background = element_rect(fill = "white"),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 20)
  ) +
  transition_time(time = year) +
  ease_aes('linear')

gdp_sector_plot_anim <- animate(gdp_sector_plot_static, 
                                nframes = 400,
                                fps = 5, 
                                width = 900, 
                                height = 600)  

# Export ----
anim_save(filename = "000_gifs/002_gdp_sector_plot_anim.gif",
          animation = gdp_sector_plot_anim)
