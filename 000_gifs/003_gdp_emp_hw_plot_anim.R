# Libraries
library(tidyverse)
library(wbstats)
library(janitor)
library(ggrepel)
library(scales)
library(gganimate)

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
## Average annual hours actually worked per worker
### https://data-explorer.oecd.org/ >
### Employment indicators >
### Average annual hours actually worked per worker >
### Download >
### Unfiltered data in tabular text (CSV)

# Import ----

## Helpers
start_date <- 2010
end_date   <- 2024
last_update_average_annual_hours_actually_worked_per_worker <- ymd("2025-06-27")

gdp_per_person_employed <- wb_data(start_date = start_date,
                                   end_date   = end_date,
                                   indicator = c('SL.GDP.PCAP.EM.KD'), 
                                   return_wide = FALSE)

average_annual_hours_actually_worked_per_worker <- read_csv(file = "000_data/003_average_annual_hours_actually_worked_per_worker.csv")

# Tidy ----
gdp_per_person_employed_clean <- gdp_per_person_employed |> 
  select(iso3c, country, date, value)

average_annual_hours_actually_worked_per_worker_clean <- average_annual_hours_actually_worked_per_worker |>  
  clean_names() |> 
  filter(between(x = time_period,
                 left  = {{start_date}}, 
                 right = {{end_date}})) |> 
  # Worker status = Employees
  ## provides a more focused view of working hours 
  ## specifically for those in traditional employment 
  ## relationships.
  filter(worker_status_2 == "Employees") |> 
  select(time_period, ref_area, obs_value)

# Merge ----
gdp_emp_hw <- average_annual_hours_actually_worked_per_worker_clean |> 
  inner_join(y = gdp_per_person_employed_clean, 
             by = join_by(time_period == date,
                          ref_area == iso3c)) |> 
  relocate(obs_value, .before = value) |> 
  arrange(ref_area, time_period) |> 
  mutate(time_period = as.integer(time_period))

# Visualization ----
gdp_emp_hw_plot_static <- gdp_emp_hw |> 
  ggplot(aes(x = obs_value, y = value, 
             label = country)) + 
  geom_point(shape = 21,
             color = umng_palette[1],
             fill  = umng_palette[2]) + 
  geom_text_repel(min.segment.length = 0,
                  seed = 1234,
                  color = umng_palette[1]) +
  scale_x_continuous(breaks = seq.int(from = floor(min(gdp_emp_hw$obs_value) / 100) * 100, 
                                      to   = ceiling(max(gdp_emp_hw$obs_value) / 100) * 100,
                                      length.out   = 6),
                     limits = c((floor(min(gdp_emp_hw$obs_value) / 100) * 100) - 50,
                                (ceiling(max(gdp_emp_hw$obs_value) / 100) * 100) + 50)) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(x = str_glue("{unique(average_annual_hours_actually_worked_per_worker$STRUCTURE_NAME)}"),
       y = str_glue("{unique(gdp_per_person_employed$indicator) }"),
       title = "Year: {frame_time}",
       # For last update source 1 check out in the page
       # in overview
       ## This needs to be update manually
       caption = str_glue("Source 1: OECD Data Explorer - OCDE
                          Source 2: World Development Indicators (WDI) − World Bank
                          Last update source 1: {last_update_average_annual_hours_actually_worked_per_worker}
                          Last update source 2: {unique(gdp_per_person_employed$last_updated)}")) +
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
  transition_time(time = time_period) +
  ease_aes('linear')

gdp_emp_hw_plot_anim <- animate(gdp_emp_hw_plot_static, 
                                nframes = 120,
                                fps = 10, 
                                width = 900, 
                                height = 600)

# Export ----
anim_save(filename = "000_gifs/003_gdp_emp_hw_plot_anim.gif",
          animation = gdp_emp_hw_plot_anim)
