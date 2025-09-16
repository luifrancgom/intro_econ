# Libraries ----
library(tidyverse)
library(fs)

# List directore
list_files <- dir_ls(path = "000_data/000_core_econ_cloud_words/clean_data/",
                     # Change this in relation to the period
                     regexp = r'(2025_2.+\.csv$)')

# Import data ----
merge_data <- list_files |> 
  map(.f = \(x) read_csv(file = x)) |> 
  bind_rows() |> 
  mutate(id = row_number(), .before = course)

# Export data 
merge_data |> 
  write_csv(file = "000_data/000_core_econ_cloud_words/merge_data/merge_data_2025-2.csv")
  