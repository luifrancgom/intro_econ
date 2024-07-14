# Libraries ----
library(tidyverse)
library(readxl)

# Extract headers ----
header <- read_excel(path = "000_data/003_untidy_TED2.xlsx", 
                     sheet = 1, 
                     n_max = 6,
                     col_names = c("row", 
                                   str_c("col", 2:2305,
                                         sep = "_")))

variables <- header |> 
  select(-c(row)) |> 
  t() |> 
  as_tibble(rownames = NULL) |> 
  select(-V4) |> 
  rename(database_label = V1,
         value_unit_description = V3,
         periodicity = V5) |> 
  separate_wider_delim(cols = V2, 
                       delim = ":",
                       names = c("unit_label", "variable_label")) |> 
  mutate(variable_label = str_trim(variable_label)) |> 
  separate_wider_regex(cols = V6,
                       c(database_code = "TED2_",
                         unit_code = "[^_]+_",
                         variable_code = ".+"))

names <- variables |> 
  mutate(across(database_code:unit_code,
                .fns = \(x) str_remove(x, "_"))) |> 
  unite(col = "name", everything(), sep = "+") |> 
  _$name

# Tidy database ----
tidy_TED2 <- read_excel(path = "000_data/003_untidy_TED2.xlsx", 
                        sheet = 1, 
                        skip = 7,
                        col_names = c("date", names))

tidy_TED2 <- tidy_TED2 |> 
  pivot_longer(cols = -c(date),
               names_to = "variables", 
               values_to = "values") |> 
  separate_wider_delim(cols = variables, 
                       delim = "+", 
                       names = names(variables)) |> 
  relocate(database_code, .before = database_label) |> 
  relocate(unit_code, .before = unit_label) |> 
  relocate(variable_code, .before = variable_label)

# Export file ----
tidy_TED2 |> 
  write_csv(file = "000_data/003_tidy_TED2.csv.gz")

# Check file ----
read_csv(file = "000_data/003_tidy_TED2.csv.gz")
