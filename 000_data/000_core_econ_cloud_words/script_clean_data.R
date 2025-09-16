# Libraries ----
library(tidyverse)
library(stringi)
library(tidytext)
library(stopwords)
library(polyglotr)

# Import data
spanish_stop_words <- stopwords(language = "es")
data_2025_2_aula_a <- read_csv(file = "000_data/000_core_econ_cloud_words/raw_data/data_2025-2_aula_a.csv") 
data_2025_2_aula_b <- read_csv(file = "000_data/000_core_econ_cloud_words/raw_data/data_2025-2_aula_b.csv") 

## Course A
data_2025_2_aula_a_clean <- data_2025_2_aula_a |>  
  set_names(nm = c("id", "raw_word")) |> 
  # Lower case conversion
  mutate(clean_word = str_to_lower(string = raw_word, locale = "es")) |> 
  # Accent removal
  mutate(clean_word = stri_trans_general(str = clean_word, id = "Latin-ASCII")) |> 
  # Punctuation removal
  mutate(clean_word = str_remove_all(string = clean_word, pattern = "[[:punct:]]")) |> 
  # Removing Leading and Trailing Whitespace
  mutate(clean_word = str_trim(string = clean_word, side = "both")) |> 
  # Eliminate stop words
  unnest_tokens(output = "clean_word", input = clean_word) |> 
  filter(!(clean_word %in% {{spanish_stop_words}})) |> 
  group_by(id, raw_word) |> 
  summarise(clean_word = paste(clean_word, collapse = " ")) |> 
  ungroup() |> 
  # Adjust terms
  # Check also line 65 for possible adjustments
  mutate(clean_word = if_else(condition = raw_word == "RESILIENTE", 
                              "resiliencia",
                              clean_word)) |> 
  # Add variables
  mutate(course = "a", .after = id) |> 
  mutate(year = 2025, .after = course) |> 
  mutate(semester = 2, .after = year) |> 
  # Delete variables
  select(-c(id)) |> 
  # Translate terms
  mutate(en_word = map_chr(.x = clean_word, 
                           .f = \(x) google_translate(text = x, target_language = "en", source_language = "es"))) |> 
  mutate(en_word = str_to_lower(string = en_word))

# Clean data
## Course B
data_2025_2_aula_b_clean <- data_2025_2_aula_b |>  
  set_names(nm = c("id", "raw_word")) |> 
  # Lower case conversion
  mutate(clean_word = str_to_lower(string = raw_word, locale = "es")) |> 
  # Accent removal
  mutate(clean_word = stri_trans_general(str = clean_word, id = "Latin-ASCII")) |> 
  # Punctuation removal
  mutate(clean_word = str_remove_all(string = clean_word, pattern = "[[:punct:]]")) |> 
  # Removing Leading and Trailing Whitespace
  mutate(clean_word = str_trim(string = clean_word, side = "both")) |> 
  # Eliminate stop words
  unnest_tokens(output = "clean_word", input = clean_word) |> 
  filter(!(clean_word %in% {{spanish_stop_words}})) |> 
  group_by(id, raw_word) |> 
  summarise(clean_word = paste(clean_word, collapse = " ")) |> 
  ungroup() |>
  # Adjust terms
  mutate(clean_word = if_else(condition = clean_word == "inflacion economica", 
                              "inflacion",
                              clean_word)) |> 
  mutate(clean_word = if_else(condition = clean_word == "narcotrafico pais", 
                              "narcotrafico",
                              clean_word)) |> 
  # Add variables
  mutate(course = "b", .after = id) |> 
  mutate(year = 2025, .after = course) |> 
  mutate(semester = 2, .after = year) |> 
  # Delete variables
  select(-c(id)) |> 
  # Translate terms
  mutate(en_word = map_chr(.x = clean_word, 
                           .f = \(x) google_translate(text = x, target_language = "en", source_language = "es"))) |> 
  mutate(en_word = str_to_lower(string = en_word))

# Export ----
data_2025_2_aula_a_clean |> 
  write_csv(file = "000_data/000_core_econ_cloud_words/clean_data/data_2025_2_aula_a.csv")


data_2025_2_aula_b_clean |> 
  write_csv(file = "000_data/000_core_econ_cloud_words/clean_data/data_2025_2_aula_b.csv")
 