# Libraries ----
library(wordcloud2)
library(ggwordcloud)
library(tidyverse)

# Import data ----
# Remember to change names
merge_data <- read_csv(
  file = "000_data/000_core_econ_cloud_words/merge_data/merge_data_2026-1.csv"
)

# Visualization ----
merge_data_word_freq_es <- merge_data |>
  count(clean_word, sort = TRUE)

merge_data_word_freq_en <- merge_data |>
  count(en_word, sort = TRUE)

set.seed(seed = 1234)
word_cloud_es <- merge_data_word_freq_es |>
  ggplot(aes(label = clean_word, size = n)) +
  geom_text_wordcloud(aes(color = n)) +
  scale_size_area(max_size = 15) +
  scale_color_gradient(low = "#2c3e50", high = "#e31a1c")

word_cloud_es

set.seed(seed = 1234)
word_cloud_en <- merge_data_word_freq_en |>
  ggplot(aes(label = en_word, size = n)) +
  geom_text_wordcloud(aes(color = n)) +
  scale_size_area(max_size = 15) +
  scale_color_gradient(low = "#2c3e50", high = "#e31a1c")

word_cloud_en

# Export ----
# Remember to change name
word_cloud_es |>
  ggsave(
    filename = "000_data/000_core_econ_cloud_words/visualization/word_cloud_es_2026-1.png",
    plot = _,
    width = 6,
    height = 4,
    dpi = 300
  )

word_cloud_en |>
  ggsave(
    filename = "000_data/000_core_econ_cloud_words/visualization/word_cloud_en_2026-1.png",
    plot = _,
    width = 6,
    height = 4,
    dpi = 300
  )
