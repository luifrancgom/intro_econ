# Libraries ----
library(tidyverse)
library(tidymodels)

# Within groups ----
tibble(member = 1:6) |> 
    rep_sample_n(size = 1, 
                 replace = FALSE, 
                 reps = 1)
