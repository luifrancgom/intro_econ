# Libraries ----
library(haven)

# Import ----
schools_tbl <- read_dta(file = "000_data/001_caratula_unica_de_la_sede_educativa_2023.dta")

# Export ----
schools_tbl |> 
  write_csv(file = "000_data/001_caratula_unica_de_la_sede_educativa_2023.csv")