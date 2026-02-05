library(imfapi)
library(tidyverse)
library(scales)

# Documentation
## https://www.imf.org/en/Home >
## DATA >
## IMF Data Portal >
## Datasets >
## Balance of Payments and International Investment Position Statistics (BOP/IIP), World and Country Group Aggregates >
## VIEW DATA
### Filter by the different parameters
#### Account if necessary
#### Username: l**********@g****.**m
#### Password: U**********4

# https://teal-insights.github.io/r-imfapi/
## List available dataflows
### id: BOP_AGG
### name: Balance of Payments and International Investment Position Statistics (BOP/IIP), World and Country Group Aggregates
dataflows_imf <- imf_get_dataflows()

##  Get the dimensions for filtering
datastructure_bop_agg <- imf_get_datastructure(
  dataflow_id = "BOP_AGG"
)

## Code lists BOP_AGG
### Check indicators
code_list_indicator <- imf_get_codelists(
  dimension_ids = "INDICATOR",
  dataflow_id = "BOP_AGG"
)
### Use "A": Annual
code_list_frequency <- imf_get_codelists(
  dimension_ids = "FREQUENCY",
  dataflow_id = "BOP_AGG"
)

### Use "USD": US dollar
code_list_type_of_transformation <- imf_get_codelists(
  dimension_ids = "TYPE_OF_TRANSFORMATION",
  dataflow_id = "BOP_AGG"
)

## Request data with filters
start_year <- 2005
end_year <- 2024

bop_col_current_account <- imf_get(
  dataflow_id = "BOP_AGG",
  dimensions = list(
    FREQUENCY = c("A"),
    COUNTRY = c("COL"),
    TYPE_OF_TRANSFORMATION = c("USD"),
    INDICATOR = c(
      # Current Account
      "CAB_NETCD",
      # Balance of Goods
      "GB_NETCD",
      # Balance of services
      "SEB_NETCD",
      # Balance of Primary Income
      "IN1B_NETCD",
      # Balance of Secondary Income
      "IN2B_NETCD"
    )
  ), 
  start_period = start_year, 
  end_period = end_year
)
