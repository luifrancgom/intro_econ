library(imfapi)

# https://teal-insights.github.io/r-imfapi/
## List available dataflows
### id: BOP
dataflows_imf <- imf_get_dataflows()

##  Get the dimensions for filtering
datastructure_bop <- imf_get_datastructure(dataflow_id = "BOP")

## Code lists BOP