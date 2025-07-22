library(tidyverse)
library(rsdmx)

# BanRep
## https://www.banrep.gov.co/ > 
## Estadísticas económicas > 
## ¡Nuevo portal de estadísticas económicas! > 
## Series de estadísticas económicas > 
## Tablas preconstruidas > 
## Sector externo, tasas de cambio y derivados > 
## Sector externo > 
## Balanza de pagos > 
## Series recientes > 
## Resumen
### Si necesita descargar la información incluida en alguna de las tablas preconstruidas, ubíquese sobre cualquier dato, pulse clic alterno (derecho) en el mouse y seleccione la opción Exportar, en la cual sugerimos descargar la versión en Excel. Recuerde que esta acción solo está disponible en la versión de escritorio del Portal de Estadísticas Económicas.
#### https://www.banrep.gov.co/es/preguntas-frecuentes/como-encontrar-series-sobre-balanza-pagos-nuevo-portal-estadisticas-economicas

# API
## https://www.imf.org/en/home >
## DATA >
## IMF Data Portal >
## Data Resources >
## API

# Documentation
## https://www.imf.org/en/home >
## DATA >
## IMF Data Portal >
## DATASETS >
## Search IMF Data: Balance of Payments (BOP) >
## Dataset - Balance of Payments (BOP) >
## VIEW DATA >
## Data Explorer >
### Display All/ By Table: Full List
### Country: Colombia
### Unit of Measure: US Dollar
### Frequency: Annual
### Period: Custom
#### 2016-01-01 | 2024-12-31

# rsdmx ----
flowref <- 'IMF.STA,BOP'
          # Current account balance excluding exceptional financing
          ### Display All/ By Table: Balance of Payments Aggregate Analytical Representation
filter_one_series <- 'COL.NETCD_T.CABXEF.USD.A'
filter_all_series <- 'COL...USD.A'
filter_specific_series <- 'COL.NETCD_T+CD_T.CABXEF+S.USD.A'
dataset <- as.data.frame(readSDMX(providerId = 'IMF_DATA',
                                  resource = 'data',
                                  flowRef = flowref,
                                  key = filter_specific_series, 
                                  start = 2000,
                                  end = 2024))

dataset |> 
  as_tibble() |> 
  count(BOP_ACCOUNTING_ENTRY)
  glimpse()
