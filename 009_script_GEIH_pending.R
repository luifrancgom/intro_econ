library(DiagrammeR)
library(tidyverse)
library(wbstats)
library(tidyquant)
library(ggrepel)
library(lubridate)
library(knitr)
library(kableExtra)
library(readxl)
library(scales)

# https://www.dane.gov.co/ > Estadísticas por tema >
# Mercado Laboral > Acceso a microdatos Acceder >
# Microdatos > Economía > Mercado Laboral >
# Mercado Laboral. >
# Gran Encuesta Integrada de Hogares - GEIH - 2023.

# Check out Módulo Ocupados
## See Cuestionario Gran Encuesta Integrada de Hogares - GEIH.
## > I. OCUPADOS
### The document with the questionnare is saved in 000_data >
### 009_cuestionario_gran_encuesta_integrada_de_hogares_GEIH.pdf

## In case of poblems importing the data use
## problems(ocupados) 
ocupados <- read_delim(file = "../../Downloads/Nov_2023/009_ocupados_noviembre_2023.csv", 
                       delim = ";", 
                       locale = locale(decimal_mark = "."),
                       col_types = cols(
                         P6430S1 = col_character(),
                         P6585S4A1 = col_integer(),
                         P6585S4A2 = col_integer(),
                         P3055S1 = col_integer(),
                         P6780S1 = col_character(),
                         P7028S1 = col_character()
                       ))

caracteristicas_generales_seguridad_social_salud_educacion <- read_delim(file = "../../Downloads/Nov_2023/009_caracteristicas_generales_seguridad_social_en_salud_y_educacion.csv",
                                                                         delim = ";",
                                                                         locale = locale(decimal_mark = "."))

ocupados_ingreso_laboral <- ocupados |> 
         # Mes
  select(MES,
         # Año
         PER,
         # Año, mes y semana de 
         # recolección de la encuesta 
         PERIODO,
         # identifica la vivienda
         DIRECTORIO,
         # identifica al hogar
         SECUENCIA_P,
         # identifica la persona
         ORDEN,
         # identifica la posición del hogar
         # dentro de la vivienda
         HOGAR,
         # Factor de expansión de personas para
         # el caso del módulo de ocupados
         FEX_C18,
         # Posición ocupacional que la persona
         # tiene en el ejercicio de su trabajo
         ## a. Obrero o empleado de empresa particular (Asalariados)
         ## b. Obrero o empleado del gobierno (Asalariados)
         ## c. Empleado doméstico (Asalariados)
         ## d. Trabajador por cuenta propia (Independientes)
         ## e. Patrón o empleador (Independientes)
         ## f. Trabajador familiar sin remuneración (Trabajadores sin remuneración)
         ## g. Jornalero o peón (Asalariados)
         ## h. Otro (Independientes)
         ### Las categorías en paréntesis corresponden
         ### a la división del módulo en el cuestionario
         ### respecto al empleo principal
         #### Asalariados responden desde la pregunta 15
         #### Independientes responden desde la pregunta
         #### 27 o 31
         #### Trabajadores sin remuneración responden 
         #### desde la pregunta 51
         P6430,
         # Ingreso monetario proveniente del empleo
         # principal de los asalariados
         ## Los ingresos en esta parte solo corresponden
         ## a dinero
         ### Pregunta 17 página 34 en el cuestionario
         #### Valor mensual
         ##### Para entender algunos datos NA ver 
         ##### variable P64430 o Pregunta 14 página 
         ##### 30 donde
         ##### d. Trabajador por cuenta propia
         ##### e. Patrón o empleador
         ##### f. Trabajador familiar sin remuneración
         ##### h. Otro 
         ##### con seguridad no responden esta pregunta
         ###### Verificar para los demás casos con
         ###### ocupados |> 
         ###### select(P6430, P6500) |> 
         ###### filter(!(P6430 %in% c(4, 5, 6, 8))) |>
         ###### filter(is.na(P6500) | P6500 == 0)
         P6500,
         # Ingresos por concepto de horas extras
         ## Verificar y chequear si los incluyo en
         ## P6500
         P6510,
         P6510S1,
         P6510S2,
         # Vivienda como parte de pago del trabajo
         P6590,
         P6590S1,
         # Ingresos en especie por el trabajo
         P6620,
         P6620S1,
         # Chequear si recibió subsidios y si los 
         # reportó en P6500
         ## Auxilio o subsidio de alimentación
         P6585S1,
         P6585S1A1,
         P6585S1A2,
         ## Auxilio o subsidio de transporte
         P6585S2,
         P6585S2A1,
         P6585S2A2,
         ## Subsidio familiar
         P6585S3,
         P6585S3A1,
         P6585S3A2,
         ## Subsidio educativo
         P6585S4,
         P6585S4A1,
         P6585S4A2,
         # Chequear si recibió primas y bonificaciones y 
         # si los reportó en P6500
         ## Primas
         P6545,
         P6545S1,
         P6545S2,
         ## Bonificación
         P6580,
         P6580S1,
         P6580S2,
         # Ingreso monetario proveniente del empleo 
         # principal de los independientes por concepto
         # de honorarios profesionales 
         ## Los ingresos en esta parte solo corresponden
         ## a honorarios profesionales
         ### Pregunta 32 página 46 en el cuestionario
         #### Se refiere al mes pasado
         ##### Para entender algunos datos NA ver 
         ##### variable P64430 o Pregunta 14 página 
         ##### 30 donde
         ##### a. Obrero o empleado de empresa particular 
         ##### b. Obrero o empleado del gobierno
         ##### c. Empleado doméstico
         ##### f. Trabajador familiar sin remuneración
         ##### g. Jornalero o peón
         ##### con seguridad no responden esta pregunta
         ###### Verificar para los demás casos con
         ###### ocupados |> 
         ###### select(P6430, P3051) |> 
         ###### filter(!(P6430 %in% c(4, 5, 6, 8))) |>
         ###### filter(is.na(P3051) | P3051 == 0)
         P3051,
         P3051S1,
         # Ingreso monetario proveniente del empleo 
         # principal de los independientes por concepto
         # de ingresos o comisiones
         ## Los ingresos en esta parte solo corresponden
         ## a ingresos o comisiones
         ### Pregunta 33 página 46 en el cuestionario
         #### Se refiere al mes pasado
         ##### Para entender algunos datos NA ver 
         ##### variable P64430 o Pregunta 14 página 
         ##### 30 donde
         ##### a. Obrero o empleado de empresa particular 
         ##### b. Obrero o empleado del gobierno
         ##### c. Empleado doméstico
         ##### f. Trabajador familiar sin remuneración
         ##### g. Jornalero o peón
         ##### con seguridad no responden esta pregunta
         ###### Verificar para los demás casos con
         ###### ocupados |> 
         ###### select(P6430, P3052) |> 
         ###### filter(!(P6430 %in% c(4, 5, 6, 8))) |>
         ###### filter(is.na(P3052) | P3052 == 0)
         P3052,
         P3052S1,
         # Alimentos como parte de pago del trabajo
         P3054,
         P3054S1,
         # Vivienda como parte de pago del trabajo
         P3055,
         P3055S1,
         # Pendiente de saber si estas variables se
         # deben incluir como ingresos
         ## P3057
         ## P3063 (En el caso de esta parte se va
         ##        para consumo del hogar por lo que
         ##        no se puede determinar de manera 
         ##        directa información para la persona)
         ## P6750
         # Trabajo secundario
         P7070,
         # Ingresos laborales
         ## Queda pendiente el algoritmo
         ## utilizado para calcular el ingreso 
         ## laboral total en base a diferentes
         ## variables del módulo
         INGLABO)

ocupados_ingreso_laboral

nivel_educativo <- caracteristicas_generales_seguridad_social_salud_educacion |> 
  select(DIRECTORIO, SECUENCIA_P, ORDEN, HOGAR,
         # Número de años cumplidos
         ## Esta parte es importante dado que
         ## el módulo se educación solo es
         ## respondido por personas de 3 años
         ## o más
         ### Los valores NA en P3042 corresponden
         ### a personas que no contestan
         P6040,
         # Solo responde personas de 3 años o más
         ## En este caso 99 corresponde a
         ## no sabe / no informa pero si se le pregunta
         P3042, P3042S1, P3042S2,
         P3043, P3043S1)

nivel_educativo |>
  filter(P3042 == 10) |> 
  count(P3042S1)

caracteristicas_generales_seguridad_social_salud_educacion

caracteristicas_generales_seguridad_social_salud_educacion |>
  filter(SECUENCIA_P != HOGAR) |> 
  count(P3042S1)

caracteristicas_generales_seguridad_social_salud_educacion |>
  filter(HOGAR > 3)
