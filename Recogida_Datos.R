library(readxl)
library(dplyr)
library(lubridate)



# Cargar los datos
pib_data <- read_excel("/Datos/Datos-PIB.xls")
ipc_data <- read_excel("/Datoss/Datos-IPC.xls")
tipo_interes_data <- read_excel("/Datos/Datos-Tasa.xlsx")

# Convertir las fechas 
ipc_data$FECHA <- as.Date(paste0(ipc_data$FECHA, "01"), format = "%YM%m%d")
ipc_data$FECHA <- paste0(lubridate::year(ipc_data$FECHA), "T", quarter(ipc_data$FECHA))

tipo_interes_data$FECHA <- as.Date(tipo_interes_data$FECHA, format = "%d %b %Y")
tipo_interes_data$FECHA <- paste0(lubridate::year(tipo_interes_data$FECHA), "T", quarter(tipo_interes_data$FECHA))


# Filtrar datos desde 1993
pib_data <- pib_data %>% filter(substr(FECHA, 1, 4) >= "1993") 
ipc_data <- ipc_data %>% filter(substr(FECHA, 1, 4) >= "1993") 
tipo_interes_data <- tipo_interes_data %>% filter(substr(FECHA, 1, 4) >= "1993") %>% filter(Tipo_Interes != "-") 

ipc_data <- ipc_data %>%
  arrange(FECHA) %>%
  group_by(FECHA) %>%
  slice(n()) %>%
  ungroup()

tipo_interes_data <-tipo_interes_data%>%
  arrange(FECHA) %>%
  group_by(FECHA) %>%
  slice(n()) %>%
  ungroup()
#No es numérico -> lo convertimos:
tipo_interes_data$Tipo_Interes <- as.numeric(as.character(tipo_interes_data$Tipo_Interes))

#Coincide numero filas
nrow(pib_data)
nrow(ipc_data)
nrow(tipo_interes_data)

merged_data <- full_join(pib_data, ipc_data, by = "FECHA") %>%
  full_join(tipo_interes_data, by = "FECHA")


# Guardamos en excel
library(openxlsx)
ruta_archivo <- "/Users/mariamolinadiaz/Downloads/Tabla_Consolidada.xlsx"

# Guarda el dataframe merged_data en un archivo Excel
write.xlsx(merged_data, ruta_archivo, rowNames = FALSE)
