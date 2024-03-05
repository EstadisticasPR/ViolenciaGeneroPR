cat("Loading global objects from global.R...\n\n")
########################################
#### Cargar bibliotecas necesarias #####
########################################
# packages <- c(
#   "tidyverse",
#   "readxl",
#   "kableExtra",
#   "zoo",
#   "here",
#   "viridis",
#   "shinythemes",
#   "plotly",
#   "DT",
#   "shinyWidgets",
#   "sf",
#   "roxygen2"
#   #"leaflet"
# )
# 
# # Instalar los paquetes si no están instalados
# not_installed <- setdiff(packages, installed.packages()[,"Package"])
# if (length(not_installed) > 0) {
#   install.packages(not_installed)
# }
# 
# # Cargar paquetes
# lapply(packages, function(pkg) {
#   library(pkg, character.only = TRUE)
# })

# cargando el dataset starwars para testing
data("starwars")

##################################################################
##### convert_mixed_columns se usa para manejar missing data #####
##################################################################
# funcion para convertir columna con varios tipos de datos a numerico
convert_mixed_columns <- function(data) {
  mixed_columns <- sapply(data, function(col) any(is.character(col) & !is.na(as.numeric(col))))
  mixed_columns_names <- names(mixed_columns)[mixed_columns]
  
  for (col in mixed_columns_names) {
    suppressWarnings({
      data[[col]] <- ifelse(data[[col]] == "N/A", NA, as.character(data[[col]]))
      data[[col]] <- as.numeric(data[[col]])
    })
  }
  
  return(data)
}


##################################################################################
#### Procesamiento de datos del Sistema de Notificacion de Muertes Violentas #####
##################################################################################
# path para Sistema_de_Notificacion_de_Muertes_Violentas
snmv <- here("data", "Sistema_de_Notificacion_de_Muertes_Violentas")

# importar los datos de homicidios por grupo de edad; homiEdad
homiEdad <- read_excel(paste0(snmv, "/svmvHomiEdad.xlsx")) %>%
  select(-Total) %>%
  filter(!grepl("Total", `Grupo de edad`) & `Grupo de edad` != "Desconocido") %>%
  pivot_longer(!`Grupo de edad`, names_to = "año", values_to = "casos") %>%
  rename(edad = `Grupo de edad`) %>%
  mutate(
    edad = factor(edad, levels = unique(edad)),
    año = factor(año)
  )

# Definir una paleta de colores personalizada
colores_homiEdad <- setNames(
  unique(homiEdad$edad), 
  scales::hue_pal()(length(unique(homiEdad$edad)))
)

# importar los datos de incidentes violentos segun tipo de muerte según el año, desde 2017-2020
inci <- read_excel(file.path(snmv, "svmvIncidentes.xlsx")) %>%
  # Seleccionar columnas y pivote largo
  select(-total) %>%
  pivot_longer(!`Tipo de Incidente`, names_to = "año", values_to = "casos") %>%
  # Filtrar datos no deseados
  filter(
    !grepl("Total de víctimas mujeres", `Tipo de Incidente`),
    `Tipo de Incidente` != "Total de incidentes"
  ) %>%
  mutate(
    `Tipo de Incidente` = factor(`Tipo de Incidente`),
    año = factor(año)
  ) %>%
  rename(
    tipo = `Tipo de Incidente`
  )

#############################################################
#### Procesamiento de datos del Departamento de Familia #####
#############################################################
# path para los datos del Departamento de Familia
dfam <- here("data", "Departamento_de_Familia", "/")

# importar los datos de maltratos; dfMalt
# importando datos del 2018
dfMalt2018 <- read_excel(paste0(dfam, "dfMalt2018.xlsx")) %>%
  mutate(Año = "2018")

# importando datos del 2019
dfMalt2019 <- read_excel(paste0(dfam, "dfMalt2019.xlsx")) %>%
  mutate(Año = "2019")

# importando datos del 2020
dfMalt2020 <- read_excel(paste0(dfam, "dfMalt2020.xlsx")) %>%
  mutate(Año = "2020")

# importando datos del 2021
dfMalt2021 <- read_excel(paste0(dfam, "dfMalt2021.xlsx")) %>%
  mutate(Año = "2021")

# importando datos del 2022
dfMalt2022 <- read_excel(paste0(dfam, "dfMalt2022.xlsx")) %>%
  mutate(Año = "2022")

dfMalt <- bind_rows(
  list(dfMalt2018, dfMalt2019, dfMalt2020, dfMalt2021, dfMalt2022)
) %>%
  rename(
    Masculino = `Cantidad Masculino`,
    Femenino = `Cantidad Femenino`
  ) %>%
  pivot_longer(
    !c(`Tipo de Maltrato`, Año),
    names_to = "Sexo",
    values_to = "Casos"
  ) %>%
  mutate(
    `Tipo de Maltrato` = factor(`Tipo de Maltrato`),
    Año = factor(Año),
    Sexo = factor(Sexo)
  ) %>%
  rename(
     Maltrato = `Tipo de Maltrato`
  )

###############################################################
##### Procesamiento de datos del Departamento de Justicia #####
###############################################################
djus <- here("data", "Departamento_de_Justicia", "/")
maps_fol <- here("data", "mapas/")

# importando delitos del 2020
deli2020 <- read_excel(paste0(djus, "djDelitos2020.xlsx")) %>% 
  convert_mixed_columns() %>%
  mutate(Año = "2020") 

# importando delitos del 2021
deli2021 <- read_excel(paste0(djus, "djDelitos2021.xlsx")) %>%
  convert_mixed_columns() %>%
  mutate(Año = "2021")

# importando delitos del 2022
deli2022 <- read_excel(paste0(djus, "djDelitos2022.xlsx")) %>%
  convert_mixed_columns() %>%
  mutate(Año = "2022")

# importando delitos del 2023
deli2023 <- read_excel(paste0(djus, "djDelitos2023.xlsx")) %>%
  convert_mixed_columns() %>%
  mutate(Año = "2023")

dfDeli <- bind_rows(
  list(deli2020, deli2021, deli2022, deli2023)
) %>%
  filter(!grepl("TOTAL", `FISCALIA DISTRITO`, ignore.case = TRUE)) %>%
  select(-TOTAL) %>%
  pivot_longer(-c(`FISCALIA DISTRITO`, Año), names_to = "Delito", values_to = "Casos") %>%
  mutate(
    Año = factor(Año),
    Delito = factor(Delito),
    `FISCALIA DISTRITO` = factor(str_to_title(tolower(`FISCALIA DISTRITO`))
    )
    )

# Crear un dataframe con las coordenadas de las fiscalías policiacas y combinar los datos de delitos con los datos geográficos de los distritos fiscales
mapaDeli <- st_read(paste0(maps_fol, "/distritos_fiscales.json")) %>%
  merge(dfDeli, by.x = "GROUP", by.y = "FISCALIA DISTRITO")


########################################
##### Actualizaciones de los Datos #####
########################################
# Fecha actualizacion de los datos de violencia domestica
actualizacion_policiaA <- "06/09/2023"

# Fecha cuando se actualizan los datos de desaparecidas
actualizacion_policiaB <- "05/03/2023"

# Fecha actualizacion delitos Ley 54
actualizacion_justiciaA <- "06/09/2023"

# Fecha actualizacion registro de personas convictas
actualizacion_justiciaB <- "06/09/2023"

# Fecha actualizacion feminicidios
actualizacion_opmA <- "05/17/2022"

# Fecha actualizacion resto de los datos de OPM
actualizacion_opmB <- "04/20/2023"

# Fecha actualizacion datos del SNMV
actualizacion_snmvA <- "12/23/2021"

# Fecha actualizacion tasas ajustadas del SNMV
actualizacion_snmvB <- "06/30/2022"

# Fecha actualizacion datos de Correcion
actualizacion_correcion <- "04/03/2023"

# Fecha actualizacion datos de Vivienda
actualizacion_vivienda <- "07/19/2023"

# Fecha actualizacion datos del Observatorio
actualizacion_observatorio <- ""

# Fecha actualizacion datos del Dept de Trabajo
actualizacion_trabajo <- "07/12/2021"

# Fecha actualizacion datos Dept Familia
actualizacion_familia <- "06/09/2021"

# Fecha actualizacion Ordenes de Proteccion
actualizacion_tribunalesA <- "02/21/2023"

# Fecha actualizacion Movimiento Casos
actualizacion_tribunalesB <- "02/21/2023"
