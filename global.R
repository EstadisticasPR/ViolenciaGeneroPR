cat("Loading global objects from global.R...\n\n")
########################################
#### Cargar bibliotecas necesarias #####
########################################
library(shiny)
library(here)
library(dplyr)
library(purrr)
library(readxl)
library(stringr)
library(tidyr)
library(shinythemes)
library(plotly)
library(DT)
library(shinyWidgets)
library(RColorBrewer)
library(rsconnect)
library(sf)
library(zoo)
library(writexl)
library(scales)
library(ggplot2)
library(leaflet)
library(grDevices)
library(htmlwidgets)
source("utils.R")

# packages <- c(
#   #"tidyverse",
#   #"testthat",
#   "shiny",
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
#   "roxygen2",
#   "htmltools",
#   "shinymanager",
#   "packrat",
#   "rsconnect",
#   "writexl"
# )
# 
# # Instalar los paquetes si no están instalados
# not_installed <- setdiff(packages, installed.packages()[,"Package"])
# if (length(not_installed) > 0) {
#   install.packages(not_installed)
# }

# Cargar paquetes
# lapply(packages, function(pkg) {
#   library(packages)
# })

# cargando el dataset starwars para testing
#data("starwars")

##################################################################
##### convert_mixed_columns se usa para manejar missing data #####
##################################################################
##################################################################################
#### Funcion para convertir columna con varios tipos de datos a numerico #####
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
##################################################################
#### Funcion para guardar dataframes por agencias #####
guardarDatos <- function(dataframes, nombres_sheets, nombre_archivo) {
  # Añadir la extensión .xlsx al nombre proporcionado
  nombre_archivo <- paste0(nombre_archivo, ".xlsx")
  
  # Guardar los dataframes en diferentes hojas del mismo archivo Excel
  writexl::write_xlsx(setNames(dataframes, nombres_sheets), nombre_archivo)
  
  # Mensaje de confirmación
  message("Archivo guardado como: ", nombre_archivo, " con ", length(dataframes), " hojas.")
}

guardarDatos_Polygons <- function(dataframe, nombre_archivo) {
  # Asegurarse de que el dataframe es de tipo 'sf'
  if (!inherits(dataframe, "sf")) {
    stop("El dataframe proporcionado no es un objeto espacial (sf).")
  }
  
  # Añadir la extensión .shp al nombre del archivo
  nombre_shapefile <- paste0(nombre_archivo, ".shp")
  
  # Guardar el dataframe espacial como un Shapefile
  st_write(dataframe, nombre_shapefile, delete_layer = TRUE)
  
  # Mensaje de confirmación
  message("Shapefile guardado como: ", nombre_shapefile)
}

##################################################################
#### Procesamiento de datos del Sistema de Notificacion de Muertes Violentas #####
##################################################################################
# path para Sistema_de_Notificacion_de_Muertes_Violentas
snmv <- here::here("data", "Sistema_de_Notificacion_de_Muertes_Violentas")

# importar los datos de homicidios por grupo de edad; homiEdad
homiEdad <- read_excel(paste0(snmv, "/svmvHomiEdad.xlsx")) %>%
  select(-Total) %>%
  filter(!grepl("Total", `Grupo de edad`) & `Grupo de edad` != "Desconocido") %>%
  pivot_longer(!`Grupo de edad`, names_to = "año", values_to = "casos") %>%
  rename(Edad = `Grupo de edad`) %>%
  rename(Año = año) %>%
  rename(Casos = casos) %>%
  replace_na(list(Casos = 0)) %>%
  mutate(
    Edad = str_replace(Edad, "^Menos de 15 años$", "menos de 15 años"),
    Edad = factor(Edad, levels = unique(Edad)),
    Año = factor(Año)
  ) %>%
  relocate(
    Año, Edad, Casos
  )

# Definir una paleta de colores personalizada
colores_homiEdad <- setNames(
  unique(homiEdad$Edad), 
  scales::hue_pal()(length(unique(homiEdad$Edad)))
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
  rename(Incidente = `Tipo de Incidente`) %>%
  rename(Año = año) %>%
  rename(Casos = casos) %>%
  replace_na(list(Casos = 0)) %>%
  relocate(
    Año, Incidente, Casos
  )

#### Guardar datos procesados de Sistema de Notificacion de Muertes Violentas ####
# dataframes <- list(homiEdad, inci) # Lista de dataframes (por ejemplo: homiEdad y inci)
# 
# # Lista de nombres de hojas correspondientes a los dataframes
# nombres_sheets <- list("homicidios_mujeres_edadV", "incidentes_violentos")
# 
# # Uso de la función
# guardarDatos(dataframes, nombres_sheets, "sistema_notificacion_muertes_violentas")


#############################################################
#### Procesamiento de datos del Departamento de Familia #####
#############################################################
# path para los datos del Departamento de Familia
dfam <- here::here("data", "Departamento_de_Familia", "/")

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
  mutate(
    `Ambos Sexos` = Masculino + Femenino
  ) %>%
  pivot_longer(
    !c(`Tipo de Maltrato`, Año),
    names_to = "Sexo",
    values_to = "Casos"
  ) %>%
  mutate(
    `Tipo de Maltrato` = factor(`Tipo de Maltrato`,
                                levels = c("Abuso Sexual", "Explotación", "Maltrato Físico",      
                                           "Negligencia", "Negligencia Educativa", "Negligencia Emocional",
                                           "Negligencia Médica", "Trata Humana", "Otro"),
                                ordered = TRUE),
    Año = factor(Año),
    Sexo = factor(Sexo)
  ) %>%
  rename(
     Maltrato = `Tipo de Maltrato`
  ) %>%
  replace_na(list(Casos = 0)) %>%
  distinct() %>%
  relocate(
    Año, Maltrato, Sexo, Casos
  )

# Crear el dataset filtrado y sumarizado de negligencia
negligencia_sum <- dfMalt %>%
  filter(grepl("Negligencia", Maltrato, ignore.case = TRUE)) %>%
  group_by(Año, Sexo) %>%
  summarise(total_casos = sum(Casos)) %>%
  mutate(Maltrato = "Negligencia") %>%
  rename(Casos = total_casos)

# Añadir el dataset de negligencia al dataset original
dfMalt <- dfMalt %>%
  filter(!grepl("Negligencia", Maltrato, ignore.case = TRUE)) %>% # Eliminar las filas de negligencia existentes
  bind_rows(negligencia_sum) %>% # Unir el dataset original con el dataset de negligencia 
  mutate(Maltrato = factor(Maltrato,
                                levels = c("Abuso Sexual", "Explotación", "Maltrato Físico",      
                                           "Trata Humana", "Otro"),
                                ordered = TRUE))


#### Guardar datos procesados de Departamento de Familia ####
# dataframes <- list(dfMalt) # Lista de dataframes (por ejemplo: homiEdad y inci)
# 
# # Lista de nombres de hojas correspondientes a los dataframes
# nombres_sheets <- list("maltrato_menores_victimas")
# 
# # Uso de la función
# guardarDatos(dataframes, nombres_sheets, "departamento_familia")

###############################################################
##### Procesamiento de datos del Departamento de Justicia #####
###############################################################
djus <- here::here("data", "Departamento_de_Justicia", "/")
maps_fol <- here::here("data", "mapas/")

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
    ),
    Delito = recode(Delito,
                    "Art3.5" = "Agresión Sexual Conyugal",
                    "Art3.2" = "Maltrato Agravado",
                    "Art3.1" = "Maltrato",
                    "Art3.3" = "Maltrato por Amenaza",
                    "Art3.4" = "Maltrato por Restricción de Libertad",
                    "Art2.8" = "Incumplimiento de la Órden de Protección")
  ) %>%
  replace_na(list(Casos = 0)) %>%
  rename(Distrito = `FISCALIA DISTRITO`) %>%
  relocate(
    Año, Distrito, Delito, Casos
  )

# Crear un dataframe con las coordenadas de las fiscalías policiacas y combinar 
# los datos de delitos con los datos geográficos de los distritos fiscales
mapaDeli <- st_read(paste0(maps_fol, "/distritos_fiscales.shp")) %>%
  merge(dfDeli, by.x = "GROUP", by.y = "Distrito") %>%
  relocate(
    Año, GROUP, Delito, geometry,Casos
  )

#### Guardar datos procesados de Departamento de Justicia ####
# dataframes <- list(dfDeli) # Lista de dataframes (por ejemplo: homiEdad y inci)
# 
# # Lista de nombres de hojas correspondientes a los dataframes
# nombres_sheets <- list("delitos_ArticuloLey54")
# 
# # Uso de la función
# guardarDatos(dataframes, nombres_sheets, "departamento_justicia")

# Uso de la función
# guardarDatos_Polygons(mapaDeli, "departamento_justicia_distritosfiscales")

###############################################################################
#### Procesamiento de datos el Departamento del Trabajo y Recursos Humanos ####
###############################################################################
dtra <- here::here("data", "departamento_de_trabajo", "/")

# importando el dataset de Casos en Supervisión de Ley 54
parLab <- read_excel(paste0(dtra, "dtpartlab.xlsx")) %>%
  rename(
    Sexo = Género
  ) %>%
  filter(Sexo != "Ambos") %>%
  pivot_longer(!Sexo, names_to = "Año", values_to = "Tasa") %>%
  mutate(
    Año = factor(Año),
    Sexo = factor(Sexo)
  ) %>%
  relocate(
    Año, Sexo, Tasa
  )

##########################################################################
#### Procesamiento de datos de la Administración de Vivienda Pública #####
##########################################################################
avp <- here::here("data", "Administracion_de_viviendas_publicas/")

#### dfAvp ####
avpAsignadas <- read_excel(paste0(avp, "/avpAsignadas2017_23.xlsx")) %>% 
  rename(región = `Región `) %>%
  pivot_longer(!región, names_to = "año", values_to = "asignadas")

avpSolicitadas <- read_excel(paste0(avp, "/avpSolicitudes2017_23.xlsx")) %>% 
  rename(región = `Región `) %>%
  pivot_longer(!región, names_to = "año", values_to = "solicitadas")

# Unir los datasets por columna "región" y "año"
dfAvp <- left_join(avpSolicitadas, avpAsignadas, by = c("región", "año")) %>% 
  filter(región != "Total")  %>%
  pivot_longer(
    !c(región, año), names_to = "status", values_to = "cantidad"
  ) %>%
  mutate(
    región = factor(región),
    status = factor(status, levels = c("solicitadas", "asignadas"))
  ) %>%
  filter(
    año != "*2023"
  ) %>%
  rename(Año = año) %>%
  rename(Región = región) %>%
  rename(Estado = status) %>%
  rename(Cantidad = cantidad) %>%
  replace_na(list(Cantidad = 0)) %>%
  relocate(
    Año, Región, Estado, Cantidad
  )

# Convertir el año a numérico para eliminar el asterisco y convertirlo a int
dfAvp$Año <- as.factor(sub("\\*", "", dfAvp$Año))

# Crear un dataframe con las coordenadas de las fiscalías policiacas y 
# combinar los datos de delitos con los datos geográficos de los distritos fiscales
mapaAvp <- st_read(paste0(maps_fol, "/regiones_vivienda.shp")) %>%
  merge(dfAvp, by.x = "GROUP", by.y = "Región") %>%
  rename(Región = GROUP) %>%
  relocate(
    Año, Región, Estado, geometry, Cantidad 
  )

#### Guardar datos procesados de Administración de Vivienda Pública ####
# dataframes <- list(dfAvp) # Lista de dataframes (por ejemplo: homiEdad y inci)
# 
# # Lista de nombres de hojas correspondientes a los dataframes
# nombres_sheets <- list("viviendasPublicas_porViolenciaDomestica")
# 
# # Uso de la función
# guardarDatos(dataframes, nombres_sheets, "administracion_vivienda_publica")

############################################################################
#### Procesamiento de datos Departamento de Corrección y Rehabilitación ####
############################################################################
dcr <- here::here("data", "Departamento_de_correccion_y_rehabilitacion", "/")

#### dcrCasosInv ####
# importando el dataset de Casos en Supervisión de Ley 54
dcrCasosInv <- read_excel(paste0(dcr, "dcrCasosInv.xlsx")) %>%
  select(-c(mes)) %>%
  group_by(tipo, year, sexo) %>%
  summarise(cantidad = sum(cantidad, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    year = factor(year),
    sexo = factor(sexo),
    tipo = factor(tipo)
  ) %>%
  rename(Año = year) %>%
  rename(Sexo = sexo) %>%
  rename(Estado = tipo) %>%
  rename(Cantidad = cantidad) %>%
  replace_na(list(Cantidad = 0)) %>%
  relocate(
    Año, Sexo, Estado, Cantidad
  )

#### dcrSentenciadas ####

dcrSentenciadas <- read_excel(paste0(dcr, "dcrSentenciadas.xlsx"))  %>%
  mutate(
    tipo = factor(tipo),
    year = factor(year)
  ) 
# Combinar mes y año en una nueva columna
dcrSentenciadas$fecha <- as.Date(paste0(dcrSentenciadas$year, "-", dcrSentenciadas$mes, "-01"))
# dcrSentenciadas <- dcrSentenciadas %>%
#   rename(Año = year) %>%
#   rename(Mes = mes) %>%
#   rename(Fecha = fecha) %>%
#   rename(Estado = tipo) %>%
#   rename(Cantidad = cantidad) %>%
#   replace_na(list(Cantidad = 0)) %>%
#   select(
#     Mes, Año, Fecha, Estado, Cantidad
#   )

# Convertir los valores de mes de numérico a nombre del mes en español
meses_es <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
              "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

dcrSentenciadas <- dcrSentenciadas %>%
  mutate(mes = factor(mes, levels = 1:12, labels = meses_es)) %>%
  rename(Año = year) %>%
  rename(Mes = mes) %>%
  rename(Fecha = fecha) %>%
  rename(Estado = tipo) %>%
  rename(Cantidad = cantidad) %>%
  replace_na(list(Cantidad = 0)) %>%
  select(
    Mes, Año, Fecha, Estado, Cantidad
  )

#### Guardar datos procesados de Departamento de Corrección y Rehabilitación ####
# dataframes <- list(dcrCasosInv, dcrSentenciadas) # Lista de dataframes (por ejemplo: homiEdad y inci)
# 
# # Lista de nombres de hojas correspondientes a los dataframes
# nombres_sheets <- list("casos_supervision_ley54", "casos_sentenciados_violencia_domestica")
# 
# # Uso de la función
# guardarDatos(dataframes, nombres_sheets, "departamento_correcion")


#########################################################
#### Procesamiento de datos del Negociado de Policía ####
#########################################################
poli <- here::here("data", "Negociado_de_Policia", "/")

#### despDF ####
meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# importando data del 2020
# desp2020 <- read_excel(paste0(poli, "npprDesp2020.xlsx")) %>% 
#   mutate(Año = "2020") %>% 
#   rename(Categoria = Mes,Total = `Total Año 2020`)

sheet_name = "2020"
desp2020 <- read_excel(paste0(poli, "npprDesp.xlsx"),
                       sheet = sheet_name) %>%
  cleanSheet_npprDesp(sheet_name)

# importando data del 2021
# desp2021 <- read_excel(paste0(poli, "npprDesp2021.xlsx")) %>%
#   mutate(Año = "2021") %>%
#   rename(Categoria = Mes, Total = `Total Año 2021`)

sheet_name = "2021"
desp2021 <- read_excel(paste0(poli, "npprDesp.xlsx"),
                       sheet = sheet_name) %>%
  cleanSheet_npprDesp(sheet_name)

# importando data del 2022
# desp2022 <- read_excel(paste0(poli, "npprDesp2022.xlsx")) %>%
#   mutate(Año = "2022") %>%
#   rename(Categoria = Mes, Total = `Total Año 2022`)

sheet_name = "2022"
desp2022 <- read_excel(paste0(poli, "npprDesp.xlsx"),
                       sheet = sheet_name) %>%
  cleanSheet_npprDesp(sheet_name)

#No incluyo 2023 porque faltan datos desde abril
# desp2023 <- read_excel("data/Negociado_de_Policia/npprDesp2023.xlsx") %>% mutate(Año = "2023") %>% rename(Categoria = Mes, Total = `Total Año 2023`)

sheet_name = "2023"
desp2023 <- read_excel(paste0(poli, "npprDesp.xlsx"),
                       sheet = sheet_name) %>%
  cleanSheet_npprDesp(sheet_name)

sheet_name = "2024"
desp2024 <- read_excel(paste0(poli, "npprDesp.xlsx"),
                       sheet = sheet_name) %>%
  cleanSheet_npprDesp(sheet_name)

# uniendo los datasets de 2020, 2021 y 2022
# despDF <- bind_rows(
#   list(desp2020, desp2021, desp2022, desp2023)
# ) %>%
#   pivot_longer(cols = -c(Categoria, Año), names_to = "Meses", values_to = "Casos") %>%
#   filter(!grepl("Total", Meses)) %>%
#   group_by(Categoria, Año) %>%
#   summarise(Casos = sum(Casos, na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(
#     Año = factor(Año),
#     Estado = factor(Categoria)
#   ) %>%
#   replace_na(list(Casos = 0)) %>%
#   select(
#     Año, Estado, Casos
#   )

despDF_list <- list(desp2020,
                    desp2021,
                    desp2022,
                    desp2023,
                    desp2024)

# Unir todos los data frames en la lista usando full_join
despDF <- despDF_list %>%
  reduce(full_join) %>%
  pivot_longer(cols = -c(Categoria, Año), names_to = "Meses", values_to = "Casos") %>%
  filter(!grepl("Total", Meses)) %>%
  group_by(Categoria, Año) %>%
  summarise(Casos = sum(Casos, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    Año = factor(Año),
    Estado = factor(Categoria)
  ) %>%
  replace_na(list(Casos = 0)) %>%
  select(
    Año, Estado, Casos
  )

#### vEdad ####
sheet_name = "2021"
vEdad2021 <- read_excel(paste0(poli, "npprVDedad.xlsx"),
                       sheet = sheet_name) %>%
  cleanSheet_npprVDedad(sheet_name)

sheet_name = "2022"
vEdad2022 <- read_excel(paste0(poli, "npprVDedad.xlsx"),
                        sheet = sheet_name) %>%
  cleanSheet_npprVDedad(sheet_name)

sheet_name = "2023"
vEdad2023 <- read_excel(paste0(poli, "npprVDedad.xlsx"),
                        sheet = sheet_name) %>%
  cleanSheet_npprVDedad(sheet_name)

# vEdad2021 <- read_excel(paste0(poli, "npprVDedad2021.xlsx")) %>% 
#   mutate(Año = "2021")
# vEdad2022 <- read_excel(paste0(poli, "npprVDedad2022.xlsx")) %>% 
#   mutate(Año = "2022")
# vEdad2023 <- read_excel(paste0(poli, "npprVDedad2023.xlsx")) %>% 
#   mutate(Año = "2023")

vEdad_list <- list(vEdad2021,
                    vEdad2022,
                    vEdad2023)

# Unir todos los data frames en la lista usando full_join
vEdad <- vEdad_list %>%
  reduce(full_join) %>%
  rename(
    Edad = `Grupos de Edad`,
    `Ambos Sexos` = Total,
    Mujeres = `Cantidad de mujeres víctimas`,
    Hombres = Masculino,
    Año = Año
  ) %>%
  pivot_longer(
    !c(Edad, Año), names_to = "Sexo", values_to = "Casos"
  ) %>%
  mutate(
    Edad = str_replace_all(Edad, c("^< 16$" = "menos de 16 años", "^65 o más$" = "65 años o más")),
    Edad = factor(Edad, levels = c("menos de 16 años", "16-17", "18-19", "20-24", "25-29", "30-34",
                                   "35-39","40-44","45-49","50-54","55-59","60-64","65 años o más",
                                   "Desconocida"),
                  ordered = TRUE),
    Año = factor(Año),
    Sexo = factor(Sexo, levels = c("Hombres", "Mujeres", "Ambos Sexos", "Desconocido"),
                  ordered = TRUE)
  ) %>%
  replace_na(list(Casos = 0)) %>%
  select(
    Año, Edad, Sexo, Casos
  )

#### inciDF ####
inci2021 <- read_excel(paste0(poli, "NPPRincidentes_2021.xlsx")) %>% 
  rename_with(~gsub("2021", "",.), contains("2021")) %>%
  rename_at(vars(2), ~ "Población") %>% 
  mutate(Año = "2021")

# faltan datos para el 2022
inci2022 <- read_excel(paste0(poli, "NPPRincidentes_2022.xlsx")) %>% 
  rename_with(~gsub("2022", "",.), contains("2022")) %>%
  rename_at(vars(2), ~"Población") %>% 
  mutate(Año = "2022")

# faltan datos desde mayo en adelante
inci2023 <- read_excel(paste0(poli,"NPPRincidentes_2023.xlsx")) %>% rename_with(~gsub("2022", "",.), contains("2022")) %>%
  rename_with(~gsub("2023", "",.), contains("2023")) %>%
  rename_at(vars(2), ~ "Población") %>%
  mutate(Año = "2023")

# dataframe con toda la data combinada
inciDF <- bind_rows(inci2021, inci2022, inci2023) %>%
  filter(`Áreas Policiacas` != "Total") %>%
  pivot_longer(cols = -c(`Áreas Policiacas`, Población, Año), names_to = "Mes", values_to = "Casos") %>%
  mutate(
    `Áreas Policiacas` = factor(str_trim(`Áreas Policiacas`)),
    Año = factor(Año),
    Meses = factor(Mes), 
    Meses_Numéricos = match(Meses, Mes),
    Fecha = as.yearmon(paste(Año, Meses_Numéricos), "%Y %m")
  ) %>%
  replace_na(list(Casos = 0)) %>%
  select(-c(Meses_Numéricos, Mes, Meses)) %>%
  relocate(
    Año, `Áreas Policiacas`, Población, Casos
  )

# Crear un dataframe con las coordenadas de las fiscalías policiacas y 
# combinar los datos de delitos con los datos geográficos de los distritos fiscales
inciMapa <- st_read(paste0(maps_fol, "/distritos_fiscales.shp")) %>%
  merge(inciDF, by.x = "GROUP", by.y = "Áreas Policiacas")

#### Guardar datos procesados de Negociado de Policía ####
# dataframes <- list(despDF, vEdad) # Lista de dataframes (por ejemplo: homiEdad y inci)
# 
# # Lista de nombres de hojas correspondientes a los dataframes
# nombres_sheets <- list("mujeres_desaparecidas_localizadas", "victimas_violencia_domestica")
# 
# # Uso de la función
# guardarDatos(dataframes, nombres_sheets, "negociado_policia")
# 

###############################################################################
#### Procesamiento de datos de la Oficina de la Procuradora de las Mujeres ####
###############################################################################
opm <- here::here("data", "Oficina_de_procuradora_de_mujeres", "/")

#### opmFemiVD ####
opmFemiVD <- read_excel(paste0(opm, "opmFemiVD.xlsx")) %>%
  mutate(Año = factor(Año)) %>%
  rename(
    Asesinatos = `Cantidad de asesinatos`,
    Tasa = `Tasa (x100,000 mujeres)`
  ) 

#### opmCasos ####
opmCasos <- read_excel(paste0(opm, "opmPartMes.xlsx")) %>%
  select(-c(month)) %>%
  group_by(tipo, year) %>%
  summarise(cantidad = sum(cantidad, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    year = factor(year),
    tipo = str_remove(tipo, " \\(.*\\)") %>%  # Quitar el texto entre paréntesis
      factor(levels = c("Acecho", "Agresión sexual", "Discrimen de género",
                        "Violencia doméstica", "Violencia en cita", "Otras"))
  ) %>%
  rename(Año = year) %>%
  rename(Razón = tipo) %>%
  rename(Cantidad = cantidad) %>%
  replace_na(list(Cantidad = 0)) %>%
  relocate(
    Año, Razón, Cantidad
  )


#### opmVic ####
opmVic <- read_excel(paste0(opm, "opmVicGraf.xlsx")) %>% 
  rename_at(vars(1,2,3,4), ~ c("género","2020", "2021", "2022")) %>%
  pivot_longer(!género, names_to = "año", values_to = "víctimas") %>%
  mutate(
    género = factor(género,
                    levels = c("Femenino", "Masculino", "Trans", "No informó")),
    año = factor(año)
  ) %>%
  rename(Año = año) %>%
  rename(Género = género) %>%
  rename(Víctimas = víctimas) %>%
  replace_na(list(Víctimas = 0)) %>%
  relocate(
    Año, Género, Víctimas
  )

#### opmMedio ####
opmMedio <- read_excel(paste0(opm, "opmMedio.xlsx")) %>% 
  rename_at(vars(2,3,4), ~ c("2020", "2021", "2022")) %>%
  pivot_longer(!`Medio de orientación`, names_to = "año", values_to = "personas atendidas") %>% 
  filter(`Medio de orientación` != "Total") %>%
  mutate(
    `Medio de orientación` = factor(`Medio de orientación`),
    año = factor(año)
  ) %>%
  rename(Año = año) %>%
  rename(Orientación = `Medio de orientación`) %>%
  rename(Cantidad = `personas atendidas`) %>%
  replace_na(list(Cantidad = 0)) %>%
  relocate(
    Año, Orientación, Cantidad
  )


#### opmServiciosMes ####
opmServiciosMes <-  read_excel(paste0(opm, "opmServiciosMes.xlsx")) %>%
  select(-c(month)) %>%
  group_by(tipo, year) %>%
  summarise(cantidad = sum(cantidad, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    tipo = factor(tipo),
    year = factor(year)
  ) %>%
  rename(Año = year) %>%
  rename(Servicio = tipo) %>%
  rename(Cantidad = cantidad) %>%
  replace_na(list(Cantidad = 0)) %>%
  relocate(
    Año, Servicio, Cantidad
  )

#### Guardar datos procesados de Oficina de la Procuradora de las Mujeres ####
# dataframes <- list(opmFemiVD, opmCasos, opmVic, opmMedio, opmServiciosMes) # Lista de dataframes (por ejemplo: homiEdad y inci)
# 
# # Lista de nombres de hojas correspondientes a los dataframes
# nombres_sheets <- list("tasa_asesinatos_mujeres_violencia_domestica", "personas_atendidas_programaCRIAS",
#                        "identidad_genero_victimas_atendidas_CRIAS", "orientaciones_ofrecidas_programaCRIAS",
#                        "servicio_alcance_programaCRIAS")
# 
# # Uso de la función
# guardarDatos(dataframes, nombres_sheets, "oficina_procuradora_mujeres")
# 

###################################################################
#### Procesamiento de datos de la Administración de Tribunales ####
###################################################################

trib <- here::here("data", "administracion_de_tribunales", "/")

#### casosCrimLey148 ####
## Solicitudes de órdenes de protección al amparo de la Ley 148 - Violencia Sexual,
## por Región Judicial y grupo de edad de la parte peticionaria

# importando datos del año fiscal 2020-2021
## faltan datos

# casosCrimLey148_20 <- read_excel(paste0(trib, "casosCrimLey148_20.xlsx")) %>%
#   rename(
#     Delito = `Año fiscal/delitos`
#   ) %>%
#   pivot_longer(
#     !Delito,
#     names_to = "Status",
#     values_to = "Casos"
#   ) %>%
#   mutate(AñoFiscal = "2020-2021") %>%
#   filter(Delito != "2020-2021")

sheet_name = "2020-2021"
casosCrimLey148_20 <- read_excel(paste0(trib, "casosCrimLey148.xlsx"),
                                 sheet = sheet_name) %>%
  rename(
    Delito = `Año fiscal/delitos`
  ) %>%
  pivot_longer(
    !Delito,
    names_to = "Status",
    values_to = "Casos"
  ) %>%
  mutate(AñoFiscal = "2020-2021") %>%
  filter(Delito != "2020-2021")

# importando datos del año fiscal 2021-2022
## faltan datos
# casosCrimLey148_21 <- read_excel(paste0(trib, "casosCrimLey148_21.xlsx")) %>%
#   rename(
#     Delito = `Año fiscal/delitos`
#   ) %>%
#   pivot_longer(
#     !Delito,
#     names_to = "Status",
#     values_to = "Casos"
#   ) %>%
#   mutate(AñoFiscal = "2021-2022") %>%
#   filter(Delito != "2021-2022*")

sheet_name = "2021-2022"
casosCrimLey148_21 <- read_excel(paste0(trib, "casosCrimLey148.xlsx"),
                                 sheet = sheet_name) %>%
  rename(
    Delito = `Año fiscal/delitos`
  ) %>%
  pivot_longer(
    !Delito,
    names_to = "Status",
    values_to = "Casos"
  ) %>%
  mutate(AñoFiscal = "2021-2022") %>%
  filter(Delito != "2021-2022")

sheet_name = "2022-2023"
casosCrimLey148_22 <- read_excel(paste0(trib, "casosCrimLey148.xlsx"),
                                 sheet = sheet_name) %>%
  rename(
    Delito = `Año fiscal/delitos`
  ) %>%
  pivot_longer(
    !Delito,
    names_to = "Status",
    values_to = "Casos"
  ) %>%
  mutate(AñoFiscal = "2022-2023") %>%
  filter(Delito != "2022-2023")

sheet_name = "2023-2024"
casosCrimLey148_23 <- read_excel(paste0(trib, "casosCrimLey148.xlsx"),
                                 sheet = sheet_name) %>%
  rename(
    Delito = `Año fiscal/delitos`
  ) %>%
  pivot_longer(
    !Delito,
    names_to = "Status",
    values_to = "Casos"
  ) %>%
  mutate(AñoFiscal = "2023-2024") %>%
  filter(Delito != "2023-2024")

## faltan datos
# Lista de data frames
casosCrimLey148_list <- list(casosCrimLey148_20, casosCrimLey148_21, casosCrimLey148_22, casosCrimLey148_23)

# Unir todos los data frames en la lista usando full_join
casosCrimLey148 <- casosCrimLey148_list %>%
  reduce(full_join) %>%
  mutate(
    AñoFiscal = case_when(
      AñoFiscal == "2020-2021" ~ "2021",
      AñoFiscal == "2021-2022" ~ "2022",
      AñoFiscal == "2022-2023" ~ "2023",
      AñoFiscal == "2023-2024" ~ "2024",
      TRUE ~ AñoFiscal
    ),
    AñoFiscal = factor(AñoFiscal),
    Status = factor(Status), 
    Delito = factor(Delito)
  ) %>%
  replace_na(list(Casos = 0)) %>%
  relocate(AñoFiscal, Status, Delito, Casos)

#### OP_148_SoliGrupEdad ####

# Solicitudes de órdenes de protección al amparo de la Ley 148 - Violencia Sexual, 
# por Región Judicial y grupo de edad de la parte peticionaria

new_names <- c("Total", "20 años o menos", "21-29", "30-39", "40-49", "50-59", "60 años o más", "No Indica")

# datos de solicitudes de órdenes de protección en el 2020-2021
# OP_148_SoliGrupEdad2020_21 <- read_excel(paste0(trib, "OP_148_SoliGrupEdad2020_21.xlsx")) %>%
#   rename_at(vars(2:9), ~ new_names) %>%
#   pivot_longer(
#     !Región,
#     names_to = "Edad",
#     values_to = "Solicitudes"
#   ) %>%
#   mutate(
#     Edad = factor(Edad, levels = unique(Edad)),
#     AñoFiscal = "2020-2021"
#   ) %>%
#   filter(
#     Edad != "Total",
#     Región != "Total"
#   )

sheet_name = "2020-2021"
OP_148_SoliGrupEdad2020_21 <- read_excel(paste0(trib, "OP_148_SoliGrupEdad.xlsx"),
                                         sheet = sheet_name) %>%
  cleanSheet_OP_148_SoliGrupEdad(sheet_name, new_names)
# rename_at(vars(2:9), ~ new_names) %>%
# pivot_longer(
#   !Región, 
#   names_to = "Edad", 
#   values_to = "Solicitudes"
# ) %>%
# mutate(
#   Edad = factor(Edad, levels = unique(Edad)),
#   AñoFiscal = "2020-2021"
# ) %>%
# filter(
#   Edad != "Total",
#   Región != "Total"
# )

# datos de solicitudes de órdenes de protección en el 2021-2022
sheet_name = "2021-2022"
OP_148_SoliGrupEdad2021_22 <- read_excel(paste0(trib, "OP_148_SoliGrupEdad.xlsx"),
                                         sheet = sheet_name) %>%
  cleanSheet_OP_148_SoliGrupEdad(sheet_name, new_names)
  # rename_at(vars(2:9), ~ new_names) %>%
  # pivot_longer(
  #   !Región, 
  #   names_to = "Edad", 
  #   values_to = "Solicitudes"
  # ) %>%
  # mutate(
  #   Edad = factor(Edad, levels = unique(Edad)),
  #   AñoFiscal = "2021-2022"
  # ) %>%
  # filter(
  #   Edad != "Total",
  #   Región != "Total"
  # )

# datos de solicitudes de órdenes de protección en el 2022-2023
sheet_name = "2022-2023"
OP_148_SoliGrupEdad2022_23 <- read_excel(paste0(trib, "OP_148_SoliGrupEdad.xlsx"),
                                         sheet = sheet_name) %>%
  cleanSheet_OP_148_SoliGrupEdad(sheet_name, new_names)
  # rename_at(vars(2:9), ~ new_names) %>%
  # pivot_longer(
  #   !Región, 
  #   names_to = "Edad", 
  #   values_to = "Solicitudes"
  # ) %>%
  # mutate(
  #   Edad = factor(Edad, levels = unique(Edad)),
  #   AñoFiscal = "2022-2023"
  # ) %>%
  # filter(
  #   Edad != "Total",
  #   Región != "Total"
  # )

# datos de solicitudes de órdenes de protección en el 2023-2024
sheet_name = "2023-2024"
OP_148_SoliGrupEdad2023_24 <- read_excel(paste0(trib, "OP_148_SoliGrupEdad.xlsx"),
                                         sheet = sheet_name) %>%
  cleanSheet_OP_148_SoliGrupEdad(sheet_name, new_names)
  # rename_at(vars(2:9), ~ new_names) %>%
  # pivot_longer(
  #   !Región, 
  #   names_to = "Edad", 
  #   values_to = "Solicitudes"
  # ) %>%
  # mutate(
  #   Edad = factor(Edad, levels = unique(Edad)),
  #   AñoFiscal = "2023-2024"
  # ) %>%
  # filter(
  #   Edad != "Total",
  #   Región != "Total"
  # )

# dataset unido
# OP_148_SoliGrupEdad <- full_join(
#   OP_148_SoliGrupEdad2020_21, OP_148_SoliGrupEdad2021_22) %>%
#   mutate(
#     AñoFiscal = ifelse(AñoFiscal == "2020-2021", "2020", 
#                          ifelse(AñoFiscal == "2021-2022", "2021", AñoFiscal)),
#     AñoFiscal = factor(AñoFiscal),
#     Región = factor(Región)
#   ) %>%
#   replace_na(list(Solicitudes = 0)) %>%
#   relocate(
#     AñoFiscal, Región, Edad, Solicitudes
#   )

# Lista de data frames de OP_148_SoliGrupEdad
OP_148_SoliGrupEdad_list <- list(OP_148_SoliGrupEdad2020_21, OP_148_SoliGrupEdad2021_22, OP_148_SoliGrupEdad2022_23, OP_148_SoliGrupEdad2023_24)

# Unir todos los data frames en la lista usando full_join
OP_148_SoliGrupEdad <- OP_148_SoliGrupEdad_list %>%
  reduce(full_join) %>%
  mutate(
    AñoFiscal = case_when(
      AñoFiscal == "2020-2021" ~ "2021",
      AñoFiscal == "2021-2022" ~ "2022",
      AñoFiscal == "2022-2023" ~ "2023",
      AñoFiscal == "2023-2024" ~ "2024",
      TRUE ~ AñoFiscal
    ),
    AñoFiscal = factor(AñoFiscal),
    Región = factor(Región)
  ) %>%
  replace_na(list(Solicitudes = 0)) %>%
  relocate(AñoFiscal, Región, Edad, Solicitudes)

#### OP_Ley148_ex_parteEmitidas ####
# Órdenes de protección ex parte emitidas al amparo de la Ley 148 - Violencia Sexual, por Región Judicial y delito

# lista con nuevos nombres de columnas para mejor interpretación
new_names <- c("Total", "Agresión Sexual", "Acoso Sexual", "Actos Lascivos", "Incesto") 

# datos de solicitudes de órdenes de protección en el 2021-2022
# OP_Ley148_ex_parteEmitidas2020_21 <- read_excel(paste0(trib, "OP_Ley148_ex_parteEmitidas2020_21.xlsx")) %>%
#   rename_at(vars(2:6), ~ new_names) %>%
#   pivot_longer(
#     !Región,
#     names_to = "Delito",
#     values_to = "ÓrdenesEmitidas"
#   ) %>%
#   mutate(
#     AñoFiscal = factor("2020-2021")
#   ) %>%
#   filter(
#     Delito != "Total"
#   )

sheet_name = "2020-2021"
OP_Ley148_ex_parteEmitidas2020_21 <- read_excel(paste0(trib, "OP_Ley148_ex_parteEmitidas.xlsx"),
                                                sheet = sheet_name) %>%
  cleanSheet_OP_Ley148_ex_parteEmitidas(sheet_name, new_names)
# rename_at(vars(2:6), ~ new_names) %>%
# pivot_longer(
#   !Región, 
#   names_to = "Delito", 
#   values_to = "ÓrdenesEmitidas"
# ) %>%
# mutate(
#   AñoFiscal = factor("2020-2021")
# ) %>%
# filter(
#   Delito != "Total"
# )   
  
  
  
# datos de solicitudes de órdenes de protección en el 2021-2022
sheet_name = "2021-2022"
OP_Ley148_ex_parteEmitidas2021_22 <- read_excel(paste0(trib, "OP_Ley148_ex_parteEmitidas.xlsx"),
                                                sheet = sheet_name) %>%
  cleanSheet_OP_Ley148_ex_parteEmitidas(sheet_name, new_names)
  # rename_at(vars(2:6), ~ new_names) %>%
  # pivot_longer(
  #   !Región, 
  #   names_to = "Delito", 
  #   values_to = "ÓrdenesEmitidas"
  # ) %>%
  # mutate(
  #   AñoFiscal = factor("2021-2022")
  # ) %>%
  # filter(
  #   Delito != "Total"
  # ) 

# datos de solicitudes de órdenes de protección en el 2022-2023
sheet_name = "2022-2023"
OP_Ley148_ex_parteEmitidas2022_23 <- read_excel(paste0(trib, "OP_Ley148_ex_parteEmitidas.xlsx"),
                                                sheet = sheet_name) %>%
  cleanSheet_OP_Ley148_ex_parteEmitidas(sheet_name, new_names)
  # rename_at(vars(2:6), ~ new_names) %>%
  # pivot_longer(
  #   !Región, 
  #   names_to = "Delito", 
  #   values_to = "ÓrdenesEmitidas"
  # ) %>%
  # mutate(
  #   AñoFiscal = factor("2022-2023")
  # ) %>%
  # filter(
  #   Delito != "Total"
  # ) 

# dataset unido
# Lista de data frames de OP_148_SoliGrupEdad
OP_Ley148_ex_parteEmitidas_list <- list(OP_Ley148_ex_parteEmitidas2020_21, OP_Ley148_ex_parteEmitidas2021_22,OP_Ley148_ex_parteEmitidas2022_23)

# Unir todos los data frames en la lista usando full_join
OP_Ley148_ex_parteEmitidas <- OP_Ley148_ex_parteEmitidas_list %>%
  reduce(full_join) %>%
  mutate(
    Región = factor(Región),
    Delito = factor(Delito),
    AñoFiscal = case_when(
      AñoFiscal == "2020-2021" ~ "2021",
      AñoFiscal == "2021-2022" ~ "2022",
      AñoFiscal == "2022-2023" ~ "2023",
      TRUE ~ AñoFiscal
    ),
    AñoFiscal = factor(AñoFiscal)
  ) %>%
  filter(
    Región != "Total"
  ) %>%
  replace_na(list(ÓrdenesEmitidas = 0)) %>%
  relocate(
    AñoFiscal, Delito, Región, ÓrdenesEmitidas
  )


# OP_Ley148_ex_parteEmitidas <- full_join(
#   OP_Ley148_ex_parteEmitidas2020_21, OP_Ley148_ex_parteEmitidas2021_22 
#   ) %>%
#   mutate(
#     Región = factor(Región),
#     Delito = factor(Delito),
#     AñoFiscal = ifelse(AñoFiscal == "2020-2021", "2020", 
#                        ifelse(AñoFiscal == "2021-2022", "2021", AñoFiscal)),
#     AñoFiscal = factor(AñoFiscal)
#   ) %>%
#   filter(
#     Región != "Total"
#   ) %>%
#   replace_na(list(ÓrdenesEmitidas = 0)) %>%
#   relocate(
#     AñoFiscal, Delito, Región, ÓrdenesEmitidas
#   )

#### OP_LEY148Archivadas ####

# Cantidad de solicitudes de órdenes de protección al amparo de la Ley 148 - Violencia Sexual archivadas por Región Judicial
new_names <- c("Total", "Solicitud Peticionaria", "Otra Razón")

# datos de solicitudes archivadas de órdenes de protección en 2020-2021
# OP_LEY148Archivadas2020_21 <- read_excel(paste0(trib, "OP_LEY148Archivadas2020_21.xlsx")) %>%
#   rename_at(vars(2:4), ~ new_names) %>%
#   pivot_longer(
#     !Región, 
#     names_to = "Razón", 
#     values_to = "ÓrdenesArchivadas"
#   ) %>%
#   mutate(
#     Región = as.character(Región), 
#     AñoFiscal = factor("2020-2021")
#   ) %>%
#   filter(
#     Razón != "Total",
#     Región != "Total"
#   ) %>%
#   mutate(
#     Región = factor(Región, levels = unique(Región)) # Convierte Región de nuevo a factor con niveles únicos
#   )

sheet_name = "2020-2021"
OP_LEY148Archivadas2020_21 <- read_excel(paste0(trib, "OP_LEY148Archivadas.xlsx"),
                                        sheet = sheet_name) %>%
  cleanSheet_OP_LEY148Archivadas(sheet_name, new_names) 

# datos de solicitudes archivadas de órdenes de protección en 2021-2022
# OP_LEY148Archivadas2021_22 <- read_excel(paste0(trib, "OP_LEY148Archivadas2021_22.xlsx")) %>%
#   rename_at(vars(2:4), ~ new_names) %>%
#   pivot_longer(
#     !Región, 
#     names_to = "Razón", 
#     values_to = "ÓrdenesArchivadas"
#   ) %>%
#   mutate(
#     Región = as.character(Región), 
#     AñoFiscal = factor("2021-2022")
#   ) %>%
#   filter(
#     Razón != "Total",
#     Región != "Total"
#   )%>%
#   mutate(
#     Región = factor(Región, levels = unique(Región)) # Convierte Región de nuevo a factor con niveles únicos
#   )

sheet_name = "2021-2022"
OP_LEY148Archivadas2021_22 <- read_excel(paste0(trib, "OP_LEY148Archivadas.xlsx"),
                                         sheet = sheet_name) %>%
  cleanSheet_OP_LEY148Archivadas(sheet_name, new_names) 

sheet_name = "2022-2023"
OP_LEY148Archivadas2022_23 <- read_excel(paste0(trib, "OP_LEY148Archivadas.xlsx"),
                                         sheet = sheet_name) %>%
  cleanSheet_OP_LEY148Archivadas(sheet_name, new_names) 

sheet_name = "2023-2024"
OP_LEY148Archivadas2023_24 <- read_excel(paste0(trib, "OP_LEY148Archivadas.xlsx"),
                                         sheet = sheet_name) %>%
  cleanSheet_OP_LEY148Archivadas(sheet_name, new_names) 

# datos de solicitudes archivadas de órdenes de protección en juntadas
# OP_LEY148Archivadas <- full_join(
#   OP_LEY148Archivadas2020_21, OP_LEY148Archivadas2021_22) %>%
#   mutate(
#     Razón = factor(Razón, 
#                    levels = c("Solicitud Peticionaria", "Otra Razón")),
#     Región = factor(Región),
#     AñoFiscal = ifelse(AñoFiscal == "2020-2021", "2020", 
#                        ifelse(AñoFiscal == "2021-2022", "2021", AñoFiscal)),
#     AñoFiscal = factor(AñoFiscal)
#   ) %>%
#   filter(
#     Región != "Total"
#   ) %>%
#   replace_na(list(ÓrdenesArchivadas = 0)) %>%
#   relocate(
#     AñoFiscal, Razón, Región, ÓrdenesArchivadas
#   )

OP_LEY148Archivadas_list <- list(OP_LEY148Archivadas2020_21,
                                 OP_LEY148Archivadas2021_22,
                                 OP_LEY148Archivadas2022_23,
                                 OP_LEY148Archivadas2023_24)

OP_LEY148Archivadas <- OP_LEY148Archivadas_list %>%
  reduce(full_join) %>%
  mutate(
    Razón = factor(Razón, 
                   levels = c("Solicitud Peticionaria", "Otra Razón")),
    Región = factor(Región),
    AñoFiscal = case_when(
      AñoFiscal == "2020-2021" ~ "2021",
      AñoFiscal == "2021-2022" ~ "2022",
      AñoFiscal == "2022-2023" ~ "2023",
      AñoFiscal == "2023-2024" ~ "2024",
      TRUE ~ as.character(AñoFiscal)
    ),
    AñoFiscal = factor(AñoFiscal)
  ) %>%
  filter(
    Región != "Total"
  ) %>%
  replace_na(list(ÓrdenesArchivadas = 0)) %>%
  relocate(
    AñoFiscal, Razón, Región, ÓrdenesArchivadas
  )


#### OP_LEY148Denegadas ####

# Cantidad de solicitudes de órdenes de protección denegadas al amparo de la Ley 148 - Violencia Sexual denegadas por Región Judicial
# lista con nuevos nombres de columnas para mejor interpretación
new_names <- c("No Aplican Disposiciones Ley148", "No Prueban Elementos")

# datos de solicitudes denegadas de órdenes de protección en 2022-2023
# OP_LEY148Denegadas2020_2021 <- read_excel(paste0(trib, "OP_LEY148Denegadas2020_21.xlsx")) %>%
#   rename_at(vars(3:4), ~ new_names) %>%
#   pivot_longer(
#     !Región, 
#     names_to = "Razón", 
#     values_to = "ÓrdenesDenegadas"
#   ) %>%
#   mutate(
#     AñoFiscal = factor("2022-2023")
#   ) %>%
#   filter(
#     Región != "Total",
#     Razón != "Total"
#   )

sheet_name = "2020-2021"
OP_LEY148Denegadas2020_21 <- read_excel(paste0(trib, "OP_LEY148Denegadas.xlsx"),
                                                sheet = sheet_name) %>%
  cleanSheet_OP_LEY148Denegadas(sheet_name, new_names) 
  # rename_at(vars(3:4), ~ new_names) %>%
  # pivot_longer(
  #   !Región, 
  #   names_to = "Razón", 
  #   values_to = "ÓrdenesDenegadas"
  # ) %>%
  # mutate(
  #   AñoFiscal = factor("2020-2021")
  # ) %>%
  # filter(
  #   Región != "Total",
  #   Razón != "Total"
  # )

# datos de solicitudes denegadas de órdenes de protección en 2021-2022
# OP_LEY148Denegadas2021_22 <- read_excel(paste0(trib, "OP_LEY148Denegadas2021_22.xlsx")) %>%
#   rename_at(vars(3:4), ~ new_names) %>%
#   pivot_longer(
#     !Región, 
#     names_to = "Razón", 
#     values_to = "ÓrdenesDenegadas"
#   ) %>%
#   mutate(
#     AñoFiscal = factor("2021-2022")
#   ) %>%
#   filter(
#     Región != "Total",
#     Razón != "Total"
#   )

sheet_name = "2021-2022"
OP_LEY148Denegadas2021_22 <- read_excel(paste0(trib, "OP_LEY148Denegadas.xlsx"),
                                          sheet = sheet_name) %>%
  cleanSheet_OP_LEY148Denegadas(sheet_name, new_names)
  # rename_at(vars(3:4), ~ new_names) %>%
  # pivot_longer(
  #   !Región, 
  #   names_to = "Razón", 
  #   values_to = "ÓrdenesDenegadas"
  # ) %>%
  # mutate(
  #   AñoFiscal = factor("2021-2022")
  # ) %>%
  # filter(
  #   Región != "Total",
  #   Razón != "Total"
  # )

sheet_name = "2022-2023"
OP_LEY148Denegadas2022_23 <- read_excel(paste0(trib, "OP_LEY148Denegadas.xlsx"),
                                        sheet = sheet_name) %>%
  cleanSheet_OP_LEY148Denegadas(sheet_name, new_names) 
  # rename_at(vars(3:4), ~ new_names) %>%
  # pivot_longer(
  #   !Región, 
  #   names_to = "Razón", 
  #   values_to = "ÓrdenesDenegadas"
  # ) %>%
  # mutate(
  #   AñoFiscal = factor("2022-2023")
  # ) %>%
  # filter(
  #   Región != "Total",
  #   Razón != "Total"
  # )

sheet_name = "2023-2024"
OP_LEY148Denegadas2023_24 <- read_excel(paste0(trib, "OP_LEY148Denegadas.xlsx"),
                                        sheet = sheet_name) %>%
  cleanSheet_OP_LEY148Denegadas(sheet_name, new_names) 
  # rename_at(vars(3:4), ~ new_names) %>%
  # pivot_longer(
  #   !Región, 
  #   names_to = "Razón", 
  #   values_to = "ÓrdenesDenegadas"
  # ) %>%
  # mutate(
  #   AñoFiscal = factor("2023-2024")
  # ) %>%
  # filter(
  #   Región != "Total",
  #   Razón != "Total"
  # )

# dataset joined
# OP_LEY148Denegadas <- full_join(
#   OP_LEY148Denegadas2020_2021, OP_LEY148Denegadas2021_22) %>%
#   mutate(
#     Región = factor(Región),
#     Razón = factor(Razón),
#     AñoFiscal = case_when(
#       AñoFiscal == "2020-2021" ~ "2020",
#       AñoFiscal == "2021-2022" ~ "2021",
#       AñoFiscal == "2022-2023" ~ "2022",
#       TRUE ~ as.character(AñoFiscal)
#     ),
#     AñoFiscal = factor(AñoFiscal)
#   ) %>%
#   replace_na(list(ÓrdenesDenegadas = 0)) %>%
#   relocate(
#     AñoFiscal, Razón, Región, ÓrdenesDenegadas
#   )


OP_LEY148Denegadas_list <- list(OP_LEY148Denegadas2020_21,
                                        OP_LEY148Denegadas2021_22,
                                        OP_LEY148Denegadas2022_23,
                                        OP_LEY148Denegadas2023_24)

# Unir todos los data frames en la lista usando full_join
OP_LEY148Denegadas <- OP_LEY148Denegadas_list %>%
  reduce(full_join) %>%
  mutate(
    Región = factor(Región),
    Razón = factor(Razón),
    AñoFiscal = case_when(
      AñoFiscal == "2020-2021" ~ "2021",
      AñoFiscal == "2021-2022" ~ "2022",
      AñoFiscal == "2022-2023" ~ "2023",
      AñoFiscal == "2023-2024" ~ "2024",
      TRUE ~ as.character(AñoFiscal)
    ),
    AñoFiscal = factor(AñoFiscal)
  ) %>%
  replace_na(list(ÓrdenesDenegadas = 0)) %>%
  relocate(
    AñoFiscal, Razón, Región, ÓrdenesDenegadas
  )


#### OP_LEY148FinalEmitidas ####

# Órdenes de protección finales emitidas al amparo de la Ley 148 - Violencia Sexual, por Región Judicial y delito

# lista con nuevos nombres de columnas para mejor interpretación
new_names <- c("Total", "Agresión Sexual", "Acoso Sexual", "Actos Lascivos", "Incesto")

# datos de solicitudes ex parte emitidas de la ley 148 en 2020-2021
# OP_LEY148FinalEmitidas2020_21 <- read_excel(paste0(trib, "OP_LEY148FinalEmitidas2020_21.xlsx")) %>%
#   rename_at(vars(2:6), ~ new_names) %>%
#   pivot_longer(
#     !Región, 
#     names_to = "Delito", 
#     values_to = "ÓrdenesEmitidas"
#   ) %>%
#   mutate(
#     AñoFiscal = factor("2020-2021")
#   ) %>%
#   filter(
#     Región != "Total",
#     Delito != "Total"
#   )

sheet_name = "2020-2021"
OP_LEY148FinalEmitidas2020_21<- read_excel(paste0(trib, "OP_LEY148FinalEmitidas.xlsx"),
                                        sheet = sheet_name) %>%
  cleanSheet_OP_LEY148FinalEmitidas(sheet_name, new_names)

# datos de solicitudes ex parte emitidas de la ley 148 en 2021-2022
# OP_LEY148FinalEmitidas2021_22 <- read_excel(paste0(trib, "OP_LEY148FinalEmitidas2021_22.xlsx")) %>%
#   rename_at(vars(2:6), ~ new_names) %>%
#   pivot_longer(
#     !Región, 
#     names_to = "Delito", 
#     values_to = "ÓrdenesEmitidas"
#   ) %>%
#   mutate(
#     AñoFiscal = factor("2021-2022")
#   ) %>%
#   filter(
#     Región != "Total",
#     Delito != "Total"
#   )

sheet_name = "2021-2022"
OP_LEY148FinalEmitidas2021_22<- read_excel(paste0(trib, "OP_LEY148FinalEmitidas.xlsx"),
                                           sheet = sheet_name) %>%
  cleanSheet_OP_LEY148FinalEmitidas(sheet_name, new_names)

sheet_name = "2022-2023"
OP_LEY148FinalEmitidas2022_23<- read_excel(paste0(trib, "OP_LEY148FinalEmitidas.xlsx"),
                                           sheet = sheet_name) %>%
  cleanSheet_OP_LEY148FinalEmitidas(sheet_name, new_names)

sheet_name = "2023-2024"
OP_LEY148FinalEmitidas2023_24<- read_excel(paste0(trib, "OP_LEY148FinalEmitidas.xlsx"),
                                           sheet = sheet_name) %>%
  cleanSheet_OP_LEY148FinalEmitidas(sheet_name, new_names)

# dataset joined
# OP_LEY148FinalEmitidas <- full_join(
#   OP_LEY148FinalEmitidas2020_21, OP_LEY148FinalEmitidas2021_22) %>%
#   mutate(
#     Región = factor(Región),
#     Delito = factor(Delito),
#     AñoFiscal = case_when(
#       AñoFiscal == "2020-2021" ~ "2020",
#       AñoFiscal == "2021-2022" ~ "2021",
#       AñoFiscal == "2022-2023" ~ "2022",
#       TRUE ~ as.character(AñoFiscal)
#     ),
#     AñoFiscal = factor(AñoFiscal)
#   ) %>%
#   replace_na(list(ÓrdenesEmitidas = 0)) %>%
#   relocate(
#     AñoFiscal, Delito, Región, ÓrdenesEmitidas
#   ) 

OP_LEY148FinalEmitidas_list <- list(OP_LEY148FinalEmitidas2020_21,
                                    OP_LEY148FinalEmitidas2021_22,
                                    OP_LEY148FinalEmitidas2022_23,
                                    OP_LEY148FinalEmitidas2023_24)

# Unir todos los data frames en la lista usando full_join
OP_LEY148FinalEmitidas <- OP_LEY148FinalEmitidas_list %>%
  reduce(full_join) %>%
  mutate(
    Región = factor(Región),
    Delito = factor(Delito),
    AñoFiscal = case_when(
      AñoFiscal == "2020-2021" ~ "2021",
      AñoFiscal == "2021-2022" ~ "2022",
      AñoFiscal == "2022-2023" ~ "2023",
      AñoFiscal == "2023-2024" ~ "2024",
      TRUE ~ as.character(AñoFiscal)
    ),
    AñoFiscal = factor(AñoFiscal)
  ) %>%
  replace_na(list(ÓrdenesEmitidas = 0)) %>%
  relocate(
    AñoFiscal, Delito, Región, ÓrdenesEmitidas
  ) 

#### OP_LEY148Genero ####
# Solicitudes de órdenes de protección al amparo de la Ley 148 - Violencia Sexual, por sexo de la parte

# OP_LEY148Genero2020_21 <- read_excel(paste0(trib, "OP_LEY148Genero2020_21.xlsx")) %>%
#   pivot_longer(
#     !Sexo, 
#     names_to = "Parte", 
#     values_to = "Solicitudes"
#   ) %>%
#   mutate(
#     AñoFiscal = factor("2020-2021")
#   ) %>%
#   filter(
#     Sexo != "Total"
#   )

sheet_name = "2020-2021"
OP_LEY148Genero2020_21<- read_excel(paste0(trib, "OP_LEY148Genero.xlsx"),
                                           sheet = sheet_name) %>%
  cleanSheet_OP_LEY148Genero(sheet_name)


# OP_LEY148Genero2021_22 <- read_excel(paste0(trib, "OP_LEY148Genero2021_22.xlsx")) %>%
#   pivot_longer(
#     !Sexo, 
#     names_to = "Parte", 
#     values_to = "Solicitudes"
#   ) %>%
#   mutate(
#     AñoFiscal = factor("2021-2022")
#   ) %>%
#   filter(
#     Sexo != "Total"
#   )

sheet_name = "2021-2022"
OP_LEY148Genero2021_22<- read_excel(paste0(trib, "OP_LEY148Genero.xlsx"),
                                    sheet = sheet_name) %>%
  cleanSheet_OP_LEY148Genero(sheet_name)

sheet_name = "2022-2023"
OP_LEY148Genero2022_23<- read_excel(paste0(trib, "OP_LEY148Genero.xlsx"),
                                    sheet = sheet_name) %>%
  cleanSheet_OP_LEY148Genero(sheet_name)

sheet_name = "2023-2024"
OP_LEY148Genero2023_24<- read_excel(paste0(trib, "OP_LEY148Genero.xlsx"),
                                    sheet = sheet_name) %>%
  cleanSheet_OP_LEY148Genero(sheet_name)


# dataset joined
# OP_LEY148Genero <- full_join(
#   OP_LEY148Genero2020_21, OP_LEY148Genero2021_22) %>%
#   mutate(
#     Sexo = factor(Sexo),
#     Parte = factor(Parte),
#     AñoFiscal = case_when(
#       AñoFiscal == "2020-2021" ~ "2020",
#       AñoFiscal == "2021-2022" ~ "2021",
#       AñoFiscal == "2022-2023" ~ "2022",
#       TRUE ~ as.character(AñoFiscal)
#     ),
#     AñoFiscal = factor(AñoFiscal)
#   ) %>%
#   replace_na(list(Solicitudes = 0)) %>%
#   relocate(
#     AñoFiscal, Parte, Sexo, Solicitudes
#   )


OP_LEY148Genero_list <- list(OP_LEY148Genero2020_21,
                             OP_LEY148Genero2021_22,
                             OP_LEY148Genero2022_23,
                             OP_LEY148Genero2023_24)

# Unir todos los data frames en la lista usando full_join
OP_LEY148Genero <- OP_LEY148Genero_list %>%
  reduce(full_join) %>%
  mutate(
    Sexo = factor(Sexo),
    Parte = factor(Parte),
    AñoFiscal = case_when(
      AñoFiscal == "2020-2021" ~ "2021",
      AñoFiscal == "2021-2022" ~ "2022",
      AñoFiscal == "2022-2023" ~ "2023",
      AñoFiscal == "2023-2024" ~ "2024",
      TRUE ~ as.character(AñoFiscal)
    ),
    AñoFiscal = factor(AñoFiscal)
  ) %>%
  replace_na(list(Solicitudes = 0)) %>%
  relocate(
    AñoFiscal, Parte, Sexo, Solicitudes
  )



#### tribCasosCrim ####

# Tribunal de Primera Instancia: Movimiento de casos criminales al amparo de la Ley Núm. 54-1989 para la prevención e intervención con la violencia doméstica

# lista con nuevos nombres de columnas para mejor interpretación 
new_names <- c("Delito", "Pendiente Inicio", "Presentados", 
               "A Resolver", "Condenas", "Absoluciones", 
               "Archivos", "Traslados", "Otros", "Total", "Pendiente Final")

# datos de movimiento de casos criminales en año fiscal 2019-2020
# tribCasosCrim19 <- read_excel(paste0(trib, "tribCasosCrim19.xlsx")) %>%
#   rename_at(vars(1:11), ~ new_names) %>%
#   pivot_longer(
#     !Delito,
#     names_to = "Casos",
#     values_to = "Cantidad"
#   ) %>%
#   mutate(
#     AñoFiscal = factor("2019-2020")
#   ) %>%
#   filter(
#     !(Delito %in% c("2019-2020", "Total")),
#     !(Casos %in% c("2019-2020", "Total")),
#   )

sheet_name = "2019-2020"
tribCasosCrim19 <- read_excel(paste0(trib, "tribCasosCrim.xlsx"),
                              sheet = sheet_name) %>%
  cleanSheet_tribCasosCrim(sheet_name, new_names)
  # rename_at(vars(1:11), ~ new_names) %>%
  # pivot_longer(
  #   !Delito, 
  #   names_to = "Casos", 
  #   values_to = "Cantidad"
  # ) %>%
  # mutate(
  #   AñoFiscal = factor("2019-2020")
  # ) %>%
  # filter(
  #   !(Casos %in% c("Total")),
  # )

# datos de movimiento de casos criminales en año fiscal 2020-2021
# tribCasosCrim20 <- read_excel(paste0(trib, "tribCasosCrim20.xlsx")) %>%
#   rename_at(vars(1:11), ~ new_names) %>%
#   pivot_longer(
#     !Delito, 
#     names_to = "Casos", 
#     values_to = "Cantidad"
#   ) %>%
#   mutate(
#     AñoFiscal = factor("2020-2021")
#   ) %>%
#   filter(
#     !(Delito %in% c("2020-2021", "Total")
#     )
#   )

sheet_name = "2020-2021"
tribCasosCrim20 <- read_excel(paste0(trib, "tribCasosCrim.xlsx"),
                              sheet = sheet_name) %>%
  cleanSheet_tribCasosCrim(sheet_name, new_names)
  # rename_at(vars(1:11), ~ new_names) %>%
  # pivot_longer(
  #   !Delito, 
  #   names_to = "Casos", 
  #   values_to = "Cantidad"
  # ) %>%
  # mutate(
  #   AñoFiscal = factor("2020-2021")
  # ) %>%
  # filter(
  #   !(Casos %in% c("Total")),
  # )


# datos de movimiento de casos criminales en año fiscal 2021-2022
# tribCasosCrim21 <- read_excel(paste0(trib, "tribCasosCrim21.xlsx")) %>%
#   rename_at(vars(1:11), ~ new_names) %>%
#   pivot_longer(
#     !Delito, 
#     names_to = "Casos", 
#     values_to = "Cantidad"
#   ) %>%
#   mutate(
#     AñoFiscal = factor("2021-2022")
#   ) %>%
#   filter(
#     !(Delito %in% c("2021-2022", "Total")
#     )
#   )

sheet_name = "2021-2022"
tribCasosCrim21 <- read_excel(paste0(trib, "tribCasosCrim.xlsx"),
                              sheet = sheet_name) %>%
  cleanSheet_tribCasosCrim(sheet_name, new_names)
  # rename_at(vars(1:11), ~ new_names) %>%
  # pivot_longer(
  #   !Delito, 
  #   names_to = "Casos", 
  #   values_to = "Cantidad"
  # ) %>%
  # mutate(
  #   AñoFiscal = factor("2021-2022")
  # ) %>%
  # filter(
  #   !(Casos %in% c("Total")),
  # )

sheet_name = "2022-2023"
tribCasosCrim22 <- read_excel(paste0(trib, "tribCasosCrim.xlsx"),
                              sheet = sheet_name) %>%
  cleanSheet_tribCasosCrim(sheet_name, new_names)
  # rename_at(vars(1:11), ~ new_names) %>%
  # pivot_longer(
  #   !Delito, 
  #   names_to = "Casos", 
  #   values_to = "Cantidad"
  # ) %>%
  # mutate(
  #   AñoFiscal = factor("2022-2023")
  # ) %>%
  # filter(
  #   !(Casos %in% c("Total")),
  # )

sheet_name = "2023-2024"
tribCasosCrim23 <- read_excel(paste0(trib, "tribCasosCrim.xlsx"),
                              sheet = sheet_name) %>%
  cleanSheet_tribCasosCrim(sheet_name, new_names)
  # rename_at(vars(1:11), ~ new_names) %>%
  # pivot_longer(
  #   !Delito, 
  #   names_to = "Casos", 
  #   values_to = "Cantidad"
  # ) %>%
  # mutate(
  #   AñoFiscal = factor("2023-2024")
  # ) %>%
  # filter(
  #   !(Casos %in% c("Total")),
  # )

# dataset joined
tribCasosCrim <- full_join(
  tribCasosCrim19, tribCasosCrim20) %>%
  full_join(tribCasosCrim21) %>%
  full_join(tribCasosCrim22) %>%
  full_join(tribCasosCrim23) %>%
  mutate(
    Delito = factor(Delito),
    Casos = factor(Casos, levels = c("A Resolver", "Absoluciones", "Archivos", "Condenas", "Pendiente Inicio", "Pendiente Final",
                                     "Presentados","Traslados","Otros"),
                   ordered = TRUE),
    AñoFiscal = case_when(
      AñoFiscal == "2019-2020" ~ "2020",
      AñoFiscal == "2020-2021" ~ "2021",
      AñoFiscal == "2021-2022" ~ "2022",
      AñoFiscal == "2022-2023" ~ "2023",
      AñoFiscal == "2023-2024" ~ "2024",
      TRUE ~ as.character(AñoFiscal)
    ),
    AñoFiscal = factor(AñoFiscal)
  ) %>%
  replace_na(list(Cantidad = 0)) %>%
  relocate(
    AñoFiscal, Delito, Casos, Cantidad
  )

#### Guardar datos procesados de Administración de Tribunales ####
# dataframes <- list(OP_148_SoliGrupEdad, OP_Ley148_ex_parteEmitidas,
#                    OP_LEY148Archivadas, OP_LEY148Denegadas, 
#                    OP_LEY148FinalEmitidas, OP_LEY148Genero, tribCasosCrim) # Lista de dataframes 
# 
# # Lista de nombres de hojas correspondientes a los dataframes
# nombres_sheets <- list("ordenes_proteccion_solicitadas", "ordenes_proteccion_exparteEmitidas",
#                        "ordenes_proteccion_exparteArchivadas", "ordenes_proteccion_denegadas",
#                        "ordenes_proteccion_emitidas", "ordenes_proteccion_emitidas_genero",
#                        "delitos_casos_tribunal")
# 
# # Uso de la función
# guardarDatos(dataframes, nombres_sheets, "administracion_tribunales")


#########################################
#### Procesamiento de datos del CAVV ####
#########################################
cavv <- here::here("data", "Prevencion_de_violencia_de_genero_CAVV", "/")
#### safekitsDF ####
safekitsDF <- read_excel(paste0(cavv, "SAFEkits.xlsx"),
                         sheet = "Data") %>%
  mutate(
    `Total de Kits` = `Total con querella` + `Total sin querella`
  ) %>%
  pivot_longer(
    !Año,
    names_to = "Kits",
    values_to = "Total"
  ) %>%
  filter(
    Kits != "Total de Kits"
  ) %>% 
  mutate(
    Kits = factor(Kits, 
                  levels = c("Total con querella", "Total sin querella")),
    Año = factor(Año)
  )


#### Guardar datos procesados de CAVV ####
# dataframes <- list(safekitsDF) # Lista de dataframes 
# 
# # Lista de nombres de hojas correspondientes a los dataframes
# nombres_sheets <- list("safe_kits")
# 
# # Uso de la función
# guardarDatos(dataframes, nombres_sheets, "centro_ayuda_victimas_violacion")


########################################
##### Actualizaciones de los Datos #####
########################################
# Fecha actualizacion de los datos de violencia domestica
actualizacion_policiaA <- "06/09/2023"

# Fecha cuando se actualizan los datos de desaparecidas
actualizacion_policiaB <- "05/03/2023"

# Fecha actualizacion delitos Ley 54
actualizacion_justiciaA <- "12/31/2023"

# Fecha actualizacion registro de personas convictas
actualizacion_justiciaB <- "06/09/2023"

# Fecha actualizacion feminicidios
actualizacion_opmA <- "05/17/2022"

# Fecha actualizacion resto de los datos de OPM
actualizacion_opmB <- "04/20/2023"

# Fecha actualizacion datos del SNMV
actualizacion_snmvA <- "12/31/2023"

# Fecha actualizacion tasas ajustadas del SNMV
actualizacion_snmvB <- "12/31/2023"

# Fecha actualizacion datos de Correcion
actualizacion_correcion <- "04/03/2023"

# Fecha actualizacion datos de Vivienda
actualizacion_vivienda <- "12/31/2022"

# Fecha actualizacion datos del Observatorio
actualizacion_observatorio <- ""

# Fecha actualizacion datos del Dept de Trabajo
actualizacion_trabajo <- "07/12/2021"

# Fecha actualizacion datos Dept Familia
actualizacion_familia <- "12/31/2022"

# Fecha actualizacion Ordenes de Proteccion
actualizacion_tribunalesA <- "02/21/2023"

# Fecha actualizacion Movimiento Casos
actualizacion_tribunalesB <- "02/21/2023"

# fecha actualización de los SAFE kits
actualizacion_caav <- "04/25/2024"

# fecha actualización del sistema
actualizacion_sistema <- "05/09/2024"

############################################
##### Credenciales para el web hosting #####
############################################
# Definir usuarios y contraseñas
credentials <- data.frame(
  user = c("manu"),
  password = c("mangu"),
  stringsAsFactors = FALSE
)
