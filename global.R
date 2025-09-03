cat("Loading global objects from global.R...\n\n")
########################################
#### Cargar bibliotecas necesarias #####
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
library(htmltools)
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

#### Funcion para convertir columna con varios tipos de datos a numerico #####
#### convert_mixed_columns####
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


##################################################################
#### Funcion para guardar dataframes por agencias #####
#### guardarDatos ####
guardarDatos <- function(dataframes, nombres_sheets, nombre_archivo) {
  # Añadir la extensión .xlsx al nombre proporcionado
  nombre_archivo <- paste0(nombre_archivo, ".xlsx")
  
  # Guardar los dataframes en diferentes hojas del mismo archivo Excel
  writexl::write_xlsx(setNames(dataframes, nombres_sheets), nombre_archivo)
  
  # Mensaje de confirmación
  message("Archivo guardado como: ", nombre_archivo, " con ", length(dataframes), " hojas.")
}
#### guardarDatos_Polygons ####
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








##################################################################################
#### Procesamiento de datos del Sistema de Notificacion de Muertes Violentas #####
##################################################################################
#### Directorio ####
snmv <- here::here("data", "Sistema_de_Notificacion_de_Muertes_Violentas")

#### homiEdad ####
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

#### inci ####
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




##################################################################################
#### Procesamiento de datos del Departamento de Familia #####
##################################################################################
#### Directorio ####
dfam <- here::here("data", "Departamento_de_Familia", "/")


#### dfMalt ####
# Años a importar
years <- 2018:2022

# Leer y limpiar los datos por hoja
dfMalt <- lapply(as.character(years), function(sheet_name) {
  read_excel(paste0(dfam, "dfMalt.xlsx"), sheet = sheet_name) %>%
    mutate(Año = sheet_name)
}) %>%
  bind_rows() %>%
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
                                           "Trata Humana", "Negligencia" ,"Otro"),
                                ordered = TRUE))


#### Guardar datos procesados de Departamento de Familia ####
# dataframes <- list(dfMalt) # Lista de dataframes (por ejemplo: homiEdad y inci)
# 
# # Lista de nombres de hojas correspondientes a los dataframes
# nombres_sheets <- list("maltrato_menores_victimas")
# 
# # Uso de la función
# guardarDatos(dataframes, nombres_sheets, "departamento_familia")






##################################################################################
##### Procesamiento de datos del Departamento de Justicia #####
##################################################################################
#### Directorio ####
djus <- here::here("data", "Departamento_de_Justicia", "/")

#### dfDeli ####
maps_fol <- here::here("data", "mapas/")

# Años a importar
years <- 2020:2023

# Importar y combinar todos los archivos
dfDeli <- lapply(years, function(year) {
  read_excel(paste0(djus, "djDelitos", year, ".xlsx")) %>%
    convert_mixed_columns() %>%
    mutate(Año = as.character(year))
}) %>%
  bind_rows() %>%
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
#### mapaDeli ####
mapaDeli <- st_read(paste0(maps_fol, "/distritos_fiscales.shp")) %>%
  merge(dfDeli, by.x = "GROUP", by.y = "Distrito") %>%
  rename(`Distrito Fiscal` = GROUP) %>%
  relocate(Año, `Distrito Fiscal`, Delito, geometry, Casos)

# Cargar el shapefile de municipios
municipios_geo <- st_read(paste0(maps_fol, "/municipios.shp"))

# Asegúrate de que los datos tengan el mismo sistema de coordenadas que tus datos
municipios_geo <- st_transform(municipios_geo, crs = 4326) # WGS84

municipios_geo <- municipios_geo %>%
  mutate(municipio = case_when(
    municipio == "A??asco" ~ "Añasco",
    municipio == "Bayam??n" ~ "Bayamón",
    municipio == "Can??vanas" ~ "Canóvanas",
    municipio == "Cata??o" ~ "Cataño",
    municipio == "Comer??o" ~ "Comerío",
    municipio == "Gu??nica" ~ "Guánica",
    municipio == "Juana D??az" ~ "Juana Díaz",
    municipio == "Las Mar??as" ~ "Las Marias",
    municipio == "Lo??za" ~ "Loíza",
    municipio == "Manat??" ~ "Manatí",
    municipio == "Mayag??ez" ~ "Mayagüez",
    municipio == "Pe??uelas" ~ "Peñuelas",
    municipio == "Rinc??n" ~ "Rincón",
    municipio == "R??o Grande" ~ "Rio Grande",
    municipio == "San Germ??n" ~ "San Germán",
    municipio == "San Sebasti??n" ~ "San Sebastián",
    TRUE ~ municipio  # mantiene los demás sin cambios
  ))%>%
  dplyr::select(municipio, geometry)

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






##################################################################################
#### Procesamiento de datos el Departamento del Trabajo y Recursos Humanos ####
##################################################################################
#### Directorio ####
dtra <- here::here("data", "departamento_de_trabajo", "/")

#### parLab ####
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





##################################################################################
#### Procesamiento de datos de la Administración de Vivienda Pública #####
##################################################################################
#### Directorio ####
avp <- here::here("data", "Administracion_de_viviendas_publicas/")

#### dfAvp ####
dfAvp <- read_excel(paste0(avp, "/administracion_vivienda_publica.xlsx"))%>%
    mutate(
      Región = factor(Región),
      Estado = factor(Estado, levels = c("solicitadas", "asignadas"))) %>%
    replace_na(list(Cantidad = 0))

# Ruta al archivo
archivo <- paste0(avp, "/avp_municipios.xlsx")

# Años de los sheets
años <- as.character(2017:2024)

# Leer y guardar cada hoja en una variable distinta con la columna Año agregada
for (año in años) {
  df <- read_excel(archivo, sheet = año)
  df$Año <- año
  assign(paste0("dfAvp_municipio", año), df)
}

# Lista de data.frames
lista_df <- mget(paste0("dfAvp_municipio", 2017:2024))

# Unión completa de todos
dfAvp_municipios <- reduce(lista_df, full_join)

dfAvp_municipios_asignadas <- dfAvp_municipios %>%
  dplyr::select(municipio = Municipios, Región, Cantidad = Asignadas, Año)

dfAvp_municipios_solicitadas <- dfAvp_municipios %>%
  dplyr::select(municipio = Municipios, Región, Cantidad = Solicitadas, Año)

municipios_geo_asig <- municipios_geo %>%
  left_join(dfAvp_municipios_asignadas, by = "municipio")

municipios_geo_sol <- municipios_geo %>%
  left_join(dfAvp_municipios_solicitadas, by = "municipio")


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

# Filtrar para "asignadas"
mapaAvp_asig <- mapaAvp %>%
  filter(Estado == "asignadas")

# Filtrar para "solicitadas"
mapaAvp_sol <- mapaAvp %>%
  filter(Estado == "solicitadas")

#### Guardar datos procesados de Administración de Vivienda Pública ####
# dataframes <- list(dfAvp) # Lista de dataframes (por ejemplo: homiEdad y inci)
# 
# # Lista de nombres de hojas correspondientes a los dataframes
# nombres_sheets <- list("viviendasPublicas_porViolenciaDomestica")
# 
# # Uso de la función
# guardarDatos(dataframes, nombres_sheets, "administracion_vivienda_publica")



##################################################################################
#### Procesamiento de datos Departamento de Corrección y Rehabilitación ####
##################################################################################
#### Directorio ####
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




##################################################################################
#### Procesamiento de datos del Negociado de Policía ####
##################################################################################
#### Directorio ####
poli <- here::here("data", "Negociado_de_Policia", "/")

#### despDF ####
meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# Años a importar
years <- 2020:2024

# Leer y limpiar los datos por hoja
despDF_list <- lapply(as.character(years), function(sheet_name) {
  read_excel(paste0(poli, "npprDesp.xlsx"), sheet = sheet_name) %>%
    cleanSheet_npprDesp(sheet_name)
})

# Unir y transformar los datos
despDF <- despDF_list %>%
  reduce(full_join) %>%
  pivot_longer(cols = -c(Categoria, Año), names_to = "Meses", values_to = "Casos") %>%
  filter(!grepl("Total", Meses, ignore.case = TRUE)) %>%
  group_by(Categoria, Año) %>%
  summarise(Casos = sum(Casos, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Año = factor(Año),
    Estado = factor(Categoria)
  ) %>%
  replace_na(list(Casos = 0)) %>%
  select(Año, Estado, Casos)

# sheet_name = "2020"
# desp2020 <- read_excel(paste0(poli, "npprDesp.xlsx"),
#                        sheet = sheet_name) %>%
#   cleanSheet_npprDesp(sheet_name)
# 
# sheet_name = "2021"
# desp2021 <- read_excel(paste0(poli, "npprDesp.xlsx"),
#                        sheet = sheet_name) %>%
#   cleanSheet_npprDesp(sheet_name)
# 
# sheet_name = "2022"
# desp2022 <- read_excel(paste0(poli, "npprDesp.xlsx"),
#                        sheet = sheet_name) %>%
#   cleanSheet_npprDesp(sheet_name)
# 
# sheet_name = "2023"
# desp2023 <- read_excel(paste0(poli, "npprDesp.xlsx"),
#                        sheet = sheet_name) %>%
#   cleanSheet_npprDesp(sheet_name)
# 
# sheet_name = "2024"
# desp2024 <- read_excel(paste0(poli, "npprDesp.xlsx"),
#                        sheet = sheet_name) %>%
#   cleanSheet_npprDesp(sheet_name)
# 
# despDF_list <- list(desp2020,
#                     desp2021,
#                     desp2022,
#                     desp2023,
#                     desp2024)
# 
# # Unir todos los data frames en la lista usando full_join
# despDF <- despDF_list %>%
#   reduce(full_join) %>%
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

#### vEdad ####
# Años a importar
years <- 2021:2024

# Leer y limpiar los datos por hoja
vEdad_list <- lapply(as.character(years), function(sheet_name) {
  read_excel(paste0(poli, "npprVDedad.xlsx"), sheet = sheet_name) %>%
    cleanSheet_npprVDedad(sheet_name)
})

# Unir y transformar los datos
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

# sheet_name = "2021"
# vEdad2021 <- read_excel(paste0(poli, "npprVDedad.xlsx"),
#                        sheet = sheet_name) %>%
#   cleanSheet_npprVDedad(sheet_name)
# 
# sheet_name = "2022"
# vEdad2022 <- read_excel(paste0(poli, "npprVDedad.xlsx"),
#                         sheet = sheet_name) %>%
#   cleanSheet_npprVDedad(sheet_name)
# 
# sheet_name = "2023"
# vEdad2023 <- read_excel(paste0(poli, "npprVDedad.xlsx"),
#                         sheet = sheet_name) %>%
#   cleanSheet_npprVDedad(sheet_name)
# 
# sheet_name = "2024"
# vEdad2024 <- read_excel(paste0(poli, "npprVDedad.xlsx"),
#                         sheet = sheet_name) %>%
#   cleanSheet_npprVDedad(sheet_name)
# 
# vEdad_list <- list(vEdad2021,
#                     vEdad2022,
#                     vEdad2023,
#                     vEdad2024)
# 
# # Unir todos los data frames en la lista usando full_join
# vEdad <- vEdad_list %>%
#   reduce(full_join) %>%
#   rename(
#     Edad = `Grupos de Edad`,
#     `Ambos Sexos` = Total,
#     Mujeres = `Cantidad de mujeres víctimas`,
#     Hombres = Masculino,
#     Año = Año
#   ) %>%
#   pivot_longer(
#     !c(Edad, Año), names_to = "Sexo", values_to = "Casos"
#   ) %>%
#   mutate(
#     Edad = str_replace_all(Edad, c("^< 16$" = "menos de 16 años", "^65 o más$" = "65 años o más")),
#     Edad = factor(Edad, levels = c("menos de 16 años", "16-17", "18-19", "20-24", "25-29", "30-34",
#                                    "35-39","40-44","45-49","50-54","55-59","60-64","65 años o más",
#                                    "Desconocida"),
#                   ordered = TRUE),
#     Año = factor(Año),
#     Sexo = factor(Sexo, levels = c("Hombres", "Mujeres", "Ambos Sexos", "Desconocido"),
#                   ordered = TRUE)
#   ) %>%
#   replace_na(list(Casos = 0)) %>%
#   select(
#     Año, Edad, Sexo, Casos
#   )

#### maltPoli ####
# Años a importar
years <- 2021:2025

# Leer y limpiar los datos por hoja
maltPoli_list <- lapply(as.character(years), function(sheet_name) {
  read_excel(paste0(poli, "nppr_maltrato.xlsx"), sheet = sheet_name) %>%
    cleanSheet_npprMalt(sheet_name)
})

# Unir y transformar los datos
maltPoli <- maltPoli_list %>%
  reduce(full_join) %>%
  rename(
    Maltrato = `Tipo de Maltrato`,
    Mujeres = Femenino,
    Hombres = Masculino,
    Año = Año
  ) %>%
  pivot_longer(
    !c(Maltrato, Año), names_to = "Sexo", values_to = "Casos"
  ) %>%
  mutate(
    Maltrato = factor(Maltrato, levels = c("Violación Orden de Protección", "Sexual", "Restricción de libertad", 
                                                               "Psicológico o emocional", "Físico", "Amenaza","Otro"),
                                ordered = TRUE),
    Año = factor(Año),
    Sexo = factor(Sexo, levels = c("Hombres", "Mujeres", "Hombre Trans", "Desconocido"),
                  ordered = TRUE)
  ) %>%
  replace_na(list(Casos = 0)) %>%
  select(
    Año, Maltrato, Sexo, Casos
  )

#### npprDS_totales ####
# sheets a importar
sheets <- c("Victimas", "Ofensores")

# Leer y limpiar los datos por hoja
npprDS_totales_list <- lapply(sheets, function(sheet_name) {
  read_excel(paste0(poli, "npprDS_totales.xlsx"), sheet = sheet_name) %>%
    cleanSheet_npprDS_totales(sheet_name)
})

npprDS_totales <- npprDS_totales_list %>%
  reduce(full_join)  %>%
  pivot_longer(
    cols = c(Mujeres, Hombres),   # columnas a pivotear
    names_to = "Sexo",            # nombre de la nueva columna
    values_to = "Casos"           # valores
  )%>%
  mutate(
    Rol = factor(Rol),
    Año = factor(Año),
    Sexo = factor(Sexo)
  ) %>%
  replace_na(list(Casos = 0)) %>%
  select(
    Año, Sexo, Casos, Rol
  )



#### npprDS_victima ####
# Años a importar
years <- 2019:2025

# Leer y limpiar los datos por hoja
npprDS_victima_list <- lapply(as.character(years), function(sheet_name) {
  read_excel(paste0(poli, "npprDS_victima.xlsx"), sheet = sheet_name) %>%
    cleanSheet_npprDS(sheet_name)
})

npprDS_victima <- npprDS_victima_list %>%
  reduce(full_join) %>%
  filter(Año != 2025) %>%
  rename(
    Edad = `Grupos de Edad`
  ) %>%
  pivot_longer(
    !c(Edad, Año), names_to = "Sexo", values_to = "Casos"
  ) %>%
  mutate(
    Edad = str_replace(Edad, "^10 años o menos$", "menos de 10 años"),
    Edad = factor(Edad, levels = unique(Edad)),
    Año = factor(Año),
    Sexo = factor(Sexo)
  ) %>%
  replace_na(list(Casos = 0)) %>%
  select(
    Año, Edad, Sexo, Casos
  )

npprDS_victima_rol <- npprDS_victima  %>%
  mutate(Rol = "Víctima")

#### npprDS_ofensores ####
# Años a importar
years <- 2019:2025

# Leer y limpiar los datos por hoja
npprDS_ofensores_list <- lapply(as.character(years), function(sheet_name) {
  read_excel(paste0(poli, "npprDS_ofensores.xlsx"), sheet = sheet_name) %>%
    cleanSheet_npprDS(sheet_name)
})

npprDS_ofensores <- npprDS_ofensores_list %>%
  reduce(full_join) %>%
  filter(Año != 2025) %>%
  rename(
    Edad = `Grupos de Edad`
  ) %>%
  pivot_longer(
    !c(Edad, Año), names_to = "Sexo", values_to = "Casos"
  ) %>%
  mutate(
    Edad = str_replace(Edad, "^10 años o menos$", "menos de 10 años"),
    Edad = factor(Edad, levels = unique(Edad)),
    Año = factor(Año),
    Sexo = factor(Sexo)
  ) %>%
  replace_na(list(Casos = 0)) %>%
  select(
    Año, Edad, Sexo, Casos
  )

npprDS_ofensores_rol <- npprDS_ofensores  %>%
  mutate(Rol = "Ofensor")

# Juntar dataframe de victima y ofensores para la descarga de datos
npprDS_rol <- dplyr::bind_rows(npprDS_victima_rol, npprDS_ofensores_rol)


#### inciDF ####
# Vector de años
años <- c("2021", "2022", "2023")

# Función para leer y procesar cada archivo
leer_datos_incidentes <- function(año) {
  data <- read_excel(paste0(poli, "NPPRincidentes_", año, ".xlsx")) %>%
    rename_with(~ gsub(año, "", .), contains(año)) %>%
    rename_at(vars(2), ~ "Población") %>%
    mutate(Año = año)
  
  # Para el año 2023, también hay columnas con "2022"
  if (año == "2023") {
    data <- data %>%
      rename_with(~ gsub("2022", "", .), contains("2022"))
  }
  
  return(data)
}

# Leer y combinar todos los años
inciDF <- lapply(años, leer_datos_incidentes) %>%
  bind_rows() %>%
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
  relocate(Año, `Áreas Policiacas`, Población, Casos)

# inci2021 <- read_excel(paste0(poli, "NPPRincidentes_2021.xlsx")) %>% 
#   rename_with(~gsub("2021", "",.), contains("2021")) %>%
#   rename_at(vars(2), ~ "Población") %>% 
#   mutate(Año = "2021")
# 
# # faltan datos para el 2022
# inci2022 <- read_excel(paste0(poli, "NPPRincidentes_2022.xlsx")) %>% 
#   rename_with(~gsub("2022", "",.), contains("2022")) %>%
#   rename_at(vars(2), ~"Población") %>% 
#   mutate(Año = "2022")
# 
# # faltan datos desde mayo en adelante
# inci2023 <- read_excel(paste0(poli,"NPPRincidentes_2023.xlsx")) %>% 
#   rename_with(~gsub("2022", "",.), contains("2022")) %>%
#   rename_with(~gsub("2023", "",.), contains("2023")) %>%
#   rename_at(vars(2), ~ "Población") %>%
#   mutate(Año = "2023")
# 
# # dataframe con toda la data combinada
# inciDF <- bind_rows(inci2021, inci2022, inci2023) %>%
#   filter(`Áreas Policiacas` != "Total") %>%
#   pivot_longer(cols = -c(`Áreas Policiacas`, Población, Año), names_to = "Mes", values_to = "Casos") %>%
#   mutate(
#     `Áreas Policiacas` = factor(str_trim(`Áreas Policiacas`)),
#     Año = factor(Año),
#     Meses = factor(Mes), 
#     Meses_Numéricos = match(Meses, Mes),
#     Fecha = as.yearmon(paste(Año, Meses_Numéricos), "%Y %m")
#   ) %>%
#   replace_na(list(Casos = 0)) %>%
#   select(-c(Meses_Numéricos, Mes, Meses)) %>%
#   relocate(
#     Año, `Áreas Policiacas`, Población, Casos
#   )

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



##################################################################################
#### Procesamiento de datos de la Oficina de la Procuradora de las Mujeres ####
##################################################################################
#### Directorio ####
opm <- here::here("data", "Oficina_de_procuradora_de_mujeres", "/")

#### opmFemiVD ####
opmFemiVD <- read_excel(paste0(opm, "opmFemiVD.xlsx")) %>%
  mutate(Año = factor(Año)) %>%
  rename(
    Asesinatos = `Cantidad de asesinatos`,
    Tasa = `Tasa (x100,000 mujeres)`
  ) 


#### opmAgresores ####
opmAgresores <- read_excel(paste0(opm, "opmAgresores.xlsx"))  %>%
  rename(
    Razón = `tipo`,
    Año = `year`,
    Cantidad = `cantidad`
  ) %>%
  mutate(Año = factor(Año),
         Razón = factor(Razón, levels = c("Acecho", "Agresión sexual", "Discrimen de género",
                                          "Violencia doméstica", "Violencia en cita", "Trata Humana", "Otras")))

#### opmCasos ####
# Niveles con "Otras" al final
niveles_completos <- c("Acecho", "Agresión sexual", "Discrimen de género",
                       "Violencia doméstica", "Violencia en cita", "Trata Humana", "Otras")

opm_mes_df <- read_excel(paste0(opm, "opmPartMes.xlsx"), sheet = "mensual")
opm_anual_df <- read_excel(paste0(opm, "opmPartMes.xlsx"), sheet = "anual")

# Limpiar con la función
opmCasos_list <- list(
  cleanSheet_OPM(opm_mes_df, "mensual", niveles_completos),
  cleanSheet_OPM(opm_anual_df, "anual", niveles_completos)
)

# Unir todo
opmCasos <- reduce(opmCasos_list, full_join)

#### opmVic ####
opmVic <- read_excel(paste0(opm, "opmVicGraf.xlsx")) %>% 
  rename_at(vars(1,2,3,4,5,6), ~ c("género","2020", "2021", "2022", "2023", "2024")) %>%
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
  rename_at(vars(2,3,4,5,6), ~ c("2020", "2021", "2022", "2023", "2024")) %>%
  mutate(across(c("2020", "2021", "2022", "2023", "2024"), as.numeric)) %>%
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



##################################################################################
#### Procesamiento de datos de la Administración de Tribunales ####
##################################################################################
#### Directorio ####
trib <- here::here("data", "administracion_de_tribunales", "/")

#### casosCrimLey148 ####
## Solicitudes de órdenes de protección al amparo de la Ley 148 - Violencia Sexual,
## por Región Judicial y grupo de edad de la parte peticionaria

#### Función para importar y procesar datos ####
leer_y_procesar <- function(sheet_name) {
  read_excel(paste0(trib, "casosCrimLey148.xlsx"), sheet = sheet_name) %>%
    rename(Delito = `Año fiscal/delitos`) %>%
    pivot_longer(!Delito, names_to = "Status", values_to = "Casos") %>%
    mutate(AñoFiscal = sheet_name) %>%
    filter(Delito != sheet_name)
}

#### Leer todos los años fiscales ####
años <- c("2020-2021", "2021-2022", "2022-2023", "2023-2024", "2024-2025")
casosCrimLey148_list <- lapply(años, leer_y_procesar)

#### Unir todos los data frames y ajustar variables ####
casosCrimLey148 <- casosCrimLey148_list %>%
  reduce(full_join) %>%
  mutate(
    AñoFiscal = recode(AñoFiscal,
                       "2020-2021" = "2021",
                       "2021-2022" = "2022",
                       "2022-2023" = "2023",
                       "2023-2024" = "2024",
                       "2024-2025" = "2025"),
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
sheet_years <- c("2020-2021", "2021-2022", "2022-2023", "2023-2024", "2024-2025")

# Leer y limpiar todos los datos
OP_148_SoliGrupEdad_list <- lapply(sheet_years, function(year) {
  read_excel(paste0(trib, "OP_148_SoliGrupEdad.xlsx"), sheet = year) %>%
    cleanSheet_OP_148_SoliGrupEdad(year, new_names)
})

# Unir los data frames y limpiar datos
OP_148_SoliGrupEdad <- bind_rows(OP_148_SoliGrupEdad_list) %>%
  mutate(
    AñoFiscal = recode(AñoFiscal,
                       "2020-2021" = "2021",
                       "2021-2022" = "2022",
                       "2022-2023" = "2023",
                       "2023-2024" = "2024",
                       "2024-2025" = "2025"),
    AñoFiscal = factor(AñoFiscal),
    Región = factor(Región)
  ) %>%
  replace_na(list(Solicitudes = 0)) %>%
  relocate(AñoFiscal, Región, Edad, Solicitudes)




#### OP_Ley148_ex_parteEmitidas ####
# Órdenes de protección ex parte emitidas al amparo de la Ley 148 - Violencia Sexual, por Región Judicial y delito
# Nuevos nombres de columnas
new_names <- c("Total", "Agresión Sexual", "Acoso Sexual", "Actos Lascivos", "Incesto") 
sheet_years <- c("2020-2021", "2021-2022", "2022-2023", "2023-2024", "2024-2025")

# Leer y limpiar los datos
OP_Ley148_ex_parteEmitidas_list <- lapply(sheet_years, function(year) {
  read_excel(paste0(trib, "OP_Ley148_ex_parteEmitidas.xlsx"), sheet = year) %>%
    cleanSheet_OP_Ley148_ex_parteEmitidas(year, new_names)
})

# Unir los data frames y procesar
OP_Ley148_ex_parteEmitidas <- bind_rows(OP_Ley148_ex_parteEmitidas_list) %>%
  mutate(
    AñoFiscal = recode(AñoFiscal,
                       "2020-2021" = "2021",
                       "2021-2022" = "2022",
                       "2022-2023" = "2023",
                       "2023-2024" = "2024",
                       "2024-2025" = "2025"),
    Región = factor(Región),
    Delito = factor(Delito),
    AñoFiscal = factor(AñoFiscal)
  ) %>%
  filter(Región != "Total") %>%
  replace_na(list(ÓrdenesEmitidas = 0)) %>%
  relocate(AñoFiscal, Delito, Región, ÓrdenesEmitidas)




#### OP_LEY148Archivadas ####
# Cantidad de solicitudes de órdenes de protección al amparo de la Ley 148 - Violencia Sexual archivadas por Región Judicial
new_names <- c("Total", "Solicitud Peticionaria", "Otra Razón")
years <- c("2020-2021", "2021-2022", "2022-2023", "2023-2024", "2024-2025")

OP_LEY148Archivadas_list <- map(years, ~ {
  read_excel(paste0(trib, "OP_LEY148Archivadas.xlsx"), sheet = .x) %>%
    cleanSheet_OP_LEY148Archivadas(.x, new_names)
})

OP_LEY148Archivadas <- OP_LEY148Archivadas_list %>%
  reduce(full_join) %>%
  mutate(
    Razón = factor(Razón, levels = c("Solicitud Peticionaria", "Otra Razón")),
    Región = factor(Región),
    AñoFiscal = factor(case_when(
      AñoFiscal == "2020-2021" ~ "2021",
      AñoFiscal == "2021-2022" ~ "2022",
      AñoFiscal == "2022-2023" ~ "2023",
      AñoFiscal == "2023-2024" ~ "2024",
      AñoFiscal == "2024-2025" ~ "2025",
      TRUE ~ as.character(AñoFiscal)
    ))
  ) %>%
  filter(Región != "Total") %>%
  replace_na(list(ÓrdenesArchivadas = 0)) %>%
  relocate(AñoFiscal, Razón, Región, ÓrdenesArchivadas)


#### OP_LEY148Denegadas ####

# Cantidad de solicitudes de órdenes de protección denegadas al amparo de la Ley 148 - Violencia Sexual denegadas por Región Judicial
# lista con nuevos nombres de columnas para mejor interpretación
new_names <- c("No Aplican Disposiciones Ley148", "No Prueban Elementos")
sheet_names <- c("2020-2021", "2021-2022", "2022-2023", "2023-2024")

OP_LEY148Denegadas_list <- map(sheet_names, ~ {
  read_excel(paste0(trib, "OP_LEY148Denegadas.xlsx"), sheet = .x) %>%
    cleanSheet_OP_LEY148Denegadas(.x, new_names)
})

OP_LEY148Denegadas <- OP_LEY148Denegadas_list %>%
  reduce(full_join) %>%
  mutate(
    Región = factor(Región),
    Razón = factor(Razón),
    AñoFiscal = factor(case_when(
      AñoFiscal == "2020-2021" ~ "2021",
      AñoFiscal == "2021-2022" ~ "2022",
      AñoFiscal == "2022-2023" ~ "2023",
      AñoFiscal == "2023-2024" ~ "2024",
      TRUE ~ as.character(AñoFiscal)
    )),
    ÓrdenesDenegadas = replace_na(ÓrdenesDenegadas, 0)
  ) %>%
  relocate(AñoFiscal, Razón, Región, ÓrdenesDenegadas)

#### OP_LEY148FinalEmitidas ####

# Órdenes de protección finales emitidas al amparo de la Ley 148 - Violencia Sexual, por Región Judicial y delito

new_names <- c("Total", "Agresión Sexual", "Acoso Sexual", "Actos Lascivos", "Incesto")
sheets <- c("2020-2021", "2021-2022", "2022-2023", "2023-2024", "2024-2025")

# Leer y limpiar todos los sheets en una lista
OP_LEY148FinalEmitidas_list <- lapply(sheets, function(sheet_name) {
  read_excel(paste0(trib, "OP_LEY148FinalEmitidas.xlsx"), sheet = sheet_name) %>%
    cleanSheet_OP_LEY148FinalEmitidas(sheet_name, new_names)
})

# Unir todos los data frames y ajustar variables
OP_LEY148FinalEmitidas <- OP_LEY148FinalEmitidas_list %>%
  reduce(full_join) %>%
  mutate(
    Región = factor(Región),
    Delito = factor(Delito),
    AñoFiscal = factor(case_when(
      AñoFiscal == "2020-2021" ~ "2021",
      AñoFiscal == "2021-2022" ~ "2022",
      AñoFiscal == "2022-2023" ~ "2023",
      AñoFiscal == "2023-2024" ~ "2024",
      AñoFiscal == "2024-2025" ~ "2025",
      TRUE ~ as.character(AñoFiscal)
    )),
    ÓrdenesEmitidas = replace_na(ÓrdenesEmitidas, 0)
  ) %>%
  relocate(AñoFiscal, Delito, Región, ÓrdenesEmitidas)



#### OP_LEY148Genero ####
# Solicitudes de órdenes de protección al amparo de la Ley 148 - Violencia Sexual, por sexo de la parte
sheet_names <- c("2020-2021", "2021-2022", "2022-2023", "2023-2024")

OP_LEY148Genero_list <- lapply(sheet_names, function(sheet) {
  read_excel(paste0(trib, "OP_LEY148Genero.xlsx"), sheet = sheet) %>%
    cleanSheet_OP_LEY148Genero(sheet)
})

OP_LEY148Genero <- OP_LEY148Genero_list %>%
  reduce(full_join) %>%
  mutate(
    Sexo = factor(Sexo),
    Parte = factor(Parte),
    AñoFiscal = factor(case_when(
      AñoFiscal == "2020-2021" ~ "2021",
      AñoFiscal == "2021-2022" ~ "2022",
      AñoFiscal == "2022-2023" ~ "2023",
      AñoFiscal == "2023-2024" ~ "2024",
      TRUE ~ as.character(AñoFiscal)
    ))
  ) %>%
  replace_na(list(Solicitudes = 0)) %>%
  relocate(AñoFiscal, Parte, Sexo, Solicitudes)


#### tribCasosCrim ####

# Tribunal de Primera Instancia: Movimiento de casos criminales al amparo de la Ley Núm. 54-1989 para la prevención e intervención con la violencia doméstica
new_names <- c("Delito", "Pendiente Inicio", "Presentados", 
               "A Resolver", "Condenas", "Absoluciones", 
               "Archivos", "Traslados", "Otros", "Total", "Pendiente Final")

years <- c("2019-2020", "2020-2021", "2021-2022", "2022-2023", "2023-2024", "2024-2025")

tribCasosCrim_list <- map(years, ~ {
  read_excel(paste0(trib, "tribCasosCrim.xlsx"), sheet = .x) %>%
    cleanSheet_tribCasosCrim(.x, new_names)
})

tribCasosCrim <- reduce(tribCasosCrim_list, full_join) %>%
  mutate(
    Delito = factor(Delito),
    Casos = factor(Casos, levels = c("A Resolver", "Absoluciones", "Archivos", "Condenas", 
                                     "Pendiente Inicio", "Pendiente Final", "Presentados",
                                     "Traslados", "Otros"), ordered = TRUE),
    AñoFiscal = recode(AñoFiscal,
                       "2019-2020" = "2020",
                       "2020-2021" = "2021",
                       "2021-2022" = "2022",
                       "2022-2023" = "2023",
                       "2023-2024" = "2024",
                       "2024-2025" = "2025"),
    AñoFiscal = factor(AñoFiscal),
  ) %>%
  replace_na(list(Cantidad = 0)) %>%
  relocate(AñoFiscal, Delito, Casos, Cantidad)

# # Tribunal de Primera Instancia: Movimiento de casos criminales al amparo de la Ley Núm. 54-1989 para la prevención e intervención con la violencia doméstica
# new_names <- c("Delito", "Pendiente Inicio", "Presentados", 
#                "A Resolver", "Condenas", "Absoluciones", 
#                "Archivos", "Traslados", "Otros", "Total", "Pendiente Final")
# 
# years <- c("2019-2020", "2020-2021", "2021-2022", "2022-2023", "2023-2024", "2024-2025")
# 
# tribCasosCrim_list <- map(years, ~ {
#   read_excel(paste0(trib, "tribCasosCrim2.xlsx"), sheet = .x) %>%
#     cleanSheet_tribCasosCrim(.x, new_names)
# })
# 
# tribCasosCrim_grande <- reduce(tribCasosCrim_list, full_join) %>%
#   mutate(
#     Delito = factor(Delito),
#     Casos = factor(Casos, levels = c("A Resolver", "Absoluciones", "Archivos", "Condenas", 
#                                      "Pendiente Inicio", "Pendiente Final", "Presentados",
#                                      "Traslados", "Otros"), ordered = TRUE),
#     AñoFiscal = recode(AñoFiscal,
#                        "2019-2020" = "2020",
#                        "2020-2021" = "2021",
#                        "2021-2022" = "2022",
#                        "2022-2023" = "2023",
#                        "2023-2024" = "2024",
#                        "2024-2025" = "2025"),
#     AñoFiscal = factor(AñoFiscal),
#     DelitoAgrupado = case_when(
#       Delito %in% c(
#         "Agresión sexual conyugal Art. 3.5", 
#         "Tentativa de agresión sexual conyugal Art 3.5"
#       ) ~ "Agresión sexual/Tentativa",
#       
#       Delito %in% c(
#         "Maltrato agravado Art. 3.2",
#         "Maltrato Art 3.1",
#         "Maltrato mediante amenaza Art 3.3",
#         "Maltrato mediante restricción de libertad Art. 3.4",
#         "Tentativa de maltrato agravado Art 3.2",
#         "Tentativa de maltrato Art 3.1",
#         "Tentativa de maltrato mediante amenaza Art 3.3",
#         "Tentativa de maltrato mediante restricción de libertad Art 3.4"
#       ) ~ "Maltrato/Tentativa",
#       
#       Delito %in% c(
#         "Tentativa de violación orden de protección Art 2.8",
#         "Violación orden de protección Art 2.8"
#       ) ~ "Violación órden de protección/Tentativa",
#       
#       TRUE ~ as.character(Delito)  # mantener los demás como están
#     ),
#     DelitoAgrupado = factor(DelitoAgrupado),
#   ) %>%
#   replace_na(list(Cantidad = 0)) %>%
#   relocate(AñoFiscal, Delito, Casos, Cantidad)
# 
# tribCasosCrim <- tribCasosCrim_grande %>%
#   group_by(AñoFiscal, DelitoAgrupado, Casos) %>%
#   summarise(Cantidad = sum(Cantidad, na.rm = TRUE), .groups = "drop") %>%
#   rename(Delito = DelitoAgrupado)
# 



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







##################################################################################
#### Procesamiento de datos del CAVV ####
##################################################################################
#### Directorio ####
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

#### edades ####
safekitsDF_edades <- read_excel(paste0(cavv, "SAFEkits.xlsx"),
                                sheet = "Edades")  %>%
  mutate(
    `Total de Kits con querella` = `Cantidad de menores` + `Cantidad de mayores` + `No identificado`
  ) %>%
  pivot_longer(
    !Año,
    names_to = "Categoria",
    values_to = "Total"
  ) %>%
  filter(
    Categoria != "Total de Kits con querella"
  ) %>%
  mutate(
    Categoria = case_when(
      Categoria == "Cantidad de menores" ~ "Menores de edad",
      Categoria == "Cantidad de mayores" ~ "Mayores de edad",
      Categoria == "No identificado" ~ "No identificado",
      TRUE ~ Categoria # Opción para manejar cualquier valor inesperado
    ),
    Categoria = factor(Categoria, 
                       levels = c("Menores de edad", "Mayores de edad", "No identificado")),
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




##################################################################################
##### Actualizaciones de los Datos #####
##################################################################################
# Fecha actualizacion de los datos de SNMV tab1
actualizacion_snmv1 <- "Última actualización: 30 de abril de 2025"

# Fecha actualizacion de los datos de SNMV tab2
actualizacion_snmv2 <- "Última actualización: 30 de abril de 2025"

# Fecha actualizacion familia
actualizacion_familia <- "Última actualización: 31 de diciembre de 2023"

# Fecha actualizacion justicia tab1
actualizacion_justicia1 <- "Última actualización: 31 de diciembre de 2023"

# Fecha actualizacion justicia tab2
actualizacion_justicia2 <- "Última actualización: 31 de diciembre de 2023"

# Fecha actualizacion avp tab1
actualizacion_avp1 <- "Última actualización: 31 de diciembre de 2024"

# Fecha actualizacion avp tab2
actualizacion_avp2 <- "Última actualización: 31 de diciembre de 2024"

# Fecha actualizacion policia tab1
actualizacion_policia1 <- "Última actualización: 20 de septiembre de 2024"

# Fecha actualizacion policia tab2
actualizacion_policia2 <- "Última actualización: 20 de septiembre de 2024"

# Fecha actualizacion policia tab3
actualizacion_policia3 <- "Última actualización: 31 de mayo de 2025"

# Fecha actualizacion policia tab4
actualizacion_policia4 <- "Última actualización: 30 de junio de 2025"

# Fecha actualizacion opm tab1
actualizacion_opm1 <- "Última actualización: 31 de diciembre de 2023"

# Fecha actualizacion opm tab2
actualizacion_opm2 <- "Última actualización: 31 de diciembre de 2024"

# Fecha actualizacion opm tab3
actualizacion_opm3 <- "Última actualización: 31 de diciembre de 2024"

# Fecha actualizacion opm tab4
actualizacion_opm4 <- "Última actualización: 31 de diciembre de 2024"

# Fecha actualizacion opm tab5
actualizacion_opm5 <- "Última actualización: 31 de diciembre de 2024"

# Fecha actualizacion opm tab6
actualizacion_opm6 <- "Última actualización: 31 de diciembre de 2023"

# Fecha actualizacion correccion y rehabilitacion tab1
actualizacion_dcr1 <- "Última actualización: 31 de octubre de 2024"

# Fecha actualizacion correccion y rehabilitacion tab2
actualizacion_dcr2 <- "Última actualización: 28 de febrero de 2023"

# Fecha actualizacion Tribunales tab1
actualizacion_tribunales1 <- "Última actualización: 27 de marzo de 2025"

# Fecha actualizacion Tribunales tab2
actualizacion_tribunales2 <- "Última actualización: 15 de octubre de 2024"

# Fecha actualizacion Tribunales tab3
actualizacion_tribunales3 <- "Última actualización: 15 de octubre de 2024"

# Fecha actualizacion Tribunales tab4
actualizacion_tribunales4 <- "Última actualización: 15 de octubre de 2024"

# Fecha actualizacion Tribunales tab5
actualizacion_tribunales5 <- "Última actualización: 15 de octubre de 2024"

# Fecha actualizacion Tribunales tab6
actualizacion_tribunales6 <- "Última actualización: 15 de octubre de 2024"

# Fecha actualizacion Tribunales tab1
actualizacion_tribunales7 <- "Última actualización: 27 de marzo de 2025"

# Fecha actualizacion datos cavv tab1
actualizacion_cavv1 <- "Última actualización: 31 de diciembre de 2024"

# Fecha actualizacion datos cavv tab2
actualizacion_cavv2 <- "Última actualización: 31 de diciembre de 2024"



##################################################################################
##### Credenciales para el web hosting #####
##################################################################################
# Definir usuarios y contraseñas
credentials <- data.frame(
  user = c("manu"),
  password = c("mangu"),
  stringsAsFactors = FALSE
)

