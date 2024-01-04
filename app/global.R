########################################
#### Cargar bibliotecas necesarias #####
########################################
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(here)
library(readxl)
library(shinythemes)

##################################################################
##### convert_mixed_columns se usa para manejar missing data #####
##################################################################
# funcion para convertir columna con varios tipos de datos a numerico
convert_mixed_columns <- function(data) {
  mixed_columns <- sapply(data, function(col) any(is.character(col) & !is.na(as.numeric(col))))
  mixed_columns_names <- names(mixed_columns)[mixed_columns]
  
  for (col in mixed_columns_names) {
    data[[col]] <- ifelse(data[[col]] == "N/A", NA, as.character(data[[col]]))
    data[[col]] <- as.numeric(data[[col]])
  }
  
  return(data)
}

##################################################################################
#### Procesamiento de datos del Sistema de Notificacion de Muertes Violentas #####
##################################################################################
# path para Sistema_de_Notificacion_de_Muertes_Violentas
snmv <- here("data", "Sistema_de_Notificacion_de_Muertes_Violentas", "/")

# importar los datos de homicidios por grupo de edad; homiEdad
homiEdad <- read_excel(paste0(snmv, "svmvhomiEdad.xlsx")) %>%
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
  scales::hue_pal()(length(grupos_edad))
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

dfMalt <- dfMalt2018 %>%
  full_join(dfMalt2019) %>%
  full_join(dfMalt2020) %>%
  full_join(dfMalt2021) %>%
  full_join(dfMalt2022) %>%
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
  )