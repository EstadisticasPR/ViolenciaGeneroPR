########################################
#### Cargar bibliotecas necesarias #####
########################################
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(here)
library(readxl)

##################################################################################
#### Procesamiento de datos del Sistema de Notificacion de Muertes Violentas #####
##################################################################################
# Datos de ejemplo
path <- here("data", "Sistema_de_Notificacion_de_Muertes_Violentas", "/")
homiEdad <- read_excel(paste0(path, "svmvhomiEdad.xlsx"))

# importar los datos de homicidios por grupo de edad
df <- homiEdad %>%
  select(-Total) %>%
  filter(!grepl("Total", `Grupo de edad`) & `Grupo de edad` != "Desconocido") %>%
  pivot_longer(!`Grupo de edad`, names_to = "año", values_to = "casos") %>%
  rename(edad = `Grupo de edad`) %>%
  mutate(
    edad = factor(edad, levels = unique(edad)),
    año = factor(año)
  )

# Definir una paleta de colores personalizada
grupos_edad <- unique(homiEdad_long_xtotals$edad)
paleta_colores <- scales::hue_pal()(length(grupos_edad))
colores_grupos_edad <- setNames(paleta_colores, grupos_edad)