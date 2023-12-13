# Paquetes utilizados----
library(bslib) #libreria que da elementos bootstrap para aplicaciones web
library(shiny) #libreria que da accesso a los paquetes de Shiny
library(shinyWidgets) #libreria que da elementos de paginas web como switches, checkboxes y botones
library(shinydashboard) #libreria que permite usar dashboards en R
library(ggplot2) #libreria que crea graficas, en cuanto le proveas los detalles de las graficas
library(sf) #libreria que lee data geografica
library(leaflet) #libreria que permite hacer mapas sofistifados a traves de javascript
library(tidyverse) #libreria que comparte datos y APIs en comun en una manera organizada
library(readxl) #libreria que permite leer archivos excel
library(datasets) #libreria que incluye datasets de R
library(DT) #libreria que permite crear tablas de datos
library(plotly) #libreria que permite crear graficas web
library(scales) #libreria que proporciona adecuadamente los mapas
library(RColorBrewer) #libreria que provee color para los mapas
library(zoo) #libreria que organiza indices, vectores y matrices

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

# Tab de Producción 
AlimentoProduccion <-sidebarLayout(
  div(id="sideBarP",sidebarPanel(
    
    # seleccionar el grupo de interés 
    selectInput(inputId = "data1", label = "Seleccione el grupo:", 
                choices = c("Carnes",'Farináceos','Cocos y Nueces',"Frutas Frescas",'Hortaliza Frescas',
                            'Leche y Producto Lacteos','Lengumbres','Hierbas y Especias'),
                selected = 'Frutas Frescas'),
    # uiOutput permite tener herramientas dinámica  
    uiOutput("produccion"),
    # selectInput(inputId='year',label='Seleccione el año base',
    #             choices=c(1991:2018)),
    uiOutput('baseYear'),
    # selectInput(inputId = "grafico", label = "Seleccione el tipo de estudio:", choices = c("Lineal"='lineal',"Exportación vs Importación"='barra'),selected = 'lineal'),
    ##uiOutput('rangeSlider')
    #actionButton("cerrar",icon = icon('magnifying-glass'), " Cerrar")
  )),
  mainPanel( id="mainP",
             uiOutput("abrirP"),         
             tabsetPanel(id = "main",
                         tabPanel('Producción'),
                         tabPanel('Comercio'),
                         tabPanel('Metadatos y Definiciones',value= 'Metadatos')),
             
             
             tags$br(),
             shinycssloaders::withSpinner( uiOutput('produccionLocal')),
             #uiOutput('produccionLocalT'),
             tags$div(shinycssloaders::withSpinner( uiOutput("data")),style="background-color: white"),
             
             
  ),
  
)