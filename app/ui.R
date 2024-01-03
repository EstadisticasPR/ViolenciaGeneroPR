# Cargar el contenido de global.R
source("global.R")

# UI
ui <- fluidPage(
  titlePanel("Análisis de Casos por Grupo de Edad y Año"),
  sidebarLayout(
    sidebarPanel(
      
      checkboxGroupInput("checkGroup", label = h3("Seleccione Grupo(s) de Edad"), 
                         choices = c("Seleccionar Todos", levels(df$edad)),
                         selected = "Seleccionar Todos"),
      
      checkboxGroupInput("checkGroup", label = h3("Seleccione Grupo(s) de Edad"), 
                         choices = paste(levels(df$edad)),
                         selected = "Seleccionar Todos"),
      actionButton("deselectAll", "Deseleccionar todo"),
      
      
      hr(),
      fluidRow(column(3, verbatimTextOutput("value"))),
      selectInput("yearInput", "Seleccionar Año:", choices = unique(df$año))
    ),
    mainPanel(
      plotOutput("linePlot"),
      plotOutput("barPlot"),
      DTOutput("dataTable")
    )
  )
)