# Cargar el contenido de global.R
source("global.R")

# UI
ui <- fluidPage(
  
  # estableciendo el tema del app
  theme = shinytheme("sandstone"),
  
  navbarPage(
    "Estadísticas de Violencia de Género en Puerto Rico",
    tabPanel(
      "Sistema de Notificación de Muertes Violentas",
      tabsetPanel(
        tabPanel(
          "homiEdad",
          titlePanel("Análisis de Casos por Grupo de Edad y Año"),
          sidebarLayout(
            sidebarPanel(
              checkboxGroupInput(
                "checkGroup",
                label = h3("Seleccione Grupo(s) de Edad"),
                choices = c("Seleccionar Todos", levels(homiEdad$edad)),
                selected = "Seleccionar Todos"
              ),
              actionButton("deselectAll", "Deseleccionar todo"),
              hr(),
              fluidRow(column(3, verbatimTextOutput("value"))),
              selectInput("yearInput", "Seleccionar Año:", choices = unique(homiEdad$año))
            ),
            mainPanel(
              plotOutput("linePlot"),
              plotOutput("barPlot"),
              DTOutput("dataTable")
            )
          )
        ),
        tabPanel("Incidentes"), # Mantener vacío
        tabPanel("Tasas"), # Mantener vacío
      )
    ),
    
    # tab del Departamento de la Familia
    tabPanel(
      "Departamento de la Familia",
      tabsetPanel(
        
        # tab de dfmalt
        tabPanel("dfmalt",
                 p("Cantidad de menores que fueron víctimas de maltrato, según sexo y tipo de maltrato: Años 2018-2022"),
        ), # Mantener vacío
        tabPanel("Información Adicional")  # Mantener vacío
      )
    ),
    
    # tab del Acerca del Dashboard
    tabPanel(
      "Acerca del Dashboard",
      tabsetPanel(
        
        # tab de dfmalt
        tabPanel("Transfondo del Proyecto"),
        tabPanel("Fuentes"), 
        tabPanel("Autores"),
        tabPanel("Contacto")
      )
    ),
    
    # tab format
    tabPanel(
      "Tab 3",
      tabsetPanel(
        
        # tab de dfmalt
        tabPanel("Subtab 3A"), # Mantener vacío
        tabPanel("Subtab 3B")  # Mantener vacío
      )
    )
  )
)
