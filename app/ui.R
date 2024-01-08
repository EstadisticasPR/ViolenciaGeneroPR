# Cargar el contenido de global.R
source("global.R")

# User Interface 
ui <- fluidPage(
  
  ### El theme (colores) de la app ###
  theme = shinytheme("sandstone"),
  
  #### Encabezado ####
  div(
    tags$div(
      tags$title('Instituto de Estadísticas de Puerto Rico'),
      
      ### Foto con enlace a la página de IEPR ###
      tags$ul(
        tags$li(
          style = 'display: inline-block; margin-right: 20px; vertical-align: middle;',
          div(
            id = 'logo_IEPR',
            tags$a(
              tags$figure(
                img(src = "ieprlogo.png", height = 60, alt = "estadisticas.pr", deleteFile = FALSE)
              ),
              href = 'https://estadisticas.pr/'
            )
          )
        ),
        
        ### Foto con enlace a la página de PARE ###
        tags$li(
          style = 'display: inline-block; margin-left: 10px; vertical-align: middle;',
          div(
            id = 'logo_PARE',
            tags$a(
              tags$figure(
                img(src = "logo_PARE.png", height = 60, alt = "PARE.gov", deleteFile = FALSE)
              ),
              href = "https://parelaviolencia.pr.gov/"
            )
          )
        ),
        
        ### Título de la App ###
        tags$li(
          style = 'display: inline-block;margin-bottom: 12px;margin-right: 25px;',
          div(
            tags$h3(
              'Estadísticas de Violencia de Género en Puerto Rico',
              style = 'font-family:"Arial Black",sans-serif;'
            )
          )
        ),
        
        style = 'list-style-type: none;
      display: flex;
      flex-wrap: wrap;
      margin: 0px;
      justify-content: space-between;
      background: pink;
      margin: 0px;
      padding-bottom: 10;
      border-radius: 0;'
      )
    ),
    style = "top: 1px;"
  ),
  
  
  # Titulo de la app
  navbarPage(
    "",
    
    #### Tab del Sistema de Notificación de Muertes Violentas ####
    tabPanel(
      "Sistema de Notificación de Muertes Violentas",
      tabsetPanel(
        
        # tab con datos de Homicidios por grupo de Edad
        tabPanel(
          "homiEdad",
          # Título del Tab
          titlePanel("Homicidios de mujeres por grupo de edad según el año"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Instituto de Estadísticas, Sistema de Notificación de Muertes Violentas"), tags$br(),
          tags$span(paste0("Actualizado: ", actualizacion_snmvA)),
          tags$span("Nota: Los datos del año 2020 son preliminares"),
          
          # Menu sidebar con widgets
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
            
            # Sección principal con los gráficos
            mainPanel(
              plotlyOutput("linePlot"),
              plotlyOutput("barPlot"),
              DTOutput("dataTable")
            )
          )
        ),
        
        # tab con datos de Incidentes
        tabPanel("Incidentes"), 
        
        # tab con datos de Tasas
        tabPanel("Tasas"), 
      )
    ),
    
    #### Tab del Departamento de la Familia ####
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
  )
)
    
    #### Tab Acerca del Dashboard ####
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
    
    #### Formato del Tab ####
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
