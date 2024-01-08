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
                "checkGroup_snmv",
                label = h3("Seleccione Grupo(s) de Edad"),
                choices = c("Seleccionar Todos", levels(homiEdad$edad)),
                selected = "Seleccionar Todos"
              ),
              actionButton("deselectAll_snmv", "Deseleccionar todo"),
              hr(),
              fluidRow(column(3, verbatimTextOutput("value"))),
              selectInput("yearInput_snmv", "Seleccionar Año:", choices = unique(homiEdad$año))
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              plotlyOutput("linePlot_snmv"),
              plotlyOutput("barPlot_snmv"),
              DTOutput("dataTable_snmv")
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
        # Tab para 
        tabPanel(
          "dfMalt", 
          # Título del Tab
          titlePanel("Cantidad de menores que fueron víctimas de maltrato, según sexo y Maltrato: Años 2018-*2022"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Departamento de la Familia"), tags$br(),
          tags$span("Actualizado:", actualizacion_familia), tags$br(),
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              checkboxGroupInput(
                "checkGroup_fam",
                label = h3("Seleccione Tipo(s) de Maltrato"),
                choices = c("Seleccionar Todos", levels(dfMalt$Maltrato)),
                selected = "Seleccionar Todos"
              ),
              actionButton("deselectAll_fam", "Deseleccionar todo"),
              hr(),
              selectInput("yearInput_fam", "Seleccionar Año:", choices = unique(dfMalt$Año))
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              plotlyOutput("linePlot_fam"),
              plotlyOutput("barPlot_fam"),
              DTOutput("dataTable_fam")
            )
          ),
          tags$span("NOTA IMPORTANTE: Las cantidades podrían representar conteos duplicados del o la menor. Esto es, debido a que el menor se cuenta cada vez que él o ella son parte de uno o múltiples referidos. Este conteo es conocido por el Departamento de la Familia como el pareo de menores-reportado. Estos datos están en proceso de revisión por el Instituto de Estadísticas de Puerto Rico. *Nota: Datos parciales y preliminares del año 2022. Están disponibles hasta noviembre de 2022."),
        )
      )
    ),
    
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