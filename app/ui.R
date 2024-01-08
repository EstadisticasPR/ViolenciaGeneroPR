# Cargar el contenido de global.R
source("global.R")

# UI
ui <- fluidPage(
  
  #### El theme (colores) de la app ####
  theme = shinytheme("sandstone"),
  
  #### Encabezado ####
  div(
    tags$div(
      tags$title('Instituto de Estadísticas de Puerto Rico'),
      
      #### Foto con enlace a la página de IEPR ####
      tags$ul(
        tags$li(
          style = 'display: inline-block;',
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
        
        #### Foto con enlace a la página de PARE ####
        tags$li(
          style = 'display: inline-block;',
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
        
        #### Título de la App ####
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
        tabPanel(
          "homiEdad",
          titlePanel("Homicidios de mujeres por grupo de edad según el año"),
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
              plotlyOutput("linePlot"),
              plotlyOutput("barPlot"),
              DTOutput("dataTable")
            )
          )
        ),
        tabPanel("Incidentes"), # Mantener vacío
        tabPanel("Tasas"), # Mantener vacío
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
