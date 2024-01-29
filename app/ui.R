# User Interface 
cat("Loading User Interface from ui.R...\n")
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
      icon = icon("exclamation-triangle"),
      tabsetPanel(
        
        # tab con datos de Homicidios por grupo de Edad
        tabPanel(
          "homiEdad_snmv",
          # Título del Tab
          titlePanel("Homicidios de mujeres por grupo de edad según el año"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Instituto de Estadísticas, Sistema de Notificación de Muertes Violentas"), tags$br(),
          tags$span(paste0("Actualizado: ", actualizacion_snmvB)),
          tags$span("Nota: Los datos del año 2020 son preliminares"),
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(

              # botón para tener el checkbox en un menu dropdown 
              dropdownButton(
                circle = "FALSE",
                label = "Seleccione Grupo(s) de Edad:", 
                status = "default", 
                size = "default",
                checkboxGroupInput(
                  "checkGroup_snmv",
                  label = "",
                  choices = levels(homiEdad$edad),
                  selected = levels(homiEdad$edad)[8]
                ),
                actionButton("deselectAll_snmv", "(De)seleccionar todo")
              ),
              
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
        
        # tab con datos de Incidentes segun el año
        tabPanel(
          "Incidentes_snmv",
          
          # Título del Tab
          titlePanel("Tipo de muerte según el año"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Instituto de Estadísticas, Sistema de Notificación de Muertes Violentas"), tags$br(),
          tags$span(paste0("Actualizado: ", actualizacion_snmvA)),
          tags$span("Nota: Los datos del año 2020 son preliminares"),
          
          sidebarLayout(
            sidebarPanel(
              # botón para tener el checkbox en un menu dropdown 
              dropdownButton(
                circle = "FALSE",
                label = "Seleccione Tipo(s) de Incidente:",
                status = "default",
                size = "default",
                checkboxGroupInput(
                  "checkGroup_snmv_A",
                  label = "",
                  choices = levels(inci$tipo),
                  selected = levels(inci$tipo)
                ),
                actionButton("deselectAll_snmv_A", "(De)seleccionar todo")
              ),

              hr()
              # fluidRow(column(3, verbatimTextOutput("value")))
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              # plotlyOutput("linePlot_snmv"),
              # plotlyOutput("barPlot_snmv"),
              # DTOutput("dataTable_snmv")
            )
          ),
            
          
          ), 
        
        # tab con datos de Tasas
        tabPanel(
          "Tasas_snmv"
          ), 
        
        # ### xiomy
        # tabPanel(
        #   "Xiomy",  # Cambiar por el nombre del tercer dfDeli
        #   # ... Estructura similar a la anterior para el tercer dfDeli
        #   tags$div(
        #     tags$h2("xiomara"),
        #     tags$img(src = "xiomy.jpg", height = 1000, width = 1000)
        #   )
        # ),
      )
    ),
    
    #### Tab del Departamento de la Familia ####
    tabPanel(
      "Departamento de la Familia",
      icon = icon("users"),
      tabsetPanel(
        # Tab para 
        tabPanel(
          "dfMalt_fam", 
          # Título del Tab
          titlePanel("Cantidad de menores que fueron víctimas de maltrato, según sexo y tipo de maltrato: Años 2018-*2022"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Departamento de la Familia"), tags$br(),
          tags$span("Actualizado:", actualizacion_familia), tags$br(),
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              dropdownButton(
                circle = "FALSE",
                label = "Seleccione Tipo(s) de Maltrato:",
                status = "default",
                width = 300,
                actionButton("deselectAll_fam", "(De)seleccionar todo"),
                checkboxGroupInput(
                  "checkGroup_fam",
                  label = h3("Seleccione Tipo(s) de Maltrato"),
                  choices = levels(dfMalt$Maltrato),
                  selected = levels(dfMalt$Maltrato)[4]
                )
              ),
              hr(),
              selectInput("yearInput_fam", "Seleccionar Año:", choices = unique(dfMalt$Año))
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              # aglomerar masculino y femenino
              plotlyOutput("linePlot_fam"),
              plotlyOutput("barPlot_fam"),
              DTOutput("dataTable_fam")
            )
          ),
          tags$span("NOTA IMPORTANTE: Las cantidades podrían representar conteos duplicados del o la menor. Esto es, debido a que el menor se cuenta cada vez que él o ella son parte de uno o múltiples referidos. Este conteo es conocido por el Departamento de la Familia como el pareo de menores-reportado. Estos datos están en proceso de revisión por el Instituto de Estadísticas de Puerto Rico. *Nota: Datos parciales y preliminares del año 2022. Están disponibles hasta noviembre de 2022."),
        )
      )
    ),
    
    #### Tab del Departamento de Justicia ####
    tabPanel(
      "Departamento de Justicia",
      icon = icon("balance-scale"),
      tabsetPanel(
        # Subtab con datos específicos para dfDeli
        tabPanel(
          "dfDeli",
          # Título del Tab
          titlePanel("Delitos a la Ley 54, casos radicados por jurisdicción y articulo de la Ley de Violencia Domestica, año 2020 a *2023"),  # Cambiar por el título adecuado
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Departamento de Justicia" ), tags$br(),
          tags$span(paste0("Actualizado: ", actualizacion_justiciaA)), tags$br(),
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              
              # botón para tener el checkbox en un menu dropdown 
              div(
                dropdownButton(
                  circle = "FALSE",
                  label = "Seleccione el/los Articulo(s):",
                  status = "default",
                  width = 350,
                  actionButton("deselectAll_just", "(De)Seleccionar todo"),
                  style = "background-color: white;",
                  checkboxGroupInput(
                    "checkGroup_just",
                    label = "Seleccione el/los Articulo(s)",
                    choices = levels(dfDeli$Delito),
                    selected = levels(dfDeli$Delito)[6]
                  )
                ),
                style = "display: inline-block; padding-right: 20px;"
              ),
              
              # botón de seleccionar input
              
              # botón para tener el checkbox en un menu dropdown 
              div(
                dropdownButton(
                  circle = "FALSE",
                  label = "Seleccionar Año:",
                  status = "default",
                  width = 350,
                  actionButton("deselectAll_year", "(De)Seleccionar todo"),
                  checkboxGroupInput(
                    "yearInput_just",
                    label = "Seleccionar Año:",
                    choices = unique(dfDeli$Año),
                    selected = levels(dfDeli$Año)
                  )
                ),
                style = "display: inline-block;"
              ),
              
              selectInput("districtInput_just", "Seleccionar Distrito:", choices = unique(dfDeli$`FISCALIA DISTRITO`))
            ),
            # Sección principal con los gráficos
            mainPanel(
              plotlyOutput("boxPlot_just"),
              plotlyOutput("barPlot_just"),
              plotOutput("deliPlot_just"),
              DTOutput("dataTable_just")
            )
          )
        ),
        
        # Subtab con datos específicos para el segundo dfDeli de la agencia
        tabPanel(
          "convic"  # Cambiar por el nombre del segundo dfDeli
          # ... Estructura similar a la anterior para el segundo dfDeli
        ),
        
        # Subtab con datos específicos para el tercer dfDeli de la agencia
        # ### yeiza
        # tabPanel(
        #   "Yeiza",  # Cambiar por el nombre del tercer dfDeli
        #   # ... Estructura similar a la anterior para el tercer dfDeli
        #   tags$div(
        #     tags$h2("yeyeyey"),
        #     tags$img(src = "yeiza.jpg", height = 1000, width = 1000)
        #   )
        # ),
      )
    ),
    
    #### Tab Acerca del Dashboard ####
    tabPanel(
      "Acerca del Dashboard",
      icon = icon("info-circle"),
      tabsetPanel(
        
        # tab de dfmalt
        tabPanel("Transfondo_del_Proyecto"),
        tabPanel("Fuentes"), 
        tabPanel("Autores"),
        
        tabPanel(
          "Contacto",
          mainPanel(
            fluidRow(
              
              column(6, 
                     h1("Instituto De Estadísticas De Puerto Rico"),
                     h4('Postal: P.O. Box 195484 | San Juan, PR 00919-5484'),
                     h4( a(href="tel:787-819-0730", 'Tel: (787) 819-0730')),
                     h4('Email: preguntas@estadisticas.pr'),
                     h4('Horario de Oficina: lunes a viernes, 8:00 am a 4:30 pm'),
                     a(href='https://www.facebook.com/estadisticas.pr',
                       icon("facebook","fa-2x"),) ,
                     a(href='https://www.instagram.com/institutodeestadisticas/' ,
                       icon('instagram','fa-2x')),
                     a(href="https://twitter.com/EstadisticasPR",
                       icon('twitter','fa-2x')),
                     a(href="https://www.youtube.com/channel/UCIZggRtE5KK0z9D39FGZtyQ",
                       icon('youtube','fa-2x'))),
              
              column(3),
              column(3,
                     br(),
                     a(img(src='IEPRlocal.png',height=200),
                       href="https://www.google.com/maps/place/Instituto+de+Estad%C3%ADsticas+de+Puerto+Rico/@18.4279999,-66.056444,17.5z/data=!4m5!3m4!1s0x8c0368a432af0b3d:0x274ac1c656b89f89!8m2!3d18.4275192!4d-66.0562994"),
                     a(img(src='IEPRmap.png',height=200),
                       href='https://www.google.com/maps/place/Instituto+de+Estad%C3%ADsticas+de+Puerto+Rico/@18.4279999,-66.056444,17.5z/data=!4m5!3m4!1s0x8c0368a432af0b3d:0x274ac1c656b89f89!8m2!3d18.4275192!4d-66.0562994'))
            )
            
          )
          )
      )
    ),
    
    #### Formato del Tab ####
    tabPanel(
      "test1",
      tabsetPanel(
        
        # tab de dfmalt
        tabPanel("test_A",
                 mainPanel(
                   DTOutput("dataTable_test"),
                   plotlyOutput("boxPlot_test"),
                   plotlyOutput("barPlot_test"),
                   plotOutput("linePlot_test")
                 )
                 ),
        tabPanel("test_B",
                 mainPanel(
                   DTOutput("dataTable_test2")
                 ))
        
      )
    ),
    
    #### Formato del Tab ####
    tabPanel(
      "test2",
      icon = icon("exclamation-triangle"),
      sidebarLayout(
        div(sidebarPanel(
          selectInput("yearInput_xiom", "Seleccionar xiom:", choices = unique(homiEdad$año)),
          selectInput("yearInput_yeiz", "Seleccionar yeiza:", choices = unique(homiEdad$año))
        )
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Homicidios por grupo de Edad",
              uiOutput("yearInput_xiom"),
              uiOutput("yearInput_yeiz")
            ),

            # tab con datos de Incidentes
            # tabPanel("Incidentes_snmv",
            #          uiOutput("yearInput_yeiz")),
            # 
            # # tab con datos de Tasas
            # tabPanel("Tasas_snmv",
            #          uiOutput("yearInput_xiom")
            #          )
          )
        )
      )
    )
    
  )
)
