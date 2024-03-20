# User Interface 
cat("Loading User Interface from ui_helpers.R...\n")
# Importar funciones auxiliares de ui_helpers.R
addResourcePath("www", "R/www/")
ui <- fluidPage(
  ### El theme (colores) de la app ###
  theme = shinytheme("sandstone"),
  
  #### Encabezado ####
  div(
    tags$div(
      tags$title('Instituto de Estadísticas de Puerto Rico'),
      
      tags$ul(
        ### Foto con enlace a la página de IEPR ###
        embedImage("logo_IEPR", "www/iepr_logo.png", "https://estadisticas.pr/", "estadisticas.pr"),
        
        ### Foto con enlace a la página de PARE ###
        embedImage("logo_PARE", "www/logo_PARE.png", "https://parelaviolencia.pr.gov/", "PARE.gov"),
        
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
        
        #### tab con datos de Homicidios por grupo de Edad ####
        tabPanel(
          "homiEdad",
          # Título del Tab
          titlePanel("Homicidios de mujeres por grupo de edad según el año"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Instituto de Estadísticas, Sistema de Notificación de Muertes Violentas"), tags$br(),
          tags$span(paste0("Actualizado: ", actualizacion_snmvB)),
          tags$span("Nota: Los datos del año 2020 son preliminares"),
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              
              # botón checkbox para seleccionar grupos de edad 
              createDropdownCheckbox(
                label = "Seleccione Grupo(s) de Edad:",
                choices = homiEdad$edad,
                selected = 8,
                id = "snmv_homiEdad_edad"
              ),
              # botón checkbox para seleccionar año
              createDropdownCheckbox(
                label = "Seleccione Año(s):",
                choices = homiEdad$año,
                selected = NULL,
                id = "snmv_homiEdad_año"
              ),
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              #plotlyOutput("linePlot_snmv"),
              plotlyOutput("barPlot_snmv"),
              DTOutput("dataTable_snmv")
            )
          )
        ),
        
        #### tab con datos de Incidentes segun el año ####
        tabPanel(
          "inci",
          
          # Título del Tab
          titlePanel("Incidentes Violentos: Tipo de muerte según el año"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Instituto de Estadísticas, Sistema de Notificación de Muertes Violentas"), tags$br(),
          tags$span(paste0("Actualizado: ", actualizacion_snmvA)),
          tags$span("Nota: Los datos del año 2020 son preliminares"),
          
          sidebarLayout(
            sidebarPanel(
              
              # botón para seleccionar tipo de incidente
              createDropdownCheckbox(
                label = "Seleccione Tipo(s) de Incidente:",
                choices = inci$tipo,
                selected = 7,
                id = "snmv_inci_tipo"
              ),
              
              # botón para seleccionar año
              createDropdownCheckbox(
                label = "Seleccione Año(s):",
                choices = inci$año,
                selected = NULL,
                id = "snmv_inci_año"
              ),
              
              hr()
            ),
            
            # Sección principal con los gráficos y datatable
            mainPanel(
              plotlyOutput("barPlot_snmv_inci"),
              DTOutput("dataTable_snmv_inci")
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
        #### tab con datos de menores víctimas de maltrato (dfMalt)  ####
        tabPanel(
          "dfMalt", 
          # Título del Tab
          titlePanel("Cantidad de menores que fueron víctimas de maltrato, según sexo y tipo de maltrato: Años 2018-*2022"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Departamento de la Familia"), tags$br(),
          tags$span("Actualizado:", actualizacion_familia), tags$br(),
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              
              # botón para seleccionar tipo de maltrato
              createDropdownCheckbox(
                label = "Seleccione Tipo(s) de Maltrato:",
                choices = dfMalt$Maltrato,
                selected = 1,
                id = "fam_dfMalt_tipo"
              ),
              
              # botón para seleccionar año
              createDropdownCheckbox(
                label = "Seleccione Año(s):",
                choices = dfMalt$Año,
                selected = NULL,
                id = "fam_dfMalt_año"
              ),
              hr(),
              # botón para seleccionar sexo
              createDropdownCheckbox(
                label = "Seleccione sexo de las víctimas:",
                choices = dfMalt$Sexo,
                selected = 1,
                id = "fam_dfMalt_sexo"
              ),
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              # aglomerar masculino y femenino
              #plotlyOutput("linePlot_fam"),
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
        #### tab con datos de delitos de violencia doméstica (dfDeli) ####
        tabPanel(
          "dfDeli",
          # Título del Tab
          # pregunta si es Distrito Fiscal se llama Jurisdicción Fiscal y si los Casos se le llaman Delitos
          titlePanel("Delitos a la Ley 54, casos radicados por jurisdicción y articulo de la Ley de Violencia Domestica"),  # Cambiar por el título adecuado
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Departamento de Justicia" ), tags$br(),
          tags$span(paste0("Actualizado: ", actualizacion_justiciaA)), tags$br(),
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              
              # botón para seleccionar delito
              createDropdownCheckbox(
                label = "Seleccione el/los Delitos(s):",
                choices = dfDeli$Delito,
                selected = 2,
                id = "just_dfDeli_delito"
              ),
              
              # botón para seleccionar año
              createDropdownCheckbox(
                label = "Seleccionar Año:",
                choices = dfDeli$Año,
                selected = NULL,
                id = "just_dfDeli_año"
              ),
              hr(),
              # botón para seleccionar distrito
              createDropdownCheckbox(
                label = "Seleccionar Distrito(s):",
                choices = dfDeli$`FISCALIA DISTRITO`,
                selected = NULL,
                id = "just_dfDeli_distrito"
              ),
              
            ),
            # Sección principal con los gráficos
            mainPanel(
              #plotlyOutput("boxPlot_just"),
              plotlyOutput("barPlot_just"),
              #plotlyOutput("map_just"),
              #plotlyOutput("facet_bartest"),
              # plotOutput("deliPlot_just"),
              DTOutput("dataTable_just")
            )
          )
        ),
        
        # Subtab con datos específicos para el mapa de las Fiscalías
        tabPanel(
          "mapaFisc",
          
          # Título del Tab 
          # pregunta si es Distrito Fiscal se llama Jurisdicción Fiscal y si los Casos se le llaman Delitos
          titlePanel("Distritos Fiscales de Puerto Rico"),  # Cambiar por el título adecuado
          
          # Sección principal con los gráficos
          mainPanel(
            plotlyOutput("map_just_mapaFisc"),
            #DTOutput("dataTable_just_mapaFisc")
          )
        ), 
        
        # Subtab con datos específicos para el mapaDeli
        tabPanel(
          "mapaDeli",
          
          # Título del Tab 
          # pregunta si es Distrito Fiscal se llama Jurisdicción Fiscal y si los Casos se le llaman Delitos
          titlePanel("Delitos a la Ley 54, casos radicados por jurisdicción y articulo de la Ley de Violencia Domestica"),  # Cambiar por el título adecuado
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Departamento de Justicia" ), tags$br(),
          tags$span(paste0("Actualizado: ", actualizacion_justiciaA)), tags$br(),
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              
              # botón para seleccionar delito
              # createDropdownCheckbox(
              #   label = "Seleccione el/los Delitos(s):",
              #   choices = mapaDeli$Delito,
              #   selected = 2,
              #   id = "just_mapaDeli_delito"
              # ),
              
              # botón para seleccionar delito
              selectInput("select_just_mapaDeli_delito", "Seleccione Delito:",
                          choices = levels(mapaDeli$Delito),
                          selected = 2),
              
              # createDropdownCheckbox(
              #   label = "Seleccionar Año:",
              #   choices = mapaDeli$Año,
              #   selected = NULL,
              #   id = "just_mapaDeli_año"
              # ),
              
              # botón para seleccionar año
              selectInput("select_just_mapaDeli_año", "Seleccione Año:",
                          choices = levels(mapaDeli$Año),
                          selected = 1)
              
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              plotlyOutput("map_just_mapaDeli"),
              DTOutput("dataTable_just_mapaDeli")
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
    
    #### Tab del Departamento del Trabajo y Recursos Humanos ####
    tabPanel(
      "Departamento del Trabajo y Recursos Humanos",
      icon = icon("briefcase"),
      tabsetPanel(
        #### tab con datos de participación laboral (dtParlab) ####
        tabPanel(
          "parLab", 
          # Título del Tab
          titlePanel("Tasa de participación laboral por género y año"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Departamento del Trabajo y Recursos Humanos"), tags$br(),
          tags$span("Actualizado:", actualizacion_trabajo), tags$br(),
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              
              # botón para seleccionar el año
              createDropdownCheckbox(
                label = "Seleccione Año(s):",
                choices = parLab$Año,
                selected = parLab$Año,
                id = "trab_parLab_año"
              ),
              
              # botón para seleccionar el sexo
              createDropdownCheckbox(
                label = "Seleccione Sexo(s):",
                choices = parLab$Sexo,
                selected = parLab$Sexo,
                id = "trab_parLab_sexo"
              ),
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              plotlyOutput("barPlot_trab_parLab"),
              DTOutput("dataTable_trab_parLab")
            )
          ),
        )
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
                     a(img(src='www/IEPRlocal.png',height=200),
                       href="https://www.google.com/maps/place/Instituto+de+Estad%C3%ADsticas+de+Puerto+Rico/@18.4279999,-66.056444,17.5z/data=!4m5!3m4!1s0x8c0368a432af0b3d:0x274ac1c656b89f89!8m2!3d18.4275192!4d-66.0562994"),
                     a(img(src='www/IEPRmap.png',height=200),
                       href='https://www.google.com/maps/place/Instituto+de+Estad%C3%ADsticas+de+Puerto+Rico/@18.4279999,-66.056444,17.5z/data=!4m5!3m4!1s0x8c0368a432af0b3d:0x274ac1c656b89f89!8m2!3d18.4275192!4d-66.0562994'))
            )
            
          )
        )
      )
    ),
    
    #### Tab de prueba ####
    # tabPanel(
    #   "test",
    #   icon = icon("exclamation-triangle"),
    #   sidebarLayout(
    #     div(sidebarPanel(
    #       selectInput("yearInput_xiom", "Seleccionar xiom:", choices = unique(homiEdad$año)),
    #       selectInput("yearInput_yeiz", "Seleccionar yeiza:", choices = unique(homiEdad$año))
    #     )
    #     ),
    #     
    #     mainPanel(
    #       tabsetPanel(
    #         tabPanel(
    #           "Homicidios por grupo de Edad",
    #           uiOutput("yearInput_xiom"),
    #           uiOutput("yearInput_yeiz")
    #         ),
    #         
    #         # tab con datos de Incidentes
    #         # tabPanel("Incidentes_snmv",
    #         #          uiOutput("yearInput_yeiz")),
    #         # 
    #         # # tab con datos de Tasas
    #         # tabPanel("Tasas_snmv",
    #         #          uiOutput("yearInput_xiom")
    #         #          )
    #       )
    #     )
    #   )
    # )
    
  )
)
