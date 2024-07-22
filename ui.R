# User Interface 
cat("Loading User Interface from ui.R...\n")
source("global.R")
# Importar funciones auxiliares de ui_helpers.R
#addResourcePath("www", "R/www/")
ui <- 
  #secure_app(
  fluidPage(
  ### El theme (colores) de la app ###
  theme = shinytheme("sandstone"),
  
  #### Encabezado ####
  div(
    tags$div(
      tags$title('Instituto de Estadísticas de Puerto Rico'),
      
      tags$ul(
        ### Foto con enlace a la página de IEPR ###
        embedImage("logo_IEPR", "iepr_logo.png", "https://estadisticas.pr/", "estadisticas.pr"),
        
        ### Foto con enlace a la página de PARE ###
        embedImage("logo_PARE", "logo_PARE.png", "https://parelaviolencia.pr.gov/", "PARE.gov"),
        
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
    
    # elimina el espacio vacío del título
    tags$style(HTML(".navbar-brand { display: none; } .navbar { min-height: 0; padding-top: 0; padding-bottom: 0; }")),
    
    #### Tab del Sistema de Notificación de Muertes Violentas ####
    tabPanel(
      lowercaseTitle("Sistema de Notificación de Muertes Violentas"),
      icon = icon("exclamation-triangle"),
      tabsetPanel(
        
        #### tab con datos de Homicidios por grupo de Edad ####
        tabPanel(
          lowercaseTitle("Homicidios de mujeres por edad"),
          # Título del Tab
          titlePanel("Homicidios de mujeres por grupo de edad según el año"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Instituto de Estadísticas, Sistema de Notificación de Muertes Violentas"), tags$br(),

          #tags$span(paste0("Actualización Datos: ", actualizacion_snmvB)),
          #tags$span(paste0("Actualización Sistema: ", actualizacion_sistema)),
          

          tags$span(paste0("Actualizado: ", actualizacion_snmvB)),

          
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
          lowercaseTitle("Tipos de incidentes violentos"),
          
          # Título del Tab
          titlePanel("Número de incidentes violentos por tipo para ambos sexos"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Instituto de Estadísticas, Sistema de Notificación de Muertes Violentas"), tags$br(),

          #tags$span(paste0("Actualización Datos: ", actualizacion_snmvA)),
          #tags$span(paste0("Actualización Sistema: ", actualizacion_sistema)),
          
          

          tags$span(paste0("Actualizado: ", actualizacion_snmvA)),

          
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
        
        #### tab con datos de Tasas ####
        # tabPanel(
        #   "Tasas_snmv"
        # ), 
        
        #### tab de Definiciones y Metadatos ####
        tabPanel(
          lowercaseTitle("Definiciones y Metadatos"),
          br(),
          tags$ul(style = "list-style-type: none; text-align: center;",
                  sectionTitle("Sistema de Notificación de Muertes Violentas", "24px")),
          fluidRow(
            column(12, uiOutput("definitionCards_snmv"))
          )
        )

        
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
      lowercaseTitle("Departamento de la Familia"),
      icon = icon("users"),
      tabsetPanel(
        #### tab con datos de menores víctimas de maltrato (dfMalt)  ####
        tabPanel(
          #HTML("<span style='text-transform: none; font-size: 14px;'>Maltrato de Menores por Sexo</span>"), 
          lowercaseTitle("Maltrato de menores por sexo"),
          # Título del Tab
          titlePanel("Cantidad de menores que fueron víctimas de maltrato por sexo y tipo de maltrato"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Departamento de la Familia"), tags$br(),
          #tags$span("Actualización Datos: ", actualizacion_familia), tags$br(),
          #tags$span(paste0("Actualización Sistema: ", actualizacion_sistema)),
          
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
              customSeparator(),
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
        ),
        
        #### tab de Definiciones y Metadatos ####
        tabPanel(
          lowercaseTitle("Definiciones y Metadatos"),
          br(),
          tags$ul(style = "list-style-type: none; text-align: center;",
                  sectionTitle("Departamento de la Familia", "24px")),
          fluidRow(
            column(12, uiOutput("definitionCards_fam"))
          )
        )
      )
    ),
    
    #### Tab del Departamento de Justicia ####
    tabPanel(
      lowercaseTitle("Departamento de Justicia"),
      icon = icon("balance-scale"),
      tabsetPanel(
        #### tab con datos de delitos de violencia doméstica (dfDeli) ####
        tabPanel(
          lowercaseTitle("Delitos Ley 54 por Distrito"),
          # Título del Tab
          # pregunta si es Distrito Fiscal se llama Jurisdicción Fiscal y si los Casos se le llaman Delitos
          titlePanel("Número de casos radicados por Distrito Fiscal y Artículo de la Ley 54"),  # Cambiar por el título adecuado
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Departamento de Justicia" ),
          #tags$span(paste0("Actualización Datos:   ", actualizacion_justiciaA)), 
          #tags$span(paste0("Actualización Sistema: ", actualizacion_sistema)),
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              
              # botón para seleccionar delito
              createDropdownCheckbox(
                label = "Seleccione Artículo(s) de ley 54:",
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
              customSeparator(),
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
        
        
        #### tab con datos del mapa delitos de violencia doméstica (mapaDeli) ####
        tabPanel(
          lowercaseTitle("Mapa de delitos Ley 54 por Distrito"),
          
          # Título del Tab 
          # pregunta si es Distrito Fiscal se llama Jurisdicción Fiscal y si los Casos se le llaman Delitos
          titlePanel("Número de casos radicados por Distrito Fiscal y Artículo de la Ley 54"),  # Cambiar por el título adecuado
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Departamento de Justicia" ), 
          #tags$span(paste0("Actualización Datos: ", actualizacion_justiciaA)),
          #tags$span(paste0("Actualización Sistema: ", actualizacion_sistema)), 
          
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
              selectInput("select_just_mapaDeli_delito", "Seleccione Artículo de la Ley 54:",
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
        # tabPanel(
        #   "convic"  # Cambiar por el nombre del segundo dfDeli
        #   # ... Estructura similar a la anterior para el segundo dfDeli
        # ),
        
        #### tab de Definiciones y Metadatos ####
        tabPanel(
          lowercaseTitle("Definiciones y Metadatos"),
          br(),
          tags$ul(style = "list-style-type: none; text-align: center;",
                  sectionTitle("Artículos de la Ley 54", "24px")),
          fluidRow(
            column(12, uiOutput("definitionCards_just"))
          ),
          
          br(),
          tags$ul(style = "list-style-type: none; text-align: center;",
                  sectionTitle("Fiscalías de Puerto Rico")),
          embedImage("Fiscalias_PR", "Fiscalias_PR.png", 
                     "https://www.justicia.pr.gov/secretarias-y-oficinas/oficina-del-jefe-de-los-fiscales/listado-de-fiscalias/",
                     "https://www.justicia.pr.gov/", size = "250"
                     ),
          # Sección principal con los gráficos
          mainPanel(
            plotlyOutput("map_just_mapaFisc"),
            #DTOutput("dataTable_just_mapaFisc")
          )
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
    # tabPanel(
    #   lowercaseTitle("Departamento del Trabajo y Recursos Humanos"),
    #   icon = icon("briefcase"),
    #   tabsetPanel(
    #     #### tab con datos de participación laboral (dtParlab) ####
    #     tabPanel(
    #       lowercaseTitle("Participación Laboral por Sexo"), 
    #       # Título del Tab
    #       titlePanel("Tasa de participación laboral por sexo"),
    #       
    #       # Fuente de Datos, Actualización
    #       tags$span("Fuente: Departamento del Trabajo y Recursos Humanos"), tags$br(),
    #       #tags$span("Actualización Datos:  ", actualizacion_trabajo), tags$br(),
    #       
    #       # Menu sidebar con widgets
    #       sidebarLayout(
    #         sidebarPanel(
    #           
    #           # botón para seleccionar el año
    #           createDropdownCheckbox(
    #             label = "Seleccione Año(s):",
    #             choices = parLab$Año,
    #             selected = parLab$Año,
    #             id = "trab_parLab_año"
    #           ),
    #           
    #           # botón para seleccionar el sexo
    #           createDropdownCheckbox(
    #             label = "Seleccione Sexo(s):",
    #             choices = parLab$Sexo,
    #             selected = parLab$Sexo,
    #             id = "trab_parLab_sexo"
    #           ),
    #         ),
    #         
    #         # Sección principal con los gráficos
    #         mainPanel(
    #           plotlyOutput("barPlot_trab_parLab"),
    #           DTOutput("dataTable_trab_parLab")
    #         )
    #       ),
    #     ),
    #     
    #     #### tab de Definiciones y Metadatos ####
    #     tabPanel(
    #       lowercaseTitle("Definiciones y Metadatos"),
    #       
    #     )
    #   )
    # ),
    
    #### Tab de la Administración de Vivienda Pública ####
    tabPanel(
      lowercaseTitle("Administración de Vivienda Pública"),
      icon = icon("house"),
      tabsetPanel(
        
        #### tab con datos de Adminsitración de Vivienda Públicas(dfAvp) ####
        tabPanel(
          lowercaseTitle("Viviendas Públicas Solicitadas y Asignadas"), 
          # Título del Tab
          titlePanel("Total de viviendas públicas solicitadas y asignadas por violencia doméstica por región de la Administración de Vivienda Pública"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Administración de Vivienda Pública"), tags$br(),
          #tags$span("Actualización Datos: ", actualizacion_vivienda), tags$br(),
          #tags$span(paste0("Actualización Sistema:   ", actualizacion_sistema)), 
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              # botón para seleccionar la región
              createDropdownCheckbox(
                label = "Seleccione Región de Vivienda:",
                choices = dfAvp$región,
                selected = dfAvp$región,
                id = "avp_dfAvp_región"
              ),
              
              # botón para seleccionar el año
              createDropdownCheckbox(
                label = "Seleccione Año(s):",
                choices = dfAvp$año,
                selected = dfAvp$año,
                id = "avp_dfAvp_año"
              ),
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              plotlyOutput("barPlot_avp_dfAvp"),
              DTOutput("dataTable_avp_dfAvp")
            )
          ),
        ),
        
        
        #### tab para el mapa de Adminsitración de Vivienda Públicas (mapaAvp) ####
        tabPanel(
          lowercaseTitle("Mapa de Viviendas Públicas Solicitadas y Asignadas"),
          
          # Título del Tab
          titlePanel("Total de viviendas públicas solicitadas y asignadas por violencia doméstica por región de la Administración de Vivienda Pública"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Administración de Vivienda Pública"),
          #tags$span("Actualización Datos:  ", actualizacion_vivienda), 
          #tags$span("Actualización Sistema:  ", actualizacion_sistema), 
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              
              # botón para seleccionar el año
              selectInput("select_avp_mapaAvp_año", "Seleccione Año:",
                          choices = levels(mapaAvp$año),
                          selected = 1),
              
              # createDropdownCheckbox(
              #   label = "Seleccionar Año:",
              #   choices = mapaDeli$Año,
              #   selected = NULL,
              #   id = "just_mapaDeli_año"
              # ),
              
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              plotlyOutput("map_avp_mapaAvp"),
              DTOutput("dataTable_avp_mapaAvp")
            )
          )
        ), 
        
        #### tab para el Definiciones y Metadatos ####
        tabPanel(
          lowercaseTitle("Definiciones y Metadatos"),
          
          # Título del Tab 
          tags$ul(style = "list-style-type: none; text-align: center; padding: 5px; padding-top: 15px;",
                  sectionTitle("Regiones de la Adminsitración de Vivienda Públicas", "24px")),
          fluidRow(
            column(12, uiOutput("definitionCards_avp"))
          ),
          
          tags$ul(style = "list-style-type: none; text-align: center;",
                  sectionTitle("Regiones de Vivienda Pública", "24px")),
          embedImage("RegionesVivienda", "RegionesVivienda.png", 
                     "https://www.avp.pr.gov/regiones-directorio.aspx",
                     "https://www.avp.pr.gov/regiones-directorio.aspx", size = "250"
          ),
          # Sección principal con los gráficos
          mainPanel(
            plotlyOutput("map_avp_mapaRegi")
          )
        )
      )
    ),
    
    #### Tab del Negociado de Policía de Puerto Rico ####
    tabPanel(
      lowercaseTitle("Negociado de Policía"),
      icon = icon("building-shield"),
      tabsetPanel(
        
        #### tab con datos de mujeres desaparecidas (despDF) ####
        tabPanel(
          lowercaseTitle("Mujeres desaparecidas y localizadas"), 
          # Título del Tab
          titlePanel("Cantidad de mujeres desaparecidas y localizadas - adultas y menores"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Negociado de Policía de Puerto Rico"), tags$br(),
          #tags$span("Actualización Datos:  ", actualizacion_policiaB), tags$br(),
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              
              # botón para seleccionar la Categoría
              createDropdownCheckbox(
                label = "Seleccione el estado de la víctima:",
                choices = despDF$Categoria,
                selected = despDF$Categoria,
                id = "poli_despDF_categoría"
              ),
              
              # botón para seleccionar el año
              createDropdownCheckbox(
                label = "Seleccione año(s):",
                choices = despDF$Año,
                selected = despDF$Año,
                id = "poli_despDF_año"
              ),
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              plotlyOutput("barPlot_poli_despDF"),
              DTOutput("dataTable_poli_despDF")
            )
          ),
        ),
        
        #### tab para el barplot de incidentes de violencia doméstica por edad de la víctima (vEdad) ####
        tabPanel(
          lowercaseTitle("Violencia Doméstica por Edad"),
          
          # Título del Tab
          titlePanel("Incidentes de violencia doméstica por edad de la víctima"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Negociado de Policía de Puerto Rico"), tags$br(),
          #tags$span("Actualización Datos:  ", actualizacion_policiaA), tags$br(),
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              
              # botón para seleccionar el año
              createDropdownCheckbox(
                label = "Seleccione Grupo(s) de Edad:",
                choices = vEdad$Edad,
                selected = vEdad$Edad,
                id = "poli_vEdad_edad"
              ),
              customSeparator(),
              # botón para seleccionar el sexo
              createDropdownCheckbox(
                label = "Seleccione sexo de las víctimas:",
                choices = vEdad$Sexo,
                selected = vEdad$Sexo[1],
                id = "poli_vEdad_sexo"
              ),
              customSeparator(),
              # botón para seleccionar el año
              createDropdownCheckbox(
                label = "Seleccione Año(s):",
                choices = vEdad$Año,
                selected = vEdad$Año,
                id = "poli_vEdad_año"
              ),
              
              # createDropdownCheckbox(
              #   label = "Seleccionar Año:",
              #   choices = mapaDeli$Año,
              #   selected = NULL,
              #   id = "just_mapaDeli_año"
              # ),
              
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              plotlyOutput("barPlot_poli_vEdad"),
              DTOutput("dataTable_poli_vEdad")
            )
          )
        ), 
        
        #### tab para el mapa de incidentes de violencia doméstica por área policiaca (inciMapa) ####
        # tabPanel(
        #   lowercaseTitle("inciMapa"),
        #   
        #   # Título del Tab
        #   titlePanel("Incidentes de violencia doméstica por área policíaca (desde enero de 2021 a abril de 2023)"),
        #   
        #   # Fuente de Datos, Actualización
        #   tags$span("Fuente: Negociado de Policía de Puerto Rico"), tags$br(),
        #   #tags$span("Actualización Datos:  ", actualizacion_policiaA), tags$br(),
        #   
        #   # Menu sidebar con widgets
        #   sidebarLayout(
        #     sidebarPanel(
        #       
        #       # botón para seleccionar año
        #       selectInput("select_poli_inciMapa_año", "Seleccione Año:",
        #                   choices = levels(inciMapa$Año),
        #                   selected = 1),
        #       
        #       # createDropdownCheckbox(
        #       #   label = "Seleccionar Año:",
        #       #   choices = mapaDeli$Año,
        #       #   selected = NULL,
        #       #   id = "just_mapaDeli_año"
        #       # ),
        #       
        #     ),
        #     
        #     # Sección principal con los gráficos
        #     mainPanel(
        #       plotlyOutput("map_poli_inciMapa")
        #       #DTOutput("dataTable_poli_inciMapa")
        #     )
        #   )
        # ), 
        
        #### tab de Definiciones y Metadatos ####
        tabPanel(
          lowercaseTitle("Definiciones y Metadatos"),
          br(),
          tags$ul(style = "list-style-type: none; text-align: center;",
                  sectionTitle("Negociado de Policía", "24px")),
          fluidRow(
            column(12, uiOutput("definitionCards_poli"))
          ),
          
          tags$ul(style = "list-style-type: none; text-align: center;",
                  sectionTitle("Regiones Policíacas", "24px")),
          embedImage("RegionesPoliciácas", "RegionesPoliciacas.png", 
                     "https://www.policia.pr.gov/",
                     "https://www.policia.pr.gov/", size = "250"
          )
        )
      )
    ),
    
    #### Tab de la Oficina de la Procuradora de las Mujeres ####
    tabPanel(
      lowercaseTitle("Oficina de la Procuradora de la Mujer"),
      icon = icon("person-dress"),
      tabsetPanel(
        
        #### tab con datos de violencia domestica (opmFemiVD) ####
        tabPanel(
          lowercaseTitle("Feminicidios por Violencia Doméstica"), 
          # Título del Tab
          titlePanel("Tasa de asesinatos de mujeres por violencia doméstica, desde 1990 a 2021 (Tasa x100,000 mujeres)"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Oficina de la Procuradora de las Mujeres"), tags$br(),
          #tags$span("Actualización Datos:  ", actualizacion_opmA), tags$br(),
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              
              # botón para seleccionar el año
              createDropdownCheckbox(
                label = "Seleccione Año(s):",
                choices = opmFemiVD$Año,
                selected = opmFemiVD$Año,
                id = "opm_opmFemiVD_año"
              ),
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              plotlyOutput("barPlot_opm_opmFemiVD"),
              DTOutput("dataTable_opm_opmFemiVD")
            )
          ),
        ),
        
        #### tab con datos de violencia domestica (opmCasos) ####
        tabPanel(
          lowercaseTitle("Casos Según Razón para Consulta"), 
          # Título del Tab
          titlePanel("Población atendida mediante el programa CRIAS según razón para consulta"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Oficina de la Procuradora de las Mujeres"), tags$br(),
          #tags$span("Actualización Datos:  ", actualizacion_opmA), tags$br(),
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              
              # botón para seleccionar el año
              createDropdownCheckbox(
                label = "Seleccione Año(s):",
                choices = opmCasos$year,
                selected = opmCasos$year,
                id = "opm_opmCasos_año"
              ),
              customSeparator(),
              # botón para seleccionar el tipo de violencia
              createDropdownCheckbox(
                label = "Seleccione Razón para Consulta:",
                choices = opmCasos$tipo,
                selected = opmCasos$tipo,
                id = "opm_opmCasos_tipo"
              ),
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              plotlyOutput("barPlot_opm_opmCasos"),
              DTOutput("dataTable_opm_opmCasos")
            )
          ),
        ),
        
        #### tab con datos del género de las víctimas (opmVic) ####
        tabPanel(
          lowercaseTitle("Género de las Víctimas Atendidas"), 
          # Título del Tab
          titlePanel("Identidad de género de víctimas asistidas por el programa CRIAS"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Oficina de la Procuradora de las Mujeres"), tags$br(),
          #tags$span("Actualización Datos:  ", actualizacion_opmB), tags$br(),
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              
              # botón para seleccionar el año
              createDropdownCheckbox(
                label = "Seleccione Año(s):",
                choices = opmVic$año,
                selected = opmVic$año,
                id = "opm_opmVic_año"
              ),
              customSeparator(),
              # botón para seleccionar el género de las victimas
              createDropdownCheckbox(
                label = "Seleccione Género(s):",
                choices = opmVic$género,
                selected = opmVic$género,
                id = "opm_opmVic_género"
              ),
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              plotlyOutput("barPlot_opm_opmVic"),
              DTOutput("dataTable_opm_opmVic")
            )
          ),
        ),
        
        #### tab con datos de orientaciones medio de comunicación (opmMedio) ####
        tabPanel(
          lowercaseTitle("Medio de Orientación a las Víctimas"), 
          # Título del Tab
          titlePanel("Orientaciones ofrecidas mediante el programa CRIAS"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Oficina de la Procuradora de las Mujeres"), tags$br(),
          #tags$span("Actualización Datos:  ", actualizacion_opmB), tags$br(),
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              
              # botón para seleccionar el año
              createDropdownCheckbox(
                label = "Seleccione Año(s):",
                choices = opmMedio$año,
                selected = opmMedio$año,
                id = "opm_opmMedio_año"
              ),
              customSeparator(),
              # botón para seleccionar el medio de orientación
              createDropdownCheckbox(
                label = "Seleccione el Medio de Orientación:",
                choices = opmMedio$`Medio de orientación`,
                selected = opmMedio$`Medio de orientación`,
                id = "opm_opmMedio_medio"
              ),
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              plotlyOutput("barPlot_opm_opmMedio"),
              DTOutput("dataTable_opm_opmMedio")
            )
          ),
        ),
        
        #### tab con datos de los servicios ofrecidos por mes (opmServiciosMes) ####
        tabPanel(
          lowercaseTitle("Servicios y Alcanze de la OPM"), 
          # Título del Tab
          titlePanel("Población atendida, servicios ofrecidos y seguimientos mediante el programa CRIAS"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Oficina de la Procuradora de las Mujeres"), tags$br(),
          #tags$span("Actualización Datos:  ", actualizacion_opmB), tags$br(),
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              
              # botón para seleccionar el año
              createDropdownCheckbox(
                label = "Seleccione Año(s):",
                choices = opmServiciosMes$year,
                selected = opmServiciosMes$year,
                id = "opm_opmServiciosMes_año"
              ),
              customSeparator(),
              # botón para seleccionar el género de las victimas
              createDropdownCheckbox(
                label = "Seleccione el tipo de servicio:",
                choices = opmServiciosMes$tipo,
                selected = opmServiciosMes$tipo,
                id = "opm_opmServiciosMes_tipo"
              ),
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              plotlyOutput("barPlot_opm_opmServiciosMes"),
              DTOutput("dataTable_opm_opmServiciosMes")
            )
          ),
        ),
        
        #### tab de Definiciones y Metadatos ####
        tabPanel(
          lowercaseTitle("Definiciones y Metadatos"),
          br(),
          tags$ul(style = "list-style-type: none; text-align: center;",
                  sectionTitle("Oficina de la Procuradora de la Mujer", "24px")),
          fluidRow(
            column(12, uiOutput("definitionCards_opm"))
          )
        )
      )
    ),
    
    
    
    #### Tab del Departamento de Corrección y Rehabilitación ####
    tabPanel(
      lowercaseTitle("Departamento de Corrección y Rehabilitación"),
      icon = icon("door-open"),
      tabsetPanel(
        #### tab con datos del género de las víctimas (dcrCasosInv) ####
        tabPanel(
        lowercaseTitle("Supervisión Ley 54: Programas Comunitarios"), 
        # Título del Tab
        titlePanel("Casos en supervisión de ley 54 en programas alternos al confinamiento por estado de investigación: Programas de Comunidad"),
      
        # Fuente de Datos, Actualización
        tags$span("Fuente: Departamento de Corrección y Rehabilitación"), tags$br(),
        #tags$span("Actualización Datos:  ", actualizacion_correcion), tags$br(),
      
        # Menu sidebar con widgets
        sidebarLayout(
          sidebarPanel(
          
            # botón para seleccionar el año
          createDropdownCheckbox(
            label = "Seleccione Año(s):",
            choices = dcrCasosInv$year,
            selected = dcrCasosInv$year,
            id = "dcr_dcrCasosInv_year"
          ),
          customSeparator(),
          # botón para seleccionar el sexo
          createDropdownCheckbox(
            label = "Seleccione el sexo:",
            choices = dcrCasosInv$sexo,
            selected = 1,
            id = "dcr_dcrCasosInv_sexo"
          ),
          customSeparator(),
          # botón para seleccionar el tipo de investigación 
          createDropdownCheckbox(
            label = "Seleccione el estado de Investigación:",
            choices = dcrCasosInv$tipo,
            selected = dcrCasosInv$tipo,
            id = "dcr_dcrCasosInv_tipo"
          ),
        ),
        
        # Sección principal con los gráficos
        mainPanel(
          plotlyOutput("barPlot_dcr_dcrCasosInv"),
          DTOutput("dataTable_dcr_dcrCasosInv")
        )
      ),
    ),
    
    #### tab con datos personas sentenciadas al Programa de Supervisión Electrónica (dcrSentenciadas) ####
    tabPanel(
      lowercaseTitle("Sentencias por Violencia Doméstica"),
      # Título del Tab
      titlePanel("Personas sentenciadas en programa de supervisión electrónica por delitos de violencia doméstica por estado del caso"),

      # Fuente de Datos, Actualización
      tags$span("Fuente: Departamento de Corrección y Rehabilitación"), tags$br(),
      #tags$span("Actualización Datos:  ", actualizacion_correcion), tags$br(),

      # Menu sidebar con widgets
      sidebarLayout(
        sidebarPanel(

          # botón para seleccionar el año
          createDropdownCheckbox(
            label = "Seleccione Año(s):",
            choices = dcrSentenciadas$year,
            selected = dcrSentenciadas$year,
            id = "dcr_dcrSentenciadas_year"
          ),
          customSeparator(),
          # botón para seleccionar el tipo de investigación
          createDropdownCheckbox(
            label = "Seleccione el estado de Caso:",
            choices = dcrSentenciadas$tipo,
            selected = dcrSentenciadas$tipo,
            id = "dcr_dcrSentenciadas_tipo"
          ),
          customSeparator(),
          # # botón para seleccionar el sexo
          # createDropdownCheckbox(
          #   label = "Seleccione el sexo:",
          #   choices = dcrSentenciadas,
          #   selected = dcrCasosInv$sexo,
          #   id = "dcr_dcrCasosInv_sexo"
          # ),
        ),

        # Sección principal con los gráficos
        mainPanel(
          plotlyOutput("barPlot_dcr_dcrSentenciadas"),
          DTOutput("dataTable_dcr_dcrSentenciadas")
        )
      )
    ),
    #### tab de Definiciones y Metadatos ####
    tabPanel(
      lowercaseTitle("Definiciones y Metadatos"),
      br(),
      tags$ul(style = "list-style-type: none; text-align: center;",
              sectionTitle("Departamento de Corrección y Rehabilitación", "24px")),
      fluidRow(
        column(12, uiOutput("definitionCards_dcr"))
          )
        )
      )
    ),
  
  #### Tab de la Administración de Tribunales ####
  tabPanel(
    lowercaseTitle("Administración de Tribunales"),
    icon = icon("building-circle-exclamation"),
    tabsetPanel(
      
      #### tab con datos de ley 148 - Violencia Sexual por grupo de edad (OP_148_SoliGrupEdad) ####
      tabPanel(
        lowercaseTitle("Órdenes de Protección Solicitadas por Edad y Región"), 
        # Título del Tab
        titlePanel("Órdenes de Protección Solicitadas por Violencia Sexual bajo Ley 148, según Grupo de Edad, Región Judicial y año fiscal"),
        
        # Fuente de Datos, Actualización
        tags$span("Fuente: Oficina de Administración de los Tribunales, Directoría de Operaciones, Oficina de Estadísticas"), tags$br(),
        #tags$span("Actualización Datos:  ", actualizacion_tribunalesB), tags$br(),
        
        # Menu sidebar con widgets
        sidebarLayout(
          sidebarPanel(
            
            # botón para seleccionar el año
            createDropdownCheckbox(
              label = "Seleccione Año(s) Fiscal:",
              choices = OP_148_SoliGrupEdad$AñoFiscal,
              selected = OP_148_SoliGrupEdad$AñoFiscal,
              id = "trib_OP_148_SoliGrupEdad_AñoFiscal"
            ),
            customSeparator(),
            # botón para seleccionar el grupo de edad
            createDropdownCheckbox(
              label = "Seleccione grupo(s) de edad:",
              choices = OP_148_SoliGrupEdad$Edad,
              selected = OP_148_SoliGrupEdad$Edad,
              id = "trib_OP_148_SoliGrupEdad_Edad"
            ),
            customSeparator(),
            # botón para seleccionar el distrito fiscal
            createDropdownCheckbox(
              label = "Seleccione Región Judicial:",
              choices = OP_148_SoliGrupEdad$Región,
              selected = OP_148_SoliGrupEdad$Región,
              id = "trib_OP_148_SoliGrupEdad_Región"
            ),
          ),
          
          # Sección principal con los gráficos
          mainPanel(
            plotlyOutput("barPlot_OP_148_SoliGrupEdad"),
            DTOutput("dataTable_OP_148_SoliGrupEdad")
          )
        ),
      ),
      
      #### tab con datos de ley 148 - Violencia Sexual por grupo de edad (OP_Ley148_ex_parteEmitidas) ####
      tabPanel(
        lowercaseTitle("Órdenes de Protección Ex Parte Emitidas por Delito Cometido y Región"), 
        # Título del Tab
        titlePanel("Órdenes de Protección Ex Parte Emitidas bajo Ley 148 según Región Judicial, Delito Cometido y año fiscal"),
        
        # Fuente de Datos, Actualización
        tags$span("Fuente: Oficina de Administración de los Tribunales, Directoría de Operaciones, Oficina de Estadísticas"), tags$br(),
        #tags$span("Actualización Datos:  ", actualizacion_tribunalesB), tags$br(),
        
        # Menu sidebar con widgets
        sidebarLayout(
          sidebarPanel(
            
            # botón para seleccionar el año fiscal
            createDropdownCheckbox(
              label = "Seleccione Año(s) Fiscal:",
              choices = OP_Ley148_ex_parteEmitidas$AñoFiscal,
              selected = OP_Ley148_ex_parteEmitidas$AñoFiscal,
              id = "trib_OP_Ley148_ex_parteEmitidas_AñoFiscal"
            ),
            customSeparator(),
            # botón para seleccionar el delito
            createDropdownCheckbox(
              label = "Seleccione Delito(s) cometido:",
              choices = OP_Ley148_ex_parteEmitidas$Delito,
              selected = OP_Ley148_ex_parteEmitidas$Delito,
              id = "trib_OP_Ley148_ex_parteEmitidas_Delito"
            ),
            customSeparator(),
            # botón para seleccionar la región fiscal
            createDropdownCheckbox(
              label = "Seleccione Región Judicial:",
              choices = OP_Ley148_ex_parteEmitidas$Región,
              selected = OP_Ley148_ex_parteEmitidas$Región,
              id = "trib_OP_Ley148_ex_parteEmitidas_Región"
            ),
            
          ),
          
          # Sección principal con los gráficos
          mainPanel(
            plotlyOutput("barPlot_OP_Ley148_ex_parteEmitidas"),
            DTOutput("dataTable_OP_Ley148_ex_parteEmitidas")
          )
        ),
      ),
      
      #### tab con datos de solicitudes de órdenes de protección archivadas (OP_LEY148Archivadas) ####
      tabPanel(
        lowercaseTitle("Órdenes de protección ex parte archivadas por razón y región"), 
        
        # Título del Tab
        titlePanel("Órdenes de protección ex parte bajo Ley 148 archivadas por razón, Región Judicial y año fiscal"),
        
        # Fuente de Datos, Actualización
        tags$span("Fuente: Oficina de Administración de los Tribunales, Directoría de Operaciones, Oficina de Estadísticas"), tags$br(),
        #tags$span("Actualización Datos:  ", actualizacion_tribunalesB), tags$br(),
        
        # Menu sidebar con widgets
        sidebarLayout(
          sidebarPanel(
            
            # botón para seleccionar el año fiscal
            createDropdownCheckbox(
              label = "Seleccione Año(s) Fiscal:",
              choices = OP_LEY148Archivadas$AñoFiscal,
              selected = OP_LEY148Archivadas$AñoFiscal,
              id = "trib_OP_LEY148Archivadas_AñoFiscal"
            ),
            customSeparator(),
            # botón para seleccionar la razón de archivado
            createDropdownCheckbox(
              label = "Seleccione Razón(s):",
              choices = OP_LEY148Archivadas$Razón,
              selected = OP_LEY148Archivadas$Razón,
              id = "trib_OP_LEY148Archivadas_Razón"
            ),
            customSeparator(),
            # botón para seleccionar la región fiscal
            createDropdownCheckbox(
              label = "Seleccione Distrito(s) Fiscal:",
              choices = OP_LEY148Archivadas$Región,
              selected = OP_LEY148Archivadas$Región,
              id = "trib_OP_LEY148Archivadas_Región"
            ),
            
          ),
          
          # Sección principal con los gráficos
          mainPanel(
            plotlyOutput("barPlot_OP_LEY148Archivadas"),
            DTOutput("dataTable_OP_LEY148Archivadas")
          )
        ),
      ),
      
      #### tab con datos de solicitudes de órdenes de protección denegadas (OP_LEY148Denegadas) ####
      tabPanel(
        lowercaseTitle("Órdenes de protección denegadas por razón del archivo y región"), 
        # Órdenes de Protección Ex Parte Archivadas por Razón del Archivo y Región
        # Título del Tab
        titlePanel("Ordenes de protección denegadas por violencia sexual bajo Ley 148 por razón de archivo, Región Judicial y año fiscal"),

        # Fuente de Datos, Actualización
        tags$span("Fuente: Oficina de Administración de los Tribunales, Directoría de Operaciones, Oficina de Estadísticas"), tags$br(),
        #tags$span("Actualización Datos:  ", actualizacion_tribunalesB), tags$br(),
        
        # Menu sidebar con widgets
        sidebarLayout(
          sidebarPanel(
            
            # botón para seleccionar el año fiscal
            createDropdownCheckbox(
              label = "Seleccione Año(s) fiscal:",
              choices = OP_LEY148Denegadas$AñoFiscal,
              selected = OP_LEY148Denegadas$AñoFiscal,
              id = "trib_OP_LEY148Denegadas_AñoFiscal"
            ),
            customSeparator(),
            # botón para seleccionar la razón de archivado
            createDropdownCheckbox(
              label = "Seleccione razón(s):",
              choices = OP_LEY148Denegadas$Razón,
              selected = OP_LEY148Denegadas$Razón,
              id = "trib_OP_LEY148Denegadas_Razón"
            ),
            customSeparator(),
            # botón para seleccionar la región fiscal
            createDropdownCheckbox(
              label = "Seleccione región judicial:",
              choices = OP_LEY148Denegadas$Región,
              selected = OP_LEY148Denegadas$Región,
              id = "trib_OP_LEY148Denegadas_Región"
            ),
          ),
          
          # Sección principal con los gráficos
          mainPanel(
            plotlyOutput("barPlot_OP_LEY148Denegadas"),
            DTOutput("dataTable_OP_LEY148Denegadas")
          )
        ),
      ),
      
      #### tab con datos de solicitudes de órdenes de protección finales emitidas (OP_LEY148FinalEmitidas) ####
      tabPanel(
        lowercaseTitle("Órdenes de Protección Emitidas por Delito Cometido y Región"), 
        # Título del Tab
        titlePanel("Órdenes de protección emitidas bajo ley 148 por delito cometido, Región Judicial y año fiscal"),
        
        # Fuente de Datos, Actualización
        tags$span("Fuente: Oficina de Administración de los Tribunales, Directoría de Operaciones, Oficina de Estadísticas"), tags$br(),
        #tags$span("Actualización Datos:  ", actualizacion_tribunalesB), tags$br(),
        
        # Menu sidebar con widgets
        sidebarLayout(
          sidebarPanel(
            
            # botón para seleccionar el año fiscal
            createDropdownCheckbox(
              label = "Seleccione Año(s) Fiscal:",
              choices = OP_LEY148FinalEmitidas$AñoFiscal,
              selected = OP_LEY148FinalEmitidas$AñoFiscal,
              id = "trib_OP_LEY148FinalEmitidas_AñoFiscal"
            ),
            customSeparator(),
            # botón para seleccionar la razón de archivado
            createDropdownCheckbox(
              label = "Seleccione Delito(s) Cometido:",
              choices = OP_LEY148FinalEmitidas$Delito,
              selected = OP_LEY148FinalEmitidas$Delito,
              id = "trib_OP_LEY148FinalEmitidas_Delito"
            ),
            customSeparator(),
            # botón para seleccionar la región fiscal
            createDropdownCheckbox(
              label = "Seleccione Región Judicial:",
              choices = OP_LEY148FinalEmitidas$Región,
              selected = OP_LEY148FinalEmitidas$Región,
              id = "trib_OP_LEY148FinalEmitidas_Región"
            ),
          ),
          
          # Sección principal con los gráficos
          mainPanel(
            plotlyOutput("barPlot_OP_LEY148FinalEmitidas"),
            DTOutput("dataTable_OP_LEY148FinalEmitidas")
          )
        ),
      ),
      
      #### tab con datos de solicitudes de órdenes de protección finales emitidas (OP_LEY148Genero) ####
      tabPanel(
        lowercaseTitle("Órdenes de Protección Emitidas por Parte y Sexo"), 
        
        # Título del Tab
        titlePanel("Órdenes de Protección Emitidas bajo Ley 148, por Sexo, la Parte y año fiscal"),
        
        # Fuente de Datos, Actualización
        tags$span("Fuente: Oficina de Administración de los Tribunales, Directoría de Operaciones, Oficina de Estadísticas"), tags$br(),
        #tags$span("Actualización Datos:  ", actualizacion_tribunalesB), tags$br(),
        
        # Menu sidebar con widgets
        sidebarLayout(
          sidebarPanel(
            
            # botón para seleccionar el año fiscal
            createDropdownCheckbox(
              label = "Seleccione Año(s) Fiscal:",
              choices = OP_LEY148Genero$AñoFiscal,
              selected = OP_LEY148Genero$AñoFiscal,
              id = "trib_OP_LEY148Genero_AñoFiscal"
            ),
            customSeparator(),
            # botón para seleccionar la parte
            createDropdownCheckbox(
              label = "Seleccione Parte(s):",
              choices = OP_LEY148Genero$Parte,
              selected = OP_LEY148Genero$Parte,
              id = "trib_OP_LEY148Genero_Parte"
            ),
            customSeparator(),
            # botón para seleccionar el sexo de la parte
            createDropdownCheckbox(
              label = "Seleccione Sexo:",
              choices = OP_LEY148Genero$Sexo,
              selected = OP_LEY148Genero$Sexo,
              id = "trib_OP_LEY148Genero_Sexo"
            ),
          ),
          
          # Sección principal con los gráficos
          mainPanel(
            plotlyOutput("barPlot_OP_LEY148Genero"),
            DTOutput("dataTable_OP_LEY148Genero")
          )
        ),
      ),
      
      
      #### tab con datos de Movimiento de Casos Criminales de Violencia Doméstica (tribCasosCrim) ####
      tabPanel(
        lowercaseTitle("Movimiento de Casos en Tribunal de Primera Instancia por Ley 54"), 
        
        # Título del Tab
        titlePanel("Movimiento de casos criminales de violencia doméstica en el tribunal de primera instancia según la ley Núm. 54 por delito cometido y año fiscal"),
        
        # Fuente de Datos, Actualización
        tags$span("Fuente: Oficina de Administración de los Tribunales, Directoría de Operaciones, Oficina de Estadísticas"), tags$br(),
        #tags$span("Actualización Datos:  ", actualizacion_tribunalesB), tags$br(),
        
        # Menu sidebar con widgets
        sidebarLayout(
          sidebarPanel(
            
            # botón para seleccionar el año fiscal
            createDropdownCheckbox(
              label = "Seleccione Año(s) Fiscal:",
              choices = tribCasosCrim$AñoFiscal,
              selected = tribCasosCrim$AñoFiscal,
              id = "trib_tribCasosCrim_AñoFiscal"
            ),
            customSeparator(),
            # botón para seleccionar el Delito
            createDropdownCheckbox(
              label = "Seleccione Delito(s):",
              choices = tribCasosCrim$Delito,
              selected = tribCasosCrim$Delito,
              id = "trib_tribCasosCrim_Delito"
            ),
            customSeparator(),
            # botón para seleccionar el estado del caso
            createDropdownCheckbox(
              label = "Seleccione Estado del Caso:",
              choices = tribCasosCrim$Casos,
              selected = 1,
              id = "trib_tribCasosCrim_Casos"
            ),
          ),
          
          # Sección principal con los gráficos
          mainPanel(
            plotlyOutput("barPlot_tribCasosCrim"),
            DTOutput("dataTable_tribCasosCrim")
          )
        ),
      ),
      
      #### tab de Definiciones y Metadatos ####
      tabPanel(
        lowercaseTitle("Definiciones y Metadatos"),
        br(),
        tags$ul(style = "list-style-type: none; text-align: center;",
                sectionTitle("Administración de Tribunales", "24px")),
        fluidRow(
          column(12, uiOutput("definitionCards_trib"))
        )
      )
    )
  ),
  #### Tab del Centro de Ayuda a Víctimas de Violación ####
  tabPanel(
    lowercaseTitle("Centro Ayuda a Víctimas de Violación"),
    icon = icon("building-circle-exclamation"),
    tabsetPanel(
      
      tabPanel(
        lowercaseTitle("Tendencia Anual del Equipo Recolecta de Violencia Sexual"), 
        # Título del Tab
        titlePanel(HTML("Tendencia anual del equipo de recolecta de evidencia de <i>SAFE Kits</i> en casos de violencia sexual por estado de querella")),
        
        # Fuente de Datos, Actualización
        tags$span("Fuente: Centro de Ayuda a Victimas de Violación, Departamento de Salud"), tags$br(),
        #tags$span("Actualización Datos:  ", actualizacion_caav), tags$br(),
        
        # Menu sidebar con widgets
        sidebarLayout(
          sidebarPanel(
            
            # botón para seleccionar el año
            createDropdownCheckbox(
              label = "Seleccione Año(s):",
              choices = safekitsDF$Año,
              selected = safekitsDF$Año,
              id = "cavv_safekitsDF_Año"
            ),
            customSeparator(),
            # botón para seleccionar el estado de querella
            createDropdownCheckbox(
              label = "Seleccione Estado de Querella:",
              choices = safekitsDF$Kits,
              selected = safekitsDF$Kits,
              id = "cavv_safekitsDF_Kits"
            )
          ),
          
          # Sección principal con los gráficos
          mainPanel(
            plotlyOutput("barPlot_safekitsDF"),
            DTOutput("dataTable_safekitsDF")
          )
        )
      ),
      
      #### tab de Definiciones y Metadatos ####
      tabPanel(
        lowercaseTitle("Definiciones y Metadatos"),
        br(),
        tags$ul(style = "list-style-type: none; text-align: center;",
                sectionTitle("Centro de Ayuda a Víctimas de Violación", "24px")),
        fluidRow(
          column(12, uiOutput("definitionCards_cavv"))
        )
      )
    )    
  ),
      
  
    #### Tab Acerca del Dashboard ####
    tabPanel(
      lowercaseTitle("Acerca del Dashboard"),
      icon = icon("info-circle"),
      tabsetPanel(
        
        # # tab del transfondo del proyecto
        # tabPanel(
        #   lowercaseTitle("Transfondo del Proyecto")
        #   ),
        # 
        # # tab de las fuentes usadas en el proyecto
        # tabPanel(
        #   lowercaseTitle("Fuentes")
        #   ), 
        
        # tab de los autores del proyecto
        tabPanel(
          lowercaseTitle("Autores"),
          div(
            authorTag(
              nombre = 'Félix A. Báez Santiago',
              email = 'felix.baez@estadisticas.pr',
              puesto = 'Programador Estadístico',
              grados = 'BS Ciencia de Datos, Iowa State University'
            ),
            
            authorTag(
              nombre = 'Manuel Mangual Martínez',
              email = 'manuel.mangual@estadisticas.pr',
              puesto = 'Supervisor del Proyecto Prevención, Apoyo, Rescate y Educación de la Violencia de Género',
              grados = c('MS Investigación y Evaluación de la Salud, Universidad de Puerto Rico',
                         'BS Biología, Universidad de Puerto Rico')
            ),
            
            authorTag(
              nombre = 'Frankie Rodríguez Rivera',
              email = 'frankie.rodriguez@estadisticas.pr',
              puesto = 'Analista de Datos',
              grados = c('MS Administración de Agencias Públicas, Universidad del Turabo',
                         'BS Trabajo Social, Universidad Interamericana')
            ),
            
            authorTag(
              nombre = 'Mariluz Bezares Salinas',
              email = 'mariluz.bezares@estadisticas.pr',
              puesto = 'Gerente de Proyectos Estadísticos',
              grados = c('MS Ciencias en Demografía, Universidad de Puerto Rico',
                         'BA Ciencias Sociales - Geografía, Universidad de Puerto Rico')
            ),
            
            authorTag(
              nombre = 'Raúl Figueroa Rodríguez',
              email = 'raul.figueroa@estadisticas.pr',
              puesto = 'Gerente de Proyectos Estadísticos NVDRS',
              grados = c('MS Demografía, Universidad de Puerto Rico',
                         'BS Ciencias Naturales, Universidad de Puerto Rico')
            ),
            
            authorTag(
              nombre = 'Dr. Orville M. Disdier Flores',
              email = 'orville.disdier@estadisticas.pr',
              puesto = 'Director Ejecutivo',
              grados = c(
                'Ed.D Liderazgo Educativo, Universidad del Turabo',
                'MS Epidemiología, Universidad de Puerto Rico',
                'BS Ciencias Naturales, Universidad de Puerto Rico'
              )
            ), 
          )
          ),

        
        # tab de la información de contacto del Proyecto
        tabPanel(
          lowercaseTitle("Contacto"),
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
# )