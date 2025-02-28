# User Interface 
cat("Loading User Interface from ui.R...\n")
source("global.R")
ui <- 
  fluidPage(
  ### El theme (colores) de la app ###
  theme = shinytheme("sandstone"),
  
  #### HTML ####
  tags$style(HTML("
                    #scrollable-plot {
                    overflow-y: scroll;
                    height: 500px;
                    }
                    #plot-title {
                    family: Arial; 
                    color: black;
                    text-align: center;
                    font-size: 16px;
                    margin-bottom: 1px;
                    }
                  ")),
  
  
  #### Encabezado ####
  div(
    tags$div(
      tags$title('Instituto de Estadísticas de Puerto Rico'),
      
      tags$ul(
        
        ### Foto con enlace a la página de IEPR ###
        embedImage("logo_IEPR", "iepr_logo.png", "https://estadisticas.pr/", "estadisticas.pr"),
        
        ### Foto con enlace a la página de PARE ###
        # embedImage("logo_PARE", "logo_PARE.png", "https://parelaviolencia.pr.gov/", "PARE.gov"),
        # style = 'padding-top: 4px;',
        
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
      justify-content: space-between;
      background: #8dc143;
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
          lowercaseTitle("Homicidios de Mujeres por Edad"),
          br(), br(),
          
          # Menu sidebar con widgets
            sidebarLayout(
              sidebarPanel(
                style = "display: flex; flex-direction: column; align-items: center;",
                
                # Seleccionar valor de la variable
                div(
                  style = "width: 100%; display: flex; justify-content: center; margin-bottom: 20px;", 
                  div(
                    style = "text-align: center; display: inline-block;", 
                    createDropdownCheckbox(
                      label = HTML("Seleccione<br>Grupo(s) de Edad:"),
                      choices = homiEdad$Edad,
                      selected = homiEdad$Edad,
                      id = "snmv_homiEdad_edad"
                    )
                  )
                ),
                
                div(
                  style = "width: 100%; display: flex; flex-direction: column; align-items: center; padding-top: 0px;",
                  div(
                    style = "width: 100%; display: flex; justify-content: space-between; align-items: center;", 
                   
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-right: 10px;", 
                      createDropdownCheckbox(
                        label = "Seleccione Año(s):",
                        choices = homiEdad$Año,
                        selected = NULL,
                        id = "snmv_homiEdad_año"
                      )
                    ),
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-left: 0px;", 
                      showDataCheckbox("showTable_snmv")
                    )
                  )
                ),
                
                # Output UI para la tabla de datos
                uiOutput("dataTableUI_snmv")
              ),
              
            
            # Sección principal con los gráficos
            mainPanel(
              style = "height: calc(100vh - 150px); padding-bottom: 10px;",
              fluidRow(
                column(12, 
                       div(id = "scrollable-plot", 
                           div(id = "plot-title", uiOutput("plot_title13")),
                           plotlyOutput("barPlot_snmv"),  height = "100%"))
              ),
              tags$div(style = "padding-bottom: 10px;"),
              tags$div(
                style = "padding-bottom: 10px;",
                div(
                  class = "card",
                  style = "padding: 15px;color: white; background-color: #3e3f3a; border-radius: 5px; margin-top: 0; width: 100%;",
                  h4(
                    strong(actualizacion_snmv1, style="margin: 0px;") 
                  ),
                  p(
                    "Los datos representados en esta gráfica corresponden a los 
                    homicidios de mujeres por grupo de edad desde el año natural 2017 al 2022.",
                    
                    style = "font-size: 16px; padding: 0px;" 
                  )
                )
              )
            )
          )
        ),
        
        #### tab con datos de Incidentes segun el año ####
        tabPanel(
          lowercaseTitle("Tipos de Incidentes Violentos"),
          br(), br(),
          
            sidebarLayout(
              sidebarPanel(
                style = "display: flex; flex-direction: column; align-items: center;",
                
                # # seleccionar valor de la variable
                div(
                  style = "width: 100%; display: flex; justify-content: center;  margin-bottom: 20px;", 
                  div(
                    style = "text-align: center; display: inline-block;",  
                    # botón para seleccionar tipo de incidente
                    createDropdownCheckbox(
                      label = HTML("Seleccione<br>Tipo(s) de Incidente:"),
                      choices = inci$Incidente,
                      selected = 7,
                      id = "snmv_inci_tipo"
                    )
                  )
                ),
                
                div(
                  style = "width: 100%; display: flex; flex-direction: column; align-items: center; padding-top: 0px;",
                  div(
                    style = "width: 100%; display: flex; justify-content: center; align-items: center;",
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-right: 10px;", 
                      # botón para seleccionar año
                      createDropdownCheckbox(
                        label = "Seleccione Año(s):",
                        choices = inci$Año,
                        selected = NULL,
                        id = "snmv_inci_año"
                      )
                    ),
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-left: 0px;", 
                      showDataCheckbox("showTable_snmv_inci")
                    )
                  )
                ),
                
                
                # Output UI para la tabla de datos
                uiOutput("dataTableUI_snmv_inci")
                
              ),
            
            
            # Sección principal con los gráficos y datatable
            mainPanel(
              style = "height: calc(100vh - 150px); padding-bottom: 10px;",
              fluidRow(
                column(12, 
                       div(id = "scrollable-plot", 
                           div(id = "plot-title", uiOutput("plot_title14")),
                           plotlyOutput("barPlot_snmv_inci"),  height = "100%"))
              ),
              tags$div(style = "padding-bottom: 10px;"),
              tags$div(
                style = "padding-bottom: 10px;",
                div(
                  class = "card",
                  style = "padding: 15px;color: white; background-color: #3e3f3a; border-radius: 5px; margin-top: 0; width: 100%;",
                  h4(
                    strong(actualizacion_snmv2, style="margin: 0px;") 
                  ),
                  p(
                    "Los datos representados en esta gráfica corresponden a la cantidad de 
                    incidentes violentos (según su tipo) desde el año natural 2017 al 2022.",
                    
                    style = "font-size: 16px; padding: 0px;" 
                  )
                )
              )
            )
          ),
        ), 
        
     
        #### tab de Definiciones y Metadatos ####
        tabPanel(
          lowercaseTitle("Definiciones y Metadatos"),
          br(),
          tags$ul(style = "list-style-type: none; text-align: center;",
                  sectionTitle("Sistema de Notificación de Muertes Violentas", "24px")),
          fluidRow(
            column(12, DTOutput("dataTable_Def_snmv"))
          )
        )
      )
    ),
    
    
    
    
    #### Tab del Departamento de la Familia ####
    tabPanel(
      lowercaseTitle("Departamento de la Familia"),
      icon = icon("users"),
      tabsetPanel(
        #### tab con datos de menores víctimas de maltrato (dfMalt)  ####
        tabPanel(
          lowercaseTitle("Maltrato de Menores por Sexo"),
          br(), br(),
          
          sidebarLayout(
            sidebarPanel(
              style = "display: flex; flex-direction: column; align-items: center;",
              
              # # seleccionar valor de la variable
              div(
                style = "width: 100%; display: flex; justify-content: center; margin-bottom: 20px;", 
                div(
                  style = "text-align: center; display: inline-block;",  
                  # botón para seleccionar tipo de maltrato
                  createDropdownCheckbox(
                    label = HTML("Seleccione <br>Tipo(s) de Maltrato:"),
                    choices = dfMalt$Maltrato,
                    selected = 1,
                    id = "fam_dfMalt_tipo"
                  ),
                  createDropdownCheckbox(
                    label = HTML("Seleccione <br> Sexo de las Víctimas:"),
                    choices = dfMalt$Sexo,
                    selected = 1,
                    id = "fam_dfMalt_sexo"
                  )
                )
              ),
              
              div(
                style = "width: 100%; display: flex; flex-direction: column; align-items: center; padding-top: 0px;",
                div(
                  style = "width: 100%; display: flex; justify-content: center; align-items: center;",
                  div(
                    style = "flex: 1; display: flex; justify-content: center; margin-right: 10px",
                    # botón para seleccionar año
                    createDropdownCheckbox(
                      label = "Seleccionar Año:",
                      choices = dfMalt$Año,
                      selected = NULL,
                      id = "fam_dfMalt_año"
                    )
                  ),
                  div(
                    style = "flex: 1; display: flex; justify-content: center; margin-left: 0px;", 
                    showDataCheckbox("showTable_fam")
                  )
                )
              ),
              
              
              # Output UI para la tabla de datos
              uiOutput("dataTableUI_fam")
              
            ),
            
            
            # Sección principal con los gráficos
            mainPanel(
              style = "height: calc(100vh - 150px); padding-bottom: 10px;",
              fluidRow(
                column(12, 
                       div(id = "scrollable-plot", 
                           div(id = "plot-title", uiOutput("plot_title1")),
                           plotlyOutput("barPlot_fam"),  height = "100%"))
              ),
              tags$div(style = "padding-bottom: 10px;"),
              tags$div(
                style = "padding-bottom: 10px;",
                div(
                  class = "card",
                  style = "padding: 15px;color: white; background-color: #3e3f3a; border-radius: 5px; margin-top: 0; width: 100%;",
                  h4(
                    strong(actualizacion_familia, style="margin: 0px;") 
                  ),
                  p(
                    "Las cantidades podrían representar conteos duplicados del o 
                    la menor. Esto es, debido a que el menor se cuenta cada vez que él o ella son 
                    parte de uno o múltiples referidos. Este conteo es conocido por el Departamento 
                    de la Familia como el pareo de menores-reportado. Estos datos están en proceso de
                    revisión por el Instituto de Estadísticas de Puerto Rico. Datos parciales
                    y preliminares del año 2022. Están disponibles hasta noviembre de 2022.",
                    
                    style = "font-size: 16px;padding: 0px;" 
                  )
                )
              )
            )
          )
        ),
        
        #### tab de Definiciones y Metadatos ####
        tabPanel(
          lowercaseTitle("Definiciones y Metadatos"),
          br(),
          tags$ul(style = "list-style-type: none; text-align: center;",
                  sectionTitle("Departamento de la Familia", "24px")),
          fluidRow(
            column(12, DTOutput("dataTable_Def_fam"))
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
          br(), br(), 
        
            sidebarLayout(
              sidebarPanel(
                style = "display: flex; flex-direction: column; align-items: center;",
                
                # # seleccionar valor de la variable
                div(
                  style = "width: 100%; display: flex; justify-content: center; margin-bottom: 20px;", 
                  div(
                    style = "text-align: center; display: inline-block;",  
                    # botón para seleccionar tipo de maltrato
                    createDropdownCheckbox(
                      label = HTML("Seleccione<br>Artículo(s) de ley 54:"),
                      choices = dfDeli$Delito,
                      selected = 2,
                      id = "just_dfDeli_delito"
                    ),
                    createDropdownCheckbox(
                      label = HTML("Seleccionar<br>Distrito(s):"),
                      choices = dfDeli$Distrito,
                      selected = NULL,
                      id = "just_dfDeli_distrito"
                    )
                  )
                ),
                
                div(
                  style = "width: 100%; display: flex; flex-direction: column; align-items: center; padding-top: 0px;",
                  div(
                    style = "width: 100%; display: flex; justify-content: center; align-items: center;",
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-right: 10px;",
                      # botón para seleccionar año
                      createDropdownCheckbox(
                        label = "Seleccionar Año:",
                        choices = dfDeli$Año,
                        selected = NULL,
                        id = "just_dfDeli_año"
                      )
                    ),
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-left: 0px;", 
                      showDataCheckbox("showTable_just")
                    )
                  )
                ),
                
                
                # Output UI para la tabla de datos
                uiOutput("dataTableUI_just")
                
              ),
            
            
            # Sección principal con los gráficos
            mainPanel(
              style = "height: calc(100vh - 150px); padding-bottom: 10px;",
              fluidRow(
                column(12, 
                       div(id = "scrollable-plot", 
                           div(id = "plot-title", uiOutput("plot_title2")),
                           plotlyOutput("barPlot_just"),  height = "100%"))
              ),
              tags$div(style = "padding-bottom: 10px;"),
              tags$div(
                style = "padding-bottom: 10px;",
                div(
                  class = "card",
                  style = "padding: 15px;color: white; background-color: #3e3f3a; border-radius: 5px; margin-top: 0; width: 100%;",
                  h4(
                    strong(actualizacion_justicia1, style="margin: 0px;") 
                  ),
                  p(
                    "Los datos representados en esta gráfica corresponden al
                    número de casos radicados por distrito fiscal y Artículo 
                    de la Ley 54 desde el año natural 2020 al 2023.",
                    
                    style = "font-size: 16px;padding: 0px;" 
                  )
                )
              )
            )
          )
        ),
        
        
        #### tab con datos del mapa delitos de violencia doméstica (mapaDeli) ####
        tabPanel(
          lowercaseTitle("Mapas de delitos Ley 54 por Distrito"),
          br(), br(),
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              # botón para seleccionar delito
              selectInput("select_just_mapaDeli_delito", "Seleccione Artículo de la Ley 54:",
                          choices = levels(mapaDeli$Delito),
                          selected = 2),
              
              # botón para seleccionar año
              selectInput("select_just_mapaDeli_año", "Seleccione Año:",
                          choices = levels(mapaDeli$Año),
                          selected = 1),
              
              # Output UI para la tabla de datos
              uiOutput("dataTableUI_just_mapaFisc")
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              fluidRow(
                column(12, 
                       div(id = "scrollable-plot", 
                           div(id = "plot-title", uiOutput("plot_title23")),
                           leafletOutput("map_just_mapaDeli", height = "450px"),  height = "100%"))
              ),
              #plotlyOutput("map_just_mapaDeli"),
              tags$div(style = "padding-bottom: 10px;"),
              tags$div(
                style = "padding-bottom: 10px;",
                div(
                  class = "card",
                  style = "padding: 15px;color: white; background-color: #3e3f3a; border-radius: 5px; margin-top: 0; width: 100%;",
                  h4(
                    strong(actualizacion_justicia2, style="margin: 0px;") 
                  ),
                  p(
                    "Los datos representados en este mapa corresponden al
                    número de casos radicados por distrito fiscal y Artículo 
                    de la Ley 54 desde el año natural 2020 al 2023.",
                    
                    style = "font-size: 16px;padding: 0px;" 
                  )
                )
              )
            )
          )
        ), 
        
        #### tab de Definiciones y Metadatos ####
        tabPanel(
          lowercaseTitle("Definiciones y Metadatos"),
          br(),
          tags$ul(style = "list-style-type: none; text-align: center;",
                  sectionTitle("Artículos de la Ley 54", "24px")),
          fluidRow(
            column(12, DTOutput("dataTable_Def_just"))
          ),
          
          tags$div(
            style = "display: flex; justify-content: center; align-items: center; text-align: center;",
            tags$ul(
              style = "list-style-type: none; padding: 0; margin: 0;",
              sectionTitle("Fiscalías de Puerto Rico", "24px"),
              br(),
              embedImage(
                "Fiscalias_PR", 
                "Fiscalias_PR.png", 
                "https://www.justicia.pr.gov/secretarias-y-oficinas/oficina-del-jefe-de-los-fiscales/listado-de-fiscalias/",
                "https://www.justicia.pr.gov/", 
                size = "250",
                padding_bottom = "20px"
              )
            )
          )
          # ,
          # # Sección principal con los gráficos
          # mainPanel(
          #   plotlyOutput("map_just_mapaFisc"),
          # )
        ),
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
          br(), br(),
            sidebarLayout(
              sidebarPanel(
                style = "display: flex; flex-direction: column; align-items: center;",
                
                # # seleccionar valor de la variable
                div(
                  style = "width: 100%; display: flex; justify-content: center; margin-bottom: 20px;", 
                  div(
                    style = "text-align: center; display: inline-block;", 
                    # botón para seleccionar la región
                    createDropdownCheckbox(
                      label = HTML("Seleccione<br>Región de Vivienda:"),
                      choices = dfAvp$Región,
                      selected = dfAvp$Región,
                      id = "avp_dfAvp_región"
                    )
                  )
                ),
                
                div(
                  style = "width: 100%; display: flex; flex-direction: column; align-items: center; padding-top: 0px;",
                  div(
                    style = "width: 100%; display: flex; justify-content: center; align-items: center;",
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-right: 10px",
                      # botón para seleccionar el año
                      createDropdownCheckbox(
                        label = "Seleccione Año(s):",
                        choices = dfAvp$Año,
                        selected = dfAvp$Año,
                        id = "avp_dfAvp_año"
                      )
                    ),
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-left: 0px;", 
                      showDataCheckbox("showTable_avp_dfAvp")
                    )
                  )
                ),
                
                
                # Output UI para la tabla de datos
                uiOutput("dataTableUI_avp_dfAvp")
                
              ),
            
            # Sección principal con los gráficos
            mainPanel(
              style = "height: calc(100vh - 150px); padding-bottom: 10px;",
              fluidRow(
                column(12, 
                       div(id = "scrollable-plot", 
                           div(id = "plot-title", uiOutput("plot_title3")),
                           plotlyOutput("barPlot_avp_dfAvp"),  height = "100%"))
              ),
              tags$div(style = "padding-bottom: 10px;"),
              tags$div(
                style = "padding-bottom: 10px;",
                div(
                  class = "card",
                  style = "padding: 15px;color: white; background-color: #3e3f3a; border-radius: 5px; margin-top: 0; width: 100%;",
                  h4(
                    strong(actualizacion_avp1, style="margin: 0px;") 
                  ),
                  p(
                    "Los datos representados en esta gráfica corresponden al
                    total de viviendas públicas solicitadas y asignadas por
                    violencia doméstica por región de la Administración de Vivienda Pública
                    desde el año natural 2017 al 2023.",
                    
                    style = "font-size: 16px;padding: 0px;" 
                  )
                )
              )
            )
          ),
        ),
        
        
        #### tab para el mapa de Adminsitración de Vivienda Públicas (mapaAvp) ####
        tabPanel(
          lowercaseTitle("Mapas de Viviendas Públicas Solicitadas y Asignadas"),
          br(), br(),
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              
              # Botón para seleccionar el año
              selectInput("select_avp_mapaAvp_año", "Seleccione Año:",
                          choices = levels(mapaAvp$Año),
                          selected = 1),
              
              # Centrar el checkbox y la tabla
              div(
                style = "flex: 0.7; display: flex; justify-content: center;",
                
                # Checkbox para mostrar/ocultar la tabla de datos
                showDataCheckbox("showTable_avp_mapaAvp"),
                ),
                # Output UI para la tabla de datos
                uiOutput("dataTableUI_avp_mapaAvp")
              
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              fluidRow(
                column(12, 
                       div(id = "scrollable-plot", 
                           div(id = "plot-title", uiOutput("plot_title24")),
                           # Mapa 1: Solicitadas
                           h5("Solicitadas", style = "text-align: center; margin-top: 10px;"),
                           leafletOutput("map_avp_mapaAvp_solicitadas", height = "200px"),
                           tags$div(style = "margin-top: 10px;"),
                           # Mapa 2: Asignadas
                           h5("Asignadas", style = "text-align: center; margin-top: 10px;"),
                           leafletOutput("map_avp_mapaAvp_asignadas", height = "200px"),
                           height = "100%"))
              ),
              tags$div(style = "padding-bottom: 10px;"),
              tags$div(
                style = "padding-bottom: 10px;",
                div(
                  class = "card",
                  style = "padding: 15px;color: white; background-color: #3e3f3a; border-radius: 5px; margin-top: 0; width: 100%;",
                  h4(
                    strong(actualizacion_avp2, style="margin: 0px;") 
                  ),
                  p(
                    "Los datos representados en este mapa corresponden al
                    total de viviendas públicas solicitadas y asignadas por
                    violencia doméstica por región de la Administración de Vivienda Pública
                    desde el año natural 2017 al 2023.",
                    
                    style = "font-size: 16px;padding: 0px;" 
                  )
                )
              )
            )
          )
        ), 
        
        #### tab para el Definiciones y Metadatos ####
        tabPanel(
          lowercaseTitle("Definiciones y Metadatos"),
          
          # Título del Tab 
          tags$ul(style = "list-style-type: none; text-align: center; padding: 5px; padding-top: 15px;",
                  sectionTitle("Regiones de la Adminsitración de Vivienda Pública", "24px")),
          fluidRow(
            column(12, DTOutput("dataTable_Def_avp"))
          ),
          
          # tags$ul(style = "list-style-type: none; text-align: center;",
          #         sectionTitle("Regiones de Vivienda Pública", "24px")),
          # embedImage("RegionesVivienda", "RegionesVivienda.png", 
          #            "https://www.avp.pr.gov/regiones-directorio.aspx",
          #            "https://www.avp.pr.gov/regiones-directorio.aspx", size = "250"
          # )
          
          tags$div(
            style = "display: flex; justify-content: center; align-items: center; text-align: center;",
            tags$ul(
              style = "list-style-type: none; padding: 0; margin: 0;",
              sectionTitle("Regiones de Administración de Vivienda Pública", "24px"),
              br(),
              embedImage(
                "RegionesVivienda", "RegionesVivienda.png", 
                "https://www.avp.pr.gov/regiones-directorio.aspx",
                "https://www.avp.pr.gov/regiones-directorio.aspx", size = "250",
                padding_bottom = "20px"
              )
            )
          )
          
          
          # ,
          # # Sección principal con los gráficos
          # mainPanel(
          #   plotlyOutput("map_avp_mapaRegi")
          # )
        )
      )
    ),
    
    
    
    
    #### Tab del Negociado de Policía de Puerto Rico ####
    tabPanel(
      lowercaseTitle("Negociado de la Policía"),
      icon = icon("building-shield"),
      tabsetPanel(
        
        #### tab con datos de mujeres desaparecidas (despDF) ####
        tabPanel(
          lowercaseTitle("Mujeres Desaparecidas y Localizadas"), 
          br(), br(),
            sidebarLayout(
              sidebarPanel(
                style = "display: flex; flex-direction: column; align-items: center;",
                
                # # seleccionar valor de la variable
                div(
                  style = "width: 100%; display: flex; justify-content: center; margin-bottom: 20px;",  
                  div(
                    style = "text-align: center; display: inline-block;",  
                    # botón para seleccionar la Categoría
                    createDropdownCheckbox(
                      label = HTML("Seleccione<br>el Estado de la Víctima:"),
                      choices = despDF$Estado,
                      selected = despDF$Estado,
                      id = "poli_despDF_categoría"
                    )
                  )
                ),
                
                div(
                  style = "width: 100%; display: flex; flex-direction: column; align-items: center; padding-top: 0px;",
                  div(
                    style = "width: 100%; display: flex; justify-content: center; align-items: center;",
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-right: 10px",
                      # botón para seleccionar el año
                      createDropdownCheckbox(
                        label = "Seleccione año(s):",
                        choices = despDF$Año,
                        selected = despDF$Año,
                        id = "poli_despDF_año"
                      )
                    ),
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-left: 0px;", 
                      showDataCheckbox("showTable_poli_despDF")
                    )
                  )
                ),
                
                # Output UI para la tabla de datos
                uiOutput("dataTableUI_poli_despDF")
                
              ),
            
            
            # Sección principal con los gráficos
            mainPanel(
              style = "height: calc(100vh - 150px); padding-bottom: 10px;",
              fluidRow(
                column(12, 
                       div(id = "scrollable-plot", 
                           div(id = "plot-title", uiOutput("plot_title15")),
                           plotlyOutput("barPlot_poli_despDF"),  height = "100%"))
              ),
              tags$div(style = "padding-bottom: 10px;"),
              tags$div(
                style = "padding-bottom: 10px;",
                div(
                  class = "card",
                  style = "padding: 15px;color: white; background-color: #3e3f3a; border-radius: 5px; margin-top: 0; width: 100%;",
                  h4(
                    strong(actualizacion_policia1, style="margin: 0px;") 
                  ),
                  p(
                    "Los datos representados en esta gráfica corresponden a la
                    cantidad de mujeres desaparecidas y localizadas (adultas y menores)
                    desde el año natural 2020 al 20 de septiembre de 2024.",
                    
                    style = "font-size: 16px;padding: 0px;" 
                  )
                )
              )
            )
          ),
        ),
        
        #### tab para el barplot de incidentes de violencia doméstica por edad de la víctima (vEdad) ####
        tabPanel(
          lowercaseTitle("Violencia Doméstica por Edad"),
          br(), br(),
          
            sidebarLayout(
              sidebarPanel(
                style = "display: flex; flex-direction: column; align-items: center;",
                
                # # seleccionar valor de la variable
                div(
                  style = "width: 100%; display: flex; justify-content: center; margin-bottom: 20px;",  
                  div(
                    style = "text-align: center; display: inline-block;", 
                    # botón para seleccionar edad
                    createDropdownCheckbox(
                      label = HTML("Seleccione <br>Grupo(s) de Edad:"),
                      choices = vEdad$Edad,
                      selected = vEdad$Edad,
                      id = "poli_vEdad_edad"
                    ),
                    # botón para seleccionar el sexo
                    createDropdownCheckbox(
                      label = HTML("Seleccione <br>Sexo de las Víctimas:"),
                      choices = vEdad$Sexo,
                      selected = vEdad$Sexo[1],
                      id = "poli_vEdad_sexo"
                    )
                  )
                ),
                
                div(
                  style = "width: 100%; display: flex; flex-direction: column; align-items: center; padding-top: 0px;",
                  div(
                    style = "width: 100%; display: flex; justify-content: center; align-items: center;",
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-right: 10px",
                      # botón para seleccionar año
                      createDropdownCheckbox(
                        label = "Seleccione Año(s):",
                        choices = vEdad$Año,
                        selected = vEdad$Año,
                        id = "poli_vEdad_año"
                      )
                    ),
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-left: 0px;", 
                      showDataCheckbox("showTable_poli_vEdad")
                    )
                  )
                ),
                
                # Output UI para la tabla de datos
                uiOutput("dataTableUI_poli_vEdad")
                
              ),
              
            # Sección principal con los gráficos
            mainPanel(
              style = "height: calc(100vh - 150px); padding-bottom: 10px;",
              fluidRow(
                column(12, 
                       div(id = "scrollable-plot", 
                           div(id = "plot-title", uiOutput("plot_title4")),
                           plotlyOutput("barPlot_poli_vEdad"),  height = "100%"))
              ),
              tags$div(style = "padding-bottom: 10px;"),
              tags$div(
                style = "padding-bottom: 10px;",
                div(
                  class = "card",
                  style = "padding: 15px;color: white; background-color: #3e3f3a; border-radius: 5px; margin-top: 0; width: 100%;",
                  h4(
                    strong(actualizacion_policia2, style="margin: 0px;") 
                  ),
                  p(
                    "Los datos representados en esta gráfica corresponden a los
                    incidentes de violencia doméstica (por edad de la víctima)
                    desde el año natural 2021 al 2023.",
                    
                    style = "font-size: 16px;padding: 0px;" 
                  )
                )
              )
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
                  sectionTitle("Negociado de la Policía", "24px")),
          fluidRow(
            column(12, DTOutput("dataTable_Def_poli"))
          ),
          
          
          tags$div(
            style = "display: flex; justify-content: center; align-items: center; text-align: center;",
            tags$ul(
              style = "list-style-type: none; padding: 0; margin: 0;",
              sectionTitle("Regiones Policíacas", "24px"),
              br(),
              embedImage(
                "RegionesPoliciácas", "RegionesPoliciacas.png", 
                "https://www.policia.pr.gov/",
                "https://www.policia.pr.gov/", size = "250",
                padding_bottom = "20px"
              )
            )
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
          br(), br(),
            sidebarLayout(
              sidebarPanel(
                style = "display: flex; flex-direction: column; align-items: center;",
                
                div(
                  style = "width: 100%; display: flex; flex-direction: column; align-items: center; padding-top: 0px; margin-bottom: 20px;",
                  div(
                    style = "width: 100%; display: flex; justify-content: center; align-items: center;",
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-right: 10px",
                      # botón para seleccionar el año
                      createDropdownCheckbox(
                        label = "Seleccione Año(s):",
                        choices = opmFemiVD$Año,
                        selected = opmFemiVD$Año,
                        id = "opm_opmFemiVD_año"
                      )
                    ),
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-left: 0px;", 
                      showDataCheckbox("showTable_opm_opmFemiVD")
                    )
                  )
                ),
                
                # Output UI para la tabla de datos
                uiOutput("dataTableUI_opm_opmFemiVD")
                
              ),
            
            
            # Sección principal con los gráficos
            mainPanel(
              style = "height: calc(100vh - 150px); padding-bottom: 10px;",
              fluidRow(
                column(12,
                       div(id = "scrollable-plot",
                           div(id = "plot-title", uiOutput("plot_title16")),
                           plotlyOutput("linePlot_opm_opmFemiVD"),  height = "100%"))
              ),
              tags$div(style = "padding-bottom: 10px;"),
              tags$div(
                style = "padding-bottom: 10px;",
                div(
                  class = "card",
                  style = "padding: 15px;color: white; background-color: #3e3f3a; border-radius: 5px; margin-top: 0; width: 100%;",
                  h4(
                    strong(actualizacion_opm1, style="margin: 0px;") 
                  ),
                  p(
                    "Los datos representados en esta gráfica corresponden a
                    la tasa de asesinatos de mujeres por violencia doméstica, 
                    desde el año natural 1990 al 2021 (Tasa x100,000 mujeres).",
                    
                    style = "font-size: 16px;padding: 0px;" 
                  )
                )
              )
            )
            
          ),
        ),
        
        #### tab con datos de violencia domestica (opmCasos) ####
        tabPanel(
          lowercaseTitle("Casos Según Razón para Consulta"), 
          br(), br(),
            sidebarLayout(
              sidebarPanel(
                style = "display: flex; flex-direction: column; align-items: center;",
                
                # # seleccionar valor de la variable
                div(
                  style = "width: 100%; display: flex; justify-content: center; margin-bottom: 20px;",  
                  div(
                    style = "text-align: center; display: inline-block;", 
                    # botón para seleccionar el tipo de violencia
                    createDropdownCheckbox(
                      label = HTML("Seleccione<br>Razón para Consulta:"),
                      choices = opmCasos$Razón,
                      selected = opmCasos$Razón,
                      id = "opm_opmCasos_tipo"
                    )
                  )
                ),
                
                div(
                  style = "width: 100%; display: flex; flex-direction: column; align-items: center; padding-top: 0px;",
                  div(
                    style = "width: 100%; display: flex; justify-content: center; align-items: center;",
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-right: 10px",
                      createDropdownCheckbox(
                      label = "Seleccione Año(s):",
                      choices = opmCasos$Año,
                      selected = opmCasos$Año,
                      id = "opm_opmCasos_año"
                     )
                    ),
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-left: 0px;", 
                      showDataCheckbox("showTable_opm_opmCasos")
                    )
                  )
                ),
                
                # Output UI para la tabla de datos
                uiOutput("dataTableUI_opm_opmCasos")
                
              ),
              
            # Sección principal con los gráficos
            mainPanel(
              style = "height: calc(100vh - 150px); padding-bottom: 10px;",
              fluidRow(
                column(12, 
                       div(id = "scrollable-plot", 
                           div(id = "plot-title", uiOutput("plot_title17")),
                           plotlyOutput("barPlot_opm_opmCasos"),  height = "100%"))
              ),
              tags$div(style = "padding-bottom: 10px;"),
              tags$div(
                style = "padding-bottom: 10px;",
                div(
                  class = "card",
                  style = "padding: 15px;color: white; background-color: #3e3f3a; border-radius: 5px; margin-top: 0; width: 100%;",
                  h4(
                    strong(actualizacion_opm2, style="margin: 0px;") 
                  ),
                  p(
                    "Los datos representados en esta gráfica corresponden a
                    la población atendida mediante el programa CRIAS según 
                    razón para consulta.",
                    
                    style = "font-size: 16px;padding: 0px;" 
                  )
                )
              )
            )
          ),
        ),
        
        #### tab con datos del género de las víctimas (opmVic) ####
        tabPanel(
          lowercaseTitle("Género de las Víctimas Atendidas"),
          br(), br(),
            sidebarLayout(
              sidebarPanel(
                style = "display: flex; flex-direction: column; align-items: center;",
                
                # # seleccionar valor de la variable
                div(
                  style = "width: 100%; display: flex; justify-content: center; margin-bottom: 20px;", 
                  div(
                    style = "text-align: center; display: inline-block;",  
                    # botón para seleccionar el género de las victimas
                    createDropdownCheckbox(
                      label = "Seleccione Género(s):",
                      choices = opmVic$Género,
                      selected = opmVic$Género,
                      id = "opm_opmVic_género"
                    )
                  )
                ),
                
                div(
                  style = "width: 100%; display: flex; flex-direction: column; align-items: center; padding-top: 0px;",
                  div(
                    style = "width: 100%; display: flex; justify-content: center; align-items: center;",
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-right: 10px",
                      createDropdownCheckbox(
                        label = "Seleccione Año(s):",
                        choices = opmVic$Año,
                        selected = opmVic$Año,
                        id = "opm_opmVic_año"
                      )
                    ),
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-left: 0px;", 
                      showDataCheckbox("showTable_opm_opmVic")
                    )
                  )
                ),
                
                # Output UI para la tabla de datos
                uiOutput("dataTableUI_opm_opmVic")
                
              ),
            
            # Sección principal con los gráficos
            mainPanel(
              style = "height: calc(100vh - 150px); padding-bottom: 10px;",
              fluidRow(
                column(12, 
                       div(id = "scrollable-plot", 
                           div(id = "plot-title", uiOutput("plot_title18")),
                           plotlyOutput("barPlot_opm_opmVic"),  height = "100%"))
              ),
              tags$div(style = "padding-bottom: 10px;"),
              tags$div(
                style = "padding-bottom: 10px;",
                div(
                  class = "card",
                  style = "padding: 15px;color: white; background-color: #3e3f3a; border-radius: 5px; margin-top: 0; width: 100%;",
                  h4(
                    strong(actualizacion_opm3, style="margin: 0px;") 
                  ),
                  p(
                    "Los datos representados en esta gráfica corresponden a
                    la identidad de género de víctimas asistidas por el programa CRIAS.",
                    
                    style = "font-size: 16px;padding: 0px;" 
                  )
                )
              )
            )
          ),
        ),
        
        #### tab con datos de orientaciones medio de comunicación (opmMedio) ####
        tabPanel(
          lowercaseTitle("Medio de Orientación a las Víctimas"),
          br(), br(),
            sidebarLayout(
              sidebarPanel(
                style = "display: flex; flex-direction: column; align-items: center;",
                
                # # seleccionar valor de la variable
                div(
                  style = "width: 100%; display: flex; justify-content: center; margin-bottom: 20px;",  
                  div(
                    style = "text-align: center; display: inline-block;", 
                    # botón para seleccionar el medio de orientación
                    createDropdownCheckbox(
                      label = HTML("Seleccione<br>el Medio de Orientación:"),
                      choices = opmMedio$Orientación,
                      selected = opmMedio$Orientación,
                      id = "opm_opmMedio_medio"
                    )
                  )
                ),
                
                div(
                  style = "width: 100%; display: flex; flex-direction: column; align-items: center; padding-top: 0px;",
                  div(
                    style = "width: 100%; display: flex; justify-content: center; align-items: center;",
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-right: 10px",
                      # botón para seleccionar el año
                      createDropdownCheckbox(
                        label = "Seleccione Año(s):",
                        choices = opmMedio$Año,
                        selected = opmMedio$Año,
                        id = "opm_opmMedio_año"
                      )
                    ),
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-left: 0px;", 
                      showDataCheckbox("showTable_opm_opmMedio")
                    )
                  )
                ),
                
                # Output UI para la tabla de datos
                uiOutput("dataTableUI_opm_opmMedio")
                
              ),
            
            # Sección principal con los gráficos
            mainPanel(
              style = "height: calc(100vh - 150px); padding-bottom: 10px;",
              fluidRow(
                column(12, 
                       div(id = "scrollable-plot", 
                           div(id = "plot-title", uiOutput("plot_title19")),
                           plotlyOutput("barPlot_opm_opmMedio"),  height = "100%"))
              ),
              tags$div(style = "padding-bottom: 10px;"),
              tags$div(
                style = "padding-bottom: 10px;",
                div(
                  class = "card",
                  style = "padding: 15px;color: white; background-color: #3e3f3a; border-radius: 5px; margin-top: 0; width: 100%;",
                  h4(
                    strong(actualizacion_opm4, style="margin: 0px;") 
                  ),
                  p(
                    "Los datos representados en esta gráfica corresponden a
                    las orientaciones ofrecidas mediante el programa CRIAS.",
                    
                    style = "font-size: 16px;padding: 0px;" 
                  )
                )
              )
            )
          ),
        ),
        
        #### tab con datos de los servicios ofrecidos por mes (opmServiciosMes) ####
        tabPanel(
          lowercaseTitle("Servicios y Alcance de la OPM"), 
          br(), br(),
            sidebarLayout(
              sidebarPanel(
                style = "display: flex; flex-direction: column; align-items: center;",
                
                # # seleccionar valor de la variable
                div(
                  style = "width: 100%; display: flex; justify-content: center; margin-bottom: 20px;",  
                  div(
                    style = "text-align: center; display: inline-block;", 
                    # botón para seleccionar el género de las victimas
                    createDropdownCheckbox(
                      label = HTML("Seleccione <br>el Tipo de Servicio:"),
                      choices = opmServiciosMes$Servicio,
                      selected = opmServiciosMes$Servicio,
                      id = "opm_opmServiciosMes_tipo"
                    )
                  )
                ),
                
                div(
                  style = "width: 100%; display: flex; flex-direction: column; align-items: center; padding-top: 0px;",
                  div(
                    style = "width: 100%; display: flex; justify-content: center; align-items: center;",
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-right: 10px",
                      # botón para seleccionar el año
                      createDropdownCheckbox(
                        label = "Seleccione Año(s):",
                        choices = opmServiciosMes$Año,
                        selected = opmServiciosMes$Año,
                        id = "opm_opmServiciosMes_año"
                      )
                    ),
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-left: 0px;", 
                      showDataCheckbox("showTable_opm_opmServiciosMes")
                    )
                  )
                ),
                
                # Output UI para la tabla de datos
                uiOutput("dataTableUI_opm_opmServiciosMes")
                
              ),
            
            # Sección principal con los gráficos
            mainPanel(
              style = "height: calc(100vh - 150px); padding-bottom: 10px;",
              fluidRow(
                column(12, 
                       div(id = "scrollable-plot", 
                           div(id = "plot-title", uiOutput("plot_title20")),
                           plotlyOutput("barPlot_opm_opmServiciosMes"),  height = "100%"))
              ),
              tags$div(style = "padding-bottom: 10px;"),
              tags$div(
                style = "padding-bottom: 10px;",
                div(
                  class = "card",
                  style = "padding: 15px;color: white; background-color: #3e3f3a; border-radius: 5px; margin-top: 0; width: 100%;",
                  h4(
                    strong(actualizacion_opm5, style="margin: 0px;") 
                  ),
                  p(
                    "Los datos representados en esta gráfica corresponden a
                    la población atendida, servicios ofrecidos y seguimientos
                    de casos mediante el programa CRIAS. Una misma persona pudo 
                    haber recibido más de un servicio alrededor de los años.",
                    
                    style = "font-size: 16px;padding: 0px;" 
                  )
                )
              )
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
            column(12, DTOutput("dataTable_Def_opm"))
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
        br(), br(),
        sidebarLayout(
          sidebarPanel(
            style = "display: flex; flex-direction: column; align-items: center;",
            
            # # seleccionar valor de la variable
            div(
              style = "width: 100%; display: flex; justify-content: center; margin-bottom: 20px;", 
              div(
                style = "text-align: center; display: inline-block;", 
                # botón para seleccionar el sexo
                createDropdownCheckbox(
                  label = "Seleccione el Sexo:",
                  choices = dcrCasosInv$Sexo,
                  selected = 1,
                  id = "dcr_dcrCasosInv_sexo"
                ),
                # botón para seleccionar el tipo de investigación 
                createDropdownCheckbox(
                  label = HTML("Seleccione  el<br> Estado de Investigación:"),
                  choices = dcrCasosInv$Estado,
                  selected = dcrCasosInv$Estado,
                  id = "dcr_dcrCasosInv_tipo"
                )
              )
            ),
            
            div(
              style = "width: 100%; display: flex; flex-direction: column; align-items: center; padding-top: 0px;",
              div(
                style = "width: 100%; display: flex; justify-content: center; align-items: center;",
                div(
                  style = "flex: 1; display: flex; justify-content: center; margin-right: 10px",
                  # botón para seleccionar el año
                  createDropdownCheckbox(
                    label = "Seleccione Año(s):",
                    choices = dcrCasosInv$Año,
                    selected = dcrCasosInv$Año,
                    id = "dcr_dcrCasosInv_year"
                  )
                ),
                div(
                  style = "flex: 1; display: flex; justify-content: center; margin-left: 0px;", 
                  showDataCheckbox("showTable_dcr_dcrCasosInv")
                )
              )
            ),
            
            # Output UI para la tabla de datos
            uiOutput("dataTableUI_dcr_dcrCasosInv")
            
          ),
        
        # Sección principal con los gráficos
        mainPanel(
          style = "height: calc(100vh - 150px); padding-bottom: 10px;",
          fluidRow(
            column(12, 
                   div(id = "scrollable-plot", 
                       div(id = "plot-title", uiOutput("plot_title21")),
                       plotlyOutput("barPlot_dcr_dcrCasosInv"),  height = "100%"))
          ),
          tags$div(style = "padding-bottom: 10px;"),
          tags$div(
            style = "padding-bottom: 10px;",
            div(
              class = "card",
              style = "padding: 15px;color: white; background-color: #3e3f3a; border-radius: 5px; margin-top: 0; width: 100%;",
              h4(
                strong(actualizacion_dcr1, style="margin: 0px;") 
              ),
              p(
                "Los datos representados en esta gráfica corresponden a
                 los casos en supervisión de ley 54 en programas alternos
                al confinamiento (por estado de investigación): Programas de Comunidad
                desde el año natural 2021 al 2024. Los datos de Investigaciones realizadas
                se encuentran hasta febrero del 2023 por el cambio de formato en sus datos.
                El Departamento de Corrección y Rehabilitación se encuentra trabajando
                con la actualización de estos. Los datos de Casos en Supervisión se 
                encuentran hasta el 31 de octubre de 2024.",
                
                style = "font-size: 16px;padding: 0px;" 
              )
            )
          )
        )
      ),
    ),
    
        #### tab con datos personas sentenciadas al Programa de Supervisión Electrónica (dcrSentenciadas) ####
    tabPanel(
      lowercaseTitle("Sentencias por Violencia Doméstica"),
      br(), br(),
        sidebarLayout(
          sidebarPanel(
            style = "display: flex; flex-direction: column; align-items: center;",
            
            # # seleccionar valor de la variable
            div(
              style = "width: 100%; display: flex; justify-content: center; margin-bottom: 20px;",  
              div(
                style = "text-align: center; display: inline-block;", 
                # botón para seleccionar el tipo de investigación
                createDropdownCheckbox(
                  label = "Seleccione el Estado del Caso:",
                  choices = dcrSentenciadas$Estado,
                  selected = dcrSentenciadas$Estado,
                  id = "dcr_dcrSentenciadas_tipo"
                )
              )
            ),
            
            div(
              style = "width: 100%; display: flex; flex-direction: column; align-items: center; padding-top: 0px;",
              div(
                style = "width: 100%; display: flex; justify-content: center; align-items: center;",
                div(
                  style = "flex: 1; display: flex; justify-content: center; margin-right: 10px",
                  # botón para seleccionar el año
                  createDropdownCheckbox(
                    label = "Seleccione Año(s):",
                    choices = dcrSentenciadas$Año,
                    selected = dcrSentenciadas$Año,
                    id = "dcr_dcrSentenciadas_year"
                  )
                ),
                div(
                  style = "flex: 1; display: flex; justify-content: center; margin-left: 0px;", 
                  showDataCheckbox("showTable_dcr_dcrSentenciadas")
                )
              )
            ),
            
            # Output UI para la tabla de datos
            uiOutput("dataTableUI_dcr_dcrSentenciadas")
            
          ),
        
        # Sección principal con los gráficos
        mainPanel(
          style = "height: calc(100vh - 150px); padding-bottom: 10px;",
          fluidRow(
            column(12, 
                   div(id = "scrollable-plot", 
                       div(id = "plot-title", uiOutput("plot_title12")),
                       plotlyOutput("barPlot_dcr_dcrSentenciadas"),  height = "100%"))
          ),
          tags$div(style = "padding-bottom: 10px;"),
          tags$div(
            style = "padding-bottom: 10px;",
            div(
              class = "card",
              style = "padding: 15px;color: white; background-color: #3e3f3a; border-radius: 5px; margin-top: 0; width: 100%;",
              h4(
                strong(actualizacion_dcr2, style="margin: 0px;") 
              ),
              p(
                "Los datos representados en esta gráfica corresponden a
                 las personas sentenciadas en programa de supervisión 
                electrónica por delitos de violencia doméstica por estado del caso 
                desde el año natural 2020 al 2023.",
                
                style = "font-size: 16px;padding: 0px;" 
              )
            )
          )
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
        column(12, DTOutput("dataTable_Def_dcr"))
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
          br(), br(),
            sidebarLayout(
              sidebarPanel(
                style = "display: flex; flex-direction: column; align-items: center;",
                
                # # seleccionar valor de la variable
                div(
                  style = "width: 100%; display: flex; justify-content: center; margin-bottom: 20px; ",  
                  div(
                    style = "text-align: center; display: inline-block;",  
                    # botón para seleccionar el grupo de edad
                    createDropdownCheckbox(
                      label = HTML("Seleccione<br> Grupo(s) de Edad:"),
                      choices = OP_148_SoliGrupEdad$Edad,
                      selected = OP_148_SoliGrupEdad$Edad,
                      id = "trib_OP_148_SoliGrupEdad_Edad"
                    ),
                    # botón para seleccionar el distrito fiscal
                    createDropdownCheckbox(
                      label = HTML("Seleccione <br> Región Judicial:"),
                      choices = OP_148_SoliGrupEdad$Región,
                      selected = OP_148_SoliGrupEdad$Región,
                      id = "trib_OP_148_SoliGrupEdad_Región"
                    )
                  )
                ),
                
                div(
                  style = "width: 100%; display: flex; flex-direction: column; align-items: center; padding-top: 0px;",
                  div(
                    style = "width: 100%; display: flex; justify-content: center; align-items: center;",
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-right: 10px",
                      # botón para seleccionar el año
                      createDropdownCheckbox_añoFiscal(
                        label = HTML("Seleccione <br> Año(s) Fiscal:"),
                        choices = OP_148_SoliGrupEdad$AñoFiscal,
                        selected = OP_148_SoliGrupEdad$AñoFiscal,
                        id = "trib_OP_148_SoliGrupEdad_AñoFiscal"
                      )
                    ),
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-left: 0px;", 
                      showDataCheckbox("showTable_OP_148_SoliGrupEdad")
                    )
                  )
                ),
                
                # Output UI para la tabla de datos
                uiOutput("dataTableUI_OP_148_SoliGrupEdad")
                
              ),
            
            # Sección principal con los gráficos
            mainPanel(
              style = "height: calc(100vh - 150px); padding-bottom: 10px;",
              fluidRow(
                column(12, 
                       div(id = "scrollable-plot", 
                           div(id = "plot-title", uiOutput("plot_title5")),
                           plotlyOutput("barPlot_OP_148_SoliGrupEdad"),  height = "100%"))
              ),
              tags$div(style = "padding-bottom: 10px;"),
              tags$div(
                style = "padding-bottom: 10px;",
                div(
                  class = "card",
                  style = "padding: 15px;color: white; background-color: #3e3f3a; border-radius: 5px; margin-top: 0; width: 100%;",
                  h4(
                    strong(actualizacion_tribunales1, style="margin: 0px;") 
                  ),
                  p(
                    "Los datos representados en esta gráfica corresponden a
                   las órdenes de protección solicitadas por violencia sexual
                    bajo la Ley 148, según grupo de edad, región judicial y año 
                    fiscal. Los datos para cada año fiscal se identifican con el
                    año en que finaliza el mismo, por ejemplo, para los datos
                    del año fiscal 2020-2021, los datos son presentados como 
                    año fiscal 2021.",
                    
                    style = "font-size: 16px;padding: 0px;" 
                  )
                )
              )
            )
          ),
        ),
        
        #### tab con datos de ley 148 - Violencia Sexual por grupo de edad (OP_Ley148_ex_parteEmitidas) ####
        tabPanel(
          lowercaseTitle("Órdenes de Protección Ex Parte Emitidas por Delito Cometido y Región"), 
          br(), br(),
            sidebarLayout(
              sidebarPanel(
                style = "display: flex; flex-direction: column; align-items: center;",
                
                # # seleccionar valor de la variable
                div(
                  style = "width: 100%; display: flex; justify-content: center; margin-bottom: 20px;", 
                  div(
                    style = "text-align: center; display: inline-block;",
                    # botón para seleccionar el delito
                    createDropdownCheckbox(
                      label = HTML("Seleccione <br> Delito(s) Cometido:"),
                      choices = OP_Ley148_ex_parteEmitidas$Delito,
                      selected = OP_Ley148_ex_parteEmitidas$Delito,
                      id = "trib_OP_Ley148_ex_parteEmitidas_Delito"
                    ),
                    # botón para seleccionar la región fiscal
                    createDropdownCheckbox(
                      label = HTML("Seleccione <br> Región Judicial:"),
                      choices = OP_Ley148_ex_parteEmitidas$Región,
                      selected = OP_Ley148_ex_parteEmitidas$Región,
                      id = "trib_OP_Ley148_ex_parteEmitidas_Región"
                    )
                  )
                ),
                
                div(
                  style = "width: 100%; display: flex; flex-direction: column; align-items: center; padding-top: 0px;",
                  div(
                    style = "width: 100%; display: flex; justify-content: center; align-items: center;",
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-right: 10px",
                      # botón para seleccionar el año fiscal
                      createDropdownCheckbox_añoFiscal(
                        label = HTML("Seleccione <br> Año(s) Fiscal:"),
                        choices = OP_Ley148_ex_parteEmitidas$AñoFiscal,
                        selected = OP_Ley148_ex_parteEmitidas$AñoFiscal,
                        id = "trib_OP_Ley148_ex_parteEmitidas_AñoFiscal"
                      )
                    ),
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-left: 0px;", 
                      showDataCheckbox("showTable_OP_Ley148_ex_parteEmitidas")
                    )
                  )
                ),
                
                # Output UI para la tabla de datos
                uiOutput("dataTableUI_OP_Ley148_ex_parteEmitidas")
                
              ),
            
            # Sección principal con los gráficos
            mainPanel(
              style = "height: calc(100vh - 150px); padding-bottom: 10px;",
              fluidRow(
                column(12, 
                       div(id = "scrollable-plot", 
                           div(id = "plot-title", uiOutput("plot_title6")),
                           plotlyOutput("barPlot_OP_Ley148_ex_parteEmitidas"),  height = "100%"))
              ),
              tags$div(style = "padding-bottom: 10px;"),
              tags$div(
                style = "padding-bottom: 10px;",
                div(
                  class = "card",
                  style = "padding: 15px;color: white; background-color: #3e3f3a; border-radius: 5px; margin-top: 0; width: 100%;",
                  h4(
                    strong(actualizacion_tribunales2, style="margin: 0px;") 
                  ),
                  p(
                    "Los datos representados en esta gráfica corresponden a
                    las órdenes de protección ex parte emitidas bajo la Ley 148
                    según región judicial, delito cometido y año fiscal.
                    Los datos para cada año fiscal se identifican con el
                    año en que finaliza el mismo, por ejemplo, para los datos
                    del año fiscal 2020-2021, los datos son presentados como 
                    año fiscal 2021.",
                    
                    style = "font-size: 16px;padding: 0px;" 
                  )
                )
              )
            )
          ),
        ),
        
        #### tab con datos de solicitudes de órdenes de protección archivadas (OP_LEY148Archivadas) ####
        tabPanel(
          lowercaseTitle("Órdenes de protección ex parte archivadas por razón y región"), 
          br(), br(),
            sidebarLayout(
              sidebarPanel(
                style = "display: flex; flex-direction: column; align-items: center;",
                
                # # seleccionar valor de la variable
                div(
                  style = "width: 100%; display: flex; justify-content: center; margin-bottom: 20px;",  
                  div(
                    style = "text-align: center; display: inline-block;", 
                    # botón para seleccionar la razón de archivado
                    createDropdownCheckbox(
                      label = "Seleccione Razón(s):",
                      choices = OP_LEY148Archivadas$Razón,
                      selected = OP_LEY148Archivadas$Razón,
                      id = "trib_OP_LEY148Archivadas_Razón"
                    ),
                    # botón para seleccionar la región fiscal
                    createDropdownCheckbox(
                      label = "Seleccione Distrito(s) Fiscal:",
                      choices = OP_LEY148Archivadas$Región,
                      selected = OP_LEY148Archivadas$Región,
                      id = "trib_OP_LEY148Archivadas_Región"
                    )
                  )
                ),
                
                div(
                  style = "width: 100%; display: flex; flex-direction: column; align-items: center; padding-top: 0px;",
                  div(
                    style = "width: 100%; display: flex; justify-content: center; align-items: center;",
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-right: 10px",
                      # botón para seleccionar el año fiscal
                      createDropdownCheckbox_añoFiscal(
                        label = HTML("Seleccione <br> Año(s) Fiscal:"),
                        choices = OP_LEY148Archivadas$AñoFiscal,
                        selected = OP_LEY148Archivadas$AñoFiscal,
                        id = "trib_OP_LEY148Archivadas_AñoFiscal"
                      )
                    ),
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-left: 0px;", 
                      showDataCheckbox("showTable_OP_LEY148Archivadas")
                    )
                  )
                ),
                
                # Output UI para la tabla de datos
                uiOutput("dataTableUI_OP_LEY148Archivadas")
                
              ),
            
            # Sección principal con los gráficos
            mainPanel(
              style = "height: calc(100vh - 150px); padding-bottom: 10px;",
              fluidRow(
                column(12, 
                       div(id = "scrollable-plot", 
                           div(id = "plot-title", uiOutput("plot_title8")),
                           plotlyOutput("barPlot_OP_LEY148Archivadas"),  height = "100%"))
              ),
              tags$div(style = "padding-bottom: 10px;"),
              tags$div(
                style = "padding-bottom: 10px;",
                div(
                  class = "card",
                  style = "padding: 15px;color: white; background-color: #3e3f3a; border-radius: 5px; margin-top: 0; width: 100%;",
                  h4(
                    strong(actualizacion_tribunales3, style="margin: 0px;") 
                  ),
                  p(
                    "Los datos representados en esta gráfica corresponden a
                    las órdenes de protección ex parte bajo la Ley 148 archivadas
                    por razón, región judicial y año fiscal.
                    Los datos para cada año fiscal se identifican con el
                    año en que finaliza el mismo, por ejemplo, para los datos
                    del año fiscal 2020-2021, los datos son presentados como 
                    año fiscal 2021.",
                    
                    style = "font-size: 16px;padding: 0px;" 
                  )
                )
              )
            )
          ),
        ),
        
        #### tab con datos de solicitudes de órdenes de protección denegadas (OP_LEY148Denegadas) ####
        tabPanel(
          lowercaseTitle("Órdenes de protección denegadas por razón del archivo y región"), 
          br(), br(),
            sidebarLayout(
              sidebarPanel(
                style = "display: flex; flex-direction: column; align-items: center;",
                
                # # seleccionar valor de la variable
                div(
                  style = "width: 100%; display: flex; justify-content: center; margin-bottom: 20px;",  
                  div(
                    style = "text-align: center; display: inline-block;", 
                    # botón para seleccionar la razón de archivado
                    createDropdownCheckbox(
                      label = "Seleccione Razón(s):",
                      choices = OP_LEY148Denegadas$Razón,
                      selected = OP_LEY148Denegadas$Razón,
                      id = "trib_OP_LEY148Denegadas_Razón"
                    ),
                    customSeparator(),
                    # botón para seleccionar la región fiscal
                    createDropdownCheckbox(
                      label = "Seleccione Región Judicial:",
                      choices = OP_LEY148Denegadas$Región,
                      selected = OP_LEY148Denegadas$Región,
                      id = "trib_OP_LEY148Denegadas_Región"
                    )
                  )
                ),
                
                div(
                  style = "width: 100%; display: flex; flex-direction: column; align-items: center; padding-top: 0px;",
                  div(
                    style = "width: 100%; display: flex; justify-content: center; align-items: center;",
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-right: 10px",
                      # botón para seleccionar el año fiscal
                      createDropdownCheckbox_añoFiscal(
                        label = HTML("Seleccione <br> Año(s) Fiscal:"),
                        choices = OP_LEY148Denegadas$AñoFiscal,
                        selected = OP_LEY148Denegadas$AñoFiscal,
                        id = "trib_OP_LEY148Denegadas_AñoFiscal"
                      )
                    ),
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-left: 0px;", 
                      showDataCheckbox("showTable_OP_LEY148Denegadas")
                    )
                  )
                ),
                
                # Output UI para la tabla de datos
                uiOutput("dataTableUI_OP_LEY148Denegadas")
                
              ),
            
            # Sección principal con los gráficos
            mainPanel(
              style = "height: calc(100vh - 150px); padding-bottom: 10px;",
              fluidRow(
                column(12, 
                       div(id = "scrollable-plot", 
                           div(id = "plot-title", uiOutput("plot_title7")),
                           plotlyOutput("barPlot_OP_LEY148Denegadas"),  height = "100%"))
              ),
              tags$div(style = "padding-bottom: 10px;"),
              tags$div(
                style = "padding-bottom: 10px;",
                div(
                  class = "card",
                  style = "padding: 15px;color: white; background-color: #3e3f3a; border-radius: 5px; margin-top: 0; width: 100%;",
                  h4(
                    strong(actualizacion_tribunales4, style="margin: 0px;") 
                  ),
                  p(
                    "Los datos representados en esta gráfica corresponden a
                    las órdenes de protección por violencia sexual denegadas
                    bajo la Ley 148 por razón de archivo, región judicial y año
                    fiscal. Los datos para cada año fiscal se identifican con el
                    año en que finaliza el mismo, por ejemplo, para los datos
                    del año fiscal 2020-2021, los datos son presentados como 
                    año fiscal 2021.",
                    
                    style = "font-size: 16px;padding: 0px;" 
                  )
                )
              )
            )
          ),
        ),
        
        #### tab con datos de solicitudes de órdenes de protección finales emitidas (OP_LEY148FinalEmitidas) ####
        tabPanel(
          lowercaseTitle("Órdenes de Protección Emitidas por Delito Cometido y Región"), 
          br(), br(),
            sidebarLayout(
              sidebarPanel(
                style = "display: flex; flex-direction: column; align-items: center;",
                
                # # seleccionar valor de la variable
                div(
                  style = "width: 100%; display: flex; justify-content: center; margin-bottom: 20px;", 
                  div(
                    style = "text-align: center; display: inline-block;", 
                    # botón para seleccionar la razón de archivado
                    createDropdownCheckbox(
                      label = HTML("Seleccione <br> Delito(s) Cometido:"),
                      choices = OP_LEY148FinalEmitidas$Delito,
                      selected = OP_LEY148FinalEmitidas$Delito,
                      id = "trib_OP_LEY148FinalEmitidas_Delito"
                    ),
                    customSeparator(),
                    # botón para seleccionar la región fiscal
                    createDropdownCheckbox(
                      label = HTML("Seleccione <br> Región Judicial:"),
                      choices = OP_LEY148FinalEmitidas$Región,
                      selected = OP_LEY148FinalEmitidas$Región,
                      id = "trib_OP_LEY148FinalEmitidas_Región"
                    )
                  )
                ),
                
                div(
                  style = "width: 100%; display: flex; flex-direction: column; align-items: center; padding-top: 0px;",
                  div(
                    style = "width: 100%; display: flex; justify-content: center; align-items: center;",
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-right: 10px;",
                      # botón para seleccionar el año fiscal
                      createDropdownCheckbox_añoFiscal(
                        label = HTML("Seleccione <br> Año(s) Fiscal:"),
                        choices = OP_LEY148FinalEmitidas$AñoFiscal,
                        selected = OP_LEY148FinalEmitidas$AñoFiscal,
                        id = "trib_OP_LEY148FinalEmitidas_AñoFiscal"
                      )
                    ),
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-left: 0px;", 
                      showDataCheckbox("showTable_OP_LEY148FinalEmitidas")
                    )
                  )
                ),
                
                # Output UI para la tabla de datos
                uiOutput("dataTableUI_OP_LEY148FinalEmitidas")
                
              ),
            
            
            # Sección principal con los gráficos
            mainPanel(
              style = "height: calc(100vh - 150px); padding-bottom: 10px;",
              fluidRow(
                column(12, 
                       div(id = "scrollable-plot", 
                           div(id = "plot-title", uiOutput("plot_title9")),
                           plotlyOutput("barPlot_OP_LEY148FinalEmitidas"),  height = "100%"))
              ),
              tags$div(style = "padding-bottom: 10px;"),
              tags$div(
                style = "padding-bottom: 10px;",
                div(
                  class = "card",
                  style = "padding: 15px;color: white; background-color: #3e3f3a; border-radius: 5px; margin-top: 0; width: 100%;",
                  h4(
                    strong(actualizacion_tribunales5, style="margin: 0px;") 
                  ),
                  p(
                    "Los datos representados en esta gráfica corresponden a
                    las órdenes de protección emitidas bajo la Ley 148 por delito
                    cometido, región judicial y año fiscal. Los datos para cada 
                    año fiscal se identifican con el año en que finaliza
                    el mismo, por ejemplo, para los datos del año fiscal 2020-2021,
                    los datos son presentados como año fiscal 2021.",
                    
                    style = "font-size: 16px;padding: 0px;" 
                  )
                )
              )
            )
          ),
        ),
        
        #### tab con datos de solicitudes de órdenes de protección finales emitidas (OP_LEY148Genero) ####
        tabPanel(
          lowercaseTitle("Órdenes de Protección Emitidas por Parte y Sexo"), 
          br(), br(),
            sidebarLayout(
              sidebarPanel(
                style = "display: flex; flex-direction: column; align-items: center;",
                
                # # seleccionar valor de la variable
                div(
                  style = "width: 100%; display: flex; justify-content: center; margin-bottom: 20px;",  
                  div(
                    style = "text-align: center; display: inline-block;",  
                    # botón para seleccionar la parte
                    createDropdownCheckbox(
                      label = "Seleccione Parte(s):",
                      choices = OP_LEY148Genero$Parte,
                      selected = OP_LEY148Genero$Parte,
                      id = "trib_OP_LEY148Genero_Parte"
                    ),
                    # botón para seleccionar el sexo de la parte
                    createDropdownCheckbox(
                      label = "Seleccione Sexo:",
                      choices = OP_LEY148Genero$Sexo,
                      selected = OP_LEY148Genero$Sexo,
                      id = "trib_OP_LEY148Genero_Sexo"
                    )
                  )
                ),
                
                div(
                  style = "width: 100%; display: flex; flex-direction: column; align-items: center; padding-top: 0px;",
                  div(
                    style = "width: 100%; display: flex; justify-content: center; align-items: center;",
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-right: 10px",
                      # botón para seleccionar el año fiscal
                      createDropdownCheckbox_añoFiscal(
                        label = HTML("Seleccione <br> Año(s) Fiscal:"),
                        choices = OP_LEY148Genero$AñoFiscal,
                        selected = OP_LEY148Genero$AñoFiscal,
                        id = "trib_OP_LEY148Genero_AñoFiscal"
                      )
                    ),
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-left: 0px;", 
                      showDataCheckbox("showTable_OP_LEY148Genero")
                    )
                  )
                ),
                
                # Output UI para la tabla de datos
                uiOutput("dataTableUI_OP_LEY148Genero")
                
              ),
            
            # Sección principal con los gráficos
            mainPanel(
              style = "height: calc(100vh - 150px); padding-bottom: 10px;",
              fluidRow(
                column(12, 
                       div(id = "scrollable-plot", 
                           div(id = "plot-title", uiOutput("plot_title10")),
                           plotlyOutput("barPlot_OP_LEY148Genero"),  height = "100%"))
              ),
              tags$div(style = "padding-bottom: 10px;"),
              tags$div(
                style = "padding-bottom: 10px;",
                div(
                  class = "card",
                  style = "padding: 15px;color: white; background-color: #3e3f3a; border-radius: 5px; margin-top: 0; width: 100%;",
                  h4(
                    strong(actualizacion_tribunales6, style="margin: 0px;") 
                  ),
                  p(
                    "Los datos representados en esta gráfica corresponden a
                    las órdenes de protección emitidas bajo la Ley 148, 
                    por sexo, la parte y año fiscal. Los datos para cada año
                    fiscal se identifican con el año en que finaliza el mismo,
                    por ejemplo, para los datos del año fiscal 2020-2021, 
                    los datos son presentados como año fiscal 2021.",
                    
                    style = "font-size: 16px;padding: 0px;" 
                  )
                )
              )
            )
          ),
        ),
        
        
        #### tab con datos de Movimiento de Casos Criminales de Violencia Doméstica (tribCasosCrim) ####
        tabPanel(
          lowercaseTitle("Movimiento de Casos en Tribunal de Primera Instancia por Ley 54"), 
          br(), br(),
            sidebarLayout(
              sidebarPanel(
                style = "display: flex; flex-direction: column; align-items: center;",
                
                # # seleccionar valor de la variable
                div(
                  style = "width: 100%; display: flex; justify-content: center; margin-bottom: 20px;", 
                  div(
                    style = "text-align: center; display: inline-block;",  
                    # botón para seleccionar el Delito
                    createDropdownCheckbox(
                      label = "Seleccione Delito(s):",
                      choices = tribCasosCrim$Delito,
                      selected = tribCasosCrim$Delito,
                      id = "trib_tribCasosCrim_Delito"
                    ),
                    # botón para seleccionar el estado del caso
                    createDropdownCheckbox(
                      label = "Seleccione Estado del Caso:",
                      choices = tribCasosCrim$Casos,
                      selected = 1,
                      id = "trib_tribCasosCrim_Casos"
                    )
                  )
                ),
                
                div(
                  style = "width: 100%; display: flex; flex-direction: column; align-items: center; padding-top: 0px;",
                  div(
                    style = "width: 100%; display: flex; justify-content: center; align-items: center;",
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-right: 10px",
                      # botón para seleccionar el año fiscal
                      createDropdownCheckbox_añoFiscal(
                        label = HTML("Seleccione <br> Año(s) Fiscal:"),
                        choices = tribCasosCrim$AñoFiscal,
                        selected = tribCasosCrim$AñoFiscal,
                        id = "trib_tribCasosCrim_AñoFiscal"
                      )
                    ),
                    div(
                      style = "flex: 1; display: flex; justify-content: center; margin-left: 0px;", 
                      showDataCheckbox("showTable_tribCasosCrim")
                    )
                  )
                ),
                
                # Output UI para la tabla de datos
                uiOutput("dataTableUI_tribCasosCrim")
                
              ),
            
            # Sección principal con los gráficos
            mainPanel(
              style = "height: calc(100vh - 150px); padding-bottom: 10px;",
              fluidRow(
                column(12, 
                       div(id = "scrollable-plot", 
                           div(id = "plot-title", uiOutput("plot_title11")),
                           plotlyOutput("barPlot_tribCasosCrim"),  height = "100%"))
              ),
              tags$div(style = "padding-bottom: 10px;"),
              tags$div(
                style = "padding-bottom: 10px;",
                div(
                  class = "card",
                  style = "padding: 15px;color: white; background-color: #3e3f3a; border-radius: 5px; margin-top: 0; width: 100%;",
                  h4(
                    strong(actualizacion_tribunales7, style="margin: 0px;") 
                  ),
                  p(
                    "Los datos representados en esta gráfica corresponden al
                    movimiento de casos criminales de violencia doméstica en 
                    el tribunal de primera instancia según la ley Núm. 54 
                    por delito cometido y año fiscal. Los datos para cada año 
                    fiscal se identifican con el año en que finaliza el mismo,
                    por ejemplo, para los datos del año fiscal 2020-2021, 
                    los datos son presentados como año fiscal 2021.",
                    
                    style = "font-size: 16px;padding: 0px;" 
                  )
                )
              )
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
            column(12, DTOutput("dataTable_Def_trib"))
          )
        )
      )
    ),
  
  
  
    #### Tab del Centro de Ayuda a Víctimas de Violación ####
       #### tab con datos de Tendencia Anual de SAFE Kits por Estado de Querella ####
  tabPanel(
    lowercaseTitle("Centro Ayuda a Víctimas de Violación"),
    icon = icon("building-circle-exclamation"),
    tabsetPanel(
      
      tabPanel(
        lowercaseTitle("Tendencia de SAFEKits por estatus"), 
        br(), br(),
          sidebarLayout(
            sidebarPanel(
              style = "display: flex; flex-direction: column; align-items: center;",
              
              # # seleccionar valor de la variable
              div(
                style = "width: 100%; display: flex; justify-content: center; margin-bottom: 20px;",  
                div(
                  style = "text-align: center; display: inline-block;",  
                  # botón para seleccionar el estado de querella
                  createDropdownCheckbox(
                    label = "Seleccione Estado de Querella:",
                    choices = safekitsDF$Kits,
                    selected = safekitsDF$Kits,
                    id = "cavv_safekitsDF_Kits"
                  )
                )
              ),
              
              div(
                style = "width: 100%; display: flex; flex-direction: column; align-items: center; padding-top: 0px;",
                div(
                  style = "width: 100%; display: flex; justify-content: center; align-items: center;",
                  div(
                    style = "flex: 1; display: flex; justify-content: center; margin-right: 10px",
                    # botón para seleccionar el año
                    createDropdownCheckbox(
                      label = "Seleccione Año(s):",
                      choices = safekitsDF$Año,
                      selected = safekitsDF$Año,
                      id = "cavv_safekitsDF_Año"
                    )
                  ),
                  div(
                    style = "flex: 1; display: flex; justify-content: center; margin-left: 0px;", 
                    showDataCheckbox("showTable_safekitsDF")
                  )
                )
              ),
              
              # Output UI para la tabla de datos
              uiOutput("dataTableUI_safekitsDF")
              
            ),
          
          # Sección principal con los gráficos
          mainPanel(
            style = "height: calc(100vh - 150px); padding-bottom: 10px;",
            fluidRow(
              column(12, 
                     div(id = "scrollable-plot", 
                         div(id = "plot-title", uiOutput("plot_title22")),
                         plotlyOutput("barPlot_safekitsDF"),  height = "100%"))
            ),
            tags$div(style = "padding-bottom: 10px;"),
            tags$div(
              style = "padding-bottom: 10px;",
              div(
                class = "card",
                style = "padding: 15px;color: white; background-color: #3e3f3a; border-radius: 5px; margin-top: 0; width: 100%;",
                h4(
                  strong(actualizacion_cavv1, style="margin: 0px;") 
                ),
                p(
                  "Los datos representados en esta gráfica corresponden a la
                  tendencia del equipo de recolecta de evidencia de los
                  SAFE Kits en casos de violencia sexual, según el estado de querella,
                  desde el año 2019 al 2024.",
                  
                  style = "font-size: 16px;padding: 0px;" 
                )
              )
            )
          )
        )
      ),
      
      #### tab con datos de Tendencia Anual de SAFE Kits por Estado de Querella (Edades) ####
      tabPanel(
        lowercaseTitle("Tendencia de SAFEKits por categoría de edad"), 
        br(), br(),
        sidebarLayout(
          sidebarPanel(
            style = "display: flex; flex-direction: column; align-items: center;",
            
            # seleccionar valor de la variable
            div(
              style = "width: 100%; display: flex; justify-content: center; margin-bottom: 20px;",
              div(
                style = "text-align: center; display: inline-block;",
                # botón para seleccionar el estado de querella
                createDropdownCheckbox(
                  label = "Seleccione Categoría:",
                  choices = safekitsDF_edades$Categoria,
                  selected = safekitsDF_edades$Categoria,
                  id = "cavv_safekitsDF_edades_Categoria"
                )
              )
            ),
            
            div(
              style = "width: 100%; display: flex; flex-direction: column; align-items: center; padding-top: 0px;",
              div(
                style = "width: 100%; display: flex; justify-content: center; align-items: center;",
                div(
                  style = "flex: 1; display: flex; justify-content: center; margin-right: 10px",
                  # botón para seleccionar el año
                  createDropdownCheckbox(
                    label = "Seleccione Año(s):",
                    choices = safekitsDF_edades$Año,
                    selected = safekitsDF_edades$Año,
                    id = "cavv_safekitsDF_edades_Año"
                  )
                ),
                div(
                  style = "flex: 1; display: flex; justify-content: center; margin-left: 0px;", 
                  showDataCheckbox("showTable_safekitsDF_edades")
                )
              )
            ),
            
            # Output UI para la tabla de datos
            uiOutput("dataTableUI_safekitsDF_edades")
            
          ),
          
          # Sección principal con los gráficos
          mainPanel(
            style = "height: calc(100vh - 150px); padding-bottom: 10px;",
            fluidRow(
              column(12, 
                     div(id = "scrollable-plot", 
                         div(id = "plot-title", uiOutput("plot_title23")),
                         plotlyOutput("barPlot_safekitsDF_edades"),  height = "100%"))
            ),
            tags$div(style = "padding-bottom: 10px;"),
            tags$div(
              style = "padding-bottom: 10px;",
              div(
                class = "card",
                style = "padding: 15px;color: white; background-color: #3e3f3a; border-radius: 5px; margin-top: 0; width: 100%;",
                h4(
                  strong(actualizacion_cavv2, style="margin: 0px;") 
                ),
                p(
                  "Los datos representados en esta gráfica corresponden a la
                  tendencia del equipo de recolecta de evidencia de los
                  SAFE Kits en casos de violencia sexual para aquellos casos con querellas
                  segun la clasificacion del individuo (menor de edad o mayor de edad)
                  desde el año 2019 al 2024.",
                  
                  style = "font-size: 16px;padding: 0px;" 
                )
              )
            )
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
          column(12, DTOutput("dataTable_Def_cavv"))
        )
      )
    )    
  ),
      
  
  
  
  
    #### Tab Acerca del Dashboard ####
    tabPanel(
      lowercaseTitle("Acerca"),
      icon = icon("info-circle"),
      tabsetPanel(
        
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
              nombre = 'Génesis N. Pérez González',
              email = 'genesis.perez@estadisticas.pr',
              puesto = 'Programadora Estadística',
              grados = c(
                'BS Ciencia de Datos, Universidad de Puerto Rico Humacao',
                'BS Matemáticas Computacionales, Universidad de Puerto Rico Humacao'
              )
            ),
            
            authorTag(
              nombre = 'Aisha M. Cruz De Jesús',
              email = 'aisha.cruz@estadisticas.pr',
              puesto = 'Programadora Estadística',
              grados = 'BS Ciencias de Computos, Universidad de Puerto Rico Arecibo'
            ),
            
            authorTag(
              nombre = 'Manuel Mangual Martínez',
              email = 'manuel.mangual@estadisticas.pr',
              puesto = 'Gerente de Proyectos: Prevención, Apoyo, Rescate y Educación de la Violencia de Género',
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
              puesto = 'Analista de Datos: Proyectos de Prevención, Apoyo, Rescate y Educación de la Violencia de Género',
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
            style = "width: 100%",
            fluidRow(
              
              column(6,
                     style = "padding-left: 30px;", 
                     br(),
                     h2(strong("Instituto De Estadísticas De Puerto Rico")),
                     br(),
                     h4(
                       a(
                         href = "https://www.google.com/maps/place/Instituto+de+Estad%C3%ADsticas+de+Puerto+Rico/@18.4279999,-66.056444,17.5z/data=!4m5!3m4!1s0x8c0368a432af0b3d:0x274ac1c656b89f89!8m2!3d18.4275192!4d-66.0562994",
                         'Postal: P.O. Box 195484 | San Juan, PR 00919-5484'
                       )
                     ),
                     h4( a(href="tel:787-819-0730", 'Tel: (787) 819-0730')),
                     h4(a(href = "mailto:preguntas@estadisticas.pr", 'Email: preguntas@estadisticas.pr')),
                     h4('Horario de Oficina: lunes a viernes, 8:00 am a 4:30 pm'),
                     a(href='https://www.facebook.com/estadisticas.pr',
                       icon("facebook","fa-2x"),) ,
                     a(href='https://www.instagram.com/institutodeestadisticas/' ,
                       icon('instagram','fa-2x')),
                     a(href="https://twitter.com/EstadisticasPR",
                       icon('twitter','fa-2x')),
                     a(href="https://www.youtube.com/channel/UCIZggRtE5KK0z9D39FGZtyQ",
                       icon('youtube','fa-2x')),
                     br(),
                     column(5,
                            style = "padding-left: 0px;",
                            br(),
                            column(3,
                                   a(img(src = 'IEPRlocal.png', height = 250),
                                     href = "https://www.google.com/maps/place/Instituto+de+Estad%C3%ADsticas+de+Puerto+Rico/@18.4279999,-66.056444,17.5z/data=!4m5!3m4!1s0x8c0368a432af0b3d:0x274ac1c656b89f89!8m2!3d18.4275192!4d-66.0562994"),
                                   style = "padding-left: 0px; padding-top: 20px;")
                     )
                     ,
                     
                     column(1,
                            br(),
                            column(3,
                                   a(img(src='IEPRmap.png', height=250),
                                     href='https://www.google.com/maps/place/Instituto+de+Estad%C3%ADsticas+de+Puerto+Rico/@18.4279999,-66.056444,17.5z/data=!4m5!3m4!1s0x8c0368a432af0b3d:0x274ac1c656b89f89!8m2!3d18.4275192!4d-66.0562994'),
                                   style = "padding-left: 0px; padding-top: 20px;")
                     )
                     
              ),
              
              
              column(6,
                     br(),
                     style = "padding-left: 80px; padding-top: 50px;",
                     div(
                       class = "card",
                       style = "padding: 30px; background-color: lightgrey; border-radius: 5px; margin-top: 10px;width: 400px; height: 470px; display: flex; flex-direction: column; justify-content: space-between;",
                       div(
                         style = "flex-grow: 1; overflow: auto;",  
                         h3("Enviar un mensaje",style="margin-top: 0px; text-align: center;"),
                         div(style = "width: 100%; margin-bottom: 10px;",
                             textInput("name", "Nombre", placeholder = "Ingrese su nombre")
                         ),
                         div(style = "width: 100%; margin-bottom: 10px;",
                             textInput("email", "Correo Electrónico", placeholder = "Ingrese su correo electrónico")
                         ),
                         div(style = "width: 100%;",
                             textAreaInput("message", "Mensaje", "", rows = 5, placeholder = "Escriba su mensaje aquí")
                         )
                       ),
                       div(style = "padding-top: 20px; padding-bottom: 0px; text-align: center;",
                           actionButton("send", "Enviar", icon = icon("paper-plane")),
                           textOutput("response")
                       )
                     )
              )
            )
          )
        )
        
      )
    ),
  
  )
)





