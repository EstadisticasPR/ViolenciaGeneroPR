# User Interface 
cat("Loading User Interface from ui_helpers.R...\n")
# Importar funciones auxiliares de ui_helpers.R
source("R/ui_helpers.R")
ui <- fluidPage(
  ### El theme (colores) de la app ###
  theme = shinytheme("sandstone"),
  
  #### Encabezado ####
  header(),
  
  # Titulo de la app
  navbarPage(
    "",
    
    #### Tab del Sistema de Notificación de Muertes Violentas ####
    tabPanel_snmv(),
    
    #### Tab del Departamento de la Familia ####
    tabPanel_fam(),
    
    #### Tab del Departamento de Justicia ####
    tabPanel_just(),
    
    #### Tab del Departamento del Trabajo y Recursos Humanos ####
    tabPanel_trab(),
    
    #### Tab Acerca del Dashboard ####
    tabPanel_about(),
    
    #### Tab de prueba ####
    tabPanel(
      "test",
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
