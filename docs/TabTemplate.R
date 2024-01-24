#### Tab Template ####

#### UI ####
tabPanel(
  "Nombre de la Agencia",  # Cambiar por el nombre de la agencia
  tabsetPanel(
    
    # Subtab con datos específicos para el primer dataset de la agencia
    tabPanel(
      "Datos 1",  # Cambiar por el nombre del primer dataset
      # Título del Tab
      titlePanel("Título del Tab"),  # Cambiar por el título adecuado
      
      # Fuente de Datos, Actualización
      tags$span("Fuente: Fuente de Datos"), tags$br(),
      tags$span(paste0("Actualizado: ", actualizacion)), tags$br(),
      tags$span("Nota: Nota sobre los datos"),
      
      # Menu sidebar con widgets
      sidebarLayout(
        sidebarPanel(
          # botón para tener el checkbox en un menu dropdown 
          dropdownButton(
            circle = "FALSE",
            label = "Seleccione Grupo(s):",
            status = "default",
            width = 350,
            actionButton("deselectAll_nueva_agencia", "(De)seleccionar todo"),
            checkboxGroupInput(
              "checkGroup_nueva_agencia",
              label = h3("Seleccione Grupo(s)"),
              choices = levels(dataset$columna),
              selected = levels(dataset$columna)
            )
          ),
          
          hr(),
          selectInput("yearInput_nueva_agencia", "Seleccionar Año:", choices = unique(dataset$yearColumna))
        ),
        
        # Sección principal con los gráficos
        mainPanel(
          plotlyOutput("linePlot_nueva_agencia"),
          plotlyOutput("barPlot_nueva_agencia"),
          DTOutput("dataTable_nueva_agencia")
        )
      )
    ),
    
    # Subtab con datos específicos para el segundo dataset de la agencia
    tabPanel(
      "Datos 2"  # Cambiar por el nombre del segundo dataset
      # ... Estructura similar a la anterior para el segundo dataset
    ),
    
    # Subtab con datos específicos para el tercer dataset de la agencia
    tabPanel(
      "Datos 3"  # Cambiar por el nombre del tercer dataset
      # ... Estructura similar a la anterior para el tercer dataset
    )
  )
)

#### Server ####

# Server
  ########## Server de la Nueva Agencia ##########
  # Filtrar el conjunto de datos según los valores seleccionados del checkGroup_nueva_agencia
  filtered_data_nueva_agencia <- reactive({
    filter(dataset_nueva_agencia, columna_nueva_agencia %in% input$checkGroup_nueva_agencia)
  })
  
  # Filtrar el conjunto de datos según los valores seleccionados del año y el checkGroup_nueva_agencia
  filtered_data_año_nueva_agencia <- reactive({
    filter(dataset_nueva_agencia, 
           año_nueva_agencia %in% input$yearInput_nueva_agencia,
           columna_nueva_agencia %in% input$checkGroup_nueva_agencia)
  })
  
  # función que activa la opción de Seleccionar Todos
  observe({
    if ("Seleccionar Todos" %in% input$checkGroup_nueva_agencia) {
      updateCheckboxGroupInput(session, "checkGroup_nueva_agencia", choices = c("Seleccionar Todos", levels(dataset_nueva_agencia$columna_nueva_agencia)),
                               selected = levels(dataset_nueva_agencia$columna_nueva_agencia))
    }
  })
  
  # función que revierte la opción de seleccionar todos
  observeEvent(input$deselectAll_nueva_agencia, {
    updateCheckboxGroupInput(session, "checkGroup_nueva_agencia", selected = character(0))
  })
  
  # arreglar issue del tooltip duplicando la columna
  output$linePlot_nueva_agencia <- renderPlotly({
    # Gráfico de línea para la evolución de casos por grupo de columna y año
    p <- ggplot(filtered_data_nueva_agencia(), aes(x = año_nueva_agencia, y = casos, group = columna_nueva_agencia, color = columna_nueva_agencia)) +
      geom_line(size = 1.3) +
      geom_point(size = 1.5) +
      scale_fill_manual(values = colores_dataset_nueva_agencia) +
      #ylim(0, max(filtered_data_nueva_agencia()$casos) + 5) +  # Establecer límites del eje y entre 0 y el máximo de casos + 5
      labs(title = "Evolución de casos por grupo y año",
           x = "Año",
           y = "Casos") +
      theme_minimal() +
      theme(legend.position = "top")
    
    # Convertir ggplot a plotly y Personalizar leyenda
    p <- ggplotly(p) %>% 
      layout(legend = list(orientation = "h", x = 0.3, y = -0.15))
    p
  })
  
  # Gráfico de barras para mostrar el total de casos por grupo de columna
  output$barPlot_nueva_agencia <- renderPlotly({
    # Gráfico de barras para el total de casos por grupo de columna
    p <- ggplot(filtered_data_nueva_agencia(), aes(x = reorder(columna_nueva_agencia, -casos), y = casos, fill = columna_nueva_agencia)) +
      geom_bar(stat = "identity", position = "stack", width = 0.7) +
      scale_fill_manual(values = colores_dataset_nueva_agencia) +
      labs(title = "Total de casos por grupo",
           x = "Grupo",
           y = "Casos") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Convertir ggplot a plotly y Personalizar leyenda
    p <- ggplotly(p) %>% 
      layout(legend = list(orientation = "h", x = 0.3, y = -0.15))
    p
  })
  
  # Tabla de datos con información detallada
  output$dataTable_nueva_agencia <- renderDT({
    datatable(
      filtered_data_año_nueva_agencia(),
      extensions = c('Buttons'), # Asegúrate de incluir la extensión 'Buttons'
      options = list(
        pageLength = 10,
        lengthMenu = c(5, nrow(filtered_data_año_nueva_agencia()) / 2, nrow(filtered_data_año_nueva_agencia())),
        scrollX = TRUE,
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        autoWidth = FALSE,
        ordering = TRUE,
        dom = 'lftpB', 
        buttons = c('copy', 'csv', 'excel')
        ),
      rownames = FALSE
    )
  })

