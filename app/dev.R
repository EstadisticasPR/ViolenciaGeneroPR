#### Tab Template ####

#### Tab del Departamento de Justicia ####
tabPanel(
  "Departamento de Justicia",
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
          # el checkbox 
          checkboxGroupInput(
            "checkGroup_just",
            label = h3("Seleccione el/los Articulo(s)"),
            choices = c("Seleccionar Todos", levels(dfDeli$Delito)),
            selected = "Seleccionar Todos"
          ),
          # botón de deselección
          actionButton("deselectAll_just", "Deseleccionar todo"),
          hr(),
          fluidRow(column(3, verbatimTextOutput("value"))),
          # botón de seleccionar input
          selectInput("yearInput_just", "Seleccionar Año:", choices = unique(dfDeli$Año))
        ),
        # Sección principal con los gráficos
        mainPanel(
          plotlyOutput("boxPlot_just"),
          plotlyOutput("barPlot_just"),
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
    tabPanel(
      "Yeiza"  # Cambiar por el nombre del tercer dfDeli
      # ... Estructura similar a la anterior para el tercer dfDeli
    )
  )
)

########## Server del Departamento de Justicia ##########
  # Filtrar el conjunto de datos según los valores seleccionados del checkGroup_nueva_agencia
  filtered_data_just <- reactive({
    filter(dfDeli,
           Delito %in% input$checkGroup_just)
  })
  
  # Filtrar el conjunto de datos según los valores seleccionados del año y el checkGroup_nueva_agencia
  filtered_data_año_just <- reactive({
    filter(dfDeli, 
           Año %in% input$yearInput_just,
           Delito %in% input$checkGroup_just)
  })
  
  # función que activa la opción de Seleccionar Todos
  observe({
    if("Seleccionar Todos" %in% input$checkGroup_just) {
      updateCheckboxGroupInput(session, "checkGroup_just", choices = c("Seleccionar Todos", levels(dfDeli$Delito)),
                               selected = levels(dfDeli$Delito))
    }
  })
  
  # función que revierte la opción de seleccionar todos
  observeEvent(input$deselectAll_just, {
    updateCheckboxGroupInput(session, "checkGroup_just", selected = character(0))
  })
  
  # # arreglar issue del tooltip duplicando la columna
  # output$boxPlot_just <- renderPlotly({
  #   # Gráfico de línea para la evolución de casos por grupo de columna y año
  #   p <- ggplot(filtered_data_just(), aes(x = Año, y = casos, group = Delito, color = Delito)) +
  #     geom_line(size = 1.3) +
  #     geom_point(size = 1.5) +
  #     #scale_fill_manual(values = colores_dfDeli_nueva_agencia) +
  #     #ylim(0, max(filtered_data_nueva_agencia()$casos) + 5) +  # Establecer límites del eje y entre 0 y el máximo
  #     theme_minimal() +
  #     labs(title = "Evolución de Casos por Grupo de Columna y Año", x = "Año", y = "Casos", color = "Grupos de Columna") +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  #   
  #   ggplotly(p)
  # })
  
  output$boxPlot_just <- renderPlotly({
    p <- ggplot(filtered_data_just(), aes(x = factor(Año), y = Casos, color = Delito)) +
      geom_boxplot(outlier.shape = NA) +  # Eliminar los puntos atípicos del boxplot
      geom_jitter(width = 0.2, alpha = 0.5) +  # Añadir los scatters
      labs(title = "Distribución de Casos por Delito", x = "Año", y = "Casos") +
      theme_minimal() +
      facet_wrap(~Delito, scales = "fixed")
    
    ggplotly(p)
  })
  
  # arreglar issue del tooltip duplicando la columna
  output$barPlot_just <- renderPlotly({
    p <- ggplot(filtered_data_año_just(), aes(x = Delito, y = Casos, fill = Delito)) +
      geom_bar(stat = "identity") +
      labs(title = "Número de Casos por Delito (Año 2023)", x = "Delito Cometido", y = "Número de Casos", fill = "Delito Cometido") +
      facet_wrap(~ Año, scales = "fixed") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("Delito", "Casos"))  # Especificamos qué información mostrar en el tooltip
  })
  
  output$dataTable_just <- renderDT({
    datatable(
      filtered_data_just(),
      extensions = c('Buttons'), # Asegúrate de incluir la extensión 'Buttons'
      options = list(
        pageLength = 10,
        lengthMenu = c(10, nrow(filtered_data_just()) / 2, nrow(filtered_data_just())),
        scrollX = TRUE,
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        autoWidth = FALSE,
        ordering = TRUE,
        dom = 'lftpB', 
        buttons = c('copy', 'csv', 'excel')
      )
    ) %>% 
      formatRound(columns = 2:ncol(filtered_data_just()), digits = 0, mark = ",")
  })
  
  observeEvent(input$yearInput_just, {
    updateSelectInput(session, "yearInput_just", selected = input$yearInput_just)
  })