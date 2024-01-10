# Cargar el contenido de global.R
source("global.R")

# Server
server <- function(input, output, session) {
  
  ########## Server del Sistema de Notificación de Muertes Violentas ##########
  # Filtrar el conjunto de datos según los valores seleccionados del checkGroup_snmv
  filtered_edad_snmv <- reactive({
    filter(homiEdad, edad %in% input$checkGroup_snmv)
  })
  
  # Filtrar el conjunto de datos según los valores seleccionados del año y el checkGroup_snmv
  filtered_edad_año_snmv <- reactive({
    filter(homiEdad, 
           año %in% input$yearInput_snmv,
           edad %in% input$checkGroup_snmv)
  })
  
  # función que activa la opción de Seleccionar Todos
  observe({
    if("Seleccionar Todos" %in% input$checkGroup_snmv) {
      updateCheckboxGroupInput(session, "checkGroup_snmv", choices = c("Seleccionar Todos", levels(homiEdad$edad)),
                               selected = levels(homiEdad$edad))
    }
  })
  
  # función que revierte la opción de seleccionar todos
  observeEvent(input$deselectAll_snmv, {
    updateCheckboxGroupInput(session, "checkGroup_snmv", selected = character(0))
  })
  
  # arreglar issue del tooltip duplicando la edad
  output$linePlot_snmv <- renderPlotly({
    # Gráfico de línea para la evolución de casos por grupo de edad y año
    p <- ggplot(filtered_edad_snmv(), aes(x = año, y = casos, group = edad, color = edad)) +
      geom_line(size = 1.3) +
      geom_point(size = 1.5) +
      scale_fill_manual(values = colores_homiEdad) +
      #ylim(0, max(filtered_edad_snmv()$casos) + 5) +  # Establecer límites del eje y entre 0 y el maximo
      theme_minimal() +
      labs(title = "Evolución de Casos por Grupo de Edad y Año", x = "Año", y = "Casos", color = "Grupos de Edad") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = c("x", "y", "color"))
  })
  
  # arreglar issue del tooltip duplicando la edad
  output$barPlot_snmv <- renderPlotly({
    p <- ggplot(filtered_edad_año_snmv(), aes(x = edad, y = casos, fill = edad)) +
      geom_bar(stat = "identity") +
      #ylim(0, max(filtered_edad_snmv()$casos) + 5) +
      labs(title = paste("Evolución de homicidios por Grupo de Edad en el Año", input$yearInput_snmv),
           x = "Grupo de Edad", y = "Casos", fill = "Grupos de Edad") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = c("x", "y"))  # Especificamos qué información mostrar en el tooltip
  })
  
  output$dataTable_snmv <- renderDT({
    datatable(
      filtered_edad_snmv(),
      extensions = c('Buttons'), # Asegúrate de incluir la extensión 'Buttons'
      options = list(
        pageLength = 10,
        lengthMenu = c(10, nrow(filtered_edad_snmv()) / 2, nrow(filtered_edad_snmv())),
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
      formatRound(columns = 2:ncol(filtered_edad_snmv()), digits = 0, mark = ",")
  })
  
  observeEvent(input$yearInput_snmv, {
    updateSelectInput(session, "yearInput_snmv", selected = input$yearInput_snmv)
  })
  
  ########## Server del Departamento de la Familia ##########
  # Filtrar el conjunto de datos según los valores seleccionados del checkGroup_fam
  filtered_data_fam <- reactive({
    filter(dfMalt, Maltrato %in% input$checkGroup_fam)
  })
  
  # Filtrar el conjunto de datos según los valores seleccionados del año y el checkGroup
  filtered_data_año_fam <- reactive({
    filter(dfMalt, 
           Año %in% input$yearInput_fam,
           Maltrato %in% input$checkGroup_fam)
  })
  
  observe({
    if ("Seleccionar Todos" %in% input$checkGroup_fam) {
      updateCheckboxGroupInput(
        session, "checkGroup_fam",
        choices = c("Seleccionar Todos", levels(dfMalt$Maltrato)),
        selected = levels(dfMalt$Maltrato)
      )
    }
  })
  
  observeEvent(input$deselectAll_fam, {
    updateCheckboxGroupInput(session, "checkGroup_fam", selected = character(0))
  })
  
  output$linePlot_fam <- renderPlotly({
    p <- ggplot(filtered_data_fam(), aes(x = Año, y = Casos, color = Maltrato, group = Maltrato)) +
      geom_line(linewidth = 1) +
      geom_point() +
      labs(title = "Casos de Maltrato por Año y Tipo", x = "Año", y = "Casos", color = "Tipo de Maltrato") +
      theme_minimal() +
      facet_wrap(~Sexo, scales = "fixed", drop = FALSE) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = c("x", "y", "color"))
  })
  
  output$barPlot_fam <- renderPlotly({
    p <- ggplot(filtered_data_año_fam(), aes(x = Maltrato, y = Casos, fill = Sexo)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = paste("Distribución de Tipos de Maltrato en el Año", input$yearInput_fam),
           x = "Tipo de Maltrato", y = "Casos", fill = "Sexo") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  output$dataTable_fam <- renderDT({
    datatable(
      filtered_data_fam(),
      extensions = c('Buttons'),
      options = list(
        pageLength = 10,
        lengthMenu = c(10, nrow(filtered_data_fam()) / 2, nrow(filtered_data_fam())),
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
      formatRound(columns = 2:ncol(filtered_data_fam()), digits = 0, mark = ",")
  })
  
  observeEvent(input$yearInput_fam, {
    updateSelectInput(session, "yearInput_fam", selected = input$yearInput_fam)
  })
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
    p <- ggplot(filtered_data_just(), aes(x = Año, y = Casos)) +
      geom_boxplot() +  # Eliminar los puntos atípicos del boxplot
      #geom_jitter(width = 0.2, alpha = 0.5) +  # Añadir los scatters
      labs(
        title = "Distribución de Casos por Delito", x = "Año", y = "Casos") +
      geom_point(aes(color = `FISCALIA DISTRITO`)) +
      theme_minimal() +
      facet_wrap(~Delito, scales = "fixed")
    
    ggplotly(p, tooltip = c("x", "y", "color"))
  })
  
  # arreglar issue del tooltip duplicando la columna
  output$barPlot_just <- renderPlotly({
    p <- ggplot(filtered_data_año_just(), aes(x = Delito, y = Casos, fill = Delito)) +
      geom_bar(stat = "identity") +
      labs(title = "Número de Casos por Delito (Año 2023)", x = "Delito Cometido", y = "Número de Casos", fill = "Delito Cometido") +
      facet_wrap(~ Año, scales = "fixed") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y", "fill"))  # Especificamos qué información mostrar en el tooltip
  })
  
  # need to add tooltip functionality; ggplotly returns an error 
  output$deliPlot_just <- renderPlot({
    filtered_data <- subset(dfDeli, Año == input$yearInput_just & Delito == input$checkGroup_just)
    
    p <- ggplot(filtered_data, aes(x = `FISCALIA DISTRITO`, y = Casos, fill = `FISCALIA DISTRITO`)) +
      geom_bar(stat = "identity", width = 0.7) +
      labs(
        title = paste("Distribución de Casos por Distrito para el Delito", input$checkGroup_just, "en el", input$yearInput_just),
        x = "Fiscalía Distrito",
        y = "Casos",
        fill = "Distrito Fiscal"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(p)
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
}
