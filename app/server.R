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
    
    ggplotly(p)
  })
  
  # arreglar issue del tooltip duplicando la edad
  output$barPlot_snmv <- renderPlotly({
    p <- ggplot(filtered_edad_año_snmv(), aes(x = edad, y = casos, fill = edad)) +
      geom_bar(stat = "identity") +
      #ylim(0, max(filtered_edad_snmv()$casos) + 5) +
      labs(title = paste("Evolución de homicidios por Grupo de Edad en el Año", input$yearInput_snmv),
           x = "Grupo de Edad", y = "Casos", fill = "Grupos de Edad") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = c("edad", "casos"))  # Especificamos qué información mostrar en el tooltip
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
    
    ggplotly(p)
  })
  
  output$barPlot_fam <- renderPlotly({
    p <- ggplot(filtered_data_año_fam(), aes(x = Maltrato, y = Casos, fill = Sexo)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = paste("Distribución de Tipos de Maltrato en el Año", input$yearInput_fam),
           x = "Tipo de Maltrato", y = "Casos", fill = "Sexo") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
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
}
