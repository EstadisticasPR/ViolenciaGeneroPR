# Cargar el contenido de global.R
source("global.R")

# Server
server <- function(input, output, session) {
  
  # Filtrar el conjunto de datos según los valores seleccionados del checkGroup
  filtered_edad_homi <- reactive({
    filter(homiEdad, edad %in% input$checkGroup)
  })
  
  # Filtrar el conjunto de datos según los valores seleccionados del año y el checkGroup
  filtered_edad_año <- reactive({
    filter(homiEdad, 
           año %in% input$yearInput,
           edad %in% input$checkGroup)
  })
  
  # función que activa la opción de Seleccionar Todos
  observe({
    if("Seleccionar Todos" %in% input$checkGroup) {
      updateCheckboxGroupInput(session, "checkGroup", choices = c("Seleccionar Todos", levels(homiEdad$edad)),
                               selected = levels(homiEdad$edad))
    }
  })
  
  # función que revierte la opción de seleccionar todos
  observeEvent(input$deselectAll, {
    updateCheckboxGroupInput(session, "checkGroup", selected = character(0))
  })
  
  # arreglar issue del tooltip duplicando la edad
  output$linePlot <- renderPlotly({
    # Gráfico de línea para la evolución de casos por grupo de edad y año
    p <- ggplot(filtered_edad_homi(), aes(x = año, y = casos, group = edad, color = edad)) +
      geom_line(size = 1.3) +
      geom_point(size = 1.5) +
      scale_fill_manual(values = colores_homiEdad) +
      #ylim(0, max(filtered_edad_homi()$casos) + 5) +  # Establecer límites del eje y entre 0 y el maximo
      theme_minimal() +
      labs(title = "Evolución de Casos por Grupo de Edad y Año", x = "Año", y = "Casos", color = "Grupos de Edad") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # arreglar issue del tooltip duplicando la edad
  output$barPlot <- renderPlotly({
    p <- ggplot(filtered_edad_año(), aes(x = edad, y = casos, fill = edad)) +
      geom_bar(stat = "identity") +
      #ylim(0, max(filtered_edad_homi()$casos) + 5) +
      labs(title = paste("Evolución de homicidios por Grupo de Edad en el Año", input$yearInput),
           x = "Grupo de Edad", y = "Casos", fill = "Grupos de Edad") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = c("edad", "casos"))  # Especificamos qué información mostrar en el tooltip
  })
  
  output$dataTable <- renderDT({
    datatable(filtered_edad_homi(),
              options = list(pageLength = 10, lengthMenu = c(5, 10, 15)))
  })
  
  observeEvent(input$yearInput, {
    updateSelectInput(session, "yearInput", selected = input$yearInput)
  })
}