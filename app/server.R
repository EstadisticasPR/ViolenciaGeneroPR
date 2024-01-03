# Cargar el contenido de global.R
source("global.R")

# Server
server <- function(input, output, session) {
  
  # Filtrar el conjunto de datos según los valores seleccionados del checkGroup
  filtered_data <- reactive({
    filter(df, edad %in% input$checkGroup)
  })
  
  # Filtrar el conjunto de datos según los valores seleccionados del año y el checkGroup
  filtered_año <- reactive({
    filter(df, 
           año %in% input$yearInput,
           edad %in% input$checkGroup)
  })
  
  # función que activa la opción de Seleccionar Todos
  observe({
    if("Seleccionar Todos" %in% input$checkGroup) {
      updateCheckboxGroupInput(session, "checkGroup", choices = c("Seleccionar Todos", levels(df$edad)),
                               selected = levels(df$edad))
    }
  })
  
  output$linePlot <- renderPlot({
    # Gráfico de línea para la evolución de casos por grupo de edad y año
    ggplot(filtered_data(), aes(x = año, y = casos, group = edad, color = edad)) +
      geom_line(size = 1.3) +
      geom_point(size = 1.5) +
      theme_minimal() +
      labs(title = "Evolución de Casos por Grupo de Edad y Año", x = "Año", y = "Casos", color = "Grupos de Edad") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$barPlot <- renderPlot({
    # Gráfico de barras para la cantidad de casos por grupo de edad en el año seleccionado
    ggplot(filtered_año(), aes(x = edad, y = casos, fill = edad)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Cantidad de Casos por Grupo de Edad en el año", input$yearInput),
           x = "Grupo de Edad", y = "Casos", fill = "Grupos de Edad") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$dataTable <- renderDT({
    datatable(filtered_data(),
              options = list(pageLength = 10, lengthMenu = c(5, 10, 15)))
  })
  
  observeEvent(input$yearInput, {
    updateSelectInput(session, "yearInput", selected = input$yearInput)
  })
}