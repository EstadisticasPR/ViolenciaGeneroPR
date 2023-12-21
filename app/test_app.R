library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# Datos de ejemplo
homiEdad_long_xtotals <- data.frame(
  edad = factor(c("Menos de 15 años", "Menos de 15 años", "Menos de 15 años", "Menos de 15 años", "15 a 19", "15 a 19", "15 a 19", "15 a 19", "20 a 24", "20 a 24")),
  año = factor(c(2017, 2018, 2019, 2020, 2017, 2018, 2019, 2020, 2017, 2018)),
  casos = c(0, 0, 1, 2, 4, 4, 2, 5, 7, 11)
)

# UI
ui <- fluidPage(
  titlePanel("Análisis de Casos por Grupo de Edad y Año"),
  sidebarLayout(
    sidebarPanel(
      selectInput("yearInput", "Seleccionar Año:", choices = unique(homiEdad_long_xtotals$año))
    ),
    mainPanel(
      plotOutput("linePlot"),
      plotOutput("barPlot"),
      DTOutput("dataTable")
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    subset(homiEdad_long_xtotals, año == input$yearInput)
  })
  
  output$linePlot <- renderPlot({
    ggplot(filtered_data(), aes(x = año, y = casos, group = edad, color = edad)) +
      geom_line(size = 1.3) +
      geom_point(size = 1.5) +
      theme_minimal() +
      labs(title = "Evolución de Casos por Grupo de Edad y Año", x = "Año", y = "Casos", color = "Grupos de Edad") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$barPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = edad, y = casos, fill = edad)) +
      geom_bar(stat = "identity") +
      labs(title = "Cantidad de Casos por Grupo de Edad", x = "Grupo de Edad", y = "Casos", fill = "Grupos de Edad") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$dataTable <- renderDT({
    datatable(filtered_data())
  })
}

# Lanzar la app Shiny
shinyApp(ui = ui, server = server)
