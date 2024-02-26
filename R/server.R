# Server
cat("Loading Server from server.R...\n")
server <- function(input, output, session) {
  # Importar el contenido de server_helpers.R
  source("R/server_helpers.R")
  
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
  
  ### funcion para el boton de deseleccionar/seleccionar
  observeEvent(input$deselectAll_snmv, {
    updateCheckboxGroup(session, "checkGroup_snmv", input, homiEdad$edad)
  })
  
  # Grafico lineal del SNMV
  output$linePlot_snmv <- renderPlotly({
    p <- renderLinePlot(filtered_edad_snmv, "año", "casos", "edad", "edad",
                   "Evolución de Casos por Grupo de Edad y Año", "Año", "Casos")
    ggplotly(p, tooltip = c("x", "y", "color"))
  })
  
  # Grafico de barras del SNMV
  output$barPlot_snmv <- renderPlotly({
    p <- renderBarPlot(filtered_edad_año_snmv, "edad", "casos", "edad",
                  paste("Evolución de homicidios por Grupo de Edad en el Año", input$yearInput_snmv),
                  "Grupo de Edad", "Casos")
    
    ggplotly(p, tooltip = c("x", "y"))  # Especificamos qué información mostrar en el tooltip
  })
  
  # Data Table del SNMV
  output$dataTable_snmv <- renderDT({
    renderDataTable(filtered_edad_snmv())
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
  
  ### funcion para el boton de deseleccionar/seleccionar
  observeEvent(input$deselectAll_fam, {
    updateCheckboxGroup(session, "checkGroup_fam", input, dfMalt$Maltrato)
  })
  
  # crear gráfico lineal
  output$linePlot_fam <- renderPlotly({
    p <- renderLinePlot(filtered_data_fam, "Año", "Casos", "Maltrato", "Maltrato",
                   "Casos de Maltrato por Año y Tipo", "Año", "Casos", facet = TRUE, facet_var = "Sexo")
    ggplotly(p, tooltip = c("x", "y", "color"))
  })
  
  # crear gráfico de barras
  output$barPlot_fam <- renderPlotly({
    p <- renderBarPlot(data = filtered_data_año_fam, x = "Maltrato", y = "Casos", fill = "Sexo",
                  title = paste("Distribución de Tipos de Maltrato en el Año", input$yearInput_fam),
                  xlab = "Tipo de Maltrato", ylab = "Casos", fillLab = "Sexo")
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # Data Table del DeptFam
  output$dataTable_fam <- renderDT({
    renderDataTable(filtered_data_fam())
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
  
  ### funcion para el boton de deseleccionar/seleccionar
  observeEvent(input$deselectAll_just, {
    updateCheckboxGroup(session, "checkGroup_just", input, dfDeli$Delito)
  })
  
  # crear gráfico de barras
  # output$boxPlot_just <- renderPlotly({
  #   renderBoxPlot(
  #     data = filtered_data_just(),
  #     x = "Año",
  #     y = "Casos",
  #     color = "FISCALIA DISTRITO",
  #     title = "Distribución de Casos por Delito",
  #     xlab = "Año",
  #     ylab = "Casos",
  #     facet = TRUE,
  #     facet_var = "Delito"
  #   )
  # })
  
  output$boxPlot_just <- renderPlotly({
    p <- ggplot(filtered_data_just(), aes(x = Año, y = Casos)) +
      geom_boxplot() +
      #geom_jitter(width = 0.1, alpha = 0.5) +  # Añadir los scatters
      labs(
        title = "Distribución de Casos por Delito", x = "Año", y = "Casos") +
      geom_point(aes(color = `FISCALIA DISTRITO`)) +
      theme_minimal() +
      facet_wrap(~Delito, scales = "fixed")

    ggplotly(p, tooltip = c("x", "y", "color"))
  })
  
  # crear el grafico de barras
  output$barPlot_just <- renderPlotly({
    p <- renderBarPlot(filtered_data_año_just, x = "Delito", y = "Casos", fill = "Delito",
                       title = "Número de Casos por Delito (Año 2023)",
                       xlab = "Delito Cometido", ylab = "Número de Casos",
                       fillLab = "Delito Cometido", facet = TRUE, 
                       facet_var = "`FISCALIA DISTRITO`")
    ggplotly(p, tooltip = c("y", "fill"))  # Especificamos qué información mostrar en el tooltip
  })
  
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
  
  # Data table del DeptJust
  output$dataTable_just <- renderDT({
    renderDataTable(filtered_data_just())
  })
  
  observeEvent(input$yearInput_just, {
    updateSelectInput(session, "yearInput_just", selected = input$yearInput_just)
  })
  ########## Server de Prueba ##########
  # Data table del DeptJust
  
  output$dataTable_test <- renderDT({
    renderDataTable(starwars)
  })
  
  ## tab 2
  output$dataTable_test2 <- renderDT({
    renderDataTable(dfMalt)
  })
}
