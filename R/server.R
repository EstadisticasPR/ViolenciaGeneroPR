# Server
cat("Loading Server from server.R...\n")
server <- function(input, output, session) {
  
  ########## Server del Sistema de Notificación de Muertes Violentas ##########
  #### Tab de Homicidios por Grupo de Edad (homiEdad) ####
  
  # Filtro para el dataset según los valores de año y edad
  homiEdad_filt <- reactive({
    filter(homiEdad, 
           año %in% input$checkGroup_snmv_homiEdad_año,
           edad %in% input$checkGroup_snmv_homiEdad_edad)
  })
  
  ### lógica para el boton de deseleccionar/seleccionar edad
  observeEvent(input$deselectAll_snmv_homiEdad_edad, {
    updateCheckboxGroup(session, "checkGroup_snmv_homiEdad_edad", input, homiEdad$edad)
  })
  
  ### lógica para el boton de deseleccionar/seleccionar año
  observeEvent(input$deselectAll_snmv_homiEdad_año, {
    updateCheckboxGroup(session, "checkGroup_snmv_homiEdad_año", input, homiEdad$año)
  })
  
  # Grafico lineal del SNMV
  # output$linePlot_snmv <- renderPlotly({
  #   p <- renderLinePlot(homiEdad_filt, "año", "casos", "edad", "edad",
  #                       "Evolución de Casos por Grupo de Edad y Año", "Año", "Casos")
  #   ggplotly(p, tooltip = c("x", "y", "color"))
  # })
  
  # Colores de las edades
  homiEdad_fill_edad <- setColorFill(homiEdad, "edad")
  # Grafico de barras de homiEdad
  output$barPlot_snmv <- renderPlotly({
    p <- renderBarPlottest(homiEdad_filt, "año", "casos", "edad",
                       paste("Evolución de homicidios por Grupo de Edad en el Año", input$yearInput_snmv),
                       "Grupo de Edad", "Casos", colorFill = homiEdad_fill_edad)
    
    ggplotly(p, tooltip = c("x", "y", "fill"))  # Especificamos qué información mostrar en el tooltip
  })
  
  # Data Table de homiEdad
  output$dataTable_snmv <- renderDT({
    renderDataTable(homiEdad_filt())
  })
  
  #### Tab de Tipo de Muerte (inci) ####
  
  # Filtro para el dataset según los valores de año y el tipo de incidente
  inci_filt <- reactive({
    filter(inci,
           tipo %in% input$checkGroup_snmv_inci_tipo,
           año %in% input$checkGroup_snmv_inci_año)
  })
  
  ### lógica para el botón de deseleccionar/seleccionar tipo de incidente
  observeEvent(input$deselectAll_snmv_inci_tipo, {
    updateCheckboxGroup(session, "checkGroup_snmv_inci_tipo", input, inci$tipo)
  })
  
  ### lógica para el botón de deseleccionar/seleccionar el año
  observeEvent(input$deselectAll_snmv_inci_año, {
    updateCheckboxGroup(session, "checkGroup_snmv_inci_año", input, inci$año)
  })
  
  # colores de los tipos de incidentes
  inci_fill_sexo <- setColorFill(inci, "tipo")
  
  # Gráfico de barras de incidentes
  output$barPlot_snmv_inci <- renderPlotly({
    p <- renderBarPlottest(inci_filt, x = "año", y = "casos", fill = "tipo",
                       paste("Comparación de incidentes violentos a lo largo de los Años"),
                       xlab = "Año", ylab = "Número de Casos", fillLab = "Tipo de Incidente",
                       colorFill = inci_fill_sexo
                       )
    
    ggplotly(p, tooltip = c("fill", "x", "y"))
  })
  
  # Data Table del SNMV
  output$dataTable_snmv_inci <- renderDT({
    renderDataTable(inci_filt())
  })
  
  ########## Server del Departamento de la Familia ##########
  #### Tab de Maltrato (dfMalt) ####
  
  # Filtrar el conjunto de datos según los valores seleccionados del checkGroup_fam
  filtered_data_fam <- reactive({
    filter(dfMalt, Maltrato %in% input$checkGroup_fam_dfMalt_tipo)
  })
  
  # Filtrar el conjunto de datos según los valores seleccionados del año y el checkGroup
  filtered_data_año_fam <- reactive({
    filter(dfMalt, 
           Año %in% input$checkGroup_fam_dfMalt_año,
           Maltrato %in% input$checkGroup_fam_dfMalt_tipo)
  })
  
  # funcion para el boton de deseleccionar/seleccionar tipo de maltrato
  observeEvent(input$deselectAll_fam_dfMalt_tipo, {
    updateCheckboxGroup(session, "checkGroup_fam_dfMalt_tipo", input, dfMalt$Maltrato)
  })
  
  # funcion para el boton de deseleccionar/seleccionar año
  observeEvent(input$deselectAll_fam_dfMalt_año, {
    updateCheckboxGroup(session, "checkGroup_fam_dfMalt_año", input, dfMalt$Año)
  })
  
  # crear gráfico lineal
  output$linePlot_fam <- renderPlotly({
    p <- renderLinePlot(filtered_data_fam, "Año", "Casos", "Maltrato", "Maltrato",
                   "Casos de Maltrato por Año y Tipo", "Año", "Casos")
    p <- p + facet_wrap(~Sexo)
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
  ########## Server del Departamento de Justicia ##########
  #### Tab de Delitos (dfDeli) ####
  # Filtrar el conjunto de datos según los valores seleccionados del checkGroup_nueva_agencia
  filtered_data_just <- reactive({
    filter(dfDeli,
           Delito %in% input$checkGroup_just)
  })
  
  # Filtrar el conjunto de datos según los valores seleccionados del año y el checkGroup_nueva_agencia
  filtered_data_año_just <- reactive({
    filter(dfDeli, 
           Año %in% input$checkGroup_just_dfDeli_año,
           Delito %in% input$checkGroup_just)
  })
  
  # filtered_map_just <- reactive({
  #   filter(mapaDeli, 
  #          Año %in% input$yearInput_just,
  #          Delito %in% input$checkGroup_just)
  # })
  
  filtered_map_just <- reactive({
    filter(mapaDeli, 
           Año %in% input$checkGroup_just_dfDeli_año,
           Delito %in% input$checkGroup_just)
  })
  
  #### funcion para el boton de deseleccionar/seleccionar
  observeEvent(input$deselectAll_just, {
    updateCheckboxGroup(session, "checkGroup_just", input, dfDeli$Delito)
  })
  
  # crear gráfico de caja
  # output$boxPlot_just <- renderPlotly({
  #   renderBoxPlot(
  #     data = filtered_data_just(),
  #     x = "Año",
  #     y = "Casos",
  #     color = "FISCALIA DISTRITO",
  #     title = "Distribución de Casos por Delito",
  #     xlab = "Año",
  #     ylab = "Casos"
  #   )
  #   p <- p + facet_wrap(~Delito)
  #   ggplotly(p)
  # })
  
  # output$boxPlot_just <- renderPlotly({
  #   p <- ggplot(filtered_data_just(), aes(x = Año, y = Casos)) +
  #     geom_boxplot() +
  #     #geom_jitter(width = 0.1, alpha = 0.5) +  # Añadir los scatters
  #     labs(
  #       title = "Distribución de Casos por Delito", x = "Año", y = "Casos") +
  #     geom_point(aes(color = `FISCALIA DISTRITO`)) +
  #     theme_minimal() +
  #     facet_wrap(~Delito, scales = "fixed")
  # 
  #   ggplotly(p, tooltip = c("x", "y", "color"))
  # })
  
  #crear el grafico de barras
  output$barPlot_just <- renderPlotly({
    p <- renderBarPlot(filtered_data_año_just, x = "Delito", y = "Casos", fill = "Delito",
                       title = "Número de Casos por Delito (Año 2023)",
                       xlab = "Delito Cometido", ylab = "Número de Casos",
                       fillLab = "Delito Cometido")
    p <- p + facet_wrap(~`FISCALIA DISTRITO`)
    ggplotly(p, tooltip = c("y", "fill"))
  })
  
  #crear el grafico de barras
  # output$facet_bartest <- renderPlotly({
  #   p <- renderBoxPlot(filtered_data_año_just, x = "Año", y = "Casos", fill = "Delito",
  #                      title = "Número de Casos por Delito (Año 2023)",
  #                      xlab = "Delito Cometido", ylab = "Número de Casos",
  #                      fillLab = "Delito Cometido", facet = TRUE,
  #                      facet_var = "Delito")
  #   ggplotly(p, tooltip = c("y", "fill"))
  # })
  
  
  output$map_just <- renderPlotly({
    p <- renderMap(
      data = filtered_map_just, fill = Casos,
      title = paste("Número de Casos por Delito"),
      fill_lab = "Delito Cometido",
      light_color = "pink",
      dark_color = "darkred"
    )
    ggplotly(p + facet_wrap(~Año), tooltip = c("text")) %>%
      style(
        text = paste("Fiscalía:", filtered_map_just()$GROUP, "<br>",
                     "Casos:", filtered_map_just()$Casos, "<br>",
                     "Municipio:", filtered_map_just()$NAME)
      )
    # ggplotly(p + facet_wrap(~Año) +
    #                 geom_sf_text(aes(label = NULL, text = paste("Fiscalía:", GROUP, "<br>",
    #                                                             "Casos:", Casos, "<br>",
    #                                                             "Municipio:", NAME)), color = "black"),
    #               tooltip = "text")
    # p <- ggplotly(p + facet_wrap(~Año) +
    #                 geom_sf_text(aes(label = paste("Fiscalía:", GROUP, "<br>",
    #                                                "Casos:", Casos, "<br>",
    #                                                "Municipio:", NAME)), size = 0, color = "black"),
    #                 tooltip = "text")
    # p$x$data[[1]]$hoverinfo <- "text"
    # p
  })
  
  # output$deliPlot_just <- renderPlot({
  #   filtered_data <- subset(dfDeli, Año == input$yearInput_just & Delito == input$checkGroup_just)
  #   
  #   p <- ggplot(filtered_data, aes(x = `FISCALIA DISTRITO`, y = Casos, fill = `FISCALIA DISTRITO`)) +
  #     geom_bar(stat = "identity", width = 0.7) +
  #     labs(
  #       title = paste("Distribución de Casos por Distrito para el Delito", input$checkGroup_just, "en el", input$yearInput_just),
  #       x = "Fiscalía Distrito",
  #       y = "Casos",
  #       fill = "Distrito Fiscal"
  #     ) +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  #   
  #   print(p)
  # })
  
  # Data table del DeptJust
  output$dataTable_just <- renderDT({
    renderDataTable(filtered_map_just())
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
  
  ########## Server del Departamento del Trabajo y Recursos Humanos ##########
  #### Tab de Participación Laboral (parLab) ####
  
  # Filtrar el conjunto de datos según los valores seleccionados del año y el tipo de incidente
  parLab_filt <- reactive({
    filter(parLab,
           Año %in% input$checkGroup_trab_parLab_año,
           Sexo %in% input$checkGroup_trab_parLab_sexo)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_trab_parLab_año, {
    updateCheckboxGroup(session, "checkGroup_trab_parLab_año", input, parLab$Año)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de sexo
  observeEvent(input$deselectAll_trab_parLab_sexo, {
    updateCheckboxGroup(session, "checkGroup_trab_parLab_sexo", input, parLab$Sexo)
  })
  
  # Grafico de barras
  output$barPlot_trab_parLab <- renderPlotly({
    p <- renderBarPlot(parLab_filt, x = "Año", y = "Tasa", fill = "Sexo",
                       paste("Tasa de participación laboral según el año natural y el sexo"),
                       xlab = "Año", ylab = "Tasa de participación", fillLab = "Sexo")

    ggplotly(p, tooltip = c("fill", "x", "y"))
  })
  
  # Data Table del SNMV
  output$dataTable_trab_parLab <- renderDT({
    renderDataTable(parLab_filt())
  })
}
