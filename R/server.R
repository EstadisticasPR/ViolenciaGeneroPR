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
    p <- renderBarPlot(homiEdad_filt, "año", "casos", "edad",
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
    p <- renderBarPlot(inci_filt, x = "año", y = "casos", fill = "tipo",
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
  
  # Filtrar el conjunto de datos según los valores seleccionados del año y el checkGroup
  dfMalt_filt <- reactive({
    filter(dfMalt, 
           Año %in% input$checkGroup_fam_dfMalt_año,
           Maltrato %in% input$checkGroup_fam_dfMalt_tipo,
           Sexo %in% input$checkGroup_fam_dfMalt_sexo)
  })
  
  # funcion para el boton de deseleccionar/seleccionar tipo de maltrato
  observeEvent(input$deselectAll_fam_dfMalt_tipo, {
    updateCheckboxGroup(session, "checkGroup_fam_dfMalt_tipo", input, dfMalt$Maltrato)
  })
  
  # funcion para el boton de deseleccionar/seleccionar año
  observeEvent(input$deselectAll_fam_dfMalt_año, {
    updateCheckboxGroup(session, "checkGroup_fam_dfMalt_año", input, dfMalt$Año)
  })
  
  # funcion para el boton de deseleccionar/seleccionar sexo
  observeEvent(input$deselectAll_fam_dfMalt_sexo, {
    updateCheckboxGroup(session, "checkGroup_fam_dfMalt_sexo", input, dfMalt$Sexo)
  })
  
  # crear gráfico lineal
  # output$linePlot_fam <- renderPlotly({
  #   p <- renderLinePlot(dfMalt_filt, "Año", "Casos", "Maltrato", "Maltrato",
  #                  "Casos de Maltrato por Año y Tipo", "Año", "Casos")
  #   p <- p + facet_wrap(~Sexo)
  #   ggplotly(p, tooltip = c("x", "y", "color"))
  # })
  
  # Colores de las edades
  dfMalt_fill_Maltrato <- setColorFill(dfMalt, "Maltrato")
  
  # crear gráfico de barras
  output$barPlot_fam <- renderPlotly({
    # p <- renderBarPlot(data = dfMalt_filt, x = "Maltrato", y = "Casos", fill = "Sexo",
    #               title = paste("Distribución de Tipos de Maltrato en el Año", input$yearInput_fam),
    #               xlab = "Tipo de Maltrato", ylab = "Casos", fillLab = "Sexo")
    p <- renderBarPlot(data = dfMalt_filt, x = "Año", y = "Casos", fill = "Maltrato",
                       title = "Distribución de Tipos de Maltrato en el Año",
                       xlab = "Año", ylab = "Número de Casos", fillLab = "Tipo de Maltrato", colorFill = dfMalt_fill_Maltrato)
    p <- p + facet_wrap(~Sexo, scales = "fixed")
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # Data Table del DeptFam
  output$dataTable_fam <- renderDT({
    renderDataTable(dfMalt_filt())
  })
  ########## Server del Departamento de Justicia ##########
  #### Tab de Delitos (dfDeli) ####
  
  # Filtrar el conjunto de datos según el año, delito o distrito seleccionado
  dfDeli_filt <- reactive({
    filter(dfDeli, 
           Año %in% input$checkGroup_just_dfDeli_año,
           Delito %in% input$checkGroup_just_dfDeli_delito,
           `FISCALIA DISTRITO` %in% input$checkGroup_just_dfDeli_distrito)
  })
  
  # funcion para el boton de deseleccionar/seleccionar el delito
  observeEvent(input$deselectAll_just_dfDeli_delito, {
    updateCheckboxGroup(session, "checkGroup_just_dfDeli_delito", input, dfDeli$Delito)
  })
  
  # funcion para el boton de deseleccionar/seleccionar el año
  observeEvent(input$deselectAll_just_dfDeli_año, {
    updateCheckboxGroup(session, "checkGroup_just_dfDeli_año", input, dfDeli$Año)
  })
  
  # funcion para el boton de deseleccionar/seleccionar el distrito fiscal
  observeEvent(input$deselectAll_just_dfDeli_distrito, {
    updateCheckboxGroup(session, "checkGroup_just_dfDeli_distrito", input, dfDeli$`FISCALIA DISTRITO`)
  })
  
  # crear gráfico de caja
  # output$boxPlot_just <- renderPlotly({
  #   renderBoxPlot(
  #     data = dfDeli_filt(),
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
  
  # Colores de las edades
  dfDeli_fill_Delito <- setColorFill(dfDeli, "Delito")
  
  #crear el grafico de barras
  output$barPlot_just <- renderPlotly({
    p <- renderBarPlot(dfDeli_filt, x = "Año", y = "Casos", fill = "Delito",
                       title = "Distribución de Casos por Distrito Fiscal",
                       xlab = "Año", ylab = "Número de Casos",
                       fillLab = "Delito Cometido", colorFill = dfDeli_fill_Delito)
    p <- p + facet_wrap(~`FISCALIA DISTRITO`)
    ggplotly(p, tooltip = c("x", "y", "fill"))
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
    renderDataTable(dfDeli_filt())
  })
  
  #### Tab del Mapa de  Delitos por Distritos Fiscales (mapaDeli) ####
  
  # Filtrar el conjunto de datos según el año, delito o distrito seleccionado
  mapaDeli_filt <- reactive({
    filter(mapaDeli, 
           Año %in% input$select_just_mapaDeli_año,
           Delito %in% input$select_just_mapaDeli_delito)
  })
  
  # funcion para el boton de deseleccionar/seleccionar el delito
  observeEvent(input$deselectAll_just_mapaDeli_delito, {
    updateCheckboxGroup(session, "checkGroup_just_mapaDeli_delito", input, mapaDeli$Delito)
  })
  
  # funcion para el boton de deseleccionar/seleccionar el año
  observeEvent(input$deselectAll_just_mapaDeli_año, {
    updateCheckboxGroup(session, "checkGroup_just_mapaDeli_año", input, mapaDeli$año)
  })
  
  # output$map_just_mapaDeli <- renderPlotly({
  #   p <- renderMap(
  #     data = mapaDeli_filt, fill = Casos,
  #     title = paste0("Distribución de Delitos por ", input$select_just_mapaDeli_delito, " en el año ", input$select_just_mapaDeli_año),
  #     fill_lab = "Delito Cometido",
  #     light_color = "pink",
  #     dark_color = "darkred"
  #   )
  #   ggplotly(p, tooltip = c("all"))
  # })
  
  output$map_just_mapaDeli <- renderPlotly({
    p <- renderMap(
      data = mapaDeli_filt, fill = Casos,
      title = paste0("Distribución de Delitos por ", input$select_just_mapaDeli_delito, " en el año ", input$select_just_mapaDeli_año),
      group = GROUP,
      fill_lab = "Delito Cometido",
      light_color = "pink",
      dark_color = "darkred"
    )
    ggplotly(p, tooltip = c("all"))
  })
  
  #### Tab del Mapa de Distritos Fiscales ####
  output$map_just_mapaFisc <- renderPlotly({
    p <- renderMapGroup(data = mapaDeli, 
                        fill = GROUP, 
                        title = "",
                        fill_lab = "Distrito Fiscal")
    ggplotly(p, tooltip = c("fill"))
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
  
  # Colores de las edades
  parLab_fill_sexo <- setColorFill(parLab, "Sexo")
  # Grafico de barras
  output$barPlot_trab_parLab <- renderPlotly({
    p <- renderBarPlot(parLab_filt, x = "Año", y = "Tasa", fill = "Sexo",
                       paste("Tasa de participación laboral según el año natural y el sexo"),
                       xlab = "Año", ylab = "Tasa de participación", fillLab = "Sexo", 
                       colorFill = parLab_fill_sexo)

    ggplotly(p, tooltip = c("fill", "x", "y"))
  })
  
  # Data Table del SNMV
  output$dataTable_trab_parLab <- renderDT({
    renderDataTable(parLab_filt())
  })
  
  ########## Tab de la Administración de Vivienda Pública ##########
  #### Tab de Administración de Vivienda Pública (dfAvp) ####
  
  # Filtrar el conjunto de datos según los valores seleccionados del año y el tipo de incidente
  dfAvp_filt <- reactive({
    filter(dfAvp,
           región %in% input$checkGroup_avp_dfAvp_región,
           año %in% input$checkGroup_avp_dfAvp_año)
  })
  
  # Filtrar el conjunto de datos según el año, delito o distrito seleccionado
  mapaAvp_filt <- reactive({
    filter(mapaAvp, 
           año %in% input$select_avp_mapaAvp_año)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de región
  observeEvent(input$deselectAll_avp_dfAvp_región, {
    updateCheckboxGroup(session, "checkGroup_avp_dfAvp_región", input, dfAvp$región)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_avp_dfAvp_año, {
    updateCheckboxGroup(session, "checkGroup_avp_dfAvp_año", input, dfAvp$año)
  })
  
  # Colores del status
  dfAvp_fill_status <- setColorFill(dfAvp, "status")
  # Grafico de barras
  output$barPlot_avp_dfAvp <- renderPlotly({
    p <- renderBarPlot(dfAvp_filt, x = "año", y = "cantidad", fill = "status",
                       paste("Total de solicitudes de vivienda pública con preferencias por violencia doméstica, Puerto Rico desde 2017 a 2023"),
                       xlab = "Año", ylab = "Cantidad de viviendas públicas", fillLab = "Estado de la Vivienda",
                       colorFill = dfAvp_fill_status)

    ggplotly(p + facet_wrap(~región), 
             tooltip = c("fill", "x", "y"))
  })
  
  # mapa de las regiones de vivienda
  output$map_avp_mapaRegi <- renderPlotly({
    p <- renderMapGroup(
      data = mapaAvp, fill = GROUP,
      title = "Regiones de Vivienda ",
      fill_lab = "Region"
    )
    ggplotly(p, tooltip = c("all"))
  })
  
  output$map_avp_mapaAvp <- renderPlotly({
    p <- renderMap(
      data = mapaAvp_filt, fill = cantidad,
      title = paste0("Total de solicitudes de vivienda pública con preferencias por violencia doméstica en el año ", input$select_avp_mapaAvp_año),
      group = GROUP,
      fill_lab = "Número de Viviendas",
      light_color = "lightgreen",
      dark_color = "darkgreen"
    )
    ggplotly(p + facet_wrap(~status),
             tooltip = c("all"))
  })
  
  # Data Table para la gráfica de barras de dfAvp
  output$dataTable_avp_dfAvp <- renderDT({
    renderDataTable(dfAvp)
  })
  
  # Data Table para el mapa de dfAvp
  output$dataTable_avp_mapaAvp <- renderDT({
    renderDataTable(mapaAvp_filt())
  })
  
  
  ########## Tab del Negociado de Policia ##########
  #### Tab con datos de mujeres desaparecidas (despDF) ####
  # Filtrar el conjunto de datos según los valores seleccionados del año y la categoria de evento
  despDF_filt <- reactive({
    filter(despDF,
           Categoria %in% input$checkGroup_poli_despDF_categoría,
           Año %in% input$checkGroup_poli_despDF_año)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de categoria
  observeEvent(input$deselectAll_poli_despDF_categoría, {
    updateCheckboxGroup(session, "checkGroup_poli_despDF_categoría", input, despDF$Categoria)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_poli_despDF_año, {
    updateCheckboxGroup(session, "checkGroup_poli_despDF_año", input, despDF$Año)
  })
  
  # Colores del status
  despDF_fill_categoria <- setColorFill(despDF, "Categoria")
  # Grafico de barras
  output$barPlot_poli_despDF <- renderPlotly({
    p <- renderBarPlot(despDF_filt, x = "Año", y = "Casos", fill = "Categoria",
                       paste("Cantidad de mujeres desaparecidas, localizadas y sin localizar"),
                       xlab = "Año", ylab = "Cantidad de Mujeres", fillLab = "Estado de la Mujer",
                       colorFill = despDF_fill_categoria)
    
    ggplotly(p, 
             tooltip = c("fill", "x", "y"))
  })
  
  
  # Data Table para el mapa de despDF
  output$dataTable_poli_despDF <- renderDT({
    renderDataTable(despDF_filt())
  })
  
  
  #### Tab con datos de victimas por edad (vEdad) ####
  # Filtrar el conjunto de datos según los valores seleccionados del el grupo de edad y año 
  vEdad_filt <- reactive({
    filter(vEdad,
           Edad %in% input$checkGroup_poli_vEdad_edad,
           Año %in% input$checkGroup_poli_vEdad_año,
           Sexo %in% input$checkGroup_poli_vEdad_sexo)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar el grupo de edad
  observeEvent(input$deselectAll_poli_vEdad_edad, {
    updateCheckboxGroup(session, "checkGroup_poli_vEdad_edad", input, vEdad$Edad)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_poli_vEdad_año, {
    updateCheckboxGroup(session, "checkGroup_poli_vEdad_año", input, vEdad$Año)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar el sexo
  observeEvent(input$deselectAll_poli_vEdad_sexo, {
    updateCheckboxGroup(session, "checkGroup_poli_vEdad_sexo", input, vEdad$Sexo)
  })
  
  # Colores del status
  vEdad_fill_edad <- setColorFill(vEdad, "Edad")
  # Grafico de barras
  output$barPlot_poli_vEdad <- renderPlotly({
    p <- renderBarPlot(vEdad_filt, x = "Año", y = "Casos", fill = "Edad",
                       paste("Incidentes de violencia doméstica por edad de la víctima"),
                       xlab = "Año", ylab = "Cantidad de Mujeres", fillLab = "Grupo de Edad",
                       colorFill = vEdad_fill_edad)
    
    ggplotly(p + facet_wrap(~Sexo), 
             tooltip = c("fill", "x", "y"))
  })
  
  # Data Table para el mapa de despDF
  output$dataTable_poli_vEdad <- renderDT({
    renderDataTable(vEdad_filt())
  })
  
  #### tab con datos de incidentes de violencia doméstica (inciDF) ####
  # Filtrar el conjunto de datos según los valores seleccionados del el grupo de edad y año 
  inciMapa_filt <- reactive({
    filter(inciMapa,
           Año %in% input$select_poli_inciMapa_año)
  })
  
  ### mapa 
  output$map_poli_inciMapa <- renderPlotly({
    p <- renderMap(
      data = inciMapa_filt, fill = Casos,
      title = paste0("Incidentes de violencia doméstica por área policíaca en el año ", input$select_poli_inciMapa_año),
      group = GROUP,
      fill_lab = "Número de incidentes de violencia doméstica",
      light_color = "lightblue",
      dark_color = "darkblue"
    )
    ggplotly(p, tooltip = c("all"))
  })
  
  # Data Table para el mapa de despDF
  # output$dataTable_poli_inciMapa <- renderDT({
  #   renderDataTable(inciMapa_filt())
  # })
  ########## Tab de la Oficina de la Procuradora de las Mujeres ##########
  #### tab con datos de asesinatos por violencia domestica (opmFemiVD) ####
  # Filtrar el conjunto de datos según los valores seleccionados del año y la categoria de evento
  opmFemiVD_filt <- reactive({
    filter(opmFemiVD,
           Año %in% input$checkGroup_opm_opmFemiVD_año)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_opm_opmFemiVD_año, {
    updateCheckboxGroup(session, "checkGroup_opm_opmFemiVD_año", input, opmFemiVD$Año)
  })
  
  # Colores del status
  #despDF_fill_categoria <- setColorFill(despDF, "Categoria")
  # Grafico de barras
  output$barPlot_opm_opmFemiVD <- renderPlotly({
    p <- renderLinePlot(data = opmFemiVD_filt, x = "Año", y = "`Cantidad de asesinatos`", group = "1",
                        color = "1", title = "Tendencia de Asesinatos a lo largo de los Años",
                        xlab = "Año", ylab = "Cantidad de Asesinatos")
    # p <- ggplot(opmFemiVD_filt(), aes(x = Año, y = `Cantidad de asesinatos`, group = 1)) +
    #   geom_line(color = "blue", linewidth = 1) +
    #   geom_point(color = "red", size = 2) +
    #   #geom_smooth(method = "lm", se = FALSE, color = "green" ) +  # Añade líneas de tendencia lineal
    #   labs(title = "Tendencia de Asesinatos a lo largo de los Años", x = "Año", y = "Cantidad de Asesinatos") +
    #   theme_minimal()
    
    ggplotly(p, 
             tooltip = c("fill", "x", "y"))
  })
  
  
  # Data Table para el mapa de despDF
  output$dataTable_opm_opmFemiVD <- renderDT({
    renderDataTable(opmFemiVD_filt())
  })
  
  #### tab con datos de casos de violencia (opmCasos) ####
  # Filtrar el conjunto de datos según los valores seleccionados del año y la categoria de evento
  opmCasos_filt <- reactive({
    filter(opmCasos,
           year %in% input$checkGroup_opm_opmCasos_año,
           tipo %in% input$checkGroup_opm_opmCasos_tipo
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_opm_opmCasos_año, {
    updateCheckboxGroup(session, "checkGroup_opm_opmCasos_año", input, opmCasos$year)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de tipo de violencia
  observeEvent(input$deselectAll_opm_opmCasos_tipo, {
    updateCheckboxGroup(session, "checkGroup_opm_opmCasos_tipo", input, opmCasos$tipo)
  })
  
  # Colores del status
  opm_fill_tipo <- setColorFill(opmCasos, "tipo")
  # Grafico de barras
  output$barPlot_opm_opmCasos <- renderPlotly({
    p <- renderBarPlot(opmCasos_filt, x = "year", y = "cantidad", fill = "tipo",
                       paste("Tendencias Mensuales de Violencia Doméstica por Año"),
                       xlab = "Año", ylab = "Cantidad de Asesinatos", fillLab = "Tipo de Violencia",
                       colorFill = opm_fill_tipo)
    
    ggplotly(p, 
             tooltip = c("fill", "x", "y"))
  })
  
  
  # Data Table para el mapa de despDF
  output$dataTable_opm_opmCasos <- renderDT({
    renderDataTable(opmCasos_filt())
  })
  #### tab con datos del género de las víctimas (opmVic) ####
  # Filtrar el conjunto de datos según los valores seleccionados del año y el género de la víctima
  opmVic_filt <- reactive({
    filter(opmVic,
           año %in% input$checkGroup_opm_opmVic_año,
           género %in% input$checkGroup_opm_opmVic_género
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_opm_opmVic_año, {
    updateCheckboxGroup(session, "checkGroup_opm_opmVic_año", input, opmVic$año)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón del género de la víctima
  observeEvent(input$deselectAll_opm_opmVic_género, {
    updateCheckboxGroup(session, "checkGroup_opm_opmVic_género", input, opmVic$género)
  })
  
  # Colores del status
  opmVic_fill_género <- setColorFill(opmVic, "género")
  # Grafico de barras
  output$barPlot_opm_opmVic <- renderPlotly({
    p <- renderBarPlot(opmVic_filt, x = "año", y = "víctimas", fill = "género",
                       paste("Identidad de género de las víctimas"),
                       xlab = "Año", ylab = "Cantidad de Víctimas", fillLab = "Género de la Víctima",
                       colorFill = opmVic_fill_género)
    
    ggplotly(p, 
             tooltip = c("fill", "x", "y"))
  })
  
  
  # Data Table para el mapa de despDF
  output$dataTable_opm_opmVic <- renderDT({
    renderDataTable(opmVic_filt())
  })
  #### tab con datos del género de las víctimas (opmMedio) ####
  # Filtrar el conjunto de datos según los valores seleccionados del año y el género de la víctima
  opmMedio_filt <- reactive({
    filter(opmMedio,
           año %in% input$checkGroup_opm_opmMedio_año,
           `Medio de orientación` %in% input$checkGroup_opm_opmMedio_medio
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_opm_opmMedio_año, {
    updateCheckboxGroup(session, "checkGroup_opm_opmMedio_año", input, opmMedio$año)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón del medio de orientación 
  observeEvent(input$deselectAll_opm_opmMedio_medio, {
    updateCheckboxGroup(session, "checkGroup_opm_opmMedio_medio", input, opmMedio$`Medio de orientación`)
  })
  
  # Colores del status
  opmMedio_fill_medio <- setColorFill(opmMedio, "Medio de orientación")
  # Grafico de barras
  output$barPlot_opm_opmMedio <- renderPlotly({
    p <- renderBarPlot(opmMedio_filt, x = "año", y = "`personas atendidas`", fill = "`Medio de orientación`",
                       title = "Orientaciones según el Medio",
                       xlab = "Año", ylab = "Cantidad de Personas Orientadas", fillLab = "`Medio de Orientación`",
                       colorFill = opmMedio_fill_medio)
    
    ggplotly(p, 
             tooltip = c("fill", "x", "y"))
  })
  
  
  # Data Table para opmMedio
  output$dataTable_opm_opmMedio <- renderDT({
    renderDataTable(opmMedio_filt())
  })
  
  #### tab con datos de los servicios ofrecidos (opmServiciosMes) ####
  # Filtrar el conjunto de datos según los valores seleccionados del año y el tipo de servicio
  opmServiciosMes_filt <- reactive({
    filter(opmServiciosMes,
           year %in% input$checkGroup_opm_opmServiciosMes_año
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_opm_opmServiciosMes_año, {
    updateCheckboxGroup(session, "checkGroup_opm_opmServiciosMes_año", input, opmServiciosMes$year)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de tipo de servicio
  observeEvent(input$deselectAll_opm_opmServiciosMes_tipo, {
    updateCheckboxGroup(session, "checkGroup_opm_opmServiciosMes_tipo", input, opmServiciosMes$tipo)
  })
  
  # Colores del status
  opmServiciosMes_fill_tipo <- setColorFill(opmServiciosMes, "tipo")
  # Grafico de barras
  output$barPlot_opm_opmServiciosMes <- renderPlotly({
    p <- renderBarPlot(opmServiciosMes_filt, x = "year", y = "cantidad", fill = "tipo",
                       title = "Población atendida, servicios ofrecidos y seguimientos",
                       xlab = "Año", ylab = "Cantidad de Servicios Ofrecidos", fillLab = "`Medio de Orientación`",
                       colorFill = opmServiciosMes_fill_tipo)
    
    ggplotly(p, 
             tooltip = c("fill", "x", "y"))
  })
  
  
  # Data Table para opmServiciosMes
  output$dataTable_opm_opmServiciosMes <- renderDT({
    renderDataTable(opmServiciosMes_filt())
  })
  
  ########## Tab del Departamento de Correción y Rehabilitación ##########
  #### tab con datos de los servicios ofrecidos (dcrCasosInv) ####
  
  # Filtrar el conjunto de datos según los valores seleccionados del año, el tipo de servicio y el sexo
  dcrCasosInv_filt <- reactive({
    filter(dcrCasosInv,
           year %in% input$checkGroup_dcr_dcrCasosInv_year,
           tipo %in% input$checkGroup_dcr_dcrCasosInv_tipo,
           sexo %in% input$checkGroup_dcr_dcrCasosInv_sexo
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_dcr_dcrCasosInv_año, {
    updateCheckboxGroup(session, "checkGroup_dcr_dcrCasosInv_año", input, dcrCasosInv$año)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de tipo de servicio
  observeEvent(input$deselectAll_dcr_dcrCasosInv_tipo, {
    updateCheckboxGroup(session, "checkGroup_dcr_dcrCasosInv_medio", input, dcrCasosInv$tipo)
  })
  
  # Colores del status
  dcrCasosInv_fill_tipo <- setColorFill(dcrCasosInv, "tipo")
  # Grafico de barras
  output$barPlot_dcr_dcrCasosInv <- renderPlotly({
    p <- renderBarPlot(dcrCasosInv_filt, x = "year", y = "cantidad", fill = "tipo",
                       title = "Casos en Supervisión",
                       xlab = "Año", ylab = "Cantidad de Servicios Ofrecidos", fillLab = "Estado de la Investigación",
                       colorFill = dcrCasosInv_fill_tipo)
    
    ggplotly(p + facet_wrap(~sexo), 
             tooltip = c("fill", "x", "y"))
  })
  
  
  # Data Table para dcrCasosInv
  output$dataTable_dcr_dcrCasosInv <- renderDT({
    renderDataTable(dcrCasosInv_filt())
  })
  
  
  
  #### tab con datos de personas sentenciadas integradas a Supervisión Electrónica (dcrSentenciadas) ####
  
  # Filtrar el conjunto de datos según los valores seleccionados del año y el estado del caso
  dcrSentenciadas_filt <- reactive({
    filter(dcrSentenciadas,
           year %in% input$checkGroup_dcr_dcrSentenciadas_year,
           tipo %in% input$checkGroup_dcr_dcrSentenciadas_tipo
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_dcr_dcrSentenciadas_year, {
    updateCheckboxGroup(session, "checkGroup_dcr_dcrSentenciadas_year", input, dcrSentenciadas$year)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón del estado de caso 
  observeEvent(input$deselectAll_dcr_dcrSentenciadas_tipo, {
    updateCheckboxGroup(session, "checkGroup_dcr_dcrSentenciadas_tipo", input, dcrSentenciadas$tipo)
  })
  
  # Colores del status
  dcrSentenciadas_fill_tipo <- setColorFill(dcrSentenciadas, "tipo")
  # Grafico de barras
  output$barPlot_dcr_dcrSentenciadas <- renderPlotly({
    p <- renderBarPlot(dcrSentenciadas_filt, x = "year", y = "cantidad", fill = "tipo",
                       title = "Personas sentenciadas por incurrir en delitos de violencia doméstica",
                       xlab = "Año", ylab = "Cantidad de Personas Sentenciadas", fillLab = "Estado del Caso",
                       colorFill = dcrSentenciadas_fill_tipo)
    
    ggplotly(p, 
             tooltip = c("fill", "x", "y"))
  })
  
  
  # Data Table para dcrSentenciadas
  output$dataTable_dcr_dcrSentenciadas <- renderDT({
    renderDataTable(dcrSentenciadas_filt())
  })
}
