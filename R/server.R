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
    p <- renderHistogram(homiEdad_filt, "año", "casos", "edad",
                       paste("Homicidios de mujeres por grupo de edad y año", input$yearInput_snmv),
                       "Año", "Número de casos", fillLab = "Grupo de edad", colorFill = homiEdad_fill_edad)
    
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
                       paste("Número de incidentes violentos por tipo para ambos sexos"),
                       xlab = "Año", ylab = "Número de casos", fillLab = "Tipo de incidente",
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
                       title = "Cantidad de menores víctimas por sexo y tipo de maltrato",
                       xlab = "Año", ylab = "Número de casos", fillLab = "Tipo de maltrato", colorFill = dfMalt_fill_Maltrato)
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
                       title = "Casos de delitos por Distrito Fiscal según Artículo de la Ley 54",
                       xlab = "Año", ylab = "Número de casos",
                       fillLab = "Artículo de Ley 54", colorFill = dfDeli_fill_Delito)
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
      # Casos de delitos por Distrito Fiscal según Artículo de la Ley 54
      title = paste0("Casos de delitos por Distrito Fiscal según el Artículo de ", input$select_just_mapaDeli_delito, " en el año ", input$select_just_mapaDeli_año),
      group = GROUP,
      fill_lab = "Número de casos",
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
                       paste("Tasa de participación laboral según el sexo"),
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
                       paste("Total de viviendas públicas solicitadas y asignadas por violencia doméstica por región"),
                       xlab = "Año", ylab = "Cantidad de viviendas públicas", fillLab = "Estado de la vivienda",
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
      title = paste0("Total de viviendas públicas solicitadas y asignadas por violencia doméstica por región en el año ", input$select_avp_mapaAvp_año),
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
                       xlab = "Año", ylab = "Cantidad de víctimas", fillLab = "Estado de la víctima",
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
    p <- renderHistogram(vEdad_filt, x = "Año", y = "Casos", fill = "Edad",
                       paste("Incidentes de violencia doméstica por edad de la víctima"),
                       xlab = "Año", ylab = "Cantidad de víctimas", fillLab = "Grupo de edad",
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
                       paste("Población atendida mediante el programa CRIAS según razón para consulta"),
                       xlab = "Año", ylab = "Cantidad de personas atendidas", fillLab = "Razón para consulta",
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
                       paste("Identidad de género de víctimas asistidas por el programa CRIAS"),
                       xlab = "Año", ylab = "Cantidad de víctimas", fillLab = "Género de la víctima",
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
                       title = "Población atendida, servicios ofrecidos y seguimientos mediante el programa CRIAS",
                       xlab = "Año", ylab = "Cantidad de personas orientadas", fillLab = "Medio de orientación",
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
                       xlab = "Año", ylab = "Cantidad de Servicios Ofrecidos", fillLab = "Tipo de Servicio",
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
  observeEvent(input$deselectAll_dcr_dcrCasosInv_year, {
    updateCheckboxGroup(session, "checkGroup_dcr_dcrCasosInv_year", input, dcrCasosInv$year)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón del estado de investigación
  observeEvent(input$deselectAll_dcr_dcrCasosInv_tipo, {
    updateCheckboxGroup(session, "checkGroup_dcr_dcrCasosInv_tipo", input, dcrCasosInv$tipo)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de sexo
  observeEvent(input$deselectAll_dcr_dcrCasosInv_sexo, {
    updateCheckboxGroup(session, "checkGroup_dcr_dcrCasosInv_sexo", input, dcrCasosInv$sexo)
  })
  
  # Colores del status
  dcrCasosInv_fill_tipo <- setColorFill(dcrCasosInv, "tipo")
  # Grafico de barras
  output$barPlot_dcr_dcrCasosInv <- renderPlotly({
    p <- renderBarPlot(dcrCasosInv_filt, x = "year", y = "cantidad", fill = "tipo",
                       title = "Casos en supervisión de Ley 54 en programas alternos de comunidad por estado de investigación",
                       xlab = "Año", ylab = "Cantidad de servicios ofrecidos", fillLab = "Estado de investigación",
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
                       title = "Personas sentenciadas en programa de supervisión Electrónica por delitos de violencia doméstica por estado del",
                       xlab = "Año", ylab = "Cantidad de personas sentenciadas", fillLab = "Estado del Caso",
                       colorFill = dcrSentenciadas_fill_tipo)
    
    ggplotly(p, 
             tooltip = c("fill", "x", "y"))
  })
  
  
  # Data Table para dcrSentenciadas
  output$dataTable_dcr_dcrSentenciadas <- renderDT({
    renderDataTable(dcrSentenciadas_filt())
  })
  ########## Tab de la Administración de Tribunales ##########
  #### tab con datos de Ley 148 - Violencia Sexual por grupo de edad (OP_148_SoliGrupEdad) ####
  
  # Filtrar el conjunto de datos según los valores seleccionados del año fiscal, el grupo de edad y el distrito fiscal
  OP_148_SoliGrupEdad_filt <- reactive({
    filter(OP_148_SoliGrupEdad,
           AñoFiscal %in% input$checkGroup_trib_OP_148_SoliGrupEdad_AñoFiscal,
           Edad %in% input$checkGroup_trib_OP_148_SoliGrupEdad_Edad,
           Región %in% input$checkGroup_trib_OP_148_SoliGrupEdad_Región
    )
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el año fiscal
  observeEvent(input$deselectAll_trib_OP_148_SoliGrupEdad_AñoFiscal, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_148_SoliGrupEdad_AñoFiscal", input, OP_148_SoliGrupEdad$AñoFiscal)
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el grupo de edad
  observeEvent(input$deselectAll_trib_OP_148_SoliGrupEdad_Edad, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_148_SoliGrupEdad_Edad", input, OP_148_SoliGrupEdad$Edad)
  })
  
  ### funcion para el botón de deseleccionar/seleccionar la región fiscal
  observeEvent(input$deselectAll_trib_OP_148_SoliGrupEdad_Región, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_148_SoliGrupEdad_Región", input, OP_148_SoliGrupEdad$Región)
  })
  
  # Colores del status
  OP_148_SoliGrupEdad_fill_edad <- setColorFill(OP_148_SoliGrupEdad, "Edad")
  # Grafico de barras
  output$barPlot_OP_148_SoliGrupEdad <- renderPlotly({
    p <- renderHistogram(OP_148_SoliGrupEdad_filt, x = "AñoFiscal", y = "Solicitudes", fill = "Edad",
                       title = "Solicitudes de órdenes de protección por Ley 148 según Región Judicial y edad de la parte solicitante",
                       xlab = "Año Fiscal", ylab = "Órdenes de Protección Solicitadas", fillLab = "Grupo de Edad",
                       colorFill = OP_148_SoliGrupEdad_fill_edad)
    
    ggplotly(p + facet_wrap(~Región), 
             tooltip = c("fill", "x", "y"))
  })
  
  # Data Table para dcrCasosInv
  output$dataTable_OP_148_SoliGrupEdad <- renderDT({
    renderDataTable(OP_148_SoliGrupEdad_filt())
  })
  
  #### (OP_Ley148_ex_parteEmitidas) ####
  
  # Filtrar el conjunto de datos según los valores seleccionados del año fiscal, el delito cometido y la región fiscal
  OP_Ley148_ex_parteEmitidas_filt <- reactive({
    filter(OP_Ley148_ex_parteEmitidas,
           AñoFiscal %in% input$checkGroup_trib_OP_Ley148_ex_parteEmitidas_AñoFiscal,
           Delito %in% input$checkGroup_trib_OP_Ley148_ex_parteEmitidas_Delito,
           Región %in% input$checkGroup_trib_OP_Ley148_ex_parteEmitidas_Región
    )
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el año fiscal
  observeEvent(input$deselectAll_trib_OP_Ley148_ex_parteEmitidas_AñoFiscal, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_Ley148_ex_parteEmitidas_AñoFiscal", input, OP_Ley148_ex_parteEmitidas$AñoFiscal)
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el delito
  observeEvent(input$deselectAll_trib_OP_Ley148_ex_parteEmitidas_Delito, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_Ley148_ex_parteEmitidas_Delito", input, OP_Ley148_ex_parteEmitidas$Delito)
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el Región
  observeEvent(input$deselectAll_trib_OP_Ley148_ex_parteEmitidas_Región, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_Ley148_ex_parteEmitidas_Región", input, OP_Ley148_ex_parteEmitidas$Región)
  })
  
  # Colores de los Delitos
  OP_Ley148_ex_parteEmitidas_fill_delito <- setColorFill(OP_Ley148_ex_parteEmitidas, "Delito")
  # Grafico de barras
  output$barPlot_OP_Ley148_ex_parteEmitidas <- renderPlotly({
    p <- renderBarPlot(OP_Ley148_ex_parteEmitidas_filt, x = "AñoFiscal", y = "ÓrdenesEmitidas", fill = "Delito",
                       title = "Órdenes de protección ex parte emitidas bajo Ley 148 según Región Judicial y delito cometido",
                       xlab = "Año fiscal", ylab = "Órdenes de protección emitidas", fillLab = "Delito cometido",
                       colorFill = OP_Ley148_ex_parteEmitidas_fill_delito)
    
    ggplotly(p + facet_wrap(~Región), 
             tooltip = c("fill", "x", "y"))
  })
  
  # Data Table para dcrCasosInv
  output$dataTable_OP_Ley148_ex_parteEmitidas <- renderDT({
    renderDataTable(OP_Ley148_ex_parteEmitidas_filt())
  })
  
  #### (OP_LEY148Archivadas) ####
  
  # Filtrar el conjunto de datos según los valores seleccionados del año fiscal, la razón de archivado y la región fiscal
  OP_LEY148Archivadas_filt <- reactive({
    filter(OP_LEY148Archivadas,
           AñoFiscal %in% input$checkGroup_trib_OP_LEY148Archivadas_AñoFiscal,
           Razón %in% input$checkGroup_trib_OP_LEY148Archivadas_Razón,
           Región %in% input$checkGroup_trib_OP_LEY148Archivadas_Región
    )
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el año fiscal
  observeEvent(input$deselectAll_trib_OP_LEY148Archivadas_AñoFiscal, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_LEY148Archivadas_AñoFiscal", input, OP_LEY148Archivadas$AñoFiscal)
  })
  
  ### funcion para el botón de deseleccionar/seleccionar la razón de archivado
  observeEvent(input$deselectAll_trib_OP_LEY148Archivadas_Razón, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_LEY148Archivadas_Razón", input, OP_LEY148Archivadas$Razón)
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el Región
  observeEvent(input$deselectAll_trib_OP_LEY148Archivadas_Región, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_LEY148Archivadas_Región", input, OP_LEY148Archivadas$Región)
  })
  
  # Colores de las razones
  OP_LEY148Archivadas_fill_Razón <- setColorFill(OP_LEY148Archivadas, "Razón")
  # Grafico de barras
  output$barPlot_OP_LEY148Archivadas <- renderPlotly({
    p <- renderBarPlot(OP_LEY148Archivadas_filt, x = "AñoFiscal", y = "ÓrdenesArchivadas", fill = "Razón",
                       title = "Órdenes de protección archivadas - violencia sexual, por Región Judicial",
                       xlab = "Año fiscal", ylab = "Órdenes de protección archivadas", fillLab = "Razón de archivo",
                       colorFill = OP_LEY148Archivadas_fill_Razón)
    
    ggplotly(p + facet_wrap(~Región), 
             tooltip = c("fill", "x", "y"))
  })
  
  # Data Table 
  output$dataTable_OP_LEY148Archivadas <- renderDT({
    renderDataTable(OP_LEY148Archivadas_filt())
  })
  
  #### (OP_LEY148Denegadas) ####
  
  # Filtrar el conjunto de datos según los valores seleccionados del año fiscal, la razón de archivado y la región fiscal
  OP_LEY148Denegadas_filt <- reactive({
    filter(OP_LEY148Denegadas,
           AñoFiscal %in% input$checkGroup_trib_OP_LEY148Denegadas_AñoFiscal,
           Razón %in% input$checkGroup_trib_OP_LEY148Denegadas_Razón,
           Región %in% input$checkGroup_trib_OP_LEY148Denegadas_Región
    )
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el año fiscal
  observeEvent(input$deselectAll_trib_OP_LEY148Denegadas_AñoFiscal, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_LEY148Denegadas_AñoFiscal", input, OP_LEY148Denegadas$AñoFiscal)
  })
  
  ### funcion para el botón de deseleccionar/seleccionar la razón de archivado
  observeEvent(input$deselectAll_trib_OP_LEY148Denegadas_Razón, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_LEY148Denegadas_Razón", input, OP_LEY148Denegadas$Razón)
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el Región
  observeEvent(input$deselectAll_trib_OP_LEY148Denegadas_Región, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_LEY148Denegadas_Región", input, OP_LEY148Denegadas$Región)
  })
  
  # Colores de las razones
  OP_LEY148Denegadas_fill_Razón <- setColorFill(OP_LEY148Denegadas, "Razón")
  # Grafico de barras
  output$barPlot_OP_LEY148Denegadas <- renderPlotly({
    p <- renderBarPlot(OP_LEY148Denegadas_filt, x = "AñoFiscal", y = "ÓrdenesDenegadas", fill = "Razón",
                       title = "Solicitudes denegadas de órdenes de protección bajo la Ley 148 por Región Judicial",
                       xlab = "Año fiscal", ylab = "Órdenes de protección denegadas", fillLab = "Razón de archivo",
                       colorFill = OP_LEY148Denegadas_fill_Razón)
    
    ggplotly(p + facet_wrap(~Región), 
             tooltip = c("fill", "x", "y"))
  })
  
  # Data Table 
  output$dataTable_OP_LEY148Denegadas <- renderDT({
    renderDataTable(OP_LEY148Denegadas_filt())
  })
  #### (OP_LEY148FinalEmitidas) ####
  
  # Filtrar el conjunto de datos según los valores seleccionados del año fiscal, el delito cometido y la región fiscal
  OP_LEY148FinalEmitidas_filt <- reactive({
    filter(OP_LEY148FinalEmitidas,
           AñoFiscal %in% input$checkGroup_trib_OP_LEY148FinalEmitidas_AñoFiscal,
           Delito %in% input$checkGroup_trib_OP_LEY148FinalEmitidas_Delito,
           Región %in% input$checkGroup_trib_OP_LEY148FinalEmitidas_Región
    )
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el año fiscal
  observeEvent(input$deselectAll_trib_OP_LEY148FinalEmitidas_AñoFiscal, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_LEY148FinalEmitidas_AñoFiscal", input, OP_LEY148FinalEmitidas$AñoFiscal)
  })
  
  ### funcion para el botón de deseleccionar/seleccionar delito cometido
  observeEvent(input$deselectAll_trib_OP_LEY148FinalEmitidas_Delito, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_LEY148FinalEmitidas_Delito", input, OP_LEY148FinalEmitidas$Delito)
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el Región
  observeEvent(input$deselectAll_trib_OP_LEY148FinalEmitidas_Región, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_LEY148FinalEmitidas_Región", input, OP_LEY148FinalEmitidas$Región)
  })
  
  # Colores de las razones
  OP_LEY148FinalEmitidas_fill_Delito <- setColorFill(OP_LEY148FinalEmitidas, "Delito")
  
  # Grafico de barras
  output$barPlot_OP_LEY148FinalEmitidas <- renderPlotly({
    p <- renderBarPlot(OP_LEY148FinalEmitidas_filt, x = "AñoFiscal", y = "ÓrdenesEmitidas", fill = "Delito",
                       title = "Órdenes de protección final emitidas según la Ley 148 en Casos de Violencia Sexual, por Región Judicial y Tipo de Delito",
                       xlab = "Año Fiscal", ylab = "Órdenes de Protección Emitidas", fillLab = "Delito Cometido",
                       colorFill = OP_LEY148FinalEmitidas_fill_Delito)
    
    ggplotly(p + facet_wrap(~Región), 
             tooltip = c("fill", "x", "y"))
  })
  
  # Data Table 
  output$dataTable_OP_LEY148FinalEmitidas <- renderDT({
    renderDataTable(OP_LEY148FinalEmitidas_filt())
  })
  #### (OP_LEY148Genero) ####
  
  # Filtrar el conjunto de datos según los valores seleccionados del año fiscal, la parte peticionaria y el sexo de la parte
  OP_LEY148Genero_filt <- reactive({
    filter(OP_LEY148Genero,
           AñoFiscal %in% input$checkGroup_trib_OP_LEY148Genero_AñoFiscal,
           Parte %in% input$checkGroup_trib_OP_LEY148Genero_Parte,
           Sexo %in% input$checkGroup_trib_OP_LEY148Genero_Sexo
    )
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el año fiscal
  observeEvent(input$deselectAll_trib_OP_LEY148Genero_AñoFiscal, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_LEY148Genero_AñoFiscal", input, OP_LEY148Genero$AñoFiscal)
  })
  
  ### funcion para el botón de deseleccionar/seleccionar la parte peticionaria
  observeEvent(input$deselectAll_trib_OP_LEY148Genero_Parte, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_LEY148Genero_Parte", input, OP_LEY148Genero$Parte)
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el sexo de la parte
  observeEvent(input$deselectAll_trib_OP_LEY148Genero_Sexo, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_LEY148Genero_Sexo", input, OP_LEY148Genero$Sexo)
  })
  
  # Colores de las partes
  OP_LEY148Genero_fill_Parte <- setColorFill(OP_LEY148Genero, "Parte")
  
  # Grafico de barras
  output$barPlot_OP_LEY148Genero <- renderPlotly({
    p <- renderBarPlot(OP_LEY148Genero_filt, x = "AñoFiscal", y = "Solicitudes", fill = "Parte",
                       title = "Órdenes de Protección Emitidas bajo Ley 148, por Sexo y la Parte",
                       xlab = "Año fiscal", ylab = "Solicitudes de órdenes de protección", fillLab = "Parte",
                       colorFill = OP_LEY148Genero_fill_Parte)
    
    ggplotly(p + facet_wrap(~Sexo), 
             tooltip = c("fill", "x", "y"))
  })
  
  # Data Table 
  output$dataTable_OP_LEY148Genero <- renderDT({
    renderDataTable(OP_LEY148Genero_filt())
  })
  #### (tribCasosCrim) ####
  
  # Filtrar el conjunto de datos según los valores seleccionados del año fiscal, el delito cometido y el estado del caso
  tribCasosCrim_filt <- reactive({
    filter(tribCasosCrim,
           AñoFiscal %in% input$checkGroup_trib_tribCasosCrim_AñoFiscal,
           Delito %in% input$checkGroup_trib_tribCasosCrim_Delito,
           Casos %in% input$checkGroup_trib_tribCasosCrim_Casos
    )
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el año fiscal
  observeEvent(input$deselectAll_trib_tribCasosCrim_AñoFiscal, {
    updateCheckboxGroup(session, "checkGroup_trib_tribCasosCrim_AñoFiscal", input, tribCasosCrim$AñoFiscal)
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el delito cometido
  observeEvent(input$deselectAll_trib_tribCasosCrim_Delito, {
    updateCheckboxGroup(session, "checkGroup_trib_tribCasosCrim_Delito", input, tribCasosCrim$Delito)
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el Estado del Caso
  observeEvent(input$deselectAll_trib_tribCasosCrim_Casos, {
    updateCheckboxGroup(session, "checkGroup_trib_tribCasosCrim_Casos", input, tribCasosCrim$Casos)
  })
  
  # Colores de los delitos cometidos
  tribCasosCrim_fill_Delito <- setColorFill(tribCasosCrim, "Delito")
  
  # Grafico de barras
  output$barPlot_tribCasosCrim <- renderPlotly({
    p <- renderBarPlot(tribCasosCrim_filt, x = "AñoFiscal", y = "Cantidad", fill = "Delito",
                       title = "Movimiento de casos en tribunal de primera instancia por ley 54 por delito cometido",
                       xlab = "Año Fiscal", ylab = "Solicitudes de Órdenes de Protección", fillLab = "Delito Cometido",
                       colorFill = tribCasosCrim_fill_Delito)
    
    ggplotly(p + facet_wrap(~Casos), 
             tooltip = c("fill", "x", "y"))
  })
  
  # Data Table 
  output$dataTable_tribCasosCrim <- renderDT({
    renderDataTable(tribCasosCrim_filt())
  })
  
  ########## Tab del Centro de Ayuda a Víctimas de Violación ##########
  #### (safekitsDF) ####
  
  # Filtrar el conjunto de datos según los valores seleccionados del año y el estado de la querella
  safekitsDF_filt <- reactive({
    filter(safekitsDF,
           Año %in% input$checkGroup_cavv_safekitsDF_Año,
           Kits %in% input$checkGroup_cavv_safekitsDF_Kits
    )
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el año
  observeEvent(input$deselectAll_cavv_safekitsDF_Año, {
    updateCheckboxGroup(session, "checkGroup_cavv_safekitsDF_Año", input, safekitsDF$Año)
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el estado de querella
  observeEvent(input$deselectAll_cavv_safekitsDF_Kits, {
    updateCheckboxGroup(session, "checkGroup_cavv_safekitsDF_Kits", input, safekitsDF$Kits)
  })
  
  # Colores de los estados de Querella
  safekitsDF_fill_Kits <- setColorFill(safekitsDF, "Kits")
  
  # Grafico de barras
  output$barPlot_safekitsDF <- renderPlotly({
    p <- renderBarPlot(safekitsDF_filt, x = "Año", y = "Total", fill = "Kits", 
                       title = "Tendencia anual del equipo de recolecta de evidencia en casos de violencia sexual por estado de querella", 
                       xlab = "Año", ylab = "Total de kits distribuidos", fillLab = "Estado de querella", 
                       colorFill = safekitsDF_fill_Kits)
    ggplotly(p, 
             tooltip = c("fill", "x", "y"))
  })
  
  
  # Data Table 
  output$dataTable_safekitsDF <- renderDT({
    renderDataTable(safekitsDF_filt())
  })
}
