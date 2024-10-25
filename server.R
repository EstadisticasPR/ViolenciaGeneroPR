# Server
cat("Loading Server from server.R...\n")
server <- function(input, output, session) {
  
  ########## Server del Sistema de Notificación de Muertes Violentas ##########
  #### Tab de Homicidios por Grupo de Edad (homiEdad) ####
  
  # Filtro para el dataset según los valores de año y edad
  homiEdad_filt <- reactive({
    filter(homiEdad, 
           Año %in% input$checkGroup_snmv_homiEdad_año,
           Edad %in% input$checkGroup_snmv_homiEdad_edad)
  })
  
  ### lógica para el boton de deseleccionar/seleccionar edad
  observeEvent(input$deselectAll_snmv_homiEdad_edad, {
    updateCheckboxGroup(session, "checkGroup_snmv_homiEdad_edad", input, homiEdad$Edad)
  })
  
  ### lógica para el boton de deseleccionar/seleccionar año
  observeEvent(input$deselectAll_snmv_homiEdad_año, {
    updateCheckboxGroup(session, "checkGroup_snmv_homiEdad_año", input, homiEdad$Año)
  })
  
  # Colores de las edades
  homiEdad_fill_edad <- setColorFill(homiEdad, "Edad")
  
  # Grafico de barras de homiEdad
  output$barPlot_snmv <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_edad <- length(input$checkGroup_snmv_homiEdad_edad) > 0
    has_año <- length(input$checkGroup_snmv_homiEdad_año) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_edad || !has_año ) {
      message <- "Seleccione Grupo de edad y Año(s) a visualizar"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
     
      # p <- renderBarPlot(homiEdad_filt, "Año", "Casos", "Edad",
      #                    paste("Homicidios de Mujeres por grupo de Edad y Año", input$yearInput_snmv),
      #                    "Año", "Cantidad de víctimas", fillLab = "Grupo de Edad", colorFill = homiEdad_fill_edad, 
      #                    emptyMessage = "Seleccione Grupo de edad y Año(s) a visualizar",barWidth = 0, xGap = 0)
      
      par(mar = c(1, 10, 3, 4))
      p <- renderBarPlot(homiEdad_filt, "Año", "Casos", "Edad",
                         title = "Homicidios de Mujeres por grupo de Edad y Año",
                         "Año", "Cantidad de víctimas", fillLab = "Grupo de Edad", colorFill = homiEdad_fill_edad, 
                         emptyMessage = "Seleccione Grupo de edad y Año(s) a visualizar",barWidth = 0, xGap = 0)
     
      p <- convert_to_plotly(p, tooltip = "text")
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(homiEdad_filt, "Año", "Casos", "Edad",
                                                 paste("Homicidios de Mujeres por grupo de Edad y Año", input$yearInput_snmv),
                                                 "Año", "Cantidad de víctimas", message)
    ggplotly(empty_plot)
  })
  
  homiEdad_filt_rename <- reactive({
    homiEdad_filt() %>% 
      rename(`Grupo de Edad` = Edad)  
  })

  # Data Table de homiEdad
  output$dataTable_snmv <- renderDT({
    renderDataTable(homiEdad_filt_rename(), "Datos: Homicidios por Grupo de Edad")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_snmv <- renderUI({
    if (input$showTable_snmv) {
      hyperlinks <- c("https://stacks.cdc.gov/view/cdc/149761",
                      "https://estadisticas.pr/")
      texts <- c("Sistema Nacional de Notificación de Muertes Violentas",
                 "Instituto de Estadísticas de Puerto Rico")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 98%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;",  
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;",
            DTOutput("dataTable_snmv")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
  })

  
  #### Tab de Tipo de Muerte (inci) ####
  
  # Filtro para el dataset según los valores de año y el tipo de incidente
  inci_filt <- reactive({
    filter(inci,
           Incidente %in% input$checkGroup_snmv_inci_tipo,
           Año %in% input$checkGroup_snmv_inci_año)
  })
  
  ### lógica para el botón de deseleccionar/seleccionar tipo de incidente
  observeEvent(input$deselectAll_snmv_inci_tipo, {
    updateCheckboxGroup(session, "checkGroup_snmv_inci_tipo", input, inci$Incidente)
  })
  
  ### lógica para el botón de deseleccionar/seleccionar el año
  observeEvent(input$deselectAll_snmv_inci_año, {
    updateCheckboxGroup(session, "checkGroup_snmv_inci_año", input, inci$Año)
  })
  
  # colores de los tipos de incidentes
  inci_fill_sexo <- setColorFill(inci, "Incidente")
  
  # Gráfico de barras de incidentes
  output$barPlot_snmv_inci <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_incidente <- length(input$checkGroup_snmv_inci_tipo) > 0
    has_año <- length(input$checkGroup_snmv_inci_año) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_incidente || !has_año ) {
      message <- "Seleccione Tipo(s) de Incidente y Año(s) a visualizar"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot(inci_filt, x = "Año", y = "Casos", fill = "Incidente",
                         paste("Incidentes Violentos ocurridos para ambos Sexos"),
                         xlab = "Año", ylab = "Número de casos", fillLab = "Tipo de Incidente",
                         colorFill = inci_fill_sexo, 
                         emptyMessage = "Seleccione Tipo(s) de Incidente y Año(s) a visualizar")
      p <- convert_to_plotly(p, tooltip = "text")
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(inci_filt, x = "Año", y = "Casos", fill = "Incidente",
                                                 paste("Incidentes Violentos ocurridos para ambos Sexos"),
                                                 xlab = "Año", ylab = "Número de casos", message)
    ggplotly(empty_plot)
  })

  inci_filt_rename <- reactive({
    inci_filt() %>% 
      rename(`Tipo de Incidente` = Incidente)  
  })
  
  # Data Table del SNMV
  output$dataTable_snmv_inci <- renderDT({
    renderDataTable(inci_filt_rename(), "Datos: Incidentes Violentos")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_snmv_inci <- renderUI({
    if (input$showTable_snmv_inci) {
      hyperlinks <- c("https://stacks.cdc.gov/view/cdc/149761",
                      "https://estadisticas.pr/")
      texts <- c("Sistema Nacional de Notificación de Muertes Violentas",
                 "Instituto de Estadísticas de Puerto Rico")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 98%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
  
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;",  
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;",  
            DTOutput("dataTable_snmv_inci")
          )
        ),
        
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
  })
  
  
  
  #### Tab de Definiciones ####
  definitions_snmv <- list(
    list(word = "Homicidio", definition = "Es un delito de resultado por el cual la conducta intencionada de un sujeto provoca la muerte de otro."),
    list(word = "Homicidio múltiple:", definition = "Se refiere a un acto en el cual una persona causa la muerte de múltiples individuos en un solo incidente."),
    list(word = "Homicidio seguido de suicidio", definition = "Se refiere a un acto en el cual una persona causa la muerte de otra(s) y luego se quita la vida. Este tipo de incidente implica dos acciones distintas pero relacionadas: primero, el homicida comete el acto de matar a otra persona, y luego, la misma persona toma su propia vida."),
    list(word = "Homicidio(s) seguido de suicidio(s), (más de dos víctimas)", definition = "Se refiere a un acto extremadamente trágico en el cual una o más personas causan la muerte de múltiples individuos antes de acabar con sus propias vidas en un solo evento"),
    list(word = "Homicidio único", definition = "Se refiere a un acto en el cual una persona causa la muerte de otra en un evento específico."),
    list(word = "Muerte accidental por arma de fuego", definition = "Evento en el cual una persona pierde la vida como resultado involuntario de la manipulación, manejo o uso incorrecto de un arma de fuego. Este tipo de incidente ocurre cuando un individuo dispara un arma de manera accidental, ya sea debido a un descuido, falta de conocimiento sobre el funcionamiento seguro del arma, o por la manipulación indebida de la misma."),
    list(word = "Muerte no determinada", definition = "Caso en el cual las circunstancias que rodean el fallecimiento de una persona no pueden ser claramente establecidas o comprendidas mediante la evidencia disponible en el momento de la investigación. Este término se utiliza cuando no hay suficiente información o evidencia forense para determinar con certeza si la muerte fue el resultado de causas naturales, accidentales, suicidas u homicidas."),
    list(word = "Muertes violentas", definition = "PRVDRS define una muerte violenta como: Suicidio (incluyendo terrorismo), Homicidio (incluyendo terrorismo), Intervención legal (excluyendo terrorismo, ejecuciones por pena de muerte, o actos de guerra), Eventos con intención no Determinada, Heridas por arma de fuego no intencional."),
    list(word = "Suicidio Único", definition = "Se refiere a un acto en el cual una persona termina deliberadamente con su propia vida, sin la participación o implicación de otras personas en el proceso. Este término implica que el individuo toma la decisión y ejecuta el acto suicida de manera independiente, sin ningún intento de causar la muerte de otras personas o de involucrar a terceros en el evento."),
    list(word = "Violencia", definition = "El uso intencional de la fuerza o el poder físico, de hecho, o como amenaza, contra uno mismo, otra persona o un grupo o comunidad, que cause o tenga gran probabilidad de causar lesiones, muerte, daños psicológicos, trastornos del desarrollo o privaciones.")
  )
  
  # Convertir lista a dataframe
  definitions_df_snmv <- do.call(rbind, lapply(definitions_snmv, as.data.frame))
  
  # Renombrar las columnas
  colnames(definitions_df_snmv) <- c("Concepto", "Definición")
  
  # Usar funcion para presentar tabla con definiciones
  output$dataTable_Def_snmv <- renderDT({
    renderDataTable_Definitions(definitions_df_snmv)
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
  
  # Colores de las edades
  dfMalt_fill_Maltrato <- setColorFill(dfMalt, "Maltrato")
  
  # Crear gráfico de barras
  output$barPlot_fam <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_fam_dfMalt_año) > 0
    has_tipo <- length(input$checkGroup_fam_dfMalt_tipo) > 0
    has_sexo <- length(input$checkGroup_fam_dfMalt_sexo) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_año || !has_tipo || !has_sexo) {
      message <- "Seleccione Tipo(s) de maltrato, Año(s) y Sexo de la víctima"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot(data = dfMalt_filt, x = "Año", y = "Casos", fill = "Maltrato",
                         title = "Casos Anuales de maltrato infantil por Sexo y Tipo",
                         xlab = "Año", ylab = "Número de casos", fillLab = "Tipo de Maltrato", 
                         colorFill = dfMalt_fill_Maltrato, 
                         emptyMessage = "Seleccione Tipo(s) de maltrato, Año(s) y Sexo de la víctima")
      p <- p + facet_wrap(~Sexo, scales = "fixed")
      p <- convert_to_plotly(p, tooltip = "text")
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(data = dfMalt_filt, x = "Año", y = "Casos", fill = "Maltrato",
                         title = "Casos Anuales de maltrato infantil por Sexo y Tipo",
                         xlab = "Año", ylab = "Número de casos", message)
    ggplotly(empty_plot)
  })
  
  dfMalt_filt_rename <- reactive({
    dfMalt_filt() %>% 
      rename(`Tipo de Maltrato` = Maltrato)  
  })
  
  
  # Data Table del DeptFam
  output$dataTable_fam <- renderDT({
    renderDataTable(dfMalt_filt_rename(), "Datos: Menores victimas de maltrato")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_fam <- renderUI({
    if (input$showTable_fam) {
      hyperlinks <- c("https://www.familia.pr.gov/")
      texts <- c("Departamento de la Familia")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 98%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;", 
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;",  
            DTOutput("dataTable_fam")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
  })
  
  
  #### Tab de Definiciones ####
  definitions_fam <- list(
    list(word = "Abuso Sexual", definition = "Incurrir en conducta sexual en presencia de un o una menor o que se utilice, voluntaria o involuntariamente, para ejecutar conducta sexual dirigida a satisfacer los deseos sexuales. También se considera cualquier acto que, de procesarse por la vía criminal, configuraría cualesquiera de varios delitos de índole sexual, tales como agresión sexual, actos lascivos, comercio de personas para actos sexuales, exposiciones obscenas, proposición obscena, producción de pornografía infantil, entre otros delitos reconocidos en el Código Penal de Puerto Rico."),
    list(word = "Explotación", definition = "Conducta obscena o utilización de una persona menor de edad para ejecutar conducta obscena. explotación de una persona menor de edad o que se permita que otra persona lo explote, incluyendo –pero sin limitarse– a utilizarla para ejecutar conducta obscena, con el fin de lucrarse o de recibir algún otro beneficio."),
    list(word = "Maltrato a Menores", definition = "Toda acción u omisión intencional del padre, de la madre o persona responsable del o de la menor que ocasione o ponga en riesgo de sufrir un daño o perjuicio a su salud e integridad física, mental o emocional."),
    list(word = "Maltrato Físico", definition = "Se refiere a cualquier trauma, lesión o condición no accidental, causada en un episodio o varios, incluyendo la falta de alimentos que, de no ser atendida, pone en riesgo la vida y salud de la persona menor de edad."),
    list(word = "Menores", definition = "Individuos que se encuentran bajo la edad legal de mayoría de edad en un contexto específico, lo que generalmente implica que aún no han alcanzado la edad en la que se les considera plenamente responsables de sus acciones según la ley."),
    list(word = "Negligencia", definition = "Es un tipo de maltrato que consiste en no cumplir con la obligación de proveer a las personas menores de edad de manera adecuada los alimentos, ropa, albergue, educación, atención a su salud, poca supervisión, no visitar, ni mantener contacto con el o la menor o incurrir en alguna de las razones reconocidas en el Código Civil de Puerto Rico para que una persona sea privada de patria potestad entre otros."),
    list(word = "Negligencia Educativa", definition = HTML("La negligencia institucional es cuando a una persona menor de edad, que está en un hogar de crianza, centro de cuidado sustituto o en una institución pública o privada, de cuido, educación, tratamiento o detención, se le cause daño o se ponga en riesgo de sufrir daño a su salud e integridad física, mental o emocional, incluyendo –pero sin limitarse– a abuso sexual. La negligencia institucional, ya sea conocida o que se sospeche que ocurre, o que ocurre como resultado de la política, prácticas y condiciones imperantes en la institución, la puede cometer:<ul>          
            <br>
            <li> Operador u operadora de un hogar de crianza; 
            <li> Cualquier empleado, empleada, funcionario o funcionaria que ofrezca servicios de cuido o que tenga bajo su control o custodia a una persona menor de edad para su cuido, educación, tratamiento o detención;
            </ul>")),
    list(word = "Negligencia Emocional", definition = "Se define como causar menoscabo o afectar la capacidad intelectual o emocional de la persona menor de edad dentro de lo que se considera normal para su edad y entorno cultural."),
    list(word = "Negligencia Médica", definition = "Situaciones en las que los proveedores de atención médica, como médicos, enfermeras u otros profesionales de la salud, no brindan el nivel adecuado de atención y cuidado a pacientes menores de edad, lo que resulta en daños físicos, emocionales o psicológicos para el paciente. Esto puede incluir errores en el diagnóstico, tratamiento inapropiado, falta de seguimiento adecuado, o cualquier otro acto u omisión que pueda considerarse una violación del estándar de cuidado aceptado en la práctica médica."),
    list(word = "Trata Humana", definition = "Se define como la captación, traslado, transporte, acogida o recepción de una persona utilizando la violencia, amenaza, engaño, rapto, fuerza, abuso de poder, abuso de una situación de vulnerabilidad u otros elementos de coacción, con el fin de someterla a explotación y lucrarse con su actividad.")
  )
  
  # Convertir lista a dataframe
  definitions_df_fam <- do.call(rbind, lapply(definitions_fam, as.data.frame))
  
  # Renombrar las columnas
  colnames(definitions_df_fam) <- c("Concepto", "Definición")
  
  # Usar funcion para presentar tabla con definiciones
  output$dataTable_Def_fam <- renderDT({
    renderDataTable_Definitions(definitions_df_fam)
  })
  
  ########## Server del Departamento de Justicia ##########
  #### Tab de Delitos (dfDeli) ####
  
  # Filtrar el conjunto de datos según el año, delito o distrito seleccionado
  dfDeli_filt <- reactive({
    filter(dfDeli, 
           Año %in% input$checkGroup_just_dfDeli_año,
           Delito %in% input$checkGroup_just_dfDeli_delito,
           Distrito %in% input$checkGroup_just_dfDeli_distrito)
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
    updateCheckboxGroup(session, "checkGroup_just_dfDeli_distrito", input, dfDeli$Distrito)
  })
  
  # Colores de las edades
  dfDeli_fill_Delito <- setColorFill(dfDeli, "Delito")
  
  # Grafico de Barras
  output$barPlot_just <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_delito <- length(input$checkGroup_just_dfDeli_delito) > 0
    has_año <- length(input$checkGroup_just_dfDeli_año) > 0
    has_distrito <- length(input$checkGroup_just_dfDeli_distrito) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_delito || !has_año || !has_distrito) {
      message <- "Seleccione Articulo(s) de Ley 54, Año(s) y Distrito(s)"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot(dfDeli_filt, x = "Año", y = "Casos", fill = "Delito",
                         title = "Radicación Anual de Casos por Distrito Fiscal según Ley 54",
                         xlab = "Año", ylab = "Cantidad de víctimas",
                         fillLab = "Artículo de Ley 54", colorFill = dfDeli_fill_Delito,
                         emptyMessage = "Seleccione Articulo(s) de Ley 54, Año(s) y Distrito(s)")
      p <- p + facet_wrap(~Distrito)
      p <- convert_to_plotly(p, tooltip = "text")
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(data = dfDeli_filt, x = "Año", y = "Casos", fill = "Delito",
                                                 title = "Casos de delitos por Distrito Fiscal según Artículo de la Ley 54",
                                                 xlab = "Año", ylab = "Cantidad de víctimas", message)
    ggplotly(empty_plot)
  })
  
  # Data table del DeptJust
  output$dataTable_just <- renderDT({
    renderDataTable(dfDeli_filt(), "Datos: Delitos según Artículo de la Ley 54")
  })
  
  
  # Crear Card con Fuentes
  output$dataTableUI_just <- renderUI({
    if (input$showTable_just) {
      hyperlinks <- c("https://www.justicia.pr.gov/")
      texts <- c("Departamento de Justicia")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 98%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;", 
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;",  
            DTOutput("dataTable_just")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
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
    updateCheckboxGroup(session, "checkGroup_just_mapaDeli_año", input, mapaDeli$Año)
  })
  
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
  
  # Crear Card con Fuentes
  output$dataTableUI_just_mapaFisc <- renderUI({
    hyperlinks <- c("https://www.justicia.pr.gov/")
    texts <- c("Departamento de Justicia")
    createFuenteDiv(hyperlinks, texts)
    
  })
  
  
  
  #### Tab de Definiciones ####
  definitions_just <- list(
    list(word = "Agresión Sexual Conyugal (Artículo 3.5)", definition = HTML("Se impondrá pena de reclusión, según se dispone más adelante, a toda persona que incurra en una relación sexual no consentida 
    con su cónyuge o ex cónyuge, o con la persona con quien cohabite o haya cohabitado, o con quien sostuviere o haya sostenido una relación consensual, o la persona con quien 
    haya procreado un hijo o hija, independientemente del sexo, estado civil, orientación sexual, identidad de género o estatus migratorio de cualquiera de las personas 
    involucradas en la relación, en cualesquiera de las circunstancias siguientes:
              <ul>    
              <br>
                <li> Si se ha compelido a incurrir en relación sexual mediante el empleo de fuerza, violencia, intimidación o amenaza de grave e inmediato daño corporal; o
                <li> Si se ha anulado o disminuido sustancialmente, sin su conocimiento o sin su consentimiento, su capacidad de consentir, a través de medios hipnóticos, narcóticos, deprimentes o estimulantes o sustancias o medios similares; o
                <li> Si por enfermedad o incapacidad mental, temporal o permanente, la víctima está incapacitada para comprender la naturaleza del acto en el momento de su realización;
                <li> Si se le obliga o induce mediante maltrato, violencia física o psicológica a participar o involucrarse en una relación sexual no deseada con terceras personas.
              </ul>")),
    list(word = "Incumplimiento de órdenes de protección (Artículo 2.8)", definition = "Cualquier violación a sabiendas de una orden de protección expedida."),
    list(word = "Maltrato (Artículo 3.1)", definition = "Toda persona que empleare fuerza física o violencia psicológica,intimidación o persecución en la persona de su cónyuge, ex cónyuge, o la persona con quien cohabita o haya cohabitado, o la persona con quien sostuviere o haya sostenido una relación consensual, o la persona con quien haya procreado un hijo o hija, independientemente del sexo, estado civil, orientación sexual, identidad de género o estatus migratorio de cualquiera de las personas involucradas en la relación, para causarle daño físico a su persona, a los bienes apreciados por ésta, excepto aquéllos que pertenecen privativamente al ofensor, o a la persona de otro o para causarle grave daño emocional, incurrirá en delito grave de cuarto grado en su mitad superior."),
    list(word = "Maltrato Agravado (Artículo 3.2)", definition = HTML("Se impondrá pena correspondiente a delito grave de tercer grado en su mitad inferior cuando en la persona del cónyuge, ex cónyuge o de la persona con quien se cohabita o se haya cohabitado, o con quien se sostiene o haya sostenido una relación consensual, o con quien se haya procreado un hijo o hija, independientemente del sexo, estado civil, orientación sexual, identidad de género o estatus migratorio de cualquiera de las personas involucradas en la relación, se incurriere en maltrato según tipificado en esta Ley, mediando una o más de las circunstancias siguientes:<br>
    <ul>
    <br>
      <li> Se penetrare en la morada de la persona o en el lugar donde esté albergada y se cometiere allí maltrato, en el caso de cónyuges o cohabitantes, independientemente del sexo, estado civil, orientación sexual, identidad de género o estatus migratorio de cualquiera de las personas involucradas en la relación, cuando éstos estuvieren separados o mediare una orden de protección ordenando el desalojo de la residencia a una de las partes; o</li>
      <li> cuando se infiriere grave daño corporal a la persona; o</li>
      <li> cuando se cometiere con arma mortífera en circunstancias que no revistiesen la intención de matar o mutilar; o</li>
      <li> cuando se cometiere en la presencia de menores de edad; o</li>
      <li> cuando se cometiere luego de mediar una orden de protección o resolución contra la persona acusada expedida en auxilio de la víctima del maltrato; o</li>
      <li> se indujere, incitare u obligare a la persona a drogarse con sustancias controladas, o cualquier otra sustancia o medio que altere la voluntad de la persona o a intoxicarse con bebidas embriagantes; o</li>
      <li> Cuando se cometiere y simultáneamente se incurriere en maltrato de un menor según definido en la Ley Núm. 177 de 1 de agosto de 2003.</li>
      <li> Si a la víctima se le obliga o induce mediante maltrato, violencia física o sicológica a participar o Involucrarse en una relación sexual no deseada con terceras personas.</li>
      <li> Cuando se cometiere contra una mujer embarazada.</li>
      <li> Cuando se cometiere contra una persona menor de dieciséis (16) años y la persona agresora sea de dieciocho (18) años o más.</li>
    </ul>")),
    list(word = "Maltrato Mediante Restricción de la Libertad (Artículo 3.4)", definition = "Maltrato mediante restricción de la libertad. Toda persona que utilice violencia o intimidación en la persona de su cónyuge, ex cónyuge, de la persona con quien cohabita o haya cohabitado, o con quien sostiene o haya sostenido una relación consensual, o la persona con quien haya procreado un hijo o hija, independientemente del sexo, estado civil, orientación sexual, identidad de género o estatus migratorio de cualquiera de las personas involucradas en la relación, o que utilice pretexto de que padece o de que una de las personas antes mencionadas padece de enfermedad o defecto mental, para restringir su libertad con el conocimiento de la víctima, incurrirá en delito grave de tercer grado en su mitad inferior."),
    list(word = "Maltrato Por Amenaza (Artículo 3.3)", definition = "Toda persona que amenaza con causarle daño a su cónyuge, ex cónyuge, a la persona con quien cohabita o con quien haya cohabitado o con quien sostiene o haya sostenido una relación consensual, o la persona con quien haya procreado un hijo o hija, independientemente del sexo, estado civil, orientación sexual, identidad de género o estatus migratorio de cualquiera de las personas involucradas en la relación, a los bienes apreciados por ésta, excepto aquéllos que pertenecen privativamente al ofensor, o a la persona de otro, incurrirá en delito grave de cuarto grado en su mitad superior.")
  )
  
  # Convertir lista a dataframe
  definitions_df_just <- do.call(rbind, lapply(definitions_just, as.data.frame))
  
  # Renombrar las columnas
  colnames(definitions_df_just) <- c("Concepto", "Definición")
  
  # Usar funcion para presentar tabla con definiciones
  output$dataTable_Def_just <- renderDT({
    renderDataTable_Definitions(definitions_df_just)
  })
  
  ########## Server del Departamento del Trabajo y Recursos Humanos ##########
  #### Tab de Participación Laboral (parLab) ####
  
  
  # Filtrar el conjunto de datos según los valores seleccionados del año y el tipo de incidente
  # parLab_filt <- reactive({
  #   filter(parLab,
  #          Año %in% input$checkGroup_trab_parLab_año,
  #          Sexo %in% input$checkGroup_trab_parLab_sexo)
  # })
  # 
  # ### funcion para el boton de deseleccionar/seleccionar del botón de año
  # observeEvent(input$deselectAll_trab_parLab_año, {
  #   updateCheckboxGroup(session, "checkGroup_trab_parLab_año", input, parLab$Año)
  # })
  # 
  # ### funcion para el boton de deseleccionar/seleccionar del botón de sexo
  # observeEvent(input$deselectAll_trab_parLab_sexo, {
  #   updateCheckboxGroup(session, "checkGroup_trab_parLab_sexo", input, parLab$Sexo)
  # })
  # 
  # # Colores de las edades
  # parLab_fill_sexo <- setColorFill(parLab, "Sexo")
  # # Grafico de barras
  # output$barPlot_trab_parLab <- renderPlotly({
  #   p <- renderBarPlot(parLab_filt, x = "Año", y = "Tasa", fill = "Sexo",
  #                      paste("Tasa de participación laboral según el sexo"),
  #                      xlab = "Año", ylab = "Tasa de participación", fillLab = "Sexo", 
  #                      colorFill = parLab_fill_sexo)
  # 
  #   ggplotly(p, tooltip = c("fill", "x", "y"))
  # })
  # 
  # # Data Table del SNMV
  # output$dataTable_trab_parLab <- renderDT({
  #   renderDataTable(parLab_filt())
  # })
  
  
  
  
  
  ########## Tab de la Administración de Vivienda Pública ##########
  #### Tab de Administración de Vivienda Pública (dfAvp) ####
  
  # Filtrar el conjunto de datos según los valores seleccionados del año y el tipo de incidente
  dfAvp_filt <- reactive({
    filter(dfAvp,
           Región %in% input$checkGroup_avp_dfAvp_región,
           Año %in% input$checkGroup_avp_dfAvp_año)
  })
  
  # Filtrar el conjunto de datos según el año, delito o distrito seleccionado
  mapaAvp_filt <- reactive({
    filter(mapaAvp, 
           Año %in% input$select_avp_mapaAvp_año)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de región
  observeEvent(input$deselectAll_avp_dfAvp_región, {
    updateCheckboxGroup(session, "checkGroup_avp_dfAvp_región", input, dfAvp$Región)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_avp_dfAvp_año, {
    updateCheckboxGroup(session, "checkGroup_avp_dfAvp_año", input, dfAvp$Año)
  })
  
  # Colores del status
  dfAvp_fill_status <- setColorFill(dfAvp, "Estado")
  
  
  # Grafico de barras
  output$barPlot_avp_dfAvp <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_region <- length(input$checkGroup_avp_dfAvp_región) > 0
    has_año <- length(input$checkGroup_avp_dfAvp_año) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_region || !has_año) {
      message <- "Seleccione Región de Vivienda y Año(s) a visualizar"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot(dfAvp_filt, x = "Año", y = "Cantidad", fill = "Estado",
                         paste("Viviendas Públicas Solicitadas y Asignadas Anualmente por Violencia Doméstica según Región."),
                         xlab = "Año", ylab = "Cantidad de viviendas públicas", fillLab = "Estado de la Vivienda",
                         colorFill = dfAvp_fill_status,
                         emptyMessage = "Seleccione Región de Vivienda y Año(s) a visualizar")
      p <- p + facet_wrap(~Región)
      p <- convert_to_plotly(p, tooltip = "text")
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(data = dfAvp_filt, x = "Año", y = "Cantidad", fill = "Estado",
                                                 paste("Viviendas Públicas Solicitadas y Asignadas Anualmente por Violencia Doméstica según Región."),
                                                 xlab = "Año", ylab = "Cantidad de viviendas públicas", message)
    ggplotly(empty_plot)
  })
  
  
  # mapa de las regiones de vivienda
  output$map_avp_mapaRegi <- renderPlotly({
    p <- renderMapGroup(
      data = mapaAvp, fill = Región,
      title = "Regiones de Vivienda ",
      fill_lab = "Region"
    )
    ggplotly(p, tooltip = c("all"))
  })
  
  output$map_avp_mapaAvp <- renderPlotly({
    p <- renderMap(
      data = mapaAvp_filt, fill = Cantidad,
      title = paste0("Total de viviendas públicas solicitadas y asignadas por violencia doméstica por región en el año ", input$select_avp_mapaAvp_año),
      group = Región,
      fill_lab = "Número de Viviendas",
      light_color = "lightgreen",
      dark_color = "darkgreen"
    )
    ggplotly(p + facet_wrap(~Estado),
             tooltip = c("all"))
  })
  
  dfAvp_rename <- reactive({
    dfAvp %>% 
      rename(`Región de Vivienda` = Región)
  })
  
  # Data Table para la gráfica de barras de dfAvp
  output$dataTable_avp_dfAvp <- renderDT({
    renderDataTable(dfAvp_rename(), "Datos: Viviendas públicas solicitadas y asignadas por violencia doméstica")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_avp_dfAvp <- renderUI({
    if (input$showTable_avp_dfAvp) {
      hyperlinks <- c("https://www.avp.pr.gov/")
      texts <- c("Administración de Vivienda Pública")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 98%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;", 
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;",  
            DTOutput("dataTable_avp_dfAvp")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
  })
  
  mapaAvp_filt_noGeom <- reactive({
    st_drop_geometry(mapaAvp_filt())%>% 
      rename(`Región de Vivienda` = Región)
  })
  
  # Data Table para el mapa de dfAvp
  output$dataTable_avp_mapaAvp <- renderDT({
    renderDataTable(mapaAvp_filt_noGeom(), "Datos: Viviendas públicas solicitadas y asignadas por violencia doméstica")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_avp_mapaAvp <- renderUI({
    if (input$showTable_avp_mapaAvp) {
      hyperlinks <- c("https://www.avp.pr.gov/")
      texts <- c("Administración de Vivienda Pública")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 98%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;", 
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;",  
            DTOutput("dataTable_avp_mapaAvp")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
  })
  
  
  #### Tab de Definiciones ####
  
  definitions_avp <- list(
    list(word = "Región", definition = "Se refiere a una división geográfica o área delimitada que comparte características similares, ya sea geográficas, culturales, económicas, políticas o administrativas. Subdivisión territorial establecida por las autoridades gubernamentales para propósitos de administración y gestión local. Estas divisiones pueden variar en tamaño y alcance dependiendo del país y su estructura administrativa."),
    list(word = "Vivienda Pública", definition = "Vivienda que es proporcionada, administrada o subsidiada por el gobierno o entidades gubernamentales con el objetivo de brindar alojamiento a personas o familias que tienen dificultades para acceder a una vivienda adecuada en el mercado privado debido a limitaciones económicas o sociales. Estas viviendas suelen estar dirigidas a personas de bajos ingresos, familias en situación de pobreza, personas sin hogar, o aquellos que enfrentan otras formas de vulnerabilidad social."),
    list(word = "Violencia Doméstica", definition = "Cuando una persona emplea fuerza física o violencia psicológica, intimidación o persecución en contra de su pareja o expareja. Esto, para causarle daño físico a su persona, a sus bienes, a otra persona o a un animal de servicio o mascota o para causarle grave daño emocional. Para que se considere violencia doméstica es necesario que exista o haya existido una relación afectiva entre las partes. Es decir, se da cuando la persona agresora es cónyuge, excónyuge, una persona con quien vive o ha vivido, con quien sostiene o haya sostenido una relación consensual o una persona con quien se haya procreado una hija o un hijo.")
  )
  
  # Convertir lista a dtaframe
  definitions_df_avp <- do.call(rbind, lapply(definitions_avp, as.data.frame))
  
  # Renombrar columnas
  colnames(definitions_df_avp) <- c("Concepto", "Definición")
  
  # Usar funcion para presentar tabla con definiciones
  output$dataTable_Def_avp <- renderDT({
    renderDataTable_Definitions(definitions_df_avp)
  })
  
  ########## Tab del Negociado de Policia ##########
  #### Tab con datos de mujeres desaparecidas (despDF) ####
  # Filtrar el conjunto de datos según los valores seleccionados del año y la categoria de evento
  despDF_filt <- reactive({
    filter(despDF,
           Estado %in% input$checkGroup_poli_despDF_categoría,
           Año %in% input$checkGroup_poli_despDF_año)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de categoria
  observeEvent(input$deselectAll_poli_despDF_categoría, {
    updateCheckboxGroup(session, "checkGroup_poli_despDF_categoría", input, despDF$Estado)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_poli_despDF_año, {
    updateCheckboxGroup(session, "checkGroup_poli_despDF_año", input, despDF$Año)
  })
  
  # Colores del status
  despDF_fill_categoria <- setColorFill(despDF, "Estado")
  
  # Grafico de barras
  
  output$barPlot_poli_despDF <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_categoria <- length(input$checkGroup_poli_despDF_categoría) > 0
    has_año <- length(input$checkGroup_poli_despDF_año) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_categoria || !has_año) {
      message <- "Seleccione Estado de la Víctima y Año(s) a visualizar"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot(despDF_filt, x = "Año", y = "Casos", fill = "Estado",
                         paste("Mujeres Desaparecidas: Localizadas y por Localizar"),
                         xlab = "Año", ylab = "Cantidad de víctimas", fillLab = "Estado de la Víctima",
                         colorFill = despDF_fill_categoria,
                         emptyMessage = "Seleccione Estado de la Víctima y Año(s) a visualizar")
      p <- convert_to_plotly(p, tooltip = "text")
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(data = despDF_filt, x = "Año", y = "Casos", fill = "Estado",
                                                 paste("Mujeres Desaparecidas: Localizadas y por Localizar"),
                                                 xlab = "Año", ylab = "Cantidad de víctimas", message)
    ggplotly(empty_plot)
  })
  
  despDF_filt_rename <- reactive({
    despDF_filt() %>% 
      rename(`Estado de la Víctima` = Estado)
  })
  
  # Data Table para el mapa de despDF
  output$dataTable_poli_despDF <- renderDT({
    renderDataTable(despDF_filt_rename(), "Datos: Mujeres desaparecidad y localizadas")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_poli_despDF <- renderUI({
    if (input$showTable_poli_despDF) {
      hyperlinks <- c("https://www.dsp.pr.gov/negociados/negociado-de-la-policia-de-puerto-rico")
      texts <- c("Negociado de Policía de Puerto Rico")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 98%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;",  
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;", 
            DTOutput("dataTable_poli_despDF")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
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
    # Verificar si hay opciones seleccionadas en cada grupo
    has_edad <- length(input$checkGroup_poli_vEdad_edad) > 0
    has_año <- length(input$checkGroup_poli_vEdad_año) > 0
    has_sexo <- length(input$checkGroup_poli_vEdad_sexo) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_edad || !has_año || !has_sexo) {
      message <- "Seleccione Grupo(s) de Edad, Sexo de la Víctima y Año(s) a visualizar"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot(vEdad_filt, x = "Año", y = "Casos", fill = "Edad",
                         paste("Incidencia de Violencia Doméstica por Edad de la Víctima"),
                         xlab = "Año", ylab = "Cantidad de víctimas", fillLab = "Grupo de Edad",
                         colorFill = vEdad_fill_edad,
                         emptyMessage = "Seleccione Grupo(s) de Edad, Sexo de la Víctima y Año(s) a visualizar",barWidth = 0, xGap = 0)
      p <- p + facet_wrap(~Sexo)
      p <- convert_to_plotly(p, tooltip = "text")
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(data = vEdad_filt, x = "Año", y = "Casos", fill = "Edad",
                                                 paste("Incidencia de Violencia Doméstica por Edad de la Víctima"),
                                                 xlab = "Año", ylab = "Cantidad de víctimas", message)
    ggplotly(empty_plot)
  })


  
  # Data Table para el mapa de despDF
  output$dataTable_poli_vEdad <- renderDT({
    renderDataTable(vEdad_filt(), "Datos: Incidentes de violencia doméstica")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_poli_vEdad  <- renderUI({
    if (input$showTable_poli_vEdad ) {
      hyperlinks <- c("https://www.dsp.pr.gov/negociados/negociado-de-la-policia-de-puerto-rico")
      texts <- c("Negociado de Policía de Puerto Rico")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 98%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;", 
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;",  
            DTOutput("dataTable_poli_vEdad")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
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
  

  #### Tab de Definiciones ####
  definitions_poli <- list(
    list(word = "Adultas Desaparecidas", definition = "Mujeres adultas cuya ubicación y paradero son desconocidos y no pueden ser determinados por familiares, amigos, o autoridades competentes. Puede surgir por razones como accidentes, secuestros, desastres naturales, o decisiones voluntarias de abandonar su entorno sin dejar rastro."),
    list(word = "Adultas Localizadas", definition = "Mujeres adultas cuyo paradero ha sido identificado y confirmado después de haber sido reportadas como desaparecidas."),
    list(word = "Adultas Sin Localizar", definition = "Mujeres adultas cuyo paradero no ha sido identificado ni confirmado tras haber sido reportadas como desaparecidas. Pueden haber sido vistas por última vez en circunstancias desconocidas, y su ubicación actual sigue siendo un misterio."),
    list(word = "Incidencia", definition = "Número de incidentes o delitos reportados o registrados por las fuerzas policiales durante un período específico en una determinada área geográfica. Usada por autoridades para medir y analizar la cantidad y tipo de delitos en una comunidad."),
    list(word = "Menores Desaparecidas", definition = "Menores femeninas cuya ubicación y paradero son desconocidos y no pueden ser determinados por familiares, amigos, o autoridades competentes. Puede surgir por razones como accidentes, secuestros, desastres naturales, o decisiones voluntarias de abandonar su entorno sin dejar rastro."),
    list(word = "Menores Localizadas", definition = "Menores femeninas cuyo paradero ha sido identificado y confirmado después de haber sido reportadas como desaparecidas."),
    list(word = "Menores Sin Localizar", definition = "Menores femeninas cuyo paradero no ha sido identificado ni confirmado tras haber sido reportadas como desaparecidas. Pueden haber sido vistas por última vez en circunstancias desconocidas, y su ubicación actual sigue siendo un misterio."),
    list(word = "Región Policiaca", definition = "Zona geográfica específica asignada a un cuerpo de policía para llevar a cabo funciones de vigilancia, patrullaje y protección del orden público. Delimitada por autoridades para organizar y distribuir eficazmente recursos policiales."),
    list(word = "Violencia Doméstica", definition = "Uso de fuerza física o violencia psicológica, intimidación o persecución contra una pareja o expareja para causar daño físico, a bienes, a otra persona, o a un animal de servicio o mascota, o para causar grave daño emocional. Requiere una relación afectiva previa o actual entre las partes."),
    list(word = "Víctima", definition = "Persona que ha sufrido daño físico, emocional, psicológico o financiero como resultado de un acto delictivo, un accidente, un desastre natural, o cualquier otro evento traumático.")
  )
  
  # Convertir lista a dataframe
  definitions_df_poli <- do.call(rbind, lapply(definitions_poli, as.data.frame))
  
  # Renombrar columnas
  colnames(definitions_df_poli) <- c("Concepto", "Definición")
  
  # Usar funcion para presentar tabla con definiciones
  output$dataTable_Def_poli <- renderDT({
    renderDataTable_Definitions(definitions_df_poli)
  })

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
  
  # Grafico de barras
  output$barPlot_opm_opmFemiVD <- renderPlotly({
    p <- renderLinePlot(data = opmFemiVD_filt, x = "Año", y = "Tasa", group = "1",
                        color = "1", title = "Tasa Anual de Asesinatos de Mujeres \n por Violencia Doméstica",
                        xlab = "Año", ylab = "Tasa por cada 100 mil mujeres",
                        emptyMessage = "Seleccione los Año(s) que desea visualizar")
    
    ggplotly(p, tooltip = "text")
  })
  
  
  # Data Table para el mapa de despDF
  output$dataTable_opm_opmFemiVD <- renderDT({
    renderDataTable(opmFemiVD_filt(), "Datos: Tasa de asesinatos de mujeres por violencia doméstica")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_opm_opmFemiVD  <- renderUI({
    if (input$showTable_opm_opmFemiVD) {
      hyperlinks <- c("https://www.mujer.pr.gov/")
      texts <- c("Oficina de la Procuradora de las Mujeres")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 98%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;",  
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;",  
            DTOutput("dataTable_opm_opmFemiVD")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
  })
  
  #### tab con datos de casos de violencia (opmCasos) ####
  # Filtrar el conjunto de datos según los valores seleccionados del año y la categoria de evento
  opmCasos_filt <- reactive({
    filter(opmCasos,
           Año %in% input$checkGroup_opm_opmCasos_año,
           Razón %in% input$checkGroup_opm_opmCasos_tipo
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_opm_opmCasos_año, {
    updateCheckboxGroup(session, "checkGroup_opm_opmCasos_año", input, opmCasos$Año)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de tipo de violencia
  observeEvent(input$deselectAll_opm_opmCasos_tipo, {
    updateCheckboxGroup(session, "checkGroup_opm_opmCasos_tipo", input, opmCasos$Razón)
  })
  
  # Colores del status
  opm_fill_tipo <- setColorFill(opmCasos, "Razón")
  
  # Grafico de barras
  output$barPlot_opm_opmCasos <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_opm_opmCasos_año) > 0
    has_tipo <- length(input$checkGroup_opm_opmCasos_tipo) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_año || !has_tipo) {
      message <- "Seleccione Razón de la consulta y Año(s) a visualizar"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot(opmCasos_filt, x = "Año", y = "Cantidad", fill = "Razón",
                         paste("Población Atendida por el Programa CRIAS: Razón de Consulta."),
                         xlab = "Año", ylab = "Cantidad de Personas Atendidas", fillLab = "Razón para Consulta",
                         colorFill = opm_fill_tipo,
                         emptyMessage = "Seleccione Razón de la consulta y Año(s) a visualizar")
      p <- convert_to_plotly(p, tooltip = "text")
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(data = opmCasos_filt, x = "Año", y = "Cantidad", fill = "Razón",
                                                 paste("Población Atendida por el Programa CRIAS: Razón de Consulta"),
                                                 xlab = "Año", ylab = "Cantidad de Personas Atendidas", message)
    ggplotly(empty_plot)
  })
  
  opmCasos_filt_rename <- reactive({
    opmCasos_filt() %>% 
      rename(`Razón para Consulta` = Razón) %>% 
      rename(`Personas Atendidas` = Cantidad)
  })
  
  # Data Table para el mapa de despDF
  output$dataTable_opm_opmCasos <- renderDT({
    renderDataTable(opmCasos_filt_rename(), "Datos: Población atendida mediante el programa CRIAS")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_opm_opmCasos  <- renderUI({
    if (input$showTable_opm_opmCasos) {
      hyperlinks <- c("https://www.mujer.pr.gov/")
      texts <- c("Oficina de la Procuradora de las Mujeres")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 98%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;", 
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;",  
            DTOutput("dataTable_opm_opmCasos")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
  })
  
  
  #### tab con datos del género de las víctimas (opmVic) ####
  # Filtrar el conjunto de datos según los valores seleccionados del año y el género de la víctima
  opmVic_filt <- reactive({
    filter(opmVic,
           Año %in% input$checkGroup_opm_opmVic_año,
           Género %in% input$checkGroup_opm_opmVic_género
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_opm_opmVic_año, {
    updateCheckboxGroup(session, "checkGroup_opm_opmVic_año", input, opmVic$Año)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón del género de la víctima
  observeEvent(input$deselectAll_opm_opmVic_género, {
    updateCheckboxGroup(session, "checkGroup_opm_opmVic_género", input, opmVic$Género)
  })
  
  # Colores del status
  opmVic_fill_género <- setColorFill(opmVic, "Género")
  
  # Grafico de barras
  output$barPlot_opm_opmVic <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_opm_opmVic_año) > 0
    has_genero <- length(input$checkGroup_opm_opmVic_género) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_año || !has_genero) {
      message <- "Seleccione Género y Año(s) a visualizar"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot(opmVic_filt, x = "Año", y = "Víctimas", fill = "Género",
                         paste("Identidad de Género de Víctimas asistidas por el Programa CRIAS"),
                         xlab = "Año", ylab = "Cantidad de Víctimas", fillLab = "Género de la Víctima",
                         colorFill = opmVic_fill_género,
                         emptyMessage = "Seleccione Género y Año(s) a visualizar")
      p <- convert_to_plotly(p, tooltip = "text")
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(data = opmVic_filt, x = "Año", y = "Víctimas", fill = "Género",
                                                 paste("Identidad de Género de Víctimas asistidas por el Programa CRIAS"),
                                                 xlab = "Año", ylab = "Cantidad de Víctimas", message)
    ggplotly(empty_plot)
  })
  
  
  # Data Table para el mapa de despDF
  output$dataTable_opm_opmVic <- renderDT({
    renderDataTable(opmVic_filt(), "Datos: Identidad de género de víctimas asistidas por el programa CRIAS")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_opm_opmVic  <- renderUI({
    if (input$showTable_opm_opmVic) {
      hyperlinks <- c("https://www.mujer.pr.gov/")
      texts <- c("Oficina de la Procuradora de las Mujeres")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 98%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;",  
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;",  
            DTOutput("dataTable_opm_opmVic")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
  })
  
  ##### tab con datos del género de las víctimas (opmMedio) ####
  # Filtrar el conjunto de datos según los valores seleccionados del año y el género de la víctima
  
  opmMedio_filt <- reactive({
    filter(opmMedio,
           Año %in% input$checkGroup_opm_opmMedio_año,
           Orientación %in% input$checkGroup_opm_opmMedio_medio
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_opm_opmMedio_año, {
    updateCheckboxGroup(session, "checkGroup_opm_opmMedio_año", input, opmMedio$Año)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón del medio de orientación
  observeEvent(input$deselectAll_opm_opmMedio_medio, {
    updateCheckboxGroup(session, "checkGroup_opm_opmMedio_medio", input, opmMedio$Orientación)
  })
  
  # Colores del status
  opmMedio_fill_medio <- setColorFill(opmMedio, "Orientación")
  
  # Grafico de barras
  output$barPlot_opm_opmMedio <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_opm_opmMedio_año) > 0
    has_medio <- length(input$checkGroup_opm_opmMedio_medio) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_año || !has_medio) {
      message <- "Seleccione Medio de Orientación y Año(s) a visualizar"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot(opmMedio_filt, x = "Año", y = "Cantidad", fill = "Orientación",
                         title = "Orientaciones brindadas por el Programa CRIAS",
                         xlab = "Año", ylab = "Cantidad de Personas Orientadas", fillLab = "Medio de Orientación",
                         colorFill = opmMedio_fill_medio,
                         emptyMessage = "Seleccione Medio de Orientación y Año(s) a visualizar")
      p <- convert_to_plotly(p, tooltip = "text")
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(data = opmMedio_filt, x = "Año", y = "Cantidad", fill = "Orientación",
                                                 title = "Orientaciones brindadas por el Programa CRIAS",
                                                 xlab = "Año", ylab = "Cantidad de Personas Orientadas", message)
    ggplotly(empty_plot)
  })
  
  opmMedio_filt_rename <- reactive({
    opmMedio_filt() %>% 
      rename(`Medio de Orientación` = Orientación) %>% 
      rename(`Personas Orientadas` = Cantidad)
  })

  # Data Table para opmMedio
  output$dataTable_opm_opmMedio <- renderDT({
    renderDataTable(opmMedio_filt_rename(), "Datos: Orientaciones ofrecidas mediante el programa CRIAS")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_opm_opmMedio  <- renderUI({
    if (input$showTable_opm_opmMedio) {
      hyperlinks <- c("https://www.mujer.pr.gov/")
      texts <- c("Oficina de la Procuradora de las Mujeres")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 98%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;",  
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;",  
            DTOutput("dataTable_opm_opmMedio")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
  })
  
  
  #### tab con datos de los servicios ofrecidos (opmServiciosMes) ####
  # Filtrar el conjunto de datos según los valores seleccionados del año y el tipo de servicio
  opmServiciosMes_filt <- reactive({
    filter(opmServiciosMes,
           Año %in% input$checkGroup_opm_opmServiciosMes_año,
           Servicio %in% input$checkGroup_opm_opmServiciosMes_tipo
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_opm_opmServiciosMes_año, {
    updateCheckboxGroup(session, "checkGroup_opm_opmServiciosMes_año", input, opmServiciosMes$Año)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de tipo de servicio
  observeEvent(input$deselectAll_opm_opmServiciosMes_tipo, {
    updateCheckboxGroup(session, "checkGroup_opm_opmServiciosMes_tipo", input, opmServiciosMes$Servicio)
  })
  
  # Colores del status
  opmServiciosMes_fill_tipo <- setColorFill(opmServiciosMes, "Servicio")
  
  # Grafico de barras
  output$barPlot_opm_opmServiciosMes <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_opm_opmServiciosMes_año) > 0
    has_tipo <- length(input$checkGroup_opm_opmServiciosMes_tipo) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_año || !has_tipo) {
      message <- "Seleccione Tipo de servicio y Año(s) a visualizar"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot(opmServiciosMes_filt, x = "Año", y = "Cantidad", fill = "Servicio",
                         title = "Atención, Servicios y Seguimiento mediante el Programa CRIAS",
                         xlab = "Año", ylab = "Cantidad de Servicios Ofrecidos", fillLab = "Tipo de Servicio",
                         colorFill = opmServiciosMes_fill_tipo,
                         emptyMessage = "Seleccione Tipo de servicio y Año(s) a visualizar")
      p <- convert_to_plotly(p, tooltip = "text")
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(data = opmServiciosMes_filt, x = "Año", y = "Cantidad", fill = "Servicio",
                                                 title = "Atención, Servicios y Seguimiento mediante el Programa CRIAS",
                                                 xlab = "Año", ylab = "Cantidad de Servicios Ofrecidos", message)
    ggplotly(empty_plot)
  })

  opmServiciosMes_filt_rename <- reactive({
    opmServiciosMes_filt() %>% 
      rename(`Tipo de Servicio` = Servicio) %>% 
      rename(`Servicios Ofrecidos` = Cantidad)
  })
  
  
  # Data Table para opmServiciosMes
  output$dataTable_opm_opmServiciosMes <- renderDT({
    renderDataTable(opmServiciosMes_filt_rename(), "Datos: Servicios ofrecidos por el programa CRIAS")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_opm_opmServiciosMes  <- renderUI({
    if (input$showTable_opm_opmServiciosMes) {
      hyperlinks <- c("https://www.mujer.pr.gov/")
      texts <- c("Oficina de la Procuradora de las Mujeres")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 98%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;", 
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;", 
            DTOutput("dataTable_opm_opmServiciosMes")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
  })
  
  #### Tab de Definiciones ####
  definitions_opm <- list(
    list(word = "Acecho (A)", definition = "Es una persona, en la mayoría de las ocasiones mujer que sufre o es sometida a un patrón o la repetición de una conducta mediante la cual se mantiene de manera constante o repetida una vigilancia, proximidad física o visual sobre una persona específica."),
    list(word = "Agresión sexual (AS)", definition = "Cualquier acto que degrada o daña el cuerpo y/o la sexualidad de la víctima y que por tanto atenta contra su libertad, dignidad e integridad física. Es una expresión de abuso de poder que implica la supremacía masculina sobre la mujer, al denigrar y concebirla como objeto."),
    list(word = "CRIAS", definition = "Centro de Respuesta Integrada de Apoyo y Servicios para la Mujer. La misma surgió de la necesidad imperante de trabajar con el problema de la desigualdad que existe contra las mujeres y trabajar particularmente con la violencia doméstica desde una perspectiva dirigida hacia la validación, orientación y coordinación de servicios de apoyo. El Centro CRIAS establece las bases para un modelo de prevención, intervención y fiscalización de los diferentes tipos de violencia que nos permite levantar información de las víctimas sobrevivientes participantes, obtener análisis de experiencias personales y manejo de actitudes ante el problema. En el mismo, se ofrecen servicios de orientación, coordinación de servicios y referidos a mujeres víctimas/sobrevivientes de violencia doméstica, agresión sexual, acecho y otras manifestaciones de violencia por razón de género."),
    list(word = "Discrimen de género (DG)", definition = "Hace referencia a «toda distinción, exclusión o restricción» que se realice en función del género de una persona con el objetivo o resultado de menoscabar o anular el reconocimiento, goce o ejercicio de sus derechos humanos. A menudo es consecuencia de los mitos y estereotipos de género tales como: las mujeres son las más aptas para ocuparse de la educación de los hijos, cocinar o limpiar, o para realizar trabajos de secretaría, enseñanza o enfermería, mientras que los hombres son líderes, buenos en economía y comercio. Esto ha dado lugar a un acceso desigual al mercado laboral, así como a un salario desigual para puestos similares, al sostenerse que las mujeres tienen peores resultados que los hombres en determinados ámbitos y, con ello, a una discriminación por género."),
    list(word = "Femenino", definition = "Se refiere a características, atributos o cualidades asociadas tradicionalmente con las mujeres o lo que se considera típicamente propio del género femenino."),
    list(word = "Feminicidios", definition = HTML("Es el crimen que consiste en matar intencionalmente a mujeres por el hecho de ser mujeres o de identificarse como tales. Las definiciones más amplias incluyen cualquier asesinato de mujeres o niñas, o el asesinato involuntario o indirecto de mujeres o niñas, «tal como demuestran algunos casos de violencia doméstica que podrían provocar la muerte de mujeres». El concepto «adquirió importancia en el marco del movimiento feminista de la década de 1970 cuando surge la expresión ‘femicidio’ como alternativa al término neutro ‘homicidio’, con el fin de reconocer y visibilizar la opresión, la desigualdad y la violencia sistemática» contra las mujeres que, en su forma más extrema, culmina en la muerte. El femicidio puede presentarse bajo diferentes formas e incluye los siguientes actos:
            <ul>   
            <br>
            <li> Femicidio íntimo, perpetrado por una pareja actual o anterior, generalmente durante o después de una relación ya violenta (por ejemplo, de violencia doméstica o violencia sexual);
            <li> El llamado crimen de honor (o asesinato de o por honor); 
            <li> El femicidio relacionado con la dote, que ocurre en un contexto de conflicto entre las familias de dos cónyuges recién casados, y es generalmente cometido por la familia política que cuestiona sobre todo el importe de la dote;
            <li> El femicidio no íntimo, cometido por un agresor que no tiene una relación íntima con la víctima, que está muy difundido en algunas partes de América Latina y que, por lo general, está precedido de actos de violencia sexual o tortura.;
            </ul>")),
    list(word = "Identidad de género", definition = "Se refiere a la manera en que una persona se identifica, cómo se reconoce o se expresa sobre sí misma, en cuanto al género que puede corresponder o no a su sexo biológico o asignado en su nacimiento."),
    list(word = "Masculino", definition = "Término utilizado para describir características, atributos o cualidades asociadas tradicionalmente con los hombres o lo que se considera típicamente propio del género masculino."),
    list(word = "Orientaciones", definition = "Direcciones o inclinaciones hacia las que se dirige o enfoca algo."),
    list(word = "Tendencia", definition = "Dirección o patrón observado en datos o eventos que muestra una inclinación hacia cierto resultado o comportamiento a lo largo del tiempo."),
    list(word = "Trans", definition = "Abreviatura comúnmente utilizada para referirse a personas que son transgénero o que tienen una identidad de género diferente de aquella que se les asignó al nacer. Las personas transgénero pueden identificarse como hombre, mujer, ambos, ninguno o con un género diferente al binario tradicional de hombre y mujer."),
    list(word = "Violencia doméstica (VD)", definition = "Definición que ofrece la Ley Núm. 54 de 1989 que sigue vigente en Puerto Rico. Violencia doméstica significa un patrón constante de empleo de fuerza física o violencia psicológica, intimidación o persecución contra una persona por parte de su cónyuge, ex cónyuge, una persona con quien cohabita o haya cohabitado, con quien sostiene o haya sostenido una relación consensual o una persona con quien se haya procreado una hija o hijo, para causarle daño físico a su persona, sus bienes u otra persona o para causarle grave daño emocional. La Ley Núm. 54 incluyó además como delito la agresión sexual entre personas que cohabitan o matrimonios como violencia doméstica."),
    list(word = "Violencia en cita (VC)", definition = "Violencia cometida por una persona que está o ha estado en una relación social de carácter romántico o íntimo con la víctima. La existencia de dicha relación se determinará con base en la declaración de la parte informante y teniendo en cuenta la duración de la relación, el tipo de relación y la frecuencia de interacción entre las personas involucradas en la relación. A los efectos de esta definición: La violencia en el noviazgo incluye, pero no se limita a, abuso sexual o físico o la amenaza de tal abuso. La violencia en el noviazgo no incluye actos cubiertos por la definición de violencia doméstica.")
  )
  
  
  # Convertir lista a dataframe
  definitions_df_opm <- do.call(rbind, lapply(definitions_opm, as.data.frame))
  
  # Renombrar columnas
  colnames(definitions_df_opm) <- c("Concepto", "Definición")
  
  # Usar funcion para presentar tabla con definiciones
  output$dataTable_Def_opm <- renderDT({
    renderDataTable_Definitions(definitions_df_opm)
  })
  
  ########## Tab del Departamento de Correción y Rehabilitación ##########
  #### tab con datos de los servicios ofrecidos (dcrCasosInv) ####
  
  # Filtrar el conjunto de datos según los valores seleccionados del año, el tipo de servicio y el sexo
  dcrCasosInv_filt <- reactive({
    filter(dcrCasosInv,
           Año %in% input$checkGroup_dcr_dcrCasosInv_year,
           Estado %in% input$checkGroup_dcr_dcrCasosInv_tipo,
           Sexo %in% input$checkGroup_dcr_dcrCasosInv_sexo
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_dcr_dcrCasosInv_year, {
    updateCheckboxGroup(session, "checkGroup_dcr_dcrCasosInv_year", input, dcrCasosInv$Año)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón del estado de investigación
  observeEvent(input$deselectAll_dcr_dcrCasosInv_tipo, {
    updateCheckboxGroup(session, "checkGroup_dcr_dcrCasosInv_tipo", input, dcrCasosInv$Estado)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de sexo
  observeEvent(input$deselectAll_dcr_dcrCasosInv_sexo, {
    updateCheckboxGroup(session, "checkGroup_dcr_dcrCasosInv_sexo", input, dcrCasosInv$Sexo)
  })
  
  # Colores del status
  dcrCasosInv_fill_tipo <- setColorFill(dcrCasosInv, "Estado")
  
  # Grafico de barras
  output$barPlot_dcr_dcrCasosInv <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_dcr_dcrCasosInv_year) > 0
    has_tipo <- length(input$checkGroup_dcr_dcrCasosInv_tipo) > 0
    has_sexo <- length(input$checkGroup_dcr_dcrCasosInv_sexo) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_año || !has_tipo || !has_sexo) {
      message <- "Seleccione Estado de la investigación, Sexo y Año(s) a visualizar"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot(dcrCasosInv_filt, x = "Año", y = "Cantidad", fill = "Estado",
                         title = "Casos en Supervisión de Ley 54: Programas Alternativos al Confinamiento",
                         xlab = "Año", ylab = "Cantidad de Servicios Ofrecidos", fillLab = "Estado de Investigación",
                         colorFill = dcrCasosInv_fill_tipo,
                         emptyMessage = "Seleccione Estado de la investigación, Sexo y Año(s) a visualizar")
      p <- p + facet_wrap(~Sexo)
      p <- convert_to_plotly(p, tooltip = "text")
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(data = dcrCasosInv_filt, x = "Año", y = "Cantidad", fill = "Estado",
                                                 title = "Casos en Supervisión de Ley 54: Programas Alternativos al Confinamiento",
                                                 xlab = "Año", ylab = "Cantidad de Servicios Ofrecidos", message)
    ggplotly(empty_plot)
  })
  

  dcrCasosInv_filt_rename <- reactive({
    dcrCasosInv_filt() %>% 
      rename(`Estado de Investigación` = Estado) 
  })
  
  # Data Table para dcrCasosInv
  output$dataTable_dcr_dcrCasosInv <- renderDT({
    renderDataTable(dcrCasosInv_filt_rename(), "Datos: Casos en supervisión de ley 54")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_dcr_dcrCasosInv  <- renderUI({
    if (input$showTable_dcr_dcrCasosInv) {
      hyperlinks <- c("https://dcr.pr.gov/")
      texts <- c("Departamento de Corrección y Rehabilitación")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 98%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;",  
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;", 
            DTOutput("dataTable_dcr_dcrCasosInv")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
  })
  
  
  
  #### tab con datos de personas sentenciadas integradas a Supervisión Electrónica (dcrSentenciadas) ####
  
  # Filtrar el conjunto de datos según los valores seleccionados del año y el estado del caso
  dcrSentenciadas_filt <- reactive({
    filter(dcrSentenciadas,
           Año %in% input$checkGroup_dcr_dcrSentenciadas_year,
           Estado %in% input$checkGroup_dcr_dcrSentenciadas_tipo
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_dcr_dcrSentenciadas_year, {
    updateCheckboxGroup(session, "checkGroup_dcr_dcrSentenciadas_year", input, dcrSentenciadas$Año)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón del estado de caso 
  observeEvent(input$deselectAll_dcr_dcrSentenciadas_tipo, {
    updateCheckboxGroup(session, "checkGroup_dcr_dcrSentenciadas_tipo", input, dcrSentenciadas$Estado)
  })
  
  # Colores del status
  dcrSentenciadas_fill_tipo <- setColorFill(dcrSentenciadas, "Estado")
  
  # Grafico de barras
  output$barPlot_dcr_dcrSentenciadas <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_dcr_dcrSentenciadas_year) > 0
    has_tipo <- length(input$checkGroup_dcr_dcrSentenciadas_tipo) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_año || !has_tipo) {
      message <- "Seleccione Estado del caso y Año(s) a visualizar"
    } else {
      upper_y_limit <- ceiling(max(eval(parse(text = "dcrSentenciadas_filt()$Cantidad")), na.rm = TRUE) * 1.2)
      
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- ggplot(dcrSentenciadas_filt(), aes(x = Fecha, y = Cantidad, fill = Estado)) +
        geom_bar(stat = "identity",
                 position = position_dodge2(width = 1, padding = 0.1),
                 aes(
                   text = paste(
                     paste0("<b>", "Cantidad de Personas Sentenciadas", ":</b> ", after_stat(y)), "<br>",
                     paste0("<b>", "Estado del Caso", ":</b> ", after_stat(fill)), "<br>"
                   )
                 )) +
        scale_fill_manual(values = dcrSentenciadas_fill_tipo) +
        scale_y_continuous(labels = function(x) scales::comma_format(big.mark = ",", decimal.mark = ".")(x) %>% paste0(" "),
                           expand = expansion(mult = c(0, 0.1))) +
        theme_minimal() +
        labs(title = "Sentenciados por Violencia Doméstica bajo \nSupervisión Electrónica",
             x = "Año", y = "Cantidad de Personas Sentenciadas", fill = "Estado del Caso") +
        coord_cartesian(ylim = c(0, upper_y_limit)) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, margin = margin(t = 10)),
          plot.title = element_text(hjust = 0.5, size = 15, colour = "black", face = "bold"),
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          plot.margin = margin(t = 45, r = 10, b = 10, l = 10))  
        
      p <- convert_to_plotly(p, tooltip = "text")
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(data = dcrSentenciadas_filt, x = "Fecha", y = "Cantidad", fill = "Estado",
                                                 title = "Sentenciados por Violencia Doméstica bajo \nSupervisión Electrónica",
                                                 xlab = "Año", ylab = "Cantidad de Personas Sentenciadas", message)
    ggplotly(empty_plot)
  })
  
  dcrSentenciadas_filt_rename <- reactive({
    dcrSentenciadas_filt() %>% 
      rename(`Estado del Caso` = Estado) 
  })

  
  # Data Table para dcrSentenciadas
  output$dataTable_dcr_dcrSentenciadas <- renderDT({
    renderDataTable(dcrSentenciadas_filt_rename(), "Datos: Programa de supervisión electrónica por delitos de violencia doméstica")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_dcr_dcrSentenciadas  <- renderUI({
    if (input$showTable_dcr_dcrSentenciadas) {
      hyperlinks <- c("https://dcr.pr.gov/")
      texts <- c("Departamento de Corrección y Rehabilitación")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 98%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;",  
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;",  
            DTOutput("dataTable_dcr_dcrSentenciadas")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
  })
  
  
  #### Tab de Definiciones ####
  definitions_dcr <- list(
    list(word = "Investigaciones realizadas", definition = "Proceso sistemático y metódico de recopilación, análisis y evaluación de información con el objetivo de obtener conclusiones, resolver problemas o generar conocimiento en un campo específico."),
    list(word = "Ley 54", definition = "Ley Núm. 54-1989, conocida como la “Ley para la Prevención e Intervención con la Violencia Doméstica”, según enmendada, establece la violencia doméstica como delito y lo define como el empleo de fuerza física o violencia psicológica, intimidación o persecución en contra de su pareja o expareja. Esto, para causarle daño físico a su persona, a sus bienes o a otra persona o para causarle grave daño emocional."),
    list(word = "Personas sentenciadas", definition = "Pronunciamiento que hace el juez o la jueza sobre la pena que se le impone a una persona acusada luego de que se determina que es culpable de cometer un delito."),
    list(word = "Programas de Comunidad", definition = "Son programas de tratamientos establecidos para que las personas convictas cumplan parte de su sentencia fuera de la institución penal. Su finalidad es promover que los convictos que estén capacitados para reintegrarse a la sociedad puedan hacerlo como parte de su rehabilitación moral y social."),
    list(word = "Programa de Supervisión Electrónica", definition = "El Programa de Monitoreo Electrónico cuenta con la Unidad Especializada de Monitoreo Electrónico (Unidad) compuesta por Oficiales Correccionales, la cual tiene la responsabilidad de supervisar y monitorear a los participantes pertenecientes al programa. Esta supervisión conlleva el verificar y atender las alertas que se activan a través del sistema de transmisión electrónica, activar el protocolo, solicitar apoyo inter agencial, avisar a la víctima y administrar pruebas toxicológicas, entre otras.")
  )
  
  # Convertir lista a dataframe
  definitions_df_dcr <- do.call(rbind, lapply(definitions_dcr, as.data.frame))
  
  # Renombrar columnas
  colnames(definitions_df_dcr) <- c("Concepto", "Definición")
  
  # Usar funcion para presentar tabla con definiciones
  output$dataTable_Def_dcr <- renderDT({
    renderDataTable_Definitions(definitions_df_dcr)
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
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_trib_OP_148_SoliGrupEdad_AñoFiscal) > 0
    has_edad <- length(input$checkGroup_trib_OP_148_SoliGrupEdad_Edad) > 0
    has_region <- length(input$checkGroup_trib_OP_148_SoliGrupEdad_Región) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_año || !has_edad || !has_region) {
      message <- "Seleccione Grupo(s) de Edad, Región Judicial y Año(s) a visualizar"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot(OP_148_SoliGrupEdad_filt, x = "AñoFiscal", y = "Solicitudes", fill = "Edad",
                         title = "Solicitudes de Órdenes de Protección bajo Ley 148 según Región Judicial y Edad",
                         xlab = "Año Fiscal", ylab = "Órdenes de Protección Solicitadas", fillLab = "Grupo de Edad",
                         colorFill = OP_148_SoliGrupEdad_fill_edad,
                         emptyMessage = "Seleccione Grupo(s) de Edad, Región Judicial y Año(s) a visualizar")
      p <- p + facet_wrap(~Región)
      p <- convert_to_plotly(p, tooltip = "text")
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(data = OP_148_SoliGrupEdad_filt, x = "AñoFiscal", y = "Solicitudes", fill = "Edad",
                                                 title = "Solicitudes de Órdenes de Protección bajo Ley 148, según Región Judicial y Edad",
                                                 xlab = "Año Fiscal", ylab = "Órdenes de Protección Solicitadas", message)
    ggplotly(empty_plot)
  })
  
  OP_148_SoliGrupEdad_filt_rename <- reactive({
    OP_148_SoliGrupEdad_filt() %>% 
      rename(`Año Fiscal` = AñoFiscal)  %>% 
      rename(`Región Judicial` = Región)
  })
  
  
  # Data Table para dcrCasosInv
  output$dataTable_OP_148_SoliGrupEdad <- renderDT({
    renderDataTable(OP_148_SoliGrupEdad_filt_rename(), "Datos: Órdenes de Protección Solicitadas por Violencia Sexual")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_OP_148_SoliGrupEdad  <- renderUI({
    if (input$showTable_OP_148_SoliGrupEdad) {
      hyperlinks <- c("https://poderjudicial.pr/mision-y-vision-de-la-rama-judicial/")
      texts <- c("Oficina de Administración de los Tribunales, Directoría de Operaciones, Oficina de Estadísticas")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 98%; margin-top: 10px; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",
       
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;",  
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;",  
            DTOutput("dataTable_OP_148_SoliGrupEdad")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
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
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_trib_OP_Ley148_ex_parteEmitidas_AñoFiscal) > 0
    has_delito <- length(input$checkGroup_trib_OP_Ley148_ex_parteEmitidas_Delito) > 0
    has_region <- length(input$checkGroup_trib_OP_Ley148_ex_parteEmitidas_Región) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_año || !has_delito || !has_region) {
      message <- "Seleccione Delito(s), Región Judicial y Año(s) a visualizar"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot(OP_Ley148_ex_parteEmitidas_filt, x = "AñoFiscal", y = "ÓrdenesEmitidas", fill = "Delito",
                         title = "Órdenes de Protección Ex Parte emitidas bajo Ley 148, según Región Judicial y delito cometido",
                         xlab = "Año fiscal", ylab = "Órdenes de Protección Emitidas", fillLab = "Delito Cometido",
                         colorFill = OP_Ley148_ex_parteEmitidas_fill_delito,
                         emptyMessage = "Seleccione Delito(s), Región Judicial y Año(s) a visualizar")
      p <- p + facet_wrap(~Región)
      p <- convert_to_plotly(p, tooltip = "text")
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(OP_Ley148_ex_parteEmitidas_filt, x = "AñoFiscal", y = "ÓrdenesEmitidas", fill = "Delito",
                                                 title = "Órdenes de Protección Ex Parte emitidas bajo Ley 148, según Región Judicial y delito cometido",
                                                 xlab = "Año fiscal", ylab = "Órdenes de Protección Emitidas", message)
    ggplotly(empty_plot)
  })
  
  OP_Ley148_ex_parteEmitidas_filt_rename <- reactive({
    OP_Ley148_ex_parteEmitidas_filt() %>%  
      rename(`Año Fiscal` = AñoFiscal)  %>% 
      rename(`Órdenes Emitidas` = ÓrdenesEmitidas) %>% 
      rename(`Región Judicial` = Región) %>% 
      rename(`Delito Cometido` = Delito)
  })
  
  
  # Data Table para dcrCasosInv
  output$dataTable_OP_Ley148_ex_parteEmitidas <- renderDT({
    renderDataTable(OP_Ley148_ex_parteEmitidas_filt_rename(), "Datos: Órdenes de Protección Ex Parte Emitidas bajo Ley 148")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_OP_Ley148_ex_parteEmitidas <- renderUI({
    if (input$showTable_OP_Ley148_ex_parteEmitidas) {
      hyperlinks <- c("https://poderjudicial.pr/mision-y-vision-de-la-rama-judicial/")
      texts <- c("Oficina de Administración de los Tribunales, Directoría de Operaciones, Oficina de Estadísticas")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 90%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;", 
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;", 
            DTOutput("dataTable_OP_Ley148_ex_parteEmitidas")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
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
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_trib_OP_LEY148Archivadas_AñoFiscal) > 0
    has_razon <- length(input$checkGroup_trib_OP_LEY148Archivadas_Razón) > 0
    has_region <- length(input$checkGroup_trib_OP_LEY148Archivadas_Región) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_año || !has_razon || !has_region) {
      message <- "Seleccione Razón, Distrito Fiscal y Año(s) a visualizar"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot(OP_LEY148Archivadas_filt, x = "AñoFiscal", y = "ÓrdenesArchivadas", fill = "Razón",
                         title = "Órdenes de Protección Ex Parte Archivadas bajo Ley 148 según Región Judicial",
                         xlab = "Año fiscal", ylab = "Órdenes de Protección Archivadas", fillLab = "Razón de Archivo",
                         colorFill = OP_LEY148Archivadas_fill_Razón,
                         emptyMessage = "Seleccione Razón, Distrito Fiscal y Año(s) a visualizar")
      p <- p + facet_wrap(~Región)
      p <- convert_to_plotly(p, tooltip = "text")
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(OP_LEY148Archivadas_filt, x = "AñoFiscal", y = "ÓrdenesArchivadas", fill = "Razón",
                                                 title = "Órdenes de Protección Ex Parte Archivadas bajo Ley 148 según Región Judicial",
                                                 xlab = "Año fiscal", ylab = "Órdenes de Protección Archivadas", message)
    ggplotly(empty_plot)
  })

  
  OP_LEY148Archivadas_filt_rename <- reactive({
    OP_LEY148Archivadas_filt() %>% 
      rename(`Año Fiscal` = AñoFiscal)  %>% 
      rename(`Distrito Fiscal` = Región) %>% 
      rename(`Órdenes Archivadas` = ÓrdenesArchivadas)%>% 
      rename(`Razón de Archivo` = Razón)
  })
  
  # Data Table 
  output$dataTable_OP_LEY148Archivadas <- renderDT({
    renderDataTable(OP_LEY148Archivadas_filt_rename(), "Datos: Órdenes de Protección Ex Parte archivadas bajo Ley 148")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_OP_LEY148Archivadas  <- renderUI({
    if (input$showTable_OP_LEY148Archivadas) {
      hyperlinks <- c("https://poderjudicial.pr/mision-y-vision-de-la-rama-judicial/")
      texts <- c("Oficina de Administración de los Tribunales, Directoría de Operaciones, Oficina de Estadísticas")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 75%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;",  
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;",  
            DTOutput("dataTable_OP_LEY148Archivadas")
          )
        ),
     
        createFuenteDiv(hyperlinks, texts)
      )
    }
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
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_trib_OP_LEY148Denegadas_AñoFiscal) > 0
    has_razon <- length(input$checkGroup_trib_OP_LEY148Denegadas_Razón) > 0
    has_region <- length(input$checkGroup_trib_OP_LEY148Denegadas_Región) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_año || !has_razon || !has_region) {
      message <- "Seleccione Razón, Región Judicial y Año(s) a visualizar"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot(OP_LEY148Denegadas_filt, x = "AñoFiscal", y = "ÓrdenesDenegadas", fill = "Razón",
                         title = "Órdenes de protección denegadas bajo Ley 148 por Razón de Archivo según Región Judicial",
                         xlab = "Año fiscal", ylab = "Órdenes de Protección Denegadas", fillLab = "Razón de Archivo",
                         colorFill = OP_LEY148Denegadas_fill_Razón,
                         emptyMessage = "Seleccione Razón, Región Judicial y Año(s) a visualizar")
      p <- p + facet_wrap(~Región)
      p <- convert_to_plotly(p, tooltip = "text")
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(OP_LEY148Denegadas_filt, x = "AñoFiscal", y = "ÓrdenesDenegadas", fill = "Razón",
                                                 title = "Órdenes de protección denegadas bajo Ley 148 por Razón de Archivo según Región Judicial",
                                                 xlab = "Año fiscal", ylab = "Órdenes de Protección Denegadas", message)
    ggplotly(empty_plot)
  })
  
  OP_LEY148Denegadas_filt_rename <- reactive({
    OP_LEY148Denegadas_filt() %>% 
      rename(`Año Fiscal` = AñoFiscal)  %>% 
      rename(`Región Judicial` = Región) %>% 
      rename(`Órdenes Denegadas` = ÓrdenesDenegadas)%>% 
      rename(`Razón de Archivo` = Razón)
  })
  
  
  # Data Table 
  output$dataTable_OP_LEY148Denegadas <- renderDT({
    renderDataTable(OP_LEY148Denegadas_filt_rename(), "Datos: Ordenes de protección denegadas por violencia sexual bajo Ley 148")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_OP_LEY148Denegadas  <- renderUI({
    if (input$showTable_OP_LEY148Denegadas) {
      hyperlinks <- c("https://poderjudicial.pr/mision-y-vision-de-la-rama-judicial/")
      texts <- c("Oficina de Administración de los Tribunales, Directoría de Operaciones, Oficina de Estadísticas")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 85%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;", 
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;",  
            DTOutput("dataTable_OP_LEY148Denegadas")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
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
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_trib_OP_LEY148FinalEmitidas_AñoFiscal) > 0
    has_delito <- length(input$checkGroup_trib_OP_LEY148FinalEmitidas_Delito) > 0
    has_region <- length(input$checkGroup_trib_OP_LEY148FinalEmitidas_Región) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_año || !has_delito || !has_region) {
      message <- "Seleccione Delito(s), Región Judicial y Año(s) a visualizar"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot(OP_LEY148FinalEmitidas_filt, x = "AñoFiscal", y = "ÓrdenesEmitidas", fill = "Delito",
                         title = "Órdenes de protección emitidas bajo Ley 148, según Región Judicial y Tipo de Delito",
                         xlab = "Año Fiscal", ylab = "Órdenes de Protección Emitidas", fillLab = "Delito Cometido",
                         colorFill = OP_LEY148FinalEmitidas_fill_Delito,
                         emptyMessage = "Seleccione Delito(s), Región Judicial y Año(s) a visualizar")
      p <- p + facet_wrap(~Región)
      p <- convert_to_plotly(p, tooltip = "text")
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(OP_LEY148FinalEmitidas_filt, x = "AñoFiscal", y = "ÓrdenesEmitidas", fill = "Delito",
                                                 title = "Órdenes de protección emitidas bajo Ley 148, según Región Judicial y Tipo de Delito",
                                                 xlab = "Año Fiscal", ylab = "Órdenes de Protección Emitidas", message)
    ggplotly(empty_plot)
  })

  OP_LEY148FinalEmitidas_filt_rename <- reactive({
    OP_LEY148FinalEmitidas_filt() %>% 
      rename(`Año Fiscal` = AñoFiscal)  %>% 
      rename(`Región Judicial` = Región) %>% 
      rename(`Órdenes Emitidas` = ÓrdenesEmitidas)%>% 
      rename(`Delito Cometido` = Delito)
  })
  
  
  # Data Table 
  output$dataTable_OP_LEY148FinalEmitidas <- renderDT({
    renderDataTable(OP_LEY148FinalEmitidas_filt_rename(), "Datos: Órdenes de protección emitidas bajo ley 148")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_OP_LEY148FinalEmitidas  <- renderUI({
    if (input$showTable_OP_LEY148FinalEmitidas) {
      hyperlinks <- c("https://poderjudicial.pr/mision-y-vision-de-la-rama-judicial/")
      texts <- c("Oficina de Administración de los Tribunales, Directoría de Operaciones, Oficina de Estadísticas")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 87%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;", 
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;", 
            DTOutput("dataTable_OP_LEY148FinalEmitidas")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
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
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_trib_OP_LEY148Genero_AñoFiscal) > 0
    has_parte <- length(input$checkGroup_trib_OP_LEY148Genero_Parte) > 0
    has_sexo <- length(input$checkGroup_trib_OP_LEY148Genero_Sexo) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_año || !has_parte || !has_sexo) {
      message <- "Seleccione Parte(s), Sexo y Año(s) a visualizar"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot(OP_LEY148Genero_filt, x = "AñoFiscal", y = "Solicitudes", fill = "Parte",
                         title = "Órdenes de Protección Emitidas bajo Ley 148, según la Parte",
                         xlab = "Año fiscal", ylab = "Solicitudes de Ordenes de Protección", fillLab = "Parte",
                         colorFill = OP_LEY148Genero_fill_Parte,
                         emptyMessage = "Seleccione Parte(s), Sexo y Año(s) a visualizar")
      p <- p + facet_wrap(~Sexo)
      p <- convert_to_plotly(p, tooltip = "text")
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(OP_LEY148Genero_filt, x = "AñoFiscal", y = "Solicitudes", fill = "Parte",
                                                 title = "Órdenes de Protección Emitidas bajo Ley 148, según la Parte",
                                                 xlab = "Año fiscal", ylab = "Solicitudes de Ordenes de Protección", message)
    ggplotly(empty_plot)
  })
  
  OP_LEY148Genero_filt_rename <- reactive({
    OP_LEY148Genero_filt() %>% 
      rename(`Año Fiscal` = AñoFiscal) 
  })
  
  # Data Table 
  output$dataTable_OP_LEY148Genero <- renderDT({
    renderDataTable(OP_LEY148Genero_filt_rename(), "Datos: Órdenes de Protección Emitidas bajo Ley 148")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_OP_LEY148Genero  <- renderUI({
    if (input$showTable_OP_LEY148Genero) {
      hyperlinks <- c("https://poderjudicial.pr/mision-y-vision-de-la-rama-judicial/")
      texts <- c("Oficina de Administración de los Tribunales, Directoría de Operaciones, Oficina de Estadísticas")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 90%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;", 
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;", 
            DTOutput("dataTable_OP_LEY148Genero")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
  })
  
  
  #### (tribCasosCrim) ####
  tribCasosCrim_filt <- reactive({
    filter(tribCasosCrim,
           AñoFiscal %in% input$checkGroup_trib_tribCasosCrim_AñoFiscal,
           Delito %in% input$checkGroup_trib_tribCasosCrim_Delito,
           Casos %in% input$checkGroup_trib_tribCasosCrim_Casos
    )
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el año fiscal
  observeEvent(input$deselectAll_trib_tribCasosCrim_Casos, {
    updateCheckboxGroup(session, "checkGroup_trib_tribCasosCrim_AñoFiscal", input, tribCasosCrim$AñoFiscal)
  })
  
  ### funcion para el botón de deseleccionar/seleccionar la parte peticionaria
  observeEvent(input$deselectAll_trib_tribCasosCrim_Casos, {
    updateCheckboxGroup(session, "checkGroup_trib_tribCasosCrim_Delito", input, tribCasosCrim$Delito)
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el sexo de la parte
  observeEvent(input$deselectAll_trib_tribCasosCrim_Casos, {
    updateCheckboxGroup(session, "checkGroup_trib_tribCasosCrim_Casos", input, tribCasosCrim$Casos)
  })
  
  # Colores de las partes
  tribCasosCrim_fill_Delito <- setColorFill(tribCasosCrim, "Delito")
  
  # Grafico de barras
  output$barPlot_tribCasosCrim <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_trib_tribCasosCrim_AñoFiscal) > 0
    has_delito <- length(input$checkGroup_trib_tribCasosCrim_Delito) > 0
    has_casos <- length(input$checkGroup_trib_tribCasosCrim_Casos) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_año || !has_delito || !has_casos) {
      message <- "Seleccione Delito(s), Estado del caso y Año(s) a visualizar"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot(tribCasosCrim_filt, x = "AñoFiscal", y = "Cantidad", fill = "Delito",
                         title = "Movimiento Anual de Casos de Violencia Doméstica en el Tribunal según Ley 54",
                         xlab = "Año Fiscal", ylab = "Solicitudes de Órdenes de Protección", fillLab = "Delito Cometido",
                         colorFill = tribCasosCrim_fill_Delito,
                         emptyMessage = "Seleccione Delito(s), Estado del caso y Año(s) a visualizar")
      p <- p + facet_wrap(~Casos)
      p <- convert_to_plotly(p, tooltip = "text")
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(tribCasosCrim_filt, x = "AñoFiscal", y = "Cantidad", fill = "Delito",
                                                 title = "Movimiento Anual de Casos de Violencia Doméstica en el Tribunal según Ley 54",
                                                 xlab = "Año Fiscal", ylab = "Solicitudes de Órdenes de Protección", message)
    ggplotly(empty_plot)
  })
  
  
  tribCasosCrim_filt_rename <- reactive({
    tribCasosCrim_filt() %>% 
      rename(`Año Fiscal` = AñoFiscal) %>% 
      rename(`Estado del Caso` = Casos)%>% 
      rename(`Delito Cometido` = Delito)
  })
  
  # Data Table 
  output$dataTable_tribCasosCrim <- renderDT({
    renderDataTable(tribCasosCrim_filt_rename(), "Datos: Ordenes de Protección según delito cometido")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_tribCasosCrim <- renderUI({
    if (input$showTable_tribCasosCrim) {
      hyperlinks <- c("https://poderjudicial.pr/mision-y-vision-de-la-rama-judicial/")
      texts <- c("Oficina de Administración de los Tribunales, Directoría de Operaciones, Oficina de Estadísticas")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 90%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;", 
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;", 
            DTOutput("dataTable_tribCasosCrim")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
  })
  
  
  
  #### Tab de Definiciones ####
  definitions_trib <- list(
    list(word = "Año Fiscal", definition = "Período de 12 meses comprendido entre el 1ro de julio de un año y el 30 de junio del año siguiente, y que se usa como el calendario presupuestario de las agencias públicas."),
    list(word = "Ley 148", definition = "Conocida como la “Ley para la Protección de las Víctimas de Violencia Sexual en Puerto Rico”, según enmendada, establece los mecanismos para la expedición de órdenes de protección para víctimas de los delitos de agresión sexual, actos lascivos, acoso sexual e incesto."),
    list(word = "Ley 54", definition = "La Ley Núm. 54-1989, conocida como la “Ley para la Prevención e Intervención con la Violencia Doméstica”, según enmendada, establece la violencia doméstica como delito y lo define como el empleo de fuerza física o violencia psicológica, intimidación o persecución en contra de su pareja o expareja. Esto, para causarle daño físico a su persona, a sus bienes o a otra persona o para causarle grave daño emocional"),
    list(word = "Organización", definition = "Poder Judicial de Puerto Rico, Oficina de Administración de los Tribunales, Directoria de Operaciones, Oficina de Estadísticas."),
    list(word = "Orden de Protección", definition = "Es un remedio civil expedido por escrito bajo el sello de un Tribunal, en la cual se dictan las medidas a una persona agresora para que ésta se abstenga de incurrir o llevar a cabo determinados actos o conducta constitutivos de violencia doméstica."),
    list(word = "Órdenes de protección ex parte", definition = "Es una orden emitida por el Tribunal de Primera Instancia luego de escuchar a la parte peticionaria (persona que solicita la orden) y hacer una determinación provisional sobre los hechos."),
    list(word = "Peticionaria", definition = "Persona que solicita una orden de protección."),
    list(word = "Región Judicial", definition = "Se refiere a la región judicial a la que corresponden los datos informados. El Tribunal de Primera Instancia se distribuye territorialmente en trece regiones judiciales. Cada región judicial está compuesta por un centro judicial y sus respectivas salas superiores y municipales."),
    list(word = "Sexo", definition = "Indica si la persona que solicita la orden de protección, en el periodo de tiempo de interés en la región judicial especificada, se identifica como hombre o mujer."),
    list(word = "Solicitudes de órdenes de protección", definition = "Se define como todas las peticiones de orden de protección realizadas en el periodo de tiempo de interés en la región judicial especificada."),
    list(word = "Violencia Sexual", definition = "Cualquier acto que degrada o daña el cuerpo y/o la sexualidad de la víctima y que por tanto atenta contra su libertad, dignidad e integridad física. Es una expresión de abuso de poder que implica la supremacía masculina sobre la mujer, al denigrar y concebirla como objeto.")
  )
  
  # Convertir lista a dataframe
  definitions_df_trib <- do.call(rbind, lapply(definitions_trib, as.data.frame))
  
  # Renombrar columnas
  colnames(definitions_df_trib) <- c("Concepto", "Definición")
  
  # Usar funcion para presentar tabla con definiciones
  output$dataTable_Def_trib <- renderDT({
    renderDataTable_Definitions(definitions_df_trib)
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
  
  # Descartar filar con Total de Kits para omitir su representacion en la grafica
  safekitsDF_filt_noTotal <- reactive({
    safekitsDF_filt() %>%  
      filter(Kits != "Total de Kits")
  })
  
  
  # Grafico de barras
  output$barPlot_safekitsDF <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_cavv_safekitsDF_Año) > 0
    has_kits <- length(input$checkGroup_cavv_safekitsDF_Kits) > 0

    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_año || !has_kits) {
      message <- "Seleccione Estado de querella y Año(s) a visualizar"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      
      # p <- renderBarPlot(safekitsDF_filt, x = "Año", y = "Total", fill = "Kits", 
      #                    title = HTML("Tendencia anual del equipo de <i>SAFE Kits</i> en casos de violencia sexual"), 
      #                    xlab = "Año", ylab = "Total de Kits Distribuidos", fillLab = "Estado de Querella", 
      #                    colorFill = safekitsDF_fill_Kits,
      #                    emptyMessage = "Seleccione Estado de querella y Año(s) a visualizar")
      
      # p <- renderBarPlot(safekitsDF_filt_noTotal, x = "Año", y = "Total", fill = "Kits",
      #                    title = HTML("Tendencia anual del equipo de <i>SAFE Kits</i> en casos de violencia sexual"),
      #                    xlab = "Año", ylab = "Total de Kits Distribuidos", fillLab = "Estado de Querella",
      #                    colorFill = safekitsDF_fill_Kits,
      #                    emptyMessage = "Seleccione Estado de querella y Año(s) a visualizar")
      
      p <- renderBarPlot_stack(safekitsDF_filt, x = "Año", y = "Total", fill = "Kits",
                         title = HTML("Tendencia Anual de <i>SAFE Kits<i> por Estado de Querella"),
                         xlab = "Año", ylab = "Total de Kits Distribuidos", fillLab = "Estado de Querella",
                         colorFill = safekitsDF_fill_Kits,
                         emptyMessage = "Seleccione Estado de querella y Año(s) a visualizar")
      
      # p <- renderBarPlot_stack2(safekitsDF_filt, x = "Año", y = "Total", fill = "Kits",
      #                          title = HTML("Tendencia anual del equipo de <i>SAFE Kits</i> en casos de violencia sexual"),
      #                          xlab = "Año", ylab = "Total de Kits Distribuidos", fillLab = "Estado de Querella",
      #                          colorFill = safekitsDF_fill_Kits,
      #                          emptyMessage = "Seleccione Estado de querella y Año(s) a visualizar")
      
      p <- convert_to_plotly(p, tooltip = "text")
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    # empty_plot <- create_empty_plot_with_message(safekitsDF_filt, x = "Año", y = "Total", fill = "Kits", 
    #                                              title = HTML("Tendencia anual del equipo de <i>SAFE Kits</i> en casos de violencia sexual por estado de querella"), 
    #                                              xlab = "Año", ylab = "Total de Kits Distribuidos", message)
    
    empty_plot <- create_empty_plot_with_message(safekitsDF_filt, x = "Año", y = "Total", fill = "Kits", 
                                                 title = HTML("Tendencia Anual de <i>SAFE Kits<i> por Estado de Querella"), 
                                                 xlab = "Año", ylab = "Total de Kits Distribuidos", message)
    
    ggplotly(empty_plot)
  })

  
  safekitsDF_filt_rename <- reactive({
    safekitsDF_filt() %>%  
      rename(`SAFE Kits` = Kits) %>%  
      rename(`Total Distribuidos` = Total)
  })
  
  
  # Data Table 
  output$dataTable_safekitsDF <- renderDT({
    renderDataTable(safekitsDF_filt_rename(), "Datos: Tendencia anual del equipo de recolecta de evidencia de SAFE Kits en casos de violencia sexual")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_safekitsDF  <- renderUI({
    if (input$showTable_safekitsDF) {
      hyperlinks <- c("https://www.salud.pr.gov/CMS/104",
                      "https://www.salud.pr.gov/")
      texts <- c("Centro de Ayuda a Victimas de Violación", 
                 "Departamento de Salud")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 98%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;",  
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;", 
            DTOutput("dataTable_safekitsDF")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
  })
  
  
  #### Tab de Definiciones ####
  definitions_cavv <- list(
    list(word = "Actos Lascivos", definition = "El Código Penal de Puerto Rico define el delito de actos lascivos como aquel en el cual, sin intentar consumar el delito de agresión sexual, se someta a otra persona a un acto que tienda a despertar, excitar o satisfacer la pasión o deseos sexuales de la persona imputada."),
    list(word = "Agresión Sexual", definition = HTML("El Código Penal de Puerto Rico define el delito de agresión sexual como llevar a cabo, o provocar que otra persona lleve a cabo, un acto orogenital o una penetración sexual (vaginal o anal, ya sea esta genital, digital o instrumental) en cualquiera de estas circunstancias:
          <ul>      
          <br>
          <li> Si a la víctima se le disminuyó, sin esta consentir o sin saberlo, su capacidad de consentir mediante algún medio hipnótico, narcótico, deprimente o estimulante.; 
          <li> Si a la víctima se le obligó al acto por medio de fuerza física, violencia o intimidación.; 
          <li> Si al momento del acto la víctima no tenía capacidad para consentir y la persona agresora lo sabía.; 
          <li> Si la víctima consintió porque se le engañó sobre la identidad de la persona agresora y creía que era otra persona.; 
          <li> Si la víctima no ha cumplido 16 años de edad.; 
          <li> Si por enfermedad o incapacidad mental la víctima no puede comprender el acto en el momento en que ocurre.;
          </ul>")),
    list(word = "Evidencia", definition = "Se refiere a cualquier información, datos, pruebas o testimonios que respaldan una afirmación, teoría o argumento. En diferentes contextos, la evidencia puede ser utilizada para respaldar conclusiones científicas, legales, filosóficas o incluso personales. La calidad y fiabilidad de la evidencia pueden variar según la fuente, el método de recopilación y el contexto en el que se utilice. En general, se busca que la evidencia sea objetiva, verificable y relevante para el tema en cuestión."),
    list(word = "Kits", definition = "Equipo utilizado por profesionales de la salud para la recuperación de evidencia forense en casos de violencia sexual. Consiste en una caja que contiene instrucciones y materiales que facilitan la recoleccion de evidencia. También conocido como <i>Sexual Assault Kit</i> o, por sus siglas en inglés, <i>SAK</i>."),
    list(word = "Kit con Querella", definition = "Kit de recolección de evidencia forense que se acompaña con la querella radicada por la víctima sobreviviente de violencia sexual. Cuenta con un número otorgado por el NPPR."),
    list(word = "Kit sin Querella", definition = "Kit de recolección de evidencia forense que no se acompaña con una querella, pues la víctima sobreviviente de violencia sexual no ha radicado querella en el NPPR."),
    list(word = "Querella", definition = "Mecanismo que tiene disponible una persona para reportar a la policía un incidente que entiende debe ser investigado por ésta. El incidente puede tratarse de uno que involucre un delito, una infracción, una persona desaparecida, entre otros."),
    list(word = "Tendencia", definition = "Dirección o patrón observado en datos o eventos que muestra una inclinación hacia cierto resultado o comportamiento a lo largo del tiempo."),
    list(word = "Violencia Sexual", definition = "Cualquier acto que degrada o daña el cuerpo y/o la sexualidad de la víctima y que por tanto atenta contra su libertad, dignidad e integridad física. Es una expresión de abuso de poder que implica la supremacía masculina sobre la mujer, al denigrar y concebirla como objeto.")
  )
  
  # Convertir lista a dataframe
  definitions_df_cavv <- do.call(rbind, lapply(definitions_cavv, as.data.frame))
  
  # Renombrar columnas
  colnames(definitions_df_cavv) <- c("Concepto", "Definición")
  
  # Usar funcion para presentar tabla con definiciones
  output$dataTable_Def_cavv <- renderDT({
    renderDataTable_Definitions(definitions_df_cavv)
  })
  
  #### tab con informacion de Contacto ####
  observeEvent(input$send, {
    req(input$email)  # Asegurar que el input del email no este vacio
    req(input$message)  # Asegurar que mensaje no este vacio
    
    tryCatch({
      send.mail(
        from = input$email,  # email del usuario
        to = "preguntas@estadisticas.pr",  # remplazar 
        subject = paste("Mensaje de", input$name),
        body = paste("Nombre:", input$name, "\nCorreo Electrónico:", input$email, "\n\nMensaje:\n", input$message),
        smtp = list(
          host.name = "smtp.your-email.com",  # Replazar con el SMTP host del email del instituto
          port = 465,  # Replazar con el SMTP port del email del instituto
          user.name = "your-smtp-username",  # Replazar con el SMTP username del email del instituto
          passwd = "your-smtp-password",  # Replazar con el SMTP password del email del instituto
          ssl = TRUE
        ),
        authenticate = TRUE,
        send = TRUE
      )
      
      output$response <- renderText("Mensaje enviado con éxito.")
    }, error = function(e) {
      output$response <- renderText(paste("Error al enviar el mensaje:", e$message))
    })
  })
  
}
