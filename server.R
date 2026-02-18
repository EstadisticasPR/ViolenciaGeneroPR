# Server
cat("Loading Server from server.R...\n")
server <- function(input, output, session) {
  
  #############################################################################
  ########## Server del Sistema de Notificación de Muertes Violentas ##########
  #############################################################################
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
  

  # Actualizar el texto del botón según el estado actual
  observe({
    inputId <- "checkGroup_snmv_homiEdad_edad"
    buttonId <- "deselectAll_snmv_homiEdad_edad"
    all_choices <- levels(homiEdad$Edad)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
    ### lógica para el boton de deseleccionar/seleccionar año
  observeEvent(input$deselectAll_snmv_homiEdad_año, {
    updateCheckboxGroup(session, "checkGroup_snmv_homiEdad_año", input, homiEdad$Año)
  })
  
  # Actualizar el texto del botón según el estado actual
  observe({
    inputId <- "checkGroup_snmv_homiEdad_año"
    buttonId <- "deselectAll_snmv_homiEdad_año"
    all_choices <- levels(homiEdad$Año)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
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
      message <- "Seleccione Grupo de Edad y Año(s) a visualizar"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot_facets(homiEdad_filt, "Año", "Casos", "Edad",
                         "Año", "Cantidad de víctimas", fillLab = "Grupo de Edad", colorFill = homiEdad_fill_edad, 
                         emptyMessage = "Seleccione Grupo de Edad y Año(s) a visualizar",barWidth = 0, xGap = 0)
 
      p <- convert_to_plotly(p, tooltip = "text") %>% layout(height = 450)
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(homiEdad_filt, "Año", "Casos", "Edad",
                                                 "Año", "Cantidad de víctimas", message)
    convert_to_plotly(empty_plot, tooltip = "text")
  })
  
  #Titulo de la Grafica
  output$plot_title_homiEdad <- renderUI({
    title <- "Homicidios de mujeres por grupo de edad y año"
  })

  
  homiEdad_filt_rename <- reactive({
    homiEdad_filt() %>% 
      rename(`Grupo de Edad` = Edad)  
  })

  # Data Table de homiEdad
  # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
  output$dataTable_snmv <- renderDT(server = FALSE, {
    renderDataTable(homiEdad_filt_rename(), "Datos: Homicidios por grupo de edad")
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
  
  # Actualizar el texto del botón según el estado actual
  observe({
    inputId <- "checkGroup_snmv_inci_tipo"
    buttonId <- "deselectAll_snmv_inci_tipo"
    all_choices <- levels(inci$Incidente)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  
  
  ### lógica para el botón de deseleccionar/seleccionar el año
  observeEvent(input$deselectAll_snmv_inci_año, {
    updateCheckboxGroup(session, "checkGroup_snmv_inci_año", input, inci$Año)
  })
  
  # Actualizar el texto del botón según el estado actual
  observe({
    inputId <- "checkGroup_snmv_inci_año"
    buttonId <- "deselectAll_snmv_inci_año"
    all_choices <- levels(inci$Año)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
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
      p <- renderBarPlot_facets(inci_filt, x = "Año", y = "Casos", fill = "Incidente",
                         xlab = "Año", ylab = "Número de casos", fillLab = "Tipo de Incidente",
                         colorFill = inci_fill_sexo, 
                         emptyMessage = "Seleccione Tipo(s) de Incidente y Año(s) a visualizar")
      p <- convert_to_plotly(p, tooltip = "text") %>% layout(height = 450)
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(inci_filt, x = "Año", y = "Casos", fill = "Incidente",
                                                 xlab = "Año", ylab = "Número de casos", message)
    convert_to_plotly(empty_plot, tooltip = "text")
  })
  
  #Titulo de la Grafica
  output$plot_title_inci <- renderUI({
    title <- "Incidentes violentos ocurridos para ambos sexos"
  })
 
  inci_filt_rename <- reactive({
    inci_filt() %>% 
      rename(`Tipo de Incidente` = Incidente)  
  })
  
  # Data Table del SNMV
  # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
  output$dataTable_snmv_inci <- renderDT(server = FALSE, {
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
  
  
  #### Tab de Publicaciones ####
  # PDF 1
  # publicationCardPDFServer("snmv_pdf1", "snmv/boletinVS2023.pdf")
  # # PDF 2
  # publicationCardPDFServer("snmv_pdf2", "snmv/reporte2022.pdf")
  # 
  # output$snmv_view_content <- renderUI({
  #   if (input$snmv_view_option == "Publicaciones") {
  #     tagList(
  #       tags$ul(
  #         style = "list-style-type: none; text-align: center;",
  #         sectionTitle("Publicaciones y Recursos", "24px")
  #       ),
  #       fluidRow(
  #         column(12, publicationCardPDF("snmv_pdf1", "Boletín Estadístico de Violencia Sexual, Año 2023", "snmv/boletinVS2023.pdf"))
  #       )
  #       # ,
  #       # fluidRow(
  #       #   column(12, publicationCardPDF("snmv_pdf2", "Reporte Estadístico 2022", "snmv/reporte2022.pdf"))
  #       # )
  #     )
  #   } else {
  #     # Dashboard de la Agencia (embed link)
  #     tags$div(
  #       fluidRow(
  #         column(12, publicationCardWeb("Dashboard de la Agencia:", 
  #                                       "https://safekits.pr.gov/?fbclid=IwAR3ooo0EgJS_8MEVB3Z5IettOyplWfi3sYQU18AZlC5yieAIMhIYOuHjHZs"))
  #       )
  #     )
  #   }
  # })
  #### Tab de Definiciones ####
  definitions_snmv <- list(
    list(word = "Homicidio", definition = "Es un delito de resultado por el cual la conducta intencionada
         de un sujeto provoca la muerte de otro."),
    list(word = "Homicidio múltiple", definition = "Se refiere a un acto en el cual una persona causa la 
         muerte de múltiples individuos en un solo incidente."),
    list(word = "Homicidio seguido de suicidio", definition = "Se refiere a un acto en el cual una persona causa 
         la muerte de otra(s) y luego se quita la vida. Este tipo de incidente implica dos acciones distintas pero
         relacionadas: primero, el homicida comete el acto de matar a otra persona, y luego, la misma persona toma 
         su propia vida."),
    list(word = "Homicidio(s) seguido de suicidio(s), (más de dos víctimas)", definition = "Se refiere a un acto 
         extremadamente trágico en el cual una o más personas causan la muerte de múltiples individuos antes de acabar
         con sus propias vidas en un solo evento."),
    list(word = "Homicidio único", definition = "Se refiere a un acto en el cual una persona causa la muerte de otra 
         en un evento específico."),
    list(word = "Intervención legal", definition = "La muerte por intervención legal se define como una muerte en la que la 
         víctima fue asesinado o murió como resultado de un oficial de la ley u otro oficial de paz (personas con autoridad legal
         específica para usar fuerza letal), incluidas las fuerzas del orden militar, que actuaron en el cumplimiento del deber.  
         El término intervención legal es una clasificación de los códigos ICD-10 y no denota la legalidad de las 
         circunstancias que rodearon la muerte. Estas muertes pueden ocurrir durante el transcurso de un oficial de la ley 
         que realiza una parada de tráfico aleatoria o dirigida, emite una citación, arresta o persigue a una víctima 
         (por ejemplo, la víctima huye o escapa del arresto), responde a un llamado para mantener el orden, minimiza los
         disturbios y / o garantiza la seguridad (por ejemplo, disturbios domésticos, para evitar una crisis de suicidio)
         u otras acciones como parte de los deberes de las fuerzas del orden."),
    list(word = "Muerte accidental por arma de fuego", definition = "Evento en el cual una persona pierde la vida 
         como resultado involuntario de la manipulación, manejo o uso incorrecto de un arma de fuego. Este tipo de 
         incidente ocurre cuando un individuo dispara un arma de manera accidental, ya sea debido a un descuido, falta
         de conocimiento sobre el funcionamiento seguro del arma, o por la manipulación indebida del arma."),
    list(word = "Muerte no determinada", definition = "Caso en el cual las circunstancias que rodean el fallecimiento
         de una persona no pueden ser claramente establecidas o comprendidas mediante la evidencia disponible en el 
         momento de la investigación. Este término se utiliza cuando no hay suficiente información o evidencia forense
         para determinar con certeza si la muerte fue el resultado de causas naturales, accidentales, suicidas u homicidas."),
    list(word = "Muertes violentas", definition = "El Sistema de Notificación de Muertes Violentas de Puerto Rico (PRVDRS por
         su siglas en inglés) define una muerte violenta como: Suicidio (incluyendo terrorismo), Homicidio (incluyendo terrorismo),
         Intervención legal (excluyendo terrorismo, ejecuciones por pena de muerte, o actos de guerra), Eventos con intención 
         no determinada, Heridas por arma de fuego no intencional."),
    list(word = "Suicidio Único", definition = "Se refiere a un acto en el cual una persona termina deliberadamente con 
         su propia vida, sin la participación o implicación de otras personas en el proceso. Este término implica que el 
         individuo toma la decisión y ejecuta el acto suicida de manera independiente, sin ningún intento de causar la 
         muerte de otras personas o de involucrar a terceros en el evento."),
    list(word = "Violencia", definition = "El uso intencional de la fuerza o el poder físico, de hecho, o como amenaza, 
         contra uno mismo, otra persona o un grupo o comunidad, que cause o tenga gran probabilidad de causar lesiones, 
         muerte, daños psicológicos, trastornos del desarrollo o privaciones.")
  )
  
  # Convertir lista a dataframe
  definitions_df_snmv <- do.call(rbind, lapply(definitions_snmv, as.data.frame))
  
  # Renombrar las columnas
  colnames(definitions_df_snmv) <- c("Concepto", "Definición")
  
  # Usar funcion para presentar tabla con definiciones
  output$dataTable_Def_snmv <- renderDT({
    renderDataTable_Definitions(definitions_df_snmv, "Sistema de Notificación de Muertes Violentas")
  })
  
  
  ###########################################################
  ########## Server del Departamento de la Familia ##########
  ###########################################################
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
  
  # Actualizar el texto del botón según el estado actual
  observe({
    inputId <- "checkGroup_fam_dfMalt_tipo"
    buttonId <- "deselectAll_fam_dfMalt_tipo"
    all_choices <- levels(dfMalt$Maltrato)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  # funcion para el boton de deseleccionar/seleccionar año
  observeEvent(input$deselectAll_fam_dfMalt_año, {
    updateCheckboxGroup(session, "checkGroup_fam_dfMalt_año", input, dfMalt$Año)
  })
  
  # Actualizar el texto del botón según el estado actual
  observe({
    inputId <- "checkGroup_fam_dfMalt_año"
    buttonId <- "deselectAll_fam_dfMalt_año"
    all_choices <- levels(dfMalt$Año)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  # funcion para el boton de deseleccionar/seleccionar sexo
  observeEvent(input$deselectAll_fam_dfMalt_sexo, {
    updateCheckboxGroup(session, "checkGroup_fam_dfMalt_sexo", input, dfMalt$Sexo)
  })
  
  # Actualizar el texto del botón según el estado actual
  observe({
    inputId <- "checkGroup_fam_dfMalt_sexo"
    buttonId <- "deselectAll_fam_dfMalt_sexo"
    all_choices <- levels(dfMalt$Sexo)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
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
      message <- "Seleccione Tipo(s) de Maltrato, Año(s) y Sexo de la víctima"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot_facets(dfMalt_filt, x = "Año", y = "Casos", fill = "Maltrato",
                         xlab = "Año", ylab = "Número de casos", fillLab = "Tipo de Maltrato", 
                         colorFill = dfMalt_fill_Maltrato, 
                         emptyMessage = "Seleccione Tipo(s) de Maltrato, Año(s) y Sexo de la víctima")
      #Altura predeterminada para la grafica.
      plot_height = 500
      numPlots = length(input$checkGroup_fam_dfMalt_sexo)
      #Llamado a la funcion calcPlotHeight para calcular la altura basado en el numero de filas.
      total_height = plotHeight(plot_height, numPlots)
      p <- p + facet_wrap(~Sexo, ncol = 2) +
        theme(panel.spacing.x = unit(0.2, "lines"), #Espacio entre las facetas en x.
              panel.spacing.y = unit(1.5, "lines")) #Espacio entre las facetas en y.
      
      p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)

      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(data = dfMalt_filt, x = "Año", y = "Casos", fill = "Maltrato",
                                                 xlab = "Año", ylab = "Número de casos", message)
    #ggplotly(empty_plot)
    convert_to_plotly(empty_plot, tooltip = "text")
  })
  
  #Titulo de la Grafica
  output$plot_title_dfMalt <- renderUI({
    title <- "Casos anuales de maltrato infantil por sexo y tipo"
  })
  
  dfMalt_filt_rename <- reactive({
    dfMalt_filt() %>% 
      rename(`Tipo de Maltrato` = Maltrato)  
  })
  
  
  # Data Table del DeptFam
  # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
  output$dataTable_fam <- renderDT(server = FALSE, {
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
  
  
  #### Tab de Publicaciones ####
  # PDF 1
  # publicationCardPDFServer("fam_pdf1", "fam/boletinVS2023.pdf")
  # # PDF 2
  # publicationCardPDFServer("fam_pdf2", "fam/reporte2022.pdf")
  # 
  # output$fam_view_content <- renderUI({
  #   if (input$fam_view_option == "Publicaciones") {
  #     tagList(
  #       tags$ul(
  #         style = "list-style-type: none; text-align: center;",
  #         sectionTitle("Publicaciones y Recursos", "24px")
  #       ),
  #       fluidRow(
  #         column(12, publicationCardPDF("fam_pdf1", "Boletín Estadístico de Violencia Sexual, Año 2023", "fam/boletinVS2023.pdf"))
  #       )
  #       # ,
  #       # fluidRow(
  #       #   column(12, publicationCardPDF("fam_pdf2", "Reporte Estadístico 2022", "fam/reporte2022.pdf"))
  #       # )
  #     )
  #   } else {
  #     # Dashboard de la Agencia (embed link)
  #     tags$div(
  #       fluidRow(
  #         column(12, publicationCardWeb("Dashboard de la Agencia:",
  #                                       "https://safekits.pr.gov/?fbclid=IwAR3ooo0EgJS_8MEVB3Z5IettOyplWfi3sYQU18AZlC5yieAIMhIYOuHjHZs"))
  #       )
  #     )
  #   }
  # })
  #### Tab de Definiciones ####
  definitions_fam <- list(
    list(word = "Abuso Sexual", definition = "Incurrir en conducta sexual en presencia de un o una menor
         o que se utilice, voluntaria o involuntariamente, para ejecutar conducta sexual dirigida a 
         satisfacer los deseos sexuales. También se considera cualquier acto que, de procesarse por la vía
         criminal, configuraría cualesquiera de varios delitos de índole sexual, tales como agresión sexual,
         actos lascivos, comercio de personas para actos sexuales, exposiciones obscenas, proposición obscena,
         producción de pornografía infantil, entre otros delitos reconocidos en el Código Penal de Puerto Rico."),
    list(word = "Explotación", definition = "Conducta obscena o utilización de una persona menor de edad para 
         ejecutar conducta obscena. Explotación de una persona menor de edad o que se permita que otra persona 
         lo explote, incluyendo –pero sin limitarse– a utilizarla para ejecutar conducta obscena, con el fin de 
         lucrarse o de recibir algún otro beneficio."),
    list(word = "Maltrato a Menores", definition = "Toda acción u omisión intencional del padre, de la madre o 
         persona responsable del o de la menor que ocasione o ponga en riesgo de sufrir un daño o perjuicio a su 
         salud e integridad física, mental o emocional."),
    list(word = "Maltrato Físico", definition = "Se refiere a cualquier trauma, lesión o condición no accidental, 
         causada en un episodio o varios, incluyendo la falta de alimentos que, de no ser atendida, pone en riesgo 
         la vida y salud de la persona menor de edad."),
    list(word = "Menores", definition = "Individuos que se encuentran bajo la edad legal de mayoría de edad en un 
         contexto específico, lo que generalmente implica que aún no han alcanzado la edad en la que se les considera
         plenamente responsables de sus acciones según la ley."),
    list(word = "Negligencia", definition = "Es un tipo de maltrato que consiste en no cumplir con la obligación de 
         proveer a las personas menores de edad de manera adecuada los alimentos, ropa, albergue, educación, atención
         a su salud, poca supervisión, no visitar, ni mantener contacto con el o la menor o incurrir en alguna de las 
         razones reconocidas en el Código Civil de Puerto Rico para que una persona sea privada de patria potestad 
         entre otros."),
    list(word = "Negligencia Educativa", definition = HTML("La negligencia educativa es cuando a una persona 
    menor de edad, que está en un hogar de crianza, centro de cuidado sustituto o en una institución pública o privada,
    de cuido, educación, tratamiento o detención, se le cause daño o se ponga en riesgo de sufrir daño a su salud e 
    integridad física, mental o emocional, incluyendo –pero sin limitarse– a abuso sexual. La negligencia institucional,
    ya sea conocida o que se sospeche que ocurre, o que ocurre como resultado de la política, prácticas y condiciones 
    imperantes en la institución, la puede cometer:<ul>          
            <br>
            <li> Operador u operadora de un hogar de crianza; 
            <li> Cualquier empleado, empleada, funcionario o funcionaria que ofrezca servicios de cuido o que tenga bajo
            su control o custodia a una persona menor de edad para su cuido, educación, tratamiento o detención.
            </ul>")),
    list(word = "Negligencia Emocional", definition = "Se define como causar menoscabo o afectar la capacidad intelectual
         o emocional de la persona menor de edad dentro de lo que se considera normal para su edad y entorno cultural."),
    list(word = "Negligencia Médica", definition = "Situaciones en que la salud de un/una menor se ve comprometida debido 
         a que los proveedores de atención médica, como médicos, enfermeras u otros profesionales de la salud, no brindan 
         el nivel adecuado de atención y cuidado a pacientes menores de edad, lo que resulta en daños físicos, emocionales 
         o psicológicos para el paciente. Esto puede incluir errores en el diagnóstico, tratamiento inapropiado, falta de 
         seguimiento adecuado, o cualquier otro acto u omisión que pueda considerarse una violación del estándar de cuidado
         aceptado en la práctica médica. "),
    list(word = "Trata Humana", definition = "Se define como la captación, traslado, transporte, acogida o recepción de 
         una persona utilizando la violencia, amenaza, engaño, rapto, fuerza, abuso de poder, abuso de una situación de 
         vulnerabilidad u otros elementos de coacción, con el fin de someterla a explotación y lucrarse con su actividad.")
  )
  
  # Convertir lista a dataframe
  definitions_df_fam <- do.call(rbind, lapply(definitions_fam, as.data.frame))
  
  # Renombrar las columnas
  colnames(definitions_df_fam) <- c("Concepto", "Definición")
  
  # Usar funcion para presentar tabla con definiciones
  output$dataTable_Def_fam <- renderDT({
    renderDataTable_Definitions(definitions_df_fam, "Departamento de la Familia")
  })
  
  
  
 
  
  
  
   #########################################################
  ########## Server del Departamento de Justicia ##########
  #########################################################
  #### Tab de Delitos (dfDeli) ####
  
  # Filtrar el conjunto de datos según el año, delito o distrito seleccionado
  dfDeli_filt <- reactive({
    filter(dfDeli, 
           Año %in% input$checkGroup_just_dfDeli_año,
           Delito %in% input$checkGroup_just_dfDeli_delito,
           Distrito %in% input$checkGroup_just_dfDeli_distrito)
  })
  
  dfDeli_total <- dfDeli %>%
    group_by(Año, Delito) %>%
    summarise(Casos = sum(Casos, na.rm = TRUE)) %>%
    ungroup()
  
  
  dfDeli_filt_total <- reactive({
    filter(dfDeli_total,
           Año %in% input$checkGroup_just_dfDeli_año,
           Delito %in% input$checkGroup_just_dfDeli_delito
    )
  })
  
  # funcion para el boton de deseleccionar/seleccionar el delito
  observeEvent(input$deselectAll_just_dfDeli_delito, {
    updateCheckboxGroup(session, "checkGroup_just_dfDeli_delito", input, dfDeli$Delito)
  })
  
  # Actualizar el texto del botón según el estado actual
  observe({
    inputId <- "checkGroup_just_dfDeli_delito"
    buttonId <- "deselectAll_just_dfDeli_delito"
    all_choices <- levels(dfDeli$Delito)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  
  # funcion para el boton de deseleccionar/seleccionar el año
  observeEvent(input$deselectAll_just_dfDeli_año, {
    updateCheckboxGroup(session, "checkGroup_just_dfDeli_año", input, dfDeli$Año)
  })
  
  observe({
    inputId <- "checkGroup_just_dfDeli_año"
    buttonId <- "deselectAll_just_dfDeli_año"
    all_choices <- levels(dfDeli$Año)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  # funcion para el boton de deseleccionar/seleccionar el distrito fiscal
  observeEvent(input$deselectAll_just_dfDeli_distrito, {
    updateCheckboxGroup(session, "checkGroup_just_dfDeli_distrito", input, dfDeli$Distrito)
  })
  
  observe({
    inputId <- "checkGroup_just_dfDeli_distrito"
    buttonId <- "deselectAll_just_dfDeli_distrito"
    all_choices <- levels(dfDeli$Distrito)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  # Colores de las edades
  dfDeli_fill_Delito <- setColorFill(dfDeli, "Delito")
  
  # Grafico de Barras
  output$barPlot_just <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_delito <- length(input$checkGroup_just_dfDeli_delito) > 0
    has_año <- length(input$checkGroup_just_dfDeli_año) > 0
    has_distrito <- length(input$checkGroup_just_dfDeli_distrito) > 0
    
    
    # Si faltan selecciones necesarias
    if (!has_año || !has_delito) {
      message <- HTML("Seleccione Delito y Año(s) a visualizar")
      empty_plot <- create_empty_plot_with_message(data = dfDeli_filt, x = "Año", y = "Casos", fill = "Delito",
                                                   xlab = "Año", ylab = "Cantidad de víctimas", message)
      
      #Titulo de la Grafica
      output$plot_title_dfDeli <- renderUI({
        title <- "Casos radicados anualmente según Ley 54"
      })
      
      return(convert_to_plotly(empty_plot, tooltip = "text"))
    }
    
    # Si NO hay región seleccionada, usar el dataset agregado
    if (!has_distrito) {
      p <- renderBarPlot_facets(dfDeli_filt_total, x = "Año", y = "Casos", fill = "Delito",
                                xlab = "Año", ylab = "Cantidad de víctimas",
                                fillLab = "Artículo de Ley 54", colorFill = dfDeli_fill_Delito,
                                emptyMessage = HTML("Seleccione Articulo(s) de Ley 54, Año(s) y Distrito(s)"))
      
      #Titulo de la Grafica
      output$plot_title_dfDeli <- renderUI({
        title <- "Total de casos radicados anualmente según Ley 54"
      })
      
      output$dataTable_just <- renderDT(server = FALSE, {
        renderDataTable(dfDeli_filt_total(), "Datos: Delitos según artículo de la Ley 54")
      })
      
      return(convert_to_plotly(p, tooltip = "text")%>% layout(height = 450))
    }
    
    #Titulo de la Grafica
    output$plot_title_dfDeli <- renderUI({
      title <- "Total de casos radicados anualmente según Ley 54 por región judicial"
    })
    
    p <- renderBarPlot_facets(dfDeli_filt, x = "Año", y = "Casos", fill = "Delito",
                              xlab = "Año", ylab = "Cantidad de víctimas",
                              fillLab = "Artículo de Ley 54", colorFill = dfDeli_fill_Delito,
                              emptyMessage = HTML("Seleccione Articulo(s) de Ley 54, Año(s) y Distrito(s)"))
    #Altura predeterminada para la grafica.
    plot_height = 500
    numPlots = length(input$checkGroup_just_dfDeli_distrito)
    #Llamado a la funcion calcPlotHeight para calcular la altura basado en el numero de filas.
    total_height = plotHeight(plot_height, numPlots)
    p <- p + facet_wrap(~Distrito, ncol = 2) +
      theme(panel.spacing.x = unit(0.2, "lines"), #Espacio entre las facetas en x.
            panel.spacing.y = unit(-0.02, "lines")) #Espacio entre las facetas en y.
    p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)
    
    
    # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
    output$dataTable_just <- renderDT(server = FALSE, {
      renderDataTable(dfDeli_filt(), "Datos: Delitos según artículo de la Ley 54")
    })
    
    return(p)
  })
  
    
    
  #   # Crear mensaje si faltan opciones seleccionadas
  #   if (!has_delito || !has_año || !has_distrito) {
  #     message <- "Seleccione Articulo(s) de Ley 54, Año(s) y Distrito(s)"
  #   } else {
  #     # Si todas las opciones están seleccionadas, crear la gráfica
  #     p <- renderBarPlot_facets(dfDeli_filt, x = "Año", y = "Casos", fill = "Delito",
  #                        xlab = "Año", ylab = "Cantidad de víctimas",
  #                        fillLab = "Artículo de Ley 54", colorFill = dfDeli_fill_Delito,
  #                        emptyMessage = "Seleccione Articulo(s) de Ley 54, Año(s) y Distrito(s)")
  #     #Altura predeterminada para la grafica.
  #     plot_height = 500
  #     numPlots = length(input$checkGroup_just_dfDeli_distrito)
  #     #Llamado a la funcion calcPlotHeight para calcular la altura basado en el numero de filas.
  #     total_height = plotHeight(plot_height, numPlots)
  #     p <- p + facet_wrap(~Distrito, ncol = 2) +
  #       theme(panel.spacing.x = unit(0.2, "lines"), #Espacio entre las facetas en x.
  #             panel.spacing.y = unit(-0.02, "lines")) #Espacio entre las facetas en y.
  #     p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)
  #     
  #     return(p)
  #   }
  #   
  #   # Crear la gráfica vacía con mensaje
  #   empty_plot <- create_empty_plot_with_message(data = dfDeli_filt, x = "Año", y = "Casos", fill = "Delito",
  #                                                xlab = "Año", ylab = "Cantidad de víctimas", message)
  #   #ggplotly(empty_plot)
  #   convert_to_plotly(empty_plot, tooltip = "text", TRUE)
  # })
  # 
  # #Llamada a funcion para generar la legenda
  # output$plot_title_dfDeli <- renderUI({
  #   title <- "Radicación anual de casos por Distrito Fiscal según Ley 54"
  # })
  # 
  # # Data table del DeptJust
  # # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
  # output$dataTable_just <- renderDT(server = FALSE, {
  #   renderDataTable(dfDeli_filt(), "Datos: Delitos según artículo de la Ley 54")
  # })
  #
  
  
  # Texto explicativo dinámico
  output$texto_Deli <- renderUI({
    regiones <- input$checkGroup_just_dfDeli_distrito
    if (is.null(regiones) || length(regiones) == 0) {
      HTML(
        "<p style='font-size: 16px;padding: 0px;'>
        Los datos representados en esta gráfica corresponden al
        total de casos radicados por Artículo
        de la Ley 54 desde el año natural 2020 al 2023.
      </p>"
      )
    } else {
      HTML(
        "<p style='font-size: 16px;padding: 0px;'>
        Los datos representados en esta gráfica corresponden al
        número de casos radicados por distrito fiscal y Artículo
        de la Ley 54 desde el año natural 2020 al 2023.
      </p>"
      )
    }
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
  
  # output$map_just_mapaDeli <- renderPlotly({
  #   # Generar el gráfico con ggplot
  #   p <- renderMap(
  #     data = mapaDeli_filt, fill = Casos,
  #     title = paste0("Casos de delitos por Distrito Fiscal según el Artículo de \n", input$select_just_mapaDeli_delito, " en el año ", input$select_just_mapaDeli_año),
  #     # group = GROUP,
  #     group = `Distrito Fiscal`,
  #     fill_lab = "Número de casos",
  #     light_color = "#E0BBE4",
  #     dark_color = "#5A189A"
  #   )
  # 
  #   # Convertir el gráfico ggplot a plotly y ajustar la leyenda y colorbar
  #   ggplotly(p, tooltip = c("all"))
  # })
  
  # Renderizar el mapa con Leaflet
  output$map_just_mapaDeli <- renderLeaflet({
    data <- mapaDeli_filt()
    renderMap(data, value_col = "Casos",
              value_col_region = "Distrito Fiscal",
              map_zoom = 8.5,
              provider = providers$CartoDB.Positron,
              municipios_geo = municipios_geo)
  })

    #Titulo de la Grafica
  output$plot_title_mapaDeli <- renderUI({
    title <- paste0("Casos de delitos por Distrito Fiscal según el Artículo de \n", input$select_just_mapaDeli_delito, " en el año ", input$select_just_mapaDeli_año)
  })
  
  #### Tab del Mapa de Distritos Fiscales ####
  # output$map_just_mapaFisc <- renderPlotly({
  #   p <- renderMapGroup(data = mapaDeli, 
  #                       fill = GROUP, 
  #                       title = "",
  #                       fill_lab = "Distrito Fiscal")
  #   ggplotly(p, tooltip = c("fill"))
  # })
  
  # Crear Card con Fuentes
  output$dataTableUI_just_mapaFisc <- renderUI({
    hyperlinks <- c("https://www.justicia.pr.gov/")
    texts <- c("Departamento de Justicia")
    createFuenteDiv(hyperlinks, texts)
    
  })
  
  
  
  #### Tab de Publicaciones ####
  # PDF 1
  # publicationCardPDFServer("just_pdf1", "just/boletinVS2023.pdf")
  # # PDF 2
  # publicationCardPDFServer("just_pdf2", "just/reporte2022.pdf")
  # 
  # output$just_view_content <- renderUI({
  #   if (input$just_view_option == "Publicaciones") {
  #     tagList(
  #       tags$ul(
  #         style = "list-style-type: none; text-align: center;",
  #         sectionTitle("Publicaciones y Recursos", "24px")
  #       ),
  #       fluidRow(
  #         column(12, publicationCardPDF("just_pdf1", "Boletín Estadístico de Violencia Sexual, Año 2023", "just/boletinVS2023.pdf"))
  #       )
  #       # ,
  #       # fluidRow(
  #       #   column(12, publicationCardPDF("just_pdf2", "Reporte Estadístico 2022", "just/reporte2022.pdf"))
  #       # )
  #     )
  #   } else {
  #     # Dashboard de la Agencia (embed link)
  #     tags$div(
  #       fluidRow(
  #         column(12, publicationCardWeb("Dashboard de la Agencia:",
  #                                       "https://safekits.pr.gov/?fbclid=IwAR3ooo0EgJS_8MEVB3Z5IettOyplWfi3sYQU18AZlC5yieAIMhIYOuHjHZs"))
  #       )
  #     )
  #   }
  # })
  #### Tab de Definiciones ####
  definitions_just <- list(
    list(word = "Agresión Sexual Conyugal (Artículo 3.5)", definition = HTML("Se impondrá pena de reclusión,
    según se dispone más adelante, a toda persona que incurra en una relación sexual no consentida 
    con su cónyuge o ex cónyuge, o con la persona con quien cohabite o haya cohabitado, o con quien sostuviera
    o haya sostenido una relación consensual, o la persona con quien 
    haya procreado un hijo o hija, independientemente del sexo, estado civil, orientación sexual, identidad de 
    género o estatus migratorio de cualquiera de las personas 
    involucradas en la relación, en cualesquiera de las circunstancias siguientes:
              <ul>    
              <br>
                <li> Si se ha compelido a incurrir en relación sexual mediante el empleo de fuerza, violencia, 
                intimidación o amenaza de grave e inmediato daño corporal; o
                <li> Si se ha anulado o disminuido sustancialmente, sin su conocimiento o sin su consentimiento, 
                su capacidad de consentir, a través de medios hipnóticos, narcóticos, deprimentes o estimulantes o 
                sustancias o medios similares; o
                <li> Si por enfermedad o incapacidad mental, temporal o permanente, la víctima está incapacitada para 
                comprender la naturaleza del acto en el momento de su realización;
                <li> Si se le obliga o induce mediante maltrato, violencia física o psicológica a participar o 
                involucrarse en una relación sexual no deseada con terceras personas.
              </ul>")),
    list(word = "Incumplimiento de Órdenes de Protección (Artículo 2.8)", definition = "Cualquier violación a sabiendas 
         de una orden de protección expedida."),
    list(word = "Maltrato (Artículo 3.1)", definition = "Toda persona que empleare fuerza física o violencia psicológica,
         intimidación o persecución en la persona de su cónyuge, ex cónyuge, o la persona con quien cohabita o haya cohabitado,
         o la persona con quien sostuviere o haya sostenido una relación consensual, o la persona con quien haya procreado 
         un hijo o hija, independientemente del sexo, estado civil, orientación sexual, identidad de género o estatus 
         migratorio de cualquiera de las personas involucradas en la relación, para causarle daño físico a su persona, 
         a los bienes apreciados por ésta, excepto aquéllos que pertenecen privativamente al ofensor, o a la persona de otro
         o para causarle grave daño emocional, incurrirá en delito grave de cuarto grado en su mitad superior."),
    list(word = "Maltrato Agravado (Artículo 3.2)", definition = HTML("Se impondrá pena correspondiente a delito grave de
    tercer grado en su mitad inferior cuando en la persona del cónyuge, ex cónyuge o de la persona con quien se cohabita 
    o se haya cohabitado, o con quien se sostiene o haya sostenido una relación consensual, o con quien se haya procreado 
    un hijo o hija, independientemente del sexo, estado civil, orientación sexual, identidad de género o estatus migratorio
    de cualquiera de las personas involucradas en la relación, se incurriere en maltrato según tipificado en esta Ley, mediando
    una o más de las circunstancias siguientes:<br>
    <ul>
    <br>
      <li> Se penetrare en la morada de la persona o en el lugar donde esté albergada y se cometiere allí maltrato, en el caso
      de cónyuges o cohabitantes, independientemente del sexo, estado civil, orientación sexual, identidad de género o estatus
      migratorio de cualquiera de las personas involucradas en la relación, cuando éstos estuvieren separados o mediare una 
      orden de protección ordenando el desalojo de la residencia a una de las partes; o</li>
      <li> Cuando se infiriere grave daño corporal a la persona; o</li>
      <li> Cuando se cometiere con arma mortífera en circunstancias que no revistiesen la intención de matar o mutilar; o</li>
      <li> Cuando se cometiere en la presencia de menores de edad; o</li>
      <li> Cuando se cometiere luego de mediar una orden de protección o resolución contra la persona acusada expedida en 
      auxilio de la víctima del maltrato; </li>
      <li> Se indujere, incitare u obligare a la persona a drogarse con sustancias controladas, o cualquier otra sustancia
      o medio que altere la voluntad de la persona o a intoxicarse con bebidas embriagantes; o</li>
      <li> Cuando se cometiere y simultáneamente se incurriere en maltrato de un menor según definido en la Ley Núm. 177 de
      1 de agosto de 2003.</li>
      <li> Si a la víctima se le obliga o induce mediante maltrato, violencia física o sicológica a participar o involucrarse
      en una relación sexual no deseada con terceras personas.</li>
      <li> Cuando se cometiere contra una mujer embarazada.</li>
      <li> Cuando se cometiere contra una persona menor de dieciséis (16) años y la persona agresora sea de dieciocho (18) 
      años o más.</li>
    </ul>")),
    list(word = "Maltrato Mediante Restricción de la Libertad (Artículo 3.4)", definition = "Toda persona que utilice violencia
         o intimidación en la persona de su cónyuge, ex cónyuge, de la persona con quien cohabita o haya cohabitado, o con quien
         sostiene o haya sostenido una relación consensual, o la persona con quien haya procreado un hijo o hija, independientemente
         del sexo, estado civil, orientación sexual, identidad de género o estatus migratorio de cualquiera de las personas involucradas
         en la relación, o que utilice pretexto de que padece o de que una de las personas antes mencionadas padece de enfermedad o 
         defecto mental, para restringir su libertad con el conocimiento de la víctima, incurrirá en delito grave de tercer grado en su
         mitad inferior."),
    list(word = "Maltrato Por Amenaza (Artículo 3.3)", definition = "Toda persona que amenaza con causarle daño a su cónyuge, ex cónyuge,
         a la persona con quien cohabita o con quien haya cohabitado o con quien sostiene o haya sostenido una relación consensual, 
         o la persona con quien haya procreado un hijo o hija, independientemente del sexo, estado civil, orientación sexual, identidad
         de género o estatus migratorio de cualquiera de las personas involucradas en la relación, a los bienes apreciados por ésta, 
         excepto aquéllos que pertenecen privativamente al ofensor, o a la persona de otro, incurrirá en delito grave de cuarto grado 
         en su mitad superior.")
  )
  
  # Convertir lista a dataframe
  definitions_df_just <- do.call(rbind, lapply(definitions_just, as.data.frame))
  
  # Renombrar las columnas
  colnames(definitions_df_just) <- c("Concepto", "Definición")
  
  # Usar funcion para presentar tabla con definiciones
  output$dataTable_Def_just <- renderDT({
    renderDataTable_Definitions(definitions_df_just, "Artículos de la Ley 54")
  })
  
  
  
  ############################################################################
  ########## Server del Departamento del Trabajo y Recursos Humanos ##########
  ############################################################################
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
  
  
  
  
  
  
  
  
  
  
  
  ##################################################################
  ########## Tab de la Administración de Vivienda Pública ##########
  ##################################################################
  #### Tab de Administración de Vivienda Pública (dfAvp_region_soli) ####
  
  # Filtrar el conjunto de datos según los valores seleccionados del año y el tipo de incidente
  dfAvp_region_soli_filt <- reactive({
    filter(dfAvp_region_soli,
           Región %in% input$checkGroup_avp_dfAvp_soli_región,
           Año %in% input$checkGroup_avp_dfAvp_soli_año)
  })
  
  dfAvp_region_soli_total <- dfAvp_region_soli %>%
    group_by(Año, Estado) %>%
    summarise(Cantidad = sum(Cantidad, na.rm = TRUE)) %>%
    ungroup()
  
  
  dfAvp_region_soli_filt_total <- reactive({
    filter(dfAvp_region_soli_total,
           Año %in% input$checkGroup_avp_dfAvp_soli_año
    )
  })
  

  
  ### funcion para el boton de deseleccionar/seleccionar del botón de región
  observeEvent(input$deselectAll_avp_dfAvp_soli_región, {
    updateCheckboxGroup(session, "checkGroup_avp_dfAvp_soli_región", input, dfAvp_region_soli$Región)
  })
  
  observe({
    inputId <- "checkGroup_avp_dfAvp_soli_región"
    buttonId <- "deselectAll_avp_dfAvp_soli_región"
    all_choices <- levels(dfAvp_region_soli$Región)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_avp_dfAvp_soli_año, {
    updateCheckboxGroup(session, "checkGroup_avp_dfAvp_soli_año", input, dfAvp_region_soli$Año)
  })
  
  observe({
    inputId <- "checkGroup_avp_dfAvp_soli_año"
    buttonId <- "deselectAll_avp_dfAvp_soli_año"
    all_choices <- levels(dfAvp_region_soli$Año)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  # Colores del status
  dfAvp_fill_status <- setColorFill(dfAvp, "Estado")
  
  
  # Grafico de barras
  output$barPlot_avp_dfAvp_soli <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_avp_dfAvp_soli_año) > 0
    has_region <- length(input$checkGroup_avp_dfAvp_soli_región) > 0
    
    
    # Si faltan selecciones necesarias
    if (!has_año) {
      message <- "Seleccione Región de Vivienda y Año(s) a visualizar"
      # Crear la gráfica vacía con mensaje
      empty_plot <- create_empty_plot_with_message(data = dfAvp_region_soli_filt, x = "Año", y = "Cantidad", fill = "Estado",
                                                   xlab = "Año", ylab = "Cantidad de viviendas públicas solicitadas", message)

      #Titulo de la Grafica
      output$plot_title_dfAvp_soli <- renderUI({
        title <- "Viviendas públicas solicitadas \nanualmente por violencia doméstica"
      })
      
      return(convert_to_plotly(empty_plot, tooltip = "text"))
    }
    
    # Si NO hay región seleccionada, usar el dataset agregado
    if (!has_region) {
      p <- renderBarPlot_facets(dfAvp_region_soli_filt_total, x = "Año", y = "Cantidad", fill = "Estado",
                                xlab = "Año", ylab = "Cantidad de viviendas públicas solicitadas", fillLab = "Estado de la Vivienda",
                                colorFill = dfAvp_fill_status,
                                emptyMessage = "Seleccione Región de Vivienda y Año(s) a visualizar")
   
      #Titulo de la Grafica
      output$plot_title_dfAvp_soli <- renderUI({
        title <- "Viviendas públicas solicitadas \nanualmente por violencia doméstica"
      })
    
      
      # Data Table para dcrCasosInv
      # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
      output$dataTable_avp_dfAvp_soli <- renderDT(server = FALSE, { 
        renderDataTable(dfAvp_region_soli_filt_total(), "Datos: Viviendas públicas solicitadas por violencia doméstica")
      })
      
      return(convert_to_plotly(p, tooltip = "text")%>% layout(height = 450))
    }
    
    #Titulo de la Grafica
    output$plot_title_dfAvp_soli <- renderUI({
      title <- "Viviendas públicas solicitadas \nanualmente por violencia doméstica según región"
    })
    
    p <- renderBarPlot_facets(dfAvp_region_soli_filt, x = "Año", y = "Cantidad", fill = "Estado",
                              xlab = "Año", ylab = "Cantidad de viviendas públicas solicitadas", fillLab = "Estado de la Vivienda",
                              colorFill = dfAvp_fill_status,
                              emptyMessage = "Seleccione Región de Vivienda y Año(s) a visualizar")
    #Altura predeterminada para la grafica.
    plot_height = 500
    numPlots = length(input$checkGroup_avp_dfAvp_soli_región)
    #Llamado a la funcion calcPlotHeight para calcular la altura basado en el numero de filas.
    total_height = plotHeight(plot_height, numPlots)
    p <- p + facet_wrap(~Región, ncol = 2) +
      theme(panel.spacing.x = unit(0.2, "lines"), #Espacio entre las facetas en x.
            panel.spacing.y = unit(-0.02, "lines")) #Espacio entre las facetas en y.
    p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)
    
    
    dfAvp_soli_filt_rename <- reactive({
      dfAvp_region_soli_filt() %>%
        rename(`Región de Vivienda` = Región)
    })
    
    # Data Table para la gráfica de barras de dfAvp
    # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
    output$dataTable_avp_dfAvp_soli <- renderDT(server = FALSE, { 
      renderDataTable(dfAvp_soli_filt_rename(), "Datos: Viviendas públicas solicitadas por violencia doméstica")
    })
    
    
    return(p)
  })
  
  
  # Texto explicativo dinámico
  output$texto_dfAvp_region_soli <- renderUI({
    regiones <- input$checkGroup_avp_dfAvp_soli_región
    if (is.null(regiones) || length(regiones) == 0) {
      HTML(
        "<p style='font-size: 16px;padding: 0px;'>
        Los datos representados en esta gráfica corresponden al
        total de viviendas públicas solicitadas por
        violencia de la Administración de Vivienda Pública
        desde el año natural 2017 al 2024.
      </p>"
      )
    } else {
      HTML(
        "<p style='font-size: 16px;padding: 0px;'>
        Los datos representados en esta gráfica corresponden al
        total de viviendas públicas solicitadas por
        violencia doméstica por región de la Administración de Vivienda Pública
        desde el año natural 2017 al 2024.
      </p>"
      )
    }
  })
  
  
  # 
  # # Grafico de barras
  # output$barPlot_avp_dfAvp_soli <- renderPlotly({
  #   # Verificar si hay opciones seleccionadas en cada grupo
  #   has_region <- length(input$checkGroup_avp_dfAvp_soli_región) > 0
  #   has_año <- length(input$checkGroup_avp_dfAvp_soli_año) > 0
  #   
  #   # Crear mensaje si faltan opciones seleccionadas
  #   if (!has_region || !has_año) {
  #     message <- "Seleccione Región de Vivienda y Año(s) a visualizar"
  #   } else {
  #     # Si todas las opciones están seleccionadas, crear la gráfica
  #     p <- renderBarPlot_facets(dfAvp_region_soli_filt, x = "Año", y = "Cantidad", fill = "Estado",
  #                               xlab = "Año", ylab = "Cantidad de viviendas públicas solicitadas", fillLab = "Estado de la Vivienda",
  #                               colorFill = dfAvp_fill_status,
  #                               emptyMessage = "Seleccione Región de Vivienda y Año(s) a visualizar")
  #     #Altura predeterminada para la grafica.
  #     plot_height = 500
  #     numPlots = length(input$checkGroup_avp_dfAvp_soli_región)
  #     #Llamado a la funcion calcPlotHeight para calcular la altura basado en el numero de filas.
  #     total_height = plotHeight(plot_height, numPlots)
  #     p <- p + facet_wrap(~Región, ncol = 2) +
  #       theme(panel.spacing.x = unit(0.2, "lines"), #Espacio entre las facetas en x.
  #             panel.spacing.y = unit(-0.02, "lines")) #Espacio entre las facetas en y.
  #     p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)
  #     
  #     return(p)
  #   }
  #   
  #   # Crear la gráfica vacía con mensaje
  #   empty_plot <- create_empty_plot_with_message(data = dfAvp_region_soli_filt, x = "Año", y = "Cantidad", fill = "Estado",
  #                                                xlab = "Año", ylab = "Cantidad de viviendas públicas solicitadas", message)
  #   convert_to_plotly(empty_plot, tooltip = "text")
  # })
  # 
  # 
  # #Titulo de la Grafica
  # output$plot_title_dfAvp_soli <- renderUI({
  #   title <- "Viviendas públicas solicitadas \nanualmente por violencia doméstica según región"
  # })
  # 
  # 
  # dfAvp_soli_filt_rename <- reactive({
  #   dfAvp_region_soli_filt() %>%
  #     rename(`Región de Vivienda` = Región)
  # })
  # 
  # # Data Table para la gráfica de barras de dfAvp
  # # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
  # output$dataTable_avp_dfAvp_soli <- renderDT(server = FALSE, { 
  #   renderDataTable(dfAvp_soli_filt_rename(), "Datos: Viviendas públicas solicitadas por violencia doméstica")
  # })
  # 
  # Crear Card con Fuentes
  output$dataTableUI_avp_dfAvp_soli <- renderUI({
    if (input$showTable_avp_dfAvp_soli) {
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
            DTOutput("dataTable_avp_dfAvp_soli")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
  })
  
  #### Tab de Administración de Vivienda Pública (dfAvp_region_asig) ####
  
  # Filtrar el conjunto de datos según los valores seleccionados del año y el tipo de incidente
  dfAvp_region_asig_filt <- reactive({
    filter(dfAvp_region_asig,
           Región %in% input$checkGroup_avp_dfAvp_asig_región,
           Año %in% input$checkGroup_avp_dfAvp_asig_año)
  })
  
  dfAvp_region_asig_total <- dfAvp_region_asig %>%
    group_by(Año, Estado) %>%
    summarise(Cantidad = sum(Cantidad, na.rm = TRUE)) %>%
    ungroup()
  
  
  dfAvp_region_asig_filt_total <- reactive({
    filter(dfAvp_region_asig_total,
           Año %in% input$checkGroup_avp_dfAvp_asig_año
    )
  })
  
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de región
  observeEvent(input$deselectAll_avp_dfAvp_asig_región, {
    updateCheckboxGroup(session, "checkGroup_avp_dfAvp_asig_región", input, dfAvp_region_asig$Región)
  })
  
  observe({
    inputId <- "checkGroup_avp_dfAvp_asig_región"
    buttonId <- "deselectAll_avp_dfAvp_asig_región"
    all_choices <- levels(dfAvp_region_asig$Región)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_avp_dfAvp_asig_año, {
    updateCheckboxGroup(session, "checkGroup_avp_dfAvp_asig_año", input, dfAvp_region_asig$Año)
  })
  
  observe({
    inputId <- "checkGroup_avp_dfAvp_asig_año"
    buttonId <- "deselectAll_avp_dfAvp_asig_año"
    all_choices <- levels(dfAvp_region_asig$Año)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  # Colores del status
  dfAvp_fill_status <- setColorFill(dfAvp, "Estado")
  
  output$barPlot_avp_dfAvp_asig <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_avp_dfAvp_asig_año) > 0
    has_region <- length(input$checkGroup_avp_dfAvp_asig_región) > 0
    
    
    # Si faltan selecciones necesarias
    if (!has_año) {
      message <- "Seleccione Región de Vivienda y Año(s) a visualizar"
      # Crear la gráfica vacía con mensaje
      empty_plot <- create_empty_plot_with_message(data = dfAvp_region_asig_filt, x = "Año", y = "Cantidad", fill = "Estado",
                                                   xlab = "Año", ylab = "Cantidad de viviendas públicas asignadas", message)
      
      #Titulo de la Grafica
      output$plot_title_dfAvp_asig <- renderUI({
        title <- "Viviendas públicas asignadas \nanualmente por violencia doméstica"
      })
      
      return(convert_to_plotly(empty_plot, tooltip = "text"))
    }
    
    # Si NO hay región seleccionada, usar el dataset agregado
    if (!has_region) {
      p <- renderBarPlot_facets(dfAvp_region_asig_filt_total, x = "Año", y = "Cantidad", fill = "Estado",
                                xlab = "Año", ylab = "Cantidad de viviendas públicas asignadas", fillLab = "Estado de la Vivienda",
                                colorFill = dfAvp_fill_status,
                                emptyMessage = "Seleccione Región de Vivienda y Año(s) a visualizar")
      
      #Titulo de la Grafica
      output$plot_title_dfAvp_asig <- renderUI({
        title <- "Viviendas públicas asignadas \nanualmente por violencia doméstica"
      })
      
      
      # Data Table para dcrCasosInv
      # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
      output$dataTable_avp_dfAvp_asig <- renderDT(server = FALSE, { 
        renderDataTable(dfAvp_region_asig_filt_total(), "Datos: Viviendas públicas asignadas por violencia doméstica")
      })
      
      return(convert_to_plotly(p, tooltip = "text")%>% layout(height = 450))
    }
    
    #Titulo de la Grafica
    output$plot_title_dfAvp_asig <- renderUI({
      title <- "Viviendas públicas asignadas \nanualmente por violencia doméstica según región"
    })
    
    p <- renderBarPlot_facets(dfAvp_region_asig_filt, x = "Año", y = "Cantidad", fill = "Estado",
                              xlab = "Año", ylab = "Cantidad de viviendas públicas asignadas", fillLab = "Estado de la Vivienda",
                              colorFill = dfAvp_fill_status,
                              emptyMessage = "Seleccione Región de Vivienda y Año(s) a visualizar")
    #Altura predeterminada para la grafica.
    plot_height = 500
    numPlots = length(input$checkGroup_avp_dfAvp_asig_región)
    #Llamado a la funcion calcPlotHeight para calcular la altura basado en el numero de filas.
    total_height = plotHeight(plot_height, numPlots)
    p <- p + facet_wrap(~Región, ncol = 2) +
      theme(panel.spacing.x = unit(0.2, "lines"), #Espacio entre las facetas en x.
            panel.spacing.y = unit(-0.02, "lines")) #Espacio entre las facetas en y.
    p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)
    
    
    dfAvp_asig_filt_rename <- reactive({
      dfAvp_region_asig_filt() %>%
        rename(`Región de Vivienda` = Región)
    })
    
    # Data Table para la gráfica de barras de dfAvp
    # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
    output$dataTable_avp_dfAvp_asig <- renderDT(server = FALSE, { 
      renderDataTable(dfAvp_asig_filt_rename(), "Datos: Viviendas públicas asignadas por violencia doméstica")
    })
    
    
    return(p)
  })
  
  
  # Texto explicativo dinámico
  output$texto_dfAvp_region_asig <- renderUI({
    regiones <- input$checkGroup_avp_dfAvp_asig_región
    if (is.null(regiones) || length(regiones) == 0) {
      HTML(
        "<p style='font-size: 16px;padding: 0px;'>
        Los datos representados en esta gráfica corresponden al
        total de viviendas públicas asignadas por
        violencia de la Administración de Vivienda Pública
        desde el año natural 2017 al 2024.
      </p>"
      )
    } else {
      HTML(
        "<p style='font-size: 16px;padding: 0px;'>
        Los datos representados en esta gráfica corresponden al
        total de viviendas públicas asignadas por
        violencia doméstica por región de la Administración de Vivienda Pública
        desde el año natural 2017 al 2024.
      </p>"
      )
    }
  })
  
  
  
  
  # # Grafico de barras
  # output$barPlot_avp_dfAvp_asig <- renderPlotly({
  #   # Verificar si hay opciones seleccionadas en cada grupo
  #   has_region <- length(input$checkGroup_avp_dfAvp_asig_región) > 0
  #   has_año <- length(input$checkGroup_avp_dfAvp_asig_año) > 0
  #   
  #   # Crear mensaje si faltan opciones seleccionadas
  #   if (!has_region || !has_año) {
  #     message <- "Seleccione Región de Vivienda y Año(s) a visualizar"
  #   } else {
  #     # Si todas las opciones están seleccionadas, crear la gráfica
  #     p <- renderBarPlot_facets(dfAvp_region_asig_filt, x = "Año", y = "Cantidad", fill = "Estado",
  #                               xlab = "Año", ylab = "Cantidad de viviendas públicas asignadas", fillLab = "Estado de la Vivienda",
  #                               colorFill = dfAvp_fill_status,
  #                               emptyMessage = "Seleccione Región de Vivienda y Año(s) a visualizar")
  #     #Altura predeterminada para la grafica.
  #     plot_height = 500
  #     numPlots = length(input$checkGroup_avp_dfAvp_asig_región)
  #     #Llamado a la funcion calcPlotHeight para calcular la altura basado en el numero de filas.
  #     total_height = plotHeight(plot_height, numPlots)
  #     p <- p + facet_wrap(~Región, ncol = 2) +
  #       theme(panel.spacing.x = unit(0.2, "lines"), #Espacio entre las facetas en x.
  #             panel.spacing.y = unit(-0.02, "lines")) #Espacio entre las facetas en y.
  #     p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)
  #     
  #     return(p)
  #   }
  #   
  #   # Crear la gráfica vacía con mensaje
  #   empty_plot <- create_empty_plot_with_message(data = dfAvp_region_asig_filt, x = "Año", y = "Cantidad", fill = "Estado",
  #                                                xlab = "Año", ylab = "Cantidad de viviendas públicas asignadas", message)
  #   convert_to_plotly(empty_plot, tooltip = "text")
  # })
  # 
  # 
  # #Titulo de la Grafica
  # output$plot_title_dfAvp_asig <- renderUI({
  #   title <- "Viviendas públicas asignadas \nanualmente por violencia doméstica según región"
  # })
  # 
  # 
  # dfAvp_asig_filt_rename <- reactive({
  #   dfAvp_region_asig_filt() %>%
  #     rename(`Región de Vivienda` = Región)
  # })
  # 
  # # Data Table para la gráfica de barras de dfAvp
  # # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
  # output$dataTable_avp_dfAvp_asig <- renderDT(server = FALSE, { 
  #   renderDataTable(dfAvp_asig_filt_rename(), "Datos: Viviendas públicas asignadas por violencia doméstica")
  # })
  
  # Crear Card con Fuentes
  output$dataTableUI_avp_dfAvp_asig <- renderUI({
    if (input$showTable_avp_dfAvp_asig) {
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
            DTOutput("dataTable_avp_dfAvp_asig")
            
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
  })
  
  #### Tab de Administración de Vivienda Pública (mapaAvp) ####

  # Filtrar el conjunto de datos según el año, delito o distrito seleccionado
  mapaAvp_filt <- reactive({
    filter(mapaAvp,
           # Visualizacion %in% input$select_avp_mapaAvp_visualizacion,
           Año %in% input$select_avp_mapaAvp_año)
  })


  mapaAvp_filt_asig <- reactive({
    filter(mapaAvp_asig,
           Año %in% input$select_avp_mapaAvp_año)
  })

  mapaAvp_filt_sol <- reactive({
    filter(mapaAvp_sol,
           Año %in% input$select_avp_mapaAvp_año)
  })

  output$map_avp_mapaAvp_asignadas <- renderLeaflet({
    data <- mapaAvp_filt_asig()
    renderMap_vivienda(data,
                       value_col = "Cantidad",
                       value_col_region = "Región",
                       map_zoom = 8,
                       provider = providers$CartoDB.Positron,
                       municipios_geo = municipios_geo_asig)
  })


  output$map_avp_mapaAvp_solicitadas <- renderLeaflet({
    data <- mapaAvp_filt_sol()
    renderMap_vivienda(data,
                       value_col = "Cantidad",
                       value_col_region = "Región",
                       map_zoom = 8,
                       provider = providers$CartoDB.Positron,
                       municipios_geo = municipios_geo_sol)
  })

  ## PARA REGION Y MUNICIPIOS
  municipios_filt_asig <- reactive({
    filter(municipios_geo_asig,
           Año %in% input$select_avp_mapaAvp_año)
  })

  municipios_filt_sol <- reactive({
    filter(municipios_geo_sol,
           Año %in% input$select_avp_mapaAvp_año)
  })

  output$map_avp_mapaAvp_asignadas <- renderLeaflet({
    data <- mapaAvp_filt_asig()
    municipios_geo_asig <- municipios_filt_asig()

    if (input$select_avp_mapaAvp_visualizacion == "Municipios") {
      renderMap_vivienda_municipio(municipios_geo_asig,
                                   value_col = "Cantidad",
                                   value_col_region = "Municipio",
                                   map_zoom = 8,
                                   provider = providers$CartoDB.Positron)
    } else {
      renderMap_vivienda_new(data,
                         value_col = "Cantidad",
                         value_col_region = "Región",
                         map_zoom = 8,
                         provider = providers$CartoDB.Positron)
    }
  })

  output$map_avp_mapaAvp_solicitadas <- renderLeaflet({
    data <- mapaAvp_filt_sol()
    municipios_geo_sol <-municipios_filt_sol()

    if (input$select_avp_mapaAvp_visualizacion == "Municipios") {
      renderMap_vivienda_municipio(municipios_geo_sol,
                                   value_col = "Cantidad",
                                   value_col_region = "Municipio",
                                   map_zoom = 8,
                                   provider = providers$CartoDB.Positron)
    } else {
      renderMap_vivienda_new(data,
                         value_col = "Cantidad",
                         value_col_region = "Región",
                         map_zoom = 8,
                         provider = providers$CartoDB.Positron)
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

  
  
  
  #### Tab de Publicaciones ####
  # PDF 1
  # publicationCardPDFServer("avp_pdf1", "avp/boletinVS2023.pdf")
  # # PDF 2
  # publicationCardPDFServer("avp_pdf2", "avp/reporte2022.pdf")
  # 
  # output$avp_view_content <- renderUI({
  #   if (input$avp_view_option == "Publicaciones") {
  #     tagList(
  #       tags$ul(
  #         style = "list-style-type: none; text-align: center;",
  #         sectionTitle("Publicaciones y Recursos", "24px")
  #       ),
  #       fluidRow(
  #         column(12, publicationCardPDF("avp_pdf1", "Boletín Estadístico de Violencia Sexual, Año 2023", "avp/boletinVS2023.pdf"))
  #       )
  #       # ,
  #       # fluidRow(
  #       #   column(12, publicationCardPDF("avp_pdf2", "Reporte Estadístico 2022", "avp/reporte2022.pdf"))
  #       # )
  #     )
  #   } else {
  #     # Dashboard de la Agencia (embed link)
  #     tags$div(
  #       fluidRow(
  #         column(12, publicationCardWeb("Dashboard de la Agencia:",
  #                                       "https://safekits.pr.gov/?fbclid=IwAR3ooo0EgJS_8MEVB3Z5IettOyplWfi3sYQU18AZlC5yieAIMhIYOuHjHZs"))
  #       )
  #     )
  #   }
  # })
  #### Tab de Definiciones ####
  
  definitions_avp <- list(
    #list(word = "Región", definition = "Se refiere a una división geográfica o área delimitada que comparte características similares, ya sea geográficas, culturales, económicas, políticas o administrativas. Subdivisión territorial establecida por las autoridades gubernamentales para propósitos de administración y gestión local. Estas divisiones pueden variar en tamaño y alcance dependiendo del país y su estructura administrativa."),
    list(word = "Región", definition = "Subdivisión territorial establecida por las autoridades gubernamentales 
         para propósitos de administración y gestión local."),
    list(word = "Vivienda Pública", definition = "Vivienda que es proporcionada, administrada o subsidiada 
         por el gobierno o entidades gubernamentales con el objetivo de brindar alojamiento a personas o familias
         que tienen dificultades para acceder a una vivienda adecuada en el mercado privado debido a limitaciones 
         económicas o sociales. Estas viviendas suelen estar dirigidas a personas de bajos ingresos, familias en situación 
         de pobreza, personas sin hogar, o aquellos que enfrentan otras formas de vulnerabilidad social."),
    list(word = "Violencia Doméstica", definition = "Definición que ofrece la Ley Núm. 54 de 1989, según enmendada, que sigue vigente
         en Puerto Rico. Violencia doméstica significa un patrón constante de empleo de fuerza física o violencia psicológica,
         intimidación o persecución contra una persona por parte de su cónyuge, excónyuge, una persona con quien cohabita o 
         haya cohabitado, con quien sostiene o haya sostenido una relación consensual o una persona con quien se haya procreado
         una hija o hijo, para causarle daño físico a su persona, sus bienes u otra persona o para causarle grave daño emocional.
         La Ley Núm. 54 incluyó, además, como delito la agresión sexual entre personas que cohabitan o matrimonios como violencia
         doméstica.")
  )
  
  # Convertir lista a dtaframe
  definitions_df_avp <- do.call(rbind, lapply(definitions_avp, as.data.frame))
  
  # Renombrar columnas
  colnames(definitions_df_avp) <- c("Concepto", "Definición")
  
  # mapa de las regiones de vivienda
  # output$map_avp_mapaRegi <- renderPlotly({
  #   p <- renderMapGroup(
  #     data = mapaAvp, fill = Región,
  #     title = "Regiones de Vivienda ",
  #     fill_lab = "Region"
  #   )
  #   ggplotly(p, tooltip = c("all"))
  # })
  
  # Usar funcion para presentar tabla con definiciones
  output$dataTable_Def_avp <- renderDT({
    renderDataTable_Definitions(definitions_df_avp, "Regiones de la Adminsitración de Vivienda Públicas")
  })
  
  
  
  #####################################################
  ########## Tab del Negociado de la Policia ##########
  #####################################################
  #### Tab con datos de mujeres desaparecidas (despDF) ####
  # Filtrar el conjunto de datos según los valores seleccionados del año y la categoria de evento
  despDF_filt <- reactive({
    despDF %>%
      filter(
        Estado %in% c(
          input$checkGroup_poli_despDF_categoría_adultas,
          input$checkGroup_poli_despDF_categoría_menores
        ),
        Año %in% input$checkGroup_poli_despDF_año
      )
  })
  
  
  despDF_filt_Adultas <- reactive({
    filter(despDF_Adultas,
           Estado %in% input$checkGroup_poli_despDF_categoría_adultas,
           Año %in% input$checkGroup_poli_despDF_año)
  })
  
  despDF_filt_Menores <- reactive({
    filter(despDF_Menores,
           Estado %in% input$checkGroup_poli_despDF_categoría_menores,
           Año %in% input$checkGroup_poli_despDF_año)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de categoria
  observeEvent(input$deselectAll_poli_despDF_categoría_adultas, {
    updateCheckboxGroup(session, "checkGroup_poli_despDF_categoría_adultas", input, despDF_Adultas$Estado)
  })
  
  observe({
    inputId <- "checkGroup_poli_despDF_categoría_adultas"
    buttonId <- "deselectAll_poli_despDF_categoría_adultas"
    all_choices <- levels(despDF_Adultas$Estado)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de categoria
  observeEvent(input$deselectAll_poli_despDF_categoría_menores, {
    updateCheckboxGroup(session, "checkGroup_poli_despDF_categoría_menores", input, despDF_Menores$Estado)
  })
  
  observe({
    inputId <- "checkGroup_poli_despDF_categoría_menores"
    buttonId <- "deselectAll_poli_despDF_categoría_menores"
    all_choices <- levels(despDF_Menores$Estado)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_poli_despDF_año, {
    updateCheckboxGroup(session, "checkGroup_poli_despDF_año", input, despDF_Adultas$Año)
  })
  
  observe({
    inputId <- "checkGroup_poli_despDF_año"
    buttonId <- "deselectAll_poli_despDF_año"
    all_choices <- levels(despDF_Adultas$Año)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  # Colores del status
  despDF_fill_categoria_adultas <- setColorFill(despDF_Adultas, "Estado")
  despDF_fill_categoria_menores <- setColorFill(despDF_Menores, "Estado")
  
  # # Descartar filtrar con menores para omitir su representacion en la grafica
  # despDF_filt_noMenores <- reactive({
  #   despDF_filt() %>%  
  #     filter(Estado != "Menores Localizadas")%>%  
  #     filter(Estado != "Menores sin Localizar")
  # })
  
  # Grafico de barras apiladas
  output$barPlot_poli_despDF_adultas <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_categoria <- length(input$checkGroup_poli_despDF_categoría_adultas) > 0
    has_año <- length(input$checkGroup_poli_despDF_año) > 0
    
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_año || !has_categoria) {
      message <- "Seleccione Estado de la Víctima y Año(s) a visualizar"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot_stack(despDF_filt_Adultas, x = "Año", y = "Casos", fill = "Estado",
                               title = HTML(""),
                               xlab = "Año", ylab = "Cantidad de víctimas", fillLab = "Estado de la Víctima",
                               colorFill = despDF_fill_categoria_adultas,
                               emptyMessage = "Seleccione Estado de la Víctima y Año(s) a visualizar")
      
      p <- convert_to_plotly(p, tooltip = "text") %>% layout(height = 450)
      
      return(p)
    }
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(despDF_filt_Adultas, x = "Año", y = "Casos", fill = "Estado", 
                                                 xlab = "Año", ylab = "Cantidad de víctimas", message)
    
    convert_to_plotly(empty_plot, tooltip = "text")
  })
  
  # # Grafico de barras
  # output$barPlot_poli_despDF <- renderPlotly({
  #   # Verificar si hay opciones seleccionadas en cada grupo
  #   has_categoria <- length(input$checkGroup_poli_despDF_categoría) > 0
  #   has_año <- length(input$checkGroup_poli_despDF_año) > 0
  #   
  #   # Crear mensaje si faltan opciones seleccionadas
  #   if (!has_categoria || !has_año) {
  #     message <- "Seleccione Estado de la Víctima y Año(s) a visualizar"
  #   } else {
  #     # Si todas las opciones están seleccionadas, crear la gráfica
  #     p <- renderBarPlot_facets(despDF_filt, x = "Año", y = "Casos", fill = "Estado",
  #                        xlab = "Año", ylab = "Cantidad de víctimas", fillLab = "Estado de la Víctima",
  #                        colorFill = despDF_fill_categoria,
  #                        emptyMessage = "Seleccione Estado de la Víctima y Año(s) a visualizar")
  #     p <- convert_to_plotly(p, tooltip = "text") %>% layout(height = 450)
  #     
  #     return(p)
  #   }
  #   
  #   # Crear la gráfica vacía con mensaje
  #   empty_plot <- create_empty_plot_with_message(data = despDF_filt, x = "Año", y = "Casos", fill = "Estado",
  #                                                xlab = "Año", ylab = "Cantidad de víctimas", message)
  #   convert_to_plotly(empty_plot, tooltip = "text")
  # })
  
  #Titulo de la Grafica Mujeres
  output$plot_title_despDF_adultas <- renderUI({
    title <- "Adultas desaparecidas: localizadas y por localizar"
  })
  
  # # Descartar filtrar con menores para omitir su representacion en la grafica
  # despDF_filt_noAdultas <- reactive({
  #   despDF_filt() %>%  
  #     filter(Estado != "Adultas Localizadas")%>%  
  #     filter(Estado != "Adultas Sin Localizar")
  # })
  
  # Grafico de barras apiladas
  output$barPlot_poli_despDF_menores <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_categoria <- length(input$checkGroup_poli_despDF_categoría_menores) > 0
    has_año <- length(input$checkGroup_poli_despDF_año) > 0
    
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_año || !has_categoria) {
      message <- "Seleccione Estado de la Víctima y Año(s) a visualizar"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot_stack(despDF_filt_Menores, x = "Año", y = "Casos", fill = "Estado",
                               title = HTML(""),
                               xlab = "Año", ylab = "Cantidad de víctimas", fillLab = "Estado de la Víctima",
                               colorFill = despDF_fill_categoria_menores,
                               emptyMessage = "Seleccione Estado de la Víctima y Año(s) a visualizar")
      
      p <- convert_to_plotly(p, tooltip = "text") %>% layout(height = 450)
      
      return(p)
    }
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(despDF_filt_Menores, x = "Año", y = "Casos", fill = "Estado", 
                                                 xlab = "Año", ylab = "Cantidad de víctimas", message)
    
    convert_to_plotly(empty_plot, tooltip = "text")
  })
  
  #Titulo de la Grafica Menores
  output$plot_title_despDF_menores <- renderUI({
    title <- "Menores desaparecidas: localizadas y por localizar"
  })
 
  despDF_filt_rename <- reactive({
    despDF_filt() %>% 
      rename(`Estado de la Víctima` = Estado)
  })
  
  # Data Table para el mapa de despDF
  output$dataTable_poli_despDF <- renderDT(server = FALSE, {
    renderDataTable(despDF_filt_rename(), "Datos: Mujeres desaparecidas y localizadas")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_poli_despDF <- renderUI({
    if (input$showTable_poli_despDF) {
      hyperlinks <- c("https://www.dsp.pr.gov/negociados/negociado-de-la-policia-de-puerto-rico")
      texts <- c("Policía de Puerto Rico")
      
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
  
  observe({
    inputId <- "checkGroup_poli_vEdad_edad"
    buttonId <- "deselectAll_poli_vEdad_edad"
    all_choices <- levels(vEdad$Edad)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_poli_vEdad_año, {
    updateCheckboxGroup(session, "checkGroup_poli_vEdad_año", input, vEdad$Año)
  })
  
  observe({
    inputId <- "checkGroup_poli_vEdad_año"
    buttonId <- "deselectAll_poli_vEdad_año"
    all_choices <- levels(vEdad$Año)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar el sexo
  observeEvent(input$deselectAll_poli_vEdad_sexo, {
    updateCheckboxGroup(session, "checkGroup_poli_vEdad_sexo", input, vEdad$Sexo)
  })
  
  observe({
    inputId <- "checkGroup_poli_vEdad_sexo"
    buttonId <- "deselectAll_poli_vEdad_sexo"
    all_choices <- levels(vEdad$Sexo)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
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
      message <- HTML("Seleccione Grupo(s) de Edad, Sexo de \n la Víctima y Año(s) a visualizar")
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot_facets(vEdad_filt, x = "Año", y = "Casos", fill = "Edad",
                         xlab = "Año", ylab = "Cantidad de víctimas", fillLab = "Grupo de Edad",
                         colorFill = vEdad_fill_edad,
                         emptyMessage = HTML("Seleccione Grupo(s) de Edad, Sexo de \n la Víctima y Año(s) a visualizar"),barWidth = 0, xGap = 0)
      #Altura predeterminada para la grafica.
      plot_height = 500
      numPlots = length(input$checkGroup_poli_vEdad_sexo)
      #Altura predeterminada para la grafica.
      total_height = plotHeight(plot_height, numPlots)
      p <- p + facet_wrap(~Sexo, ncol = 2)+
        theme(panel.spacing.x = unit(0.4, "lines"), #Espacio entre las facetas en x.
              panel.spacing.y = unit(1.75, "lines")) #Espacio entre las facetas en y.
      p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(data = vEdad_filt, x = "Año", y = "Casos", fill = "Edad",
                                                 xlab = "Año", ylab = "Cantidad de víctimas", message)
    convert_to_plotly(empty_plot, tooltip = "text")
  })

  #Titulo de la Grafica
  output$plot_title_vEdad <- renderUI({
    title <- "Incidencia de violencia doméstica por edad de la víctima"
  })
  
  
  
  # Heatmap: ejes = Sexo (x) vs. Tipo de maltrato (y), color = Casos, facetas = Año
  output$heatmap_poli_vEdad <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_edad <- length(input$checkGroup_poli_vEdad_edad) > 0
    has_año <- length(input$checkGroup_poli_vEdad_año) > 0
    has_sexo <- length(input$checkGroup_poli_vEdad_sexo) > 0
    
    if (!has_año || !has_edad || !has_sexo) {
      message <- "Seleccione Grupo(s) de Edad, Sexo de \n la Víctima y Año(s) a visualizar"
      empty_plot <- create_empty_plot_with_message(
        data   = vEdad_filt,
        x      = "Edad",
        y      = "Año",
        fill   = "Casos",
        xlab   = "Edad",
        ylab   = "Número de casos",
        message = message
      )
      return(convert_to_plotly(empty_plot, tooltip = "text"))
    }
    
    p <- renderHeatmap_facets(
      data  = vEdad_filt,
      x     = "Edad",
      y     = "Año",
      value = "Casos",
      facet = "Sexo",
      xlab  = "Edad",
      ylab  = "Número de casos",
      fillLab = "Grupo de Edad",
      emptyMessage = "Seleccione Grupo(s) de Edad, Sexo de \n la Víctima y Año(s) a visualizar",
      lowColor = "white", highColor = "#3b4175"
    )
    
    # Altura dinámica según # de facetas (años seleccionados)
    plot_height <- 500
    numPlots    <- length(input$checkGroup_poli_vEdad_sexo)
    total_height <- plotHeight(plot_height, numPlots)
    
    p <- p + facet_wrap(~Sexo, ncol = 2) +
      theme(panel.spacing.x = unit(0.2, "lines"),
            panel.spacing.y = unit(1.5, "lines"))
    
    convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>%
      layout(height = total_height)
  })
  
 
  # Data Table para el mapa de despDF
  # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
  output$dataTable_poli_vEdad <- renderDT(server = FALSE, {
    renderDataTable(vEdad_filt(), "Datos: Incidentes de violencia doméstica")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_poli_vEdad  <- renderUI({
    if (input$showTable_poli_vEdad ) {
      hyperlinks <- c("https://www.dsp.pr.gov/negociados/negociado-de-la-policia-de-puerto-rico")
      texts <- c("Policía de Puerto Rico")
      
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
  
  
  #### Tab con datos de victimas por sexo (maltPoli) ####
  # Filtrar el conjunto de datos según los valores seleccionados del el grupo de edad y año 
  maltPoli_filt <- reactive({
    filter(maltPoli,
           Maltrato %in% input$checkGroup_poli_Malt,
           Año %in% input$checkGroup_poli_Malt_año,
           Sexo %in% input$checkGroup_poli_Malt_sexo)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar el grupo de edad
  observeEvent(input$deselectAll_poli_Malt, {
    updateCheckboxGroup(session, "checkGroup_poli_Malt", input, maltPoli$Maltrato)
  })
  
  observe({
    inputId <- "checkGroup_poli_Malt"
    buttonId <- "deselectAll_poli_Malt"
    all_choices <- levels(maltPoli$Maltrato)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_poli_Malt_año, {
    updateCheckboxGroup(session, "checkGroup_poli_Malt_año", input, maltPoli$Año)
  })
  
  observe({
    inputId <- "checkGroup_poli_Malt_año"
    buttonId <- "deselectAll_poli_Malt_año"
    all_choices <- levels(maltPoli$Año)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar el sexo
  observeEvent(input$deselectAll_poli_Malt_sexo, {
    updateCheckboxGroup(session, "checkGroup_poli_Malt_sexo", input, maltPoli$Sexo)
  })
  
  observe({
    inputId <- "checkGroup_poli_Malt_sexo"
    buttonId <- "deselectAll_poli_Malt_sexo"
    all_choices <- levels(maltPoli$Sexo)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  # Colores del status
  maltPoli_fill_malt <- setColorFill(maltPoli, "Maltrato")
  
  
  # Grafico de barras
  
  output$barPlot_poli_Malt <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_malt <- length(input$checkGroup_poli_Malt) > 0
    has_año <- length(input$checkGroup_poli_Malt_año) > 0
    has_sexo <- length(input$checkGroup_poli_Malt_sexo) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_malt || !has_año || !has_sexo) {
      message <- HTML("Seleccione Tipo(s) de Maltrato, Sexo de \n la Víctima y Año(s) a visualizar")
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot_facets(maltPoli_filt, x = "Año", y = "Casos", fill = "Maltrato",
                                xlab = "Año", ylab = "Cantidad de víctimas", fillLab = "Maltrato",
                                colorFill = maltPoli_fill_malt,
                                emptyMessage = HTML("Seleccione Grupo(s) de Edad, Sexo de \n la Víctima y Año(s) a visualizar"),barWidth = 0, xGap = 0)
      #Altura predeterminada para la grafica.
      plot_height = 500
      numPlots = length(input$checkGroup_poli_Malt_sexo)
      #Altura predeterminada para la grafica.
      total_height = plotHeight(plot_height, numPlots)
      p <- p + facet_wrap(~Sexo, ncol = 2)+
        theme(panel.spacing.x = unit(0.4, "lines"), #Espacio entre las facetas en x.
              panel.spacing.y = unit(1.75, "lines")) #Espacio entre las facetas en y.
      p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(data = maltPoli_filt, x = "Año", y = "Casos", fill = "Maltrato",
                                                 xlab = "Año", ylab = "Cantidad de víctimas", message)
    convert_to_plotly(empty_plot, tooltip = "text")
  })
  
  #Titulo de la Grafica
  output$plot_title_maltPoli <- renderUI({
    title <- "Incidencia de casos de maltrato por sexo de la víctima"
  })
  
  # Data Table para el mapa de despDF
  # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
  output$dataTable_poli_Malt <- renderDT(server = FALSE, {
    renderDataTable(maltPoli_filt(), "Datos: Incidentes de maltrato")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_poli_Malt  <- renderUI({
    if (input$showTable_poli_Malt ) {
      hyperlinks <- c("https://www.dsp.pr.gov/negociados/negociado-de-la-policia-de-puerto-rico")
      texts <- c("Policía de Puerto Rico")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 98%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;", 
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;",  
            DTOutput("dataTable_poli_Malt")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
  })
  
  #### Tab con datos de victimas y agresores de delitos sexuales por sexo (npprDS_totales, npprDS_victima y npprDS_ofensores) ####
  # Filtrar el conjunto de datos según los valores seleccionados del el grupo de edad y año 
  npprDS_totales_filt <- reactive({
    filter(npprDS_totales,
           Año %in% input$checkGroup_npprDS_año,
           Sexo %in% input$checkGroup_npprDS_sexo)
  })
  
  npprDS_victima_filt <- reactive({
    filter(npprDS_victima,
           Edad %in% input$checkGroup_npprDS_edad,
           Año %in% input$checkGroup_npprDS_año,
           Sexo %in% input$checkGroup_npprDS_sexo)
  })
  
  npprDS_ofensores_filt <- reactive({
    filter(npprDS_ofensores,
           Edad %in% input$checkGroup_npprDS_edad,
           Año %in% input$checkGroup_npprDS_año,
           Sexo %in% input$checkGroup_npprDS_sexo)
  })
  
  npprDS_rol_filt <- reactive({
    filter(npprDS_rol,
           Edad %in% input$checkGroup_npprDS_edad,
           Año %in% input$checkGroup_npprDS_año,
           Sexo %in% input$checkGroup_npprDS_sexo)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar el grupo de edad
  observeEvent(input$deselectAll_npprDS_edad, {
    updateCheckboxGroup(session, "checkGroup_npprDS_edad", input, npprDS_victima$Edad)
  })
  
  observe({
    inputId <- "checkGroup_npprDS_edad"
    buttonId <- "deselectAll_npprDS_edad"
    all_choices <- levels(npprDS_victima$Edad)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_npprDS_año, {
    updateCheckboxGroup(session, "checkGroup_npprDS_año", input, npprDS_victima$Año)
  })
  
  observe({
    inputId <- "checkGroup_npprDS_año"
    buttonId <- "deselectAll_npprDS_año"
    all_choices <- levels(npprDS_victima$Año)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar el sexo
  observeEvent(input$deselectAll_npprDS_sexo, {
    updateCheckboxGroup(session, "checkGroup_npprDS_sexo", input, npprDS_victima$Sexo)
  })
  
  observe({
    inputId <- "checkGroup_npprDS_sexo"
    buttonId <- "deselectAll_npprDS_sexo"
    all_choices <- levels(npprDS_victima$Sexo)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  # Colores del status
  npprDS_totales_fill <- setColorFill(npprDS_totales, "Sexo")
  npprDS_victima_fill <- setColorFill(npprDS_victima, "Edad")
  
  # Grafico de barras totales
  output$barPlot_npprDS_totales <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_npprDS_año) > 0
    has_sexo <- length(input$checkGroup_npprDS_sexo) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_año || !has_sexo) {
      message <- HTML("Seleccione Sexo y Año(s) a visualizar")
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot_facets(npprDS_totales_filt, x = "Año", y = "Casos", fill = "Sexo",
                                xlab = "Año", ylab = "Casos", fillLab = "Sexo",
                                colorFill = npprDS_totales_fill,
                                emptyMessage = HTML("Seleccione Sexo y Año(s) a visualizar"),barWidth = 0, xGap = 0)
      #Altura predeterminada para la grafica.
      plot_height = 500
      numPlots = length(input$checkGroup_npprDS_sexo)
      #Altura predeterminada para la grafica.
      total_height = plotHeight(plot_height, numPlots)
      p <- p + facet_wrap(~Rol, ncol = 2)+
        theme(panel.spacing.x = unit(0.4, "lines"), #Espacio entre las facetas en x.
              panel.spacing.y = unit(1.75, "lines")) #Espacio entre las facetas en y.
      p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(data = npprDS_totales_filt, x = "Año", y = "Casos", fill = "Sexo",
                                                 xlab = "Año", ylab = "Casos", message)
    convert_to_plotly(empty_plot, tooltip = "text")
  })
  
  #Titulo de la Grafica
  output$plot_title_npprDS_totales <- renderUI({
    title <- "Total de casos de delitos sexuales por sexo de las victimas y ofensores"
  })
  
  
  # Grafico de barras victimas
  output$barPlot_npprDS_victima <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_edad <- length(input$checkGroup_npprDS_edad) > 0
    has_año <- length(input$checkGroup_npprDS_año) > 0
    has_sexo <- length(input$checkGroup_npprDS_sexo) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_edad || !has_año || !has_sexo) {
      message <- HTML("Seleccione Grupo(s) de Edad, Sexo \n y Año(s) a visualizar")
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot_facets(npprDS_victima_filt, x = "Año", y = "Casos", fill = "Edad",
                                xlab = "Año", ylab = "Cantidad de víctimas", fillLab = "Edad",
                                colorFill = npprDS_victima_fill,
                                emptyMessage = HTML("Seleccione Grupo(s) de Edad, Sexo \n y Año(s) a visualizar"),barWidth = 0, xGap = 0)
      #Altura predeterminada para la grafica.
      plot_height = 500
      numPlots = length(input$checkGroup_npprDS_sexo)
      #Altura predeterminada para la grafica.
      total_height = plotHeight(plot_height, numPlots)
      p <- p + facet_wrap(~Sexo, ncol = 2)+
        theme(panel.spacing.x = unit(0.4, "lines"), #Espacio entre las facetas en x.
              panel.spacing.y = unit(1.75, "lines")) #Espacio entre las facetas en y.
      p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(data = npprDS_victima_filt, x = "Año", y = "Casos", fill = "Edad",
                                                 xlab = "Año", ylab = "Cantidad de víctimas", message)
    convert_to_plotly(empty_plot, tooltip = "text")
  })
  
  #Titulo de la Grafica
  output$plot_title_npprDS_victima <- renderUI({
    title <- "Casos de delitos sexuales por sexo de la víctima"
  })
  
  # Grafico de barras ofensores
  output$barPlot_npprDS_ofensores <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_edad <- length(input$checkGroup_npprDS_edad) > 0
    has_año <- length(input$checkGroup_npprDS_año) > 0
    has_sexo <- length(input$checkGroup_npprDS_sexo) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_edad || !has_año || !has_sexo) {
      message <- HTML("Seleccione Grupo(s) de Edad, Sexo \n y Año(s) a visualizar")
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot_facets(npprDS_ofensores_filt, x = "Año", y = "Casos", fill = "Edad",
                                xlab = "Año", ylab = "Cantidad de ofensores", fillLab = "Edad",
                                colorFill = npprDS_victima_fill,
                                emptyMessage = HTML("Seleccione Grupo(s) de Edad, Sexo \n y Año(s) a visualizar"),barWidth = 0, xGap = 0)
      #Altura predeterminada para la grafica.
      plot_height = 500
      numPlots = length(input$checkGroup_npprDS_sexo)
      #Altura predeterminada para la grafica.
      total_height = plotHeight(plot_height, numPlots)
      p <- p + facet_wrap(~Sexo, ncol = 2)+
        theme(panel.spacing.x = unit(0.4, "lines"), #Espacio entre las facetas en x.
              panel.spacing.y = unit(1.75, "lines")) #Espacio entre las facetas en y.
      p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(data = npprDS_ofensores_filt, x = "Año", y = "Casos", fill = "Edad",
                                                 xlab = "Año", ylab = "Cantidad de ofensores", message)
    convert_to_plotly(empty_plot, tooltip = "text")
  })
  
  #Titulo de la Grafica
  output$plot_title_npprDS_ofensores <- renderUI({
    title <- "Casos de delitos sexuales por sexo de los ofensores"
  })
  
  # Data Table 
  # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
  output$dataTable_npprDS <- renderDT(server = FALSE, {
    renderDataTable(npprDS_rol_filt(), "Datos: Delitos Sexuales")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_npprDS  <- renderUI({
    if (input$showTable_npprDS) {
      hyperlinks <- c("https://www.dsp.pr.gov/negociados/negociado-de-la-policia-de-puerto-rico")
      texts <- c("Policía de Puerto Rico")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 98%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;", 
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;",  
            DTOutput("dataTable_npprDS")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
  })
  
  
  #### Tab con datos de victimas y agresores de delitos sexuales por region (npprDS_region) ####
  
  mapa_npprDS_region_filt <- reactive({
    filter(mapa_npprDS_region,
           # Visualizacion %in% input$select_avp_mapaAvp_visualizacion,
           Año %in% input$select_mapa_npprDS_region_año)
  })
  
  mapa_npprDS_victimas_mujeres_filt <- reactive({
    filter(mapa_npprDS_victimas_mujeres, 
           Año %in% input$select_mapa_npprDS_region_año)
  })
  
  mapa_npprDS_victimas_hombres_filt <- reactive({
    filter(mapa_npprDS_victimas_hombres, 
           Año %in% input$select_mapa_npprDS_region_año)
  })
  
  mapa_npprDS_ofensores_mujeres_filt <- reactive({
    filter(mapa_npprDS_ofensores_mujeres, 
           Año %in% input$select_mapa_npprDS_region_año)
  })
  
  mapa_npprDS_ofensores_hombres_filt <- reactive({
    filter(mapa_npprDS_ofensores_hombres, 
           Año %in% input$select_mapa_npprDS_region_año)
  })
  
  
  output$map_npprDS_victimas_mujeres<- renderLeaflet({
    data <- mapa_npprDS_victimas_mujeres_filt()
    renderMap_npprDS(data,
                       value_col = "Casos",
                       value_col_region = "Región",
                       map_zoom = 8,
                       provider = providers$CartoDB.Positron,
                       municipios_geo = municipios_geo_asig)
  })
  
  
  output$map_npprDS_victimas_hombres <- renderLeaflet({
    data <- mapa_npprDS_victimas_hombres_filt()
    renderMap_npprDS(data,
                       value_col = "Casos",
                       value_col_region = "Región",
                       map_zoom = 8,
                       provider = providers$CartoDB.Positron,
                       municipios_geo = municipios_geo_sol)
  })
  
  output$map_npprDS_ofensores_mujeres<- renderLeaflet({
    data <- mapa_npprDS_ofensores_mujeres_filt()
    renderMap_npprDS(data,
                       value_col = "Casos",
                       value_col_region = "Región",
                       map_zoom = 8,
                       provider = providers$CartoDB.Positron,
                       municipios_geo = municipios_geo_asig)
  })
  
  
  output$map_npprDS_ofensores_hombres <- renderLeaflet({
    data <- mapa_npprDS_ofensores_hombres_filt()
    renderMap_npprDS  (data,
                       value_col = "Casos",
                       value_col_region = "Región",
                       map_zoom = 8,
                       provider = providers$CartoDB.Positron,
                       municipios_geo = municipios_geo_sol)
  })
  
  
  
  #Titulo de la Grafica
  output$plot_title_npprDS_region <- renderUI({
    title <- paste0("Total de casos de delitos sexuales \npor región en el año ", input$select_mapa_npprDS_region_año)
  })
  
  
  mapa_npprDS_region_filt_rename <- reactive({
      st_drop_geometry(mapa_npprDS_region_filt())%>% 
      rename(`Región Policiaca` = Región)
  })
  
  # Data Table para la gráfica de barras de dfAvp
  # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
  output$dataTable_npprDS_region <- renderDT(server = FALSE, { 
    renderDataTable(mapa_npprDS_region_filt_rename(), "Datos: Total de casos de delitos sexuales")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_npprDS_region <- renderUI({
    if (input$showTable_npprDS_region) {
      hyperlinks <- c("https://www.dsp.pr.gov/negociados/negociado-de-la-policia-de-puerto-rico")
      texts <- c("Policía de Puerto Rico")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 98%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;", 
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;",  
            DTOutput("dataTable_npprDS_region")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
  })
  
  #### Tab con datos de victimas y agresores de delitos sexuales por sexo (npprDS_victimas_agrupados) ####
  # Filtrar el conjunto de datos según los valores seleccionados del el grupo de edad y año 
  npprDS_victimas_agrupados_filt <- reactive({
    filter(npprDS_victimas_agrupados,
           Edad %in% input$checkGroup_npprDS_victimas_agrupados_edad,
           Año %in% input$checkGroup_npprDS_victimas_agrupados_año,
           Sexo %in% input$checkGroup_npprDS_victimas_agrupados_sexo)
  })
  
  npprDS_ofensores_agrupados_filt <- reactive({
    filter(npprDS_ofensores_agrupados,
           Edad %in% input$checkGroup_npprDS_victimas_agrupados_edad,
           Año %in% input$checkGroup_npprDS_victimas_agrupados_año,
           Sexo %in% input$checkGroup_npprDS_victimas_agrupados_sexo)
  })
  
  
  
  ### funcion para el boton de deseleccionar/seleccionar el grupo de edad
  observeEvent(input$deselectAll_npprDS_victimas_agrupados_edad, {
    updateCheckboxGroup(session, "checkGroup_npprDS_victimas_agrupados_edad", input, npprDS_victimas_agrupados$Edad)
  })
  
  observe({
    inputId <- "checkGroup_npprDS_victimas_agrupados_edad"
    buttonId <- "deselectAll_npprDS_victimas_agrupados_edad"
    all_choices <- levels(npprDS_victimas_agrupados$Edad)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_npprDS_victimas_agrupados_año, {
    updateCheckboxGroup(session, "checkGroup_npprDS_victimas_agrupados_año", input, npprDS_victimas_agrupados$Año)
  })
  
  observe({
    inputId <- "checkGroup_npprDS_victimas_agrupados_año"
    buttonId <- "deselectAll_npprDS_victimas_agrupados_año"
    all_choices <- levels(npprDS_victimas_agrupados$Año)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar el sexo
  observeEvent(input$deselectAll_npprDS_victimas_agrupados_sexo, {
    updateCheckboxGroup(session, "checkGroup_npprDS_victimas_agrupados_sexo", input, npprDS_victimas_agrupados$Sexo)
  })
  
  observe({
    inputId <- "checkGroup_npprDS_victimas_agrupados_sexo"
    buttonId <- "deselectAll_npprDS_victimas_agrupados_sexo"
    all_choices <- levels(npprDS_victimas_agrupados$Sexo)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  npprDS_victimas_agrupados_fill <- setColorFill(npprDS_victimas_agrupados, "Edad")
  
  # Grafico de linea
  output$barPlot_npprDS_victimas_agrupados <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    # Verificar si hay opciones seleccionadas en cada grupo
    has_edad <- length(input$checkGroup_npprDS_victimas_agrupados_edad) > 0
    has_año <- length(input$checkGroup_npprDS_victimas_agrupados_año) > 0
    has_sexo <- length(input$checkGroup_npprDS_victimas_agrupados_sexo) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_edad || !has_año || !has_sexo) {
      message <- "Seleccione Grupo(s) de Edad, Sexo y Año(s) que desea visualizar"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      # p <- renderLinePlot_poli(data = npprDS_victimas_agrupados_filt, x = "Año", y = "Casos", group = "Edad",
      #                     color = "Edad", title = "",
      #                     xlab = "Año", ylab = "Número de Casos",
      #                     emptyMessage = "Seleccione Grupo(s) de Edad, Sexo y Año(s) que desea visualizar")
      # 
      # #Altura predeterminada para la grafica.
      # plot_height = 500
      # numPlots = length(input$checkGroup_npprDS_victimas_agrupados_sexo)
      # #Altura predeterminada para la grafica.
      # total_height = plotHeight(plot_height, numPlots)
      # p <- p + facet_wrap(~Sexo, ncol = 2)+
      #   theme(panel.spacing.x = unit(0.4, "lines"), #Espacio entre las facetas en x.
      #         panel.spacing.y = unit(1.75, "lines")) #Espacio entre las facetas en y.
      # p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)
      
      p <- renderBarPlot_facets(npprDS_victimas_agrupados_filt, x = "Año", y = "Casos", fill = "Edad",
                                xlab = "Año", ylab = "Cantidad de víctimas", fillLab = "Edad",
                                colorFill = npprDS_victimas_agrupados_fill,
                                emptyMessage = HTML("Seleccione Grupo(s) de Edad, Sexo y Año(s) que desea visualizar"),barWidth = 0, xGap = 0)
      #Altura predeterminada para la grafica.
      plot_height = 500
      numPlots = length(input$checkGroup_npprDS_victimas_agrupados_sexo)
      #Altura predeterminada para la grafica.
      total_height = plotHeight(plot_height, numPlots)
      p <- p + facet_wrap(~Sexo, ncol = 2)+
        theme(panel.spacing.x = unit(0.4, "lines"), #Espacio entre las facetas en x.
              panel.spacing.y = unit(1.75, "lines")) #Espacio entre las facetas en y.
      p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)

      return(p)
    }

    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(data = npprDS_victimas_agrupados_filt, x = "Año", y = "Casos", fill = " ",
                                                 xlab = "Año", ylab = "Número de Casos", message)
    convert_to_plotly(empty_plot, tooltip = "text")
  })
  
  
  #Titulo de la Grafica
  output$plot_title_npprDS_victimas_agrupados <- renderUI({
    title <- "Total de casos de delitos sexuales por sexo de las VICTIMAS y categoría de edad"
  })
  
  
  # Grafico de linea
  output$barPlot_npprDS_ofensores_agrupados <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    # Verificar si hay opciones seleccionadas en cada grupo
    has_edad <- length(input$checkGroup_npprDS_victimas_agrupados_edad) > 0
    has_año <- length(input$checkGroup_npprDS_victimas_agrupados_año) > 0
    has_sexo <- length(input$checkGroup_npprDS_victimas_agrupados_sexo) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_edad || !has_año || !has_sexo) {
      message <- "Seleccione Grupo(s) de Edad, Sexo y Año(s) que desea visualizar"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      # p <- renderLinePlot_poli(data = npprDS_victimas_agrupados_filt, x = "Año", y = "Casos", group = "Edad",
      #                     color = "Edad", title = "",
      #                     xlab = "Año", ylab = "Número de Casos",
      #                     emptyMessage = "Seleccione Grupo(s) de Edad, Sexo y Año(s) que desea visualizar")
      # 
      # #Altura predeterminada para la grafica.
      # plot_height = 500
      # numPlots = length(input$checkGroup_npprDS_victimas_agrupados_sexo)
      # #Altura predeterminada para la grafica.
      # total_height = plotHeight(plot_height, numPlots)
      # p <- p + facet_wrap(~Sexo, ncol = 2)+
      #   theme(panel.spacing.x = unit(0.4, "lines"), #Espacio entre las facetas en x.
      #         panel.spacing.y = unit(1.75, "lines")) #Espacio entre las facetas en y.
      # p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)
      
      p <- renderBarPlot_facets(npprDS_ofensores_agrupados_filt, x = "Año", y = "Casos", fill = "Edad",
                                xlab = "Año", ylab = "Cantidad de ofensores", fillLab = "Edad",
                                colorFill = npprDS_victimas_agrupados_fill,
                                emptyMessage = HTML("Seleccione Grupo(s) de Edad, Sexo y Año(s) que desea visualizar"),barWidth = 0, xGap = 0)
      #Altura predeterminada para la grafica.
      plot_height = 500
      numPlots = length(input$checkGroup_npprDS_victimas_agrupados_sexo)
      #Altura predeterminada para la grafica.
      total_height = plotHeight(plot_height, numPlots)
      p <- p + facet_wrap(~Sexo, ncol = 2)+
        theme(panel.spacing.x = unit(0.4, "lines"), #Espacio entre las facetas en x.
              panel.spacing.y = unit(1.75, "lines")) #Espacio entre las facetas en y.
      p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(data = npprDS_ofensores_agrupados_filt, x = "Año", y = "Casos", fill = " ",
                                                 xlab = "Año", ylab = "Número de Casos", message)
    convert_to_plotly(empty_plot, tooltip = "text")
  })
  
  
  #Titulo de la Grafica
  output$plot_title_npprDS_ofensores_agrupados <- renderUI({
    title <- "Total de casos de delitos sexuales por sexo de los OFENSORES y categoría de edad"
  })
  
  # Data Table 
  # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
  output$dataTable_npprDS_victimas_agrupados <- renderDT(server = FALSE, {
    renderDataTable(npprDS_victimas_agrupados_filt(), "Datos: Delitos Sexuales")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_npprDS_victimas_agrupados  <- renderUI({
    if (input$showTable_npprDS_victimas_agrupados) {
      hyperlinks <- c("https://www.dsp.pr.gov/negociados/negociado-de-la-policia-de-puerto-rico")
      texts <- c("Policía de Puerto Rico")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 98%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;", 
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;",  
            DTOutput("dataTable_npprDS_victimas_agrupados")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
  })
  
  
  #### Tab con datos de victimas y agresores de delitos sexuales por sexo (npprDS_relacion) ####
  # Filtrar el conjunto de datos según los valores seleccionados de region, tipo de relacion y año 
  npprDS_relacion_filt <- reactive({
    filter(npprDS_relacion,
           Año %in% input$checkGroup_npprDS_relacion_año,
           Relación %in% input$checkGroup_npprDS_relacion_relacion,
           Región %in% input$checkGroup_npprDS_relacion_region)
  })
  
  npprDS_relacion_total <- npprDS_relacion %>%
    group_by(Año, Relación) %>%
    summarise(Casos = sum(Casos, na.rm = TRUE)) %>%
    ungroup()
  
  npprDS_relacion_filt_total <- reactive({
    filter(npprDS_relacion_total,
           Año %in% input$checkGroup_npprDS_relacion_año,
           Relación %in% input$checkGroup_npprDS_relacion_relacion
    )
  })
  
  
  ### funcion para el boton de deseleccionar/seleccionar el grupo de edad
  observeEvent(input$deselectAll_npprDS_relacion_relacion, {
    updateCheckboxGroup(session, "checkGroup_npprDS_relacion_relacion", input, npprDS_relacion$Relación)
  })
  
  observe({
    inputId <- "checkGroup_npprDS_relacion_relacion"
    buttonId <- "deselectAll_npprDS_relacion_relacion"
    all_choices <- levels(npprDS_relacion$Relación)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_npprDS_relacion_año, {
    updateCheckboxGroup(session, "checkGroup_npprDS_relacion_año", input, npprDS_relacion$Año)
  })
  
  observe({
    inputId <- "checkGroup_npprDS_relacion_año"
    buttonId <- "deselectAll_npprDS_relacion_año"
    all_choices <- levels(npprDS_relacion$Año)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar el sexo 
  observeEvent(input$deselectAll_npprDS_relacion_region, {
    updateCheckboxGroup(session, "checkGroup_npprDS_relacion_region", input, npprDS_relacion$Región)
  })
  
  observe({
    inputId <- "checkGroup_npprDS_relacion_region"
    buttonId <- "deselectAll_npprDS_relacion_region"
    all_choices <- levels(npprDS_relacion$Región)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  # Colores del status
  npprDS_relacion_fill <- setColorFill(npprDS_relacion, "Relación")
  
  # # Grafico de barras totales
  # output$barPlot_npprDS_relacion <- renderPlotly({
  #   # Verificar si hay opciones seleccionadas en cada grupo
  #   has_año <- length(input$checkGroup_npprDS_relacion_año) > 0
  #   has_relacion <- length(input$checkGroup_npprDS_relacion_relacion) > 0
  #   has_region <- length(input$checkGroup_npprDS_relacion_region) > 0
  #   
  #   # Crear mensaje si faltan opciones seleccionadas
  #   if (!has_año || !has_relacion || !has_region) {
  #     message <- HTML("Seleccione Región, Tipo(s) de Relación \n y Año(s) a visualizar")
  #   } else {
  #     # Si todas las opciones están seleccionadas, crear la gráfica
  #     p <- renderBarPlot_facets(npprDS_relacion_filt, x = "Año", y = "Casos", fill = "Relación",
  #                               xlab = "Año", ylab = "Casos", fillLab = "Relación",
  #                               colorFill = npprDS_relacion_fill,
  #                               emptyMessage = HTML("Seleccione Región, Tipo(s) de Relación \n y Año(s) a visualizar"),
  #                               barWidth = 0, xGap = 0)
  #     #Altura predeterminada para la grafica.
  #     plot_height = 500
  #     numPlots = length(input$checkGroup_npprDS_relacion_region)
  #     #Altura predeterminada para la grafica.
  #     total_height = plotHeight(plot_height, numPlots)
  #     p <- p + facet_wrap(~Región, ncol = 2)+
  #       theme(panel.spacing.x = unit(0.4, "lines"), #Espacio entre las facetas en x.
  #             panel.spacing.y = unit(1.75, "lines")) #Espacio entre las facetas en y.
  #     p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)
  #     
  #     return(p)
  #   }
  #   
  #   # Crear la gráfica vacía con mensaje
  #   empty_plot <- create_empty_plot_with_message(data = npprDS_relacion_filt, x = "Año", y = "Casos", fill = "Relación",
  #                                                xlab = "Año", ylab = "Casos", message)
  #   convert_to_plotly(empty_plot, tooltip = "text")
  # })
  # 
  
  # Grafico de barras
  output$barPlot_npprDS_relacion <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_npprDS_relacion_año) > 0
    has_relacion <- length(input$checkGroup_npprDS_relacion_relacion) > 0
    has_region <- length(input$checkGroup_npprDS_relacion_region) > 0
    
    
    # Si faltan selecciones necesarias
    if (!has_año || !has_relacion) {
      message <- HTML("Seleccione Tipo(s) de Relación \n y Año(s) a visualizar")
      empty_plot <- create_empty_plot_with_message(npprDS_relacion_filt, x = "Año", y = "Casos", fill = "Relación",
                                                   xlab = "Año", ylab = "Casos", message)
      #Titulo de la Grafica
      output$plot_title_npprDS_relacion <- renderUI({
        title <- "Total de casos de delitos sexuales por tipo de relación ofensor/víctima"
      })
      
      return(convert_to_plotly(empty_plot, tooltip = "text"))
    }
    
    # Si NO hay región seleccionada, usar el dataset agregado
    if (!has_region) {
      p <- renderBarPlot_facets(npprDS_relacion_filt_total, x = "Año", y = "Casos", fill = "Relación",
                                xlab = "Año", ylab = "Casos", fillLab = "Relación",
                                colorFill = npprDS_relacion_fill,
                                emptyMessage = HTML("Seleccione Región, Tipo(s) de Relación \n y Año(s) a visualizar"))
      
      #Titulo de la Grafica
      output$plot_title_npprDS_relacion <- renderUI({
        title <- "Total de casos de delitos sexuales según tipo de relación ofensor/víctima"
      })
      
      # Data Table
      # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
      output$dataTable_npprDS_relacion <- renderDT(server = FALSE, {
        renderDataTable(npprDS_relacion_filt_total(), "Datos: Delitos Sexuales según tipo de relación ofensor/víctima")
      })
      
      return(convert_to_plotly(p, tooltip = "text")%>% layout(height = 450))
    }
    
    #Titulo de la Grafica
    output$plot_title_npprDS_relacion <- renderUI({
      title <- "Total de casos de delitos sexuales según región y tipo de relación ofensor/víctima"
    })
    
    p <- renderBarPlot_facets(npprDS_relacion_filt, x = "Año", y = "Casos", fill = "Relación",
                              xlab = "Año", ylab = "Casos", fillLab = "Relación",
                              colorFill = npprDS_relacion_fill,
                              emptyMessage = HTML("Seleccione Región, Tipo(s) de Relación \n y Año(s) a visualizar"))
    #Altura predeterminada para la grafica.
    plot_height = 500
    numPlots = length(input$checkGroup_npprDS_relacion_region)
    #Llamado a la funcion calcPlotHeight para calcular la altura basado en el numero de filas.
    total_height = plotHeight(plot_height, numPlots)
    p <- p + facet_wrap(~Región, ncol = 2) +
      theme(panel.spacing.x = unit(0.2, "lines"), #Espacio entre las facetas en x.
            panel.spacing.y = unit(-0.02, "lines")) #Espacio entre las facetas en y.
    p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)
    
    # Data Table
    # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
    output$dataTable_npprDS_relacion <- renderDT(server = FALSE, {
      renderDataTable(npprDS_relacion_filt(), "Datos: Delitos Sexuales según tipo de relación ofensor/víctima")
    })
    
    return(p)
  })
  
  
  
  # Texto explicativo dinámico
  output$texto_npprDS_relacion <- renderUI({
    regiones <- input$checkGroup_npprDS_relacion_region
    if (is.null(regiones) || length(regiones) == 0) {
      HTML(
        "<p style='font-size: 16px;padding: 0px;'>
        Los datos representados en esta gráfica corresponden a los
        datos de delitos sexuales según el tipo de relación
        ofensor/víctima desde el año natural 2019 al 2025.
        
      </p>"
      )
    } else {
      HTML(
        "<p style='font-size: 16px;padding: 0px;'>
        Los datos representados en esta gráfica corresponden a los
        datos de delitos sexuales según región y tipo de relación
        ofensor/víctima desde el año natural 2019 al 2025.
      </p>"
      )
    }
  })
  
  
  # Crear Card con Fuentes
  output$dataTableUI_npprDS_relacion  <- renderUI({
    if (input$showTable_npprDS_relacion) {
      hyperlinks <- c("https://www.dsp.pr.gov/negociados/negociado-de-la-policia-de-puerto-rico")
      texts <- c("Policía de Puerto Rico")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 98%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;", 
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;",  
            DTOutput("dataTable_npprDS_relacion")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
  })
  
  
  
  #### Tab con datos de victimas de delitos sexuales por sexo y tipo de delito (npprDS_tiposdelitos) ####
  # Filtrar el conjunto de datos según los valores seleccionados de sexo, tipo de delito y año 
  npprDS_tiposdelitos_filt <- reactive({
    filter(npprDS_tiposdelitos,
           Año %in% input$checkGroup_npprDS_tiposdelitos_año,
           Delitos %in% input$checkGroup_npprDS_tiposdelitos_delito,
           Sexo %in% input$checkGroup_npprDS_tiposdelitos_sexo)
  })
  
  ### funcion para el boton de deseleccionar/seleccionar el tipo de delito
  observeEvent(input$deselectAll_npprDS_tiposdelitos_delito, {
    updateCheckboxGroup(session, "checkGroup_npprDS_tiposdelitos_delito", input, npprDS_tiposdelitos$Delitos)
  })
  
  observe({
    inputId <- "checkGroup_npprDS_tiposdelitos_delito"
    buttonId <- "deselectAll_npprDS_tiposdelitos_delito"
    all_choices <- levels(npprDS_tiposdelitos$Delitos)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_npprDS_tiposdelitos_año, {
    updateCheckboxGroup(session, "checkGroup_npprDS_tiposdelitos_año", input, npprDS_tiposdelitos$Año)
  })
  
  observe({
    inputId <- "checkGroup_npprDS_tiposdelitos_año"
    buttonId <- "deselectAll_npprDS_tiposdelitos_año"
    all_choices <- levels(npprDS_tiposdelitos$Año)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar el sexo
  observeEvent(input$deselectAll_npprDS_tiposdelitos_sexo, {
    updateCheckboxGroup(session, "checkGroup_npprDS_tiposdelitos_sexo", input, npprDS_tiposdelitos$Sexo)
  })
  
  observe({
    inputId <- "checkGroup_npprDS_tiposdelitos_sexo"
    buttonId <- "deselectAll_npprDS_tiposdelitos_sexo"
    all_choices <- levels(npprDS_tiposdelitos$Sexo)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  # Colores del status
  npprDS_tiposdelitos_fill <- setColorFill(npprDS_tiposdelitos, "Delitos")
  
  # Grafico de barras totales
  output$barPlot_npprDS_tiposdelitos <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_npprDS_tiposdelitos_año) > 0
    has_sexo <- length(input$checkGroup_npprDS_tiposdelitos_sexo) > 0
    has_delito <- length(input$checkGroup_npprDS_tiposdelitos_delito) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_año || !has_sexo || !has_delito) {
      message <- HTML("Seleccione Sexo, Delito(s) y Año(s) a visualizar")
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot_facets(npprDS_tiposdelitos_filt, x = "Año", y = "Casos", fill = "Delitos",
                                xlab = "Año", ylab = "Casos", fillLab = "Delitos",
                                colorFill = npprDS_tiposdelitos_fill,
                                emptyMessage = HTML("Seleccione Sexo, Delito(s) y Año(s) a visualizar"),barWidth = 0, xGap = 0)
      #Altura predeterminada para la grafica.
      plot_height = 500
      numPlots = length(input$checkGroup_npprDS_sexo)
      #Altura predeterminada para la grafica.
      total_height = plotHeight(plot_height, numPlots)
      p <- p + facet_wrap(~Sexo, ncol = 2)+
        theme(panel.spacing.x = unit(0.4, "lines"), #Espacio entre las facetas en x.
              panel.spacing.y = unit(1.75, "lines")) #Espacio entre las facetas en y.
      p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(data = npprDS_tiposdelitos_filt, x = "Año", y = "Casos", fill = "Delitos",
                                                 xlab = "Año", ylab = "Casos", message)
    convert_to_plotly(empty_plot, tooltip = "text")
  })
  
  #Titulo de la Grafica
  output$plot_title_npprDS_tiposdelitos <- renderUI({
    title <- "Total de casos de delitos sexuales por sexo de las victimas y tipo de delito"
  })
  
  
  # Data Table 
  # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
  output$dataTable_npprDS_tiposdelitos <- renderDT(server = FALSE, {
    renderDataTable(npprDS_tiposdelitos_filt(), "Datos: Delitos Sexuales")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_npprDS_tiposdelitos  <- renderUI({
    if (input$showTable_npprDS_tiposdelitos) {
      hyperlinks <- c("https://www.dsp.pr.gov/negociados/negociado-de-la-policia-de-puerto-rico")
      texts <- c("Policía de Puerto Rico")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 98%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;", 
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;",  
            DTOutput("dataTable_npprDS_tiposdelitos")
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
  

  #### Tab de Publicaciones ####
  # # PDF 1
  # publicationCardPDFServer("cavv_pdf1", "cavv/boletinVS2023.pdf")
  # # PDF 2
  # publicationCardPDFServer("pdf2", "snmv/reporte2022.pdf")
  # 
  output$poli_view_content <- renderUI({
    if (input$snmv_view_option == "Publicaciones") {
      tagList(
        tags$ul(
          style = "list-style-type: none; text-align: center;",
          sectionTitle("Publicaciones y Recursos", "24px")
        )
        # ,
        # fluidRow(
        #   column(12, publicationCardPDF("cavv_pdf1", "Boletín Estadístico de Violencia Sexual, Año 2023", "cavv/boletinVS2023.pdf"))
        # )
        # ,
        # fluidRow(
        #   column(12, publicationCardPDF("pdf2", "Reporte Estadístico 2022", "snmv/reporte2022.pdf"))
        # )
      )
    } else {
      # Dashboard de la Agencia (embed link)
      tags$div(
        fluidRow(
          column(12, publicationCardWeb("Dashboard de la Agencia:", 
                                        "https://app.powerbigov.us/view?r=eyJrIjoiN2M3NTdiZTQtNTgyZS00NGMyLTgxYjAtZWQ4ZTE5NmMwNDJmIiwidCI6ImUwYzIyNzAyLTA5MmYtNGRhYi1hNTkyLWZhYjUyZGRlNGMxZiJ9"))
        )
      )
    }
  })
  
  #### Tab de Definiciones ####
  definitions_poli <- list(
    list(word = "Adultas Desaparecidas", definition = "Mujeres adultas cuya ubicación y paradero son desconocidos y no pueden 
         ser determinados por familiares, amigos, o autoridades competentes. Esto puede surgir por razones como accidentes, 
         secuestros, desastres naturales, o decisiones voluntarias de abandonar su entorno sin dejar rastro."),
    list(word = "Adultas Localizadas", definition = "Mujeres adultas cuyo paradero ha sido identificado y confirmado después
         de haber sido reportadas como desaparecidas."),
    list(word = "Adultas Sin Localizar", definition = "Mujeres adultas cuyo paradero no ha sido identificado ni confirmado 
         tras haber sido reportadas como desaparecidas. Pueden haber sido vistas por última vez en circunstancias desconocidas,
         y su ubicación actual sigue siendo un misterio."),
    list(word = "Incidencia", definition = "Número de incidentes o delitos reportados o registrados por las fuerzas policiales
         durante un período específico en una determinada área geográfica. Usada por autoridades para medir y analizar la 
         cantidad y tipo de delitos en una comunidad."),
    list(word = "Menores Desaparecidas", definition = "Menores femeninas cuya ubicación y paradero son desconocidos y no pueden
         ser determinados por familiares, amigos, o autoridades competentes. Esto puede surgir por razones como accidentes, 
         secuestros, desastres naturales, o decisiones voluntarias de abandonar su entorno sin dejar rastro."),
    list(word = "Menores Localizadas", definition = "Menores femeninas cuyo paradero ha sido identificado y confirmado después
         de haber sido reportadas como desaparecidas."),
    list(word = "Menores Sin Localizar", definition = "Menores femeninas cuyo paradero no ha sido identificado ni confirmado 
         tras haber sido reportadas como desaparecidas. Pueden haber sido vistas por última vez en circunstancias desconocidas,
         y su ubicación actual sigue siendo un misterio."),
    list(word = "Región Policiaca", definition = "Zona geográfica específica asignada a un cuerpo de policía para llevar a cabo
         funciones de vigilancia, patrullaje y protección del orden público. Delimitada por autoridades para organizar y 
         distribuir eficazmente recursos policiales."),
    list(word = "Violencia Doméstica", definition = "Definición que ofrece la Ley Núm. 54 de 1989, según enmendada, que sigue vigente
         en Puerto Rico. Violencia doméstica significa un patrón constante de empleo de fuerza física o violencia psicológica,
         intimidación o persecución contra una persona por parte de su cónyuge, excónyuge, una persona con quien cohabita o 
         haya cohabitado, con quien sostiene o haya sostenido una relación consensual o una persona con quien se haya procreado
         una hija o hijo, para causarle daño físico a su persona, sus bienes u otra persona o para causarle grave daño emocional.
         La Ley Núm. 54 incluyó, además, como delito la agresión sexual entre personas que cohabitan o matrimonios como violencia
         doméstica."),
    list(word = "Víctima", definition = "Persona que ha sufrido daño físico, emocional, psicológico o financiero como resultado
         de un acto delictivo, un accidente, un desastre natural, o cualquier otro evento traumático.")
  )
  
  # Convertir lista a dataframe
  definitions_df_poli <- do.call(rbind, lapply(definitions_poli, as.data.frame))
  
  # Renombrar columnas
  colnames(definitions_df_poli) <- c("Concepto", "Definición")
  
  # Usar funcion para presentar tabla con definiciones
  output$dataTable_Def_poli <- renderDT({
    renderDataTable_Definitions(definitions_df_poli, "Policía de Puerto Rico")
  })

  
  
  ########################################################################
  ########## Tab de la Oficina de la Procuradora de las Mujeres ##########
  ########################################################################
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
  
  observe({
    inputId <- "checkGroup_opm_opmFemiVD_año"
    buttonId <- "deselectAll_opm_opmFemiVD_año"
    all_choices <- levels(opmFemiVD$Año)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  
  # Grafico de linea
  output$linePlot_opm_opmFemiVD <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_opm_opmFemiVD_año) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_año) {
      message <- "Seleccione los Año(s) que desea visualizar"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderLinePlot(data = opmFemiVD_filt, x = "Año", y = "Tasa", group = "1",
                          color = "1", title = "",
                          xlab = "Año", ylab = "Tasa por cada 100 mil mujeres",
                          emptyMessage = "Seleccione los Año(s) que desea visualizar")
      
      p <- convert_to_plotly(p, tooltip = "text") %>% layout(height = 450)
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(data = opmFemiVD_filt, x = "Año", y = "Tasa", fill = " ",
                                                 xlab = "Año", ylab = "Tasa por cada 100 mil mujeres", message)
    convert_to_plotly(empty_plot, tooltip = "text")
  })
  
  
  #Titulo de la Grafica
  output$plot_title_opmFemiVD <- renderUI({
    title <- "Tasa anual de asesinatos de mujeres por violencia doméstica del año 1990 al 2024"
  })

  # Data Table para el mapa de despDF
  # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
  output$dataTable_opm_opmFemiVD <- renderDT(server = FALSE, {
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
  
  
  #### tab con datos de agresores segun tipo de situacion (opmAgresores) ####
  # Filtrar el conjunto de datos según los valores seleccionados del año y la categoria de evento
  opmAgresores_filt <- reactive({
    filter(opmAgresores,
           Año %in% input$checkGroup_opm_opmAgresores_año,
           Razón %in% input$checkGroup_opm_opmAgresores_tipo
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_opm_opmAgresores_año, {
    updateCheckboxGroup(session, "checkGroup_opm_opmAgresores_año", input, opmAgresores$Año)
  })
  
  observe({
    inputId <- "checkGroup_opm_opmAgresores_año"
    buttonId <- "deselectAll_opm_opmAgresores_año"
    all_choices <- levels(opmAgresores$Año)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de tipo de violencia
  observeEvent(input$deselectAll_opm_opmAgresores_tipo, {
    updateCheckboxGroup(session, "checkGroup_opm_opmAgresores_tipo", input, opmAgresores$Razón)
  })
  
  observe({
    inputId <- "checkGroup_opm_opmAgresores_tipo"
    buttonId <- "deselectAll_opm_opmAgresores_tipo"
    all_choices <- levels(opmAgresores$Razón)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  # Colores del status
  opm_fill_tipo <- setColorFill(opmAgresores, "Razón")
  
  # Grafico de barras
  output$barPlot_opm_opmAgresores <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_opm_opmAgresores_año) > 0
    has_tipo <- length(input$checkGroup_opm_opmAgresores_tipo) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_año || !has_tipo) {
      message <- "Seleccione Tipo de Situación y Año(s) a visualizar"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot_facets(opmAgresores_filt, x = "Año", y = "Cantidad", fill = "Razón",
                                xlab = "Año", ylab = "Cantidad de Agresores", fillLab = "Tipo de situación",
                                colorFill = opm_fill_tipo,
                                emptyMessage = "Seleccione Tipo de Situación y Año(s) a visualizar")
      p <- convert_to_plotly(p, tooltip = "text") %>% layout(height = 450)
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(data = opmAgresores_filt, x = "Año", y = "Cantidad", fill = "Razón",
                                                 xlab = "Año", ylab = "Cantidad de Agresores", message)
    convert_to_plotly(empty_plot, tooltip = "text")
  })
  
  #Titulo de la Grafica
  output$plot_title_opmAgresores <- renderUI({
    title <- "Agresores según el tipo de situación"
  })
  
  opmAgresores_filt_rename <- reactive({
    opmAgresores_filt() %>% 
      rename(`Tipo de Situación` = Razón) %>% 
      rename(`Cantidad de Agresores` = Cantidad)
  })
  
  # Data Table para el mapa de despDF
  # output$dataTable_opm_opmAgresores <- renderDT({
  #   renderDataTable(opmAgresores_filt_rename(), "Datos: Agresores según el tipo de situación")
  # })
  
  output$dataTable_opm_opmAgresores <- renderDT(server = FALSE, {
    renderDataTable(opmAgresores_filt_rename(), "Datos: Agresores según el tipo de situación")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_opm_opmAgresores  <- renderUI({
    if (input$showTable_opm_opmAgresores) {
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
            DTOutput("dataTable_opm_opmAgresores")
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
  
  observe({
    inputId <- "checkGroup_opm_opmCasos_año"
    buttonId <- "deselectAll_opm_opmCasos_año"
    all_choices <- levels(opmCasos$Año)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de tipo de violencia
  observeEvent(input$deselectAll_opm_opmCasos_tipo, {
    updateCheckboxGroup(session, "checkGroup_opm_opmCasos_tipo", input, opmCasos$Razón)
  })
  
  observe({
    inputId <- "checkGroup_opm_opmCasos_tipo"
    buttonId <- "deselectAll_opm_opmCasos_tipo"
    all_choices <- levels(opmCasos$Razón)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
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
      p <- renderBarPlot_facets(opmCasos_filt, x = "Año", y = "Cantidad", fill = "Razón",
                         xlab = "Año", ylab = "Cantidad de Personas Atendidas", fillLab = "Razón para Consulta",
                         colorFill = opm_fill_tipo,
                         emptyMessage = "Seleccione Razón de la consulta y Año(s) a visualizar")
      p <- convert_to_plotly(p, tooltip = "text") %>% layout(height = 450)
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(data = opmCasos_filt, x = "Año", y = "Cantidad", fill = "Razón",
                                                 xlab = "Año", ylab = "Cantidad de Personas Atendidas", message)
    convert_to_plotly(empty_plot, tooltip = "text")
  })
  
  #Titulo de la Grafica
  output$plot_title_opmCasos <- renderUI({
    title <- "Población atendida por el programa CRIAS: razón de consulta"
  })

  opmCasos_filt_rename <- reactive({
    opmCasos_filt() %>% 
      rename(`Razón para Consulta` = Razón) %>% 
      rename(`Personas Atendidas` = Cantidad)
  })
  
  # Data Table para el mapa de despDF
  # output$dataTable_opm_opmCasos <- renderDT({
  #   renderDataTable(opmCasos_filt_rename(), "Datos: Población atendida mediante el programa CRIAS")
  # })
  
  output$dataTable_opm_opmCasos <- renderDT(server = FALSE, {
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
  
  observe({
    inputId <- "checkGroup_opm_opmVic_año"
    buttonId <- "deselectAll_opm_opmVic_año"
    all_choices <- levels(opmVic$Año)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón del género de la víctima
  observeEvent(input$deselectAll_opm_opmVic_género, {
    updateCheckboxGroup(session, "checkGroup_opm_opmVic_género", input, opmVic$Género)
  })
  
  observe({
    inputId <- "checkGroup_opm_opmVic_género"
    buttonId <- "deselectAll_opm_opmVic_género"
    all_choices <- levels(opmVic$Género)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
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
      p <- renderBarPlot_facets(opmVic_filt, x = "Año", y = "Víctimas", fill = "Género",
                         xlab = "Año", ylab = "Cantidad de Víctimas", fillLab = "Género de la Víctima",
                         colorFill = opmVic_fill_género,
                         emptyMessage = "Seleccione Género y Año(s) a visualizar")
      p <- convert_to_plotly(p, tooltip = "text") %>% layout(height = 450)
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(data = opmVic_filt, x = "Año", y = "Víctimas", fill = "Género",
                                                 xlab = "Año", ylab = "Cantidad de Víctimas", message)
    convert_to_plotly(empty_plot, tooltip = "text")
  })
  
  #Titulo de la Grafica
  output$plot_title_opmVic <- renderUI({
    title <- "Identidad de género de víctimas asistidas por el programa CRIAS"
  })

  # Data Table para el mapa de despDF
  # output$dataTable_opm_opmVic <- renderDT({
  #   renderDataTable(opmVic_filt(), "Datos: Identidad de género de víctimas asistidas por el programa CRIAS")
  # })
  
  output$dataTable_opm_opmVic <- renderDT(server = FALSE, {
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
  
  observe({
    inputId <- "checkGroup_opm_opmMedio_año"
    buttonId <- "deselectAll_opm_opmMedio_año"
    all_choices <- levels(opmMedio$Año)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón del medio de orientación
  observeEvent(input$deselectAll_opm_opmMedio_medio, {
    updateCheckboxGroup(session, "checkGroup_opm_opmMedio_medio", input, opmMedio$Orientación)
  })
  
  observe({
    inputId <- "checkGroup_opm_opmMedio_medio"
    buttonId <- "deselectAll_opm_opmMedio_medio"
    all_choices <- levels(opmMedio$Orientación)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
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
      p <- renderBarPlot_facets(opmMedio_filt, x = "Año", y = "Cantidad", fill = "Orientación",
                         xlab = "Año", ylab = "Cantidad de Personas Orientadas", fillLab = "Medio de Orientación",
                         colorFill = opmMedio_fill_medio,
                         emptyMessage = "Seleccione Medio de Orientación y Año(s) a visualizar")
      p <- convert_to_plotly(p, tooltip = "text") %>% layout(height = 450)
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(data = opmMedio_filt, x = "Año", y = "Cantidad", fill = "Orientación",
                                                 xlab = "Año", ylab = "Cantidad de Personas Orientadas", message)
    convert_to_plotly(empty_plot, tooltip = "text")
  })
  
  #Titulo de la Grafica
  output$plot_title_opmMedio <- renderUI({
    title <- "Orientaciones brindadas por el programa CRIAS"
  })

  opmMedio_filt_rename <- reactive({
    opmMedio_filt() %>% 
      rename(`Medio de Orientación` = Orientación) %>% 
      rename(`Personas Orientadas` = Cantidad)
  })

  # Data Table para opmMedio
  # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
  output$dataTable_opm_opmMedio <- renderDT(server = FALSE, {
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
  
  observe({
    inputId <- "checkGroup_opm_opmServiciosMes_año"
    buttonId <- "deselectAll_opm_opmServiciosMes_año"
    all_choices <- levels(opmServiciosMes$Año)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de tipo de servicio
  observeEvent(input$deselectAll_opm_opmServiciosMes_tipo, {
    updateCheckboxGroup(session, "checkGroup_opm_opmServiciosMes_tipo", input, opmServiciosMes$Servicio)
  })
  
  observe({
    inputId <- "checkGroup_opm_opmServiciosMes_tipo"
    buttonId <- "deselectAll_opm_opmServiciosMes_tipo"
    all_choices <- levels(opmServiciosMes$Servicio)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
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
      p <- renderBarPlot_facets(opmServiciosMes_filt, x = "Año", y = "Cantidad", fill = "Servicio",
                         xlab = "Año", ylab = "Cantidad de Servicios Ofrecidos", fillLab = "Tipo de Servicio",
                         colorFill = opmServiciosMes_fill_tipo,
                         emptyMessage = "Seleccione Tipo de servicio y Año(s) a visualizar")
      p <- convert_to_plotly(p, tooltip = "text") %>% layout(height = 450)
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(data = opmServiciosMes_filt, x = "Año", y = "Cantidad", fill = "Servicio",
                                                 xlab = "Año", ylab = "Cantidad de Servicios Ofrecidos", message)
    convert_to_plotly(empty_plot, tooltip = "text")
  })
  
  #Titulo de la Grafica
  output$plot_title_opmServiciosMes <- renderUI({
    title <- "Atención, servicios y seguimiento de casos mediante el programa CRIAS"
  })
  
  opmServiciosMes_filt_rename <- reactive({
    opmServiciosMes_filt() %>% 
      rename(`Tipo de Servicio` = Servicio) %>% 
      rename(`Servicios Ofrecidos` = Cantidad)
  })
  
  
  # Data Table para opmServiciosMes
  # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
  output$dataTable_opm_opmServiciosMes <- renderDT(server = FALSE, {
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
  
  #### Tab de Publicaciones ####
  # PDF 1
  # publicationCardPDFServer("opm_pdf1", "opm/boletinVS2023.pdf")
  # # PDF 2
  # publicationCardPDFServer("opm_pdf2", "opm/reporte2022.pdf")
  # 
  # output$opm_view_content <- renderUI({
  #   if (input$opm_view_option == "Publicaciones") {
  #     tagList(
  #       tags$ul(
  #         style = "list-style-type: none; text-align: center;",
  #         sectionTitle("Publicaciones y Recursos", "24px")
  #       ),
  #       fluidRow(
  #         column(12, publicationCardPDF("opm_pdf1", "Boletín Estadístico de Violencia Sexual, Año 2023", "opm/boletinVS2023.pdf"))
  #       )
  #       # ,
  #       # fluidRow(
  #       #   column(12, publicationCardPDF("opm_pdf2", "Reporte Estadístico 2022", "opm/reporte2022.pdf"))
  #       # )
  #     )
  #   } else {
  #     # Dashboard de la Agencia (embed link)
  #     tags$div(
  #       fluidRow(
  #         column(12, publicationCardWeb("Dashboard de la Agencia:",
  #                                       "https://safekits.pr.gov/?fbclid=IwAR3ooo0EgJS_8MEVB3Z5IettOyplWfi3sYQU18AZlC5yieAIMhIYOuHjHZs"))
  #       )
  #     )
  #   }
  # })
  #### Tab de Definiciones ####
  definitions_opm <- list(
    list(word = "Acecho (A)", definition = "Es el patrón o la repetición de una conducta mediante 
         la cual se mantiene de manera constante o repetida una vigilancia, proximidad física o 
         visual sobre una persona específica, en la mayora der las ocasiones, una mujer."),
    list(word = "Agresión sexual (AS)", definition = "Cualquier acto que degrada o daña el cuerpo
         y/o la sexualidad de la víctima y que por tanto atenta contra su libertad, dignidad e integridad
         física. Es una expresión de abuso de poder que implica la supremacía masculina sobre la mujer, 
         al denigrar y concebirla como objeto."),
    list(word = "CRIAS", definition = "Centro de Respuesta Integrada de Apoyo y Servicios para la Mujer.
         La misma surgió de la necesidad imperante de trabajar con el problema de la desigualdad que existe
         contra las mujeres y trabajar particularmente con la violencia doméstica desde una perspectiva 
         dirigida hacia la validación, orientación y coordinación de servicios de apoyo. El Centro CRIAS
         establece las bases para un modelo de prevención, intervención y fiscalización de los diferentes
         tipos de violencia que nos permite levantar información de las víctimas sobrevivientes participantes,
         obtener análisis de experiencias personales y manejo de actitudes ante el problema. En el mismo, se 
         ofrecen servicios de orientación, coordinación de servicios y referidos a mujeres víctimas/sobrevivientes
         de violencia doméstica, agresión sexual, acecho y otras manifestaciones de violencia por razón de género."),
    list(word = "Discrimen de género (DG)", definition = "Hace referencia a «toda distinción, exclusión o restricción»
         que se realice en función del género de una persona con el objetivo o resultado de menoscabar o anular el 
         reconocimiento, goce o ejercicio de sus derechos humanos. A menudo es consecuencia de los mitos y estereotipos
         de género tales como: las mujeres son las más aptas para ocuparse de la educación de los hijos, cocinar o 
         limpiar, o para realizar trabajos de secretaría, enseñanza o enfermería, mientras que los hombres son líderes,
         buenos en economía y comercio. Esto ha dado lugar a un acceso desigual al mercado laboral, así como a un salario
         desigual para puestos similares, al sostenerse que las mujeres tienen peores resultados que los hombres en 
         determinados ámbitos y, con ello, a una discriminación por género."),
    list(word = "Femenino", definition = "Se refiere a características, atributos o cualidades asociadas tradicionalmente
         con las mujeres o lo que se considera típicamente propio del género femenino."),
    list(word = "Feminicidios", definition = HTML("Es el crimen que consiste en matar intencionalmente a mujeres por el
    hecho de ser mujeres o de identificarse como tales. Las definiciones más amplias incluyen cualquier asesinato de 
    mujeres o niñas, o el asesinato involuntario o indirecto de mujeres o niñas, «tal como demuestran algunos casos de 
    violencia doméstica que podrían provocar la muerte de mujeres». El concepto «adquirió importancia en el marco del 
    movimiento feminista de la década de 1970 cuando surge la expresión ‘femicidio’ como alternativa al término neutro 
    ‘homicidio’, con el fin de reconocer y visibilizar la opresión, la desigualdad y la violencia sistemática» contra 
    las mujeres que, en su forma más extrema, culmina en la muerte. El femicidio puede presentarse bajo diferentes formas
    e incluye los siguientes actos:
            <ul>   
            <br>
            <li> Femicidio íntimo, perpetrado por una pareja actual o anterior, generalmente durante o después de una 
            relación ya violenta (por ejemplo, de violencia doméstica o violencia sexual);
            <li> El llamado crimen de honor (o asesinato de o por honor); 
            <li> El femicidio relacionado con la dote, que ocurre en un contexto de conflicto entre las familias de
            dos cónyuges recién casados, y es generalmente cometido por la familia política que cuestiona sobre todo
            el importe de la dote;
            <li> El femicidio no íntimo, cometido por un agresor que no tiene una relación íntima con la víctima,
            que está muy difundido en algunas partes de América Latina y que, por lo general, está precedido de 
            actos de violencia sexual o tortura.
            </ul>")),
    list(word = "Identidad de género", definition = "Se refiere a la manera en que una persona se identifica, cómo
         se reconoce o se expresa sobre sí misma, en cuanto al género que puede corresponder o no a su sexo biológico
         o asignado en su nacimiento."),
    list(word = "Masculino", definition = "Término utilizado para describir características, atributos o cualidades 
         asociadas tradicionalmente con los hombres o lo que se considera típicamente propio del género masculino."),
    list(word = "Orientaciones", definition = "Direcciones o inclinaciones hacia las que se dirige o enfoca algo."),
    list(word = "Tendencia", definition = "Dirección o patrón observado en datos o eventos que muestra una inclinación 
         hacia cierto resultado o comportamiento a lo largo del tiempo."),
    list(word = "Trans", definition = "Abreviatura comúnmente utilizada para referirse a personas que son transgénero 
         o que tienen una identidad de género diferente de aquella que se les asignó al nacer. Las personas transgénero
         pueden identificarse como hombre, mujer, ambos, ninguno o con un género diferente al binario tradicional de 
         hombre y mujer."),
    list(word = "Violencia doméstica (VD)", definition = "Definición que ofrece la Ley Núm. 54 de 1989 que sigue vigente
         en Puerto Rico. Violencia doméstica significa un patrón constante de empleo de fuerza física o violencia 
         psicológica, intimidación o persecución contra una persona por parte de su cónyuge, ex cónyuge, una persona
         con quien cohabita o haya cohabitado, con quien sostiene o haya sostenido una relación consensual o una persona
         con quien se haya procreado una hija o hijo, para causarle daño físico a su persona, sus bienes u otra persona
         o para causarle grave daño emocional. La Ley Núm. 54 incluyó además como delito la agresión sexual entre personas
         que cohabitan o matrimonios como violencia doméstica."),
    list(word = "Violencia en cita (VC)", definition = "Violencia cometida por una persona que está o ha estado en una
         relación social de carácter romántico o íntimo con la víctima. La existencia de dicha relación se determinará
         con base en la declaración de la parte informante y teniendo en cuenta la duración de la relación, el tipo 
         de relación y la frecuencia de interacción entre las personas involucradas en la relación. A los efectos de
         esta definición: la violencia en cita incluye, pero no se limita a, abuso sexual o físico o la amenaza
         de tal abuso. La violencia en cita no incluye actos cubiertos por la definición de violencia doméstica.")
  )
  
  
  # Convertir lista a dataframe
  definitions_df_opm <- do.call(rbind, lapply(definitions_opm, as.data.frame))
  
  # Renombrar columnas
  colnames(definitions_df_opm) <- c("Concepto", "Definición")
  
  # Usar funcion para presentar tabla con definiciones
  output$dataTable_Def_opm <- renderDT({
    renderDataTable_Definitions(definitions_df_opm, "Oficina de la Procuradora de la Mujer")
  })
  
  
  
  ########################################################################
  ########## Tab del Departamento de Correción y Rehabilitación ##########
  ########################################################################
  #### tab con datos de los servicios ofrecidos (dcrCasosInv) ####
  
  # Filtrar el conjunto de datos según los valores seleccionados del año, el tipo de servicio y el sexo
  # dcrCasosInv_filt <- reactive({
  #   filter(dcrCasosInv,
  #          Año %in% input$checkGroup_dcr_dcrCasosInv_year,
  #          Estado %in% input$checkGroup_dcr_dcrCasosInv_tipo,
  #          Sexo %in% input$checkGroup_dcr_dcrCasosInv_sexo
  #   )
  # })
  
  dcrCasosInv_filt <- reactive({
    filter(dcrCasosInv_supervision,
           Año %in% input$checkGroup_dcr_dcrCasosInv_year,
           # Estado %in% input$checkGroup_dcr_dcrCasosInv_tipo,
           Sexo %in% input$checkGroup_dcr_dcrCasosInv_sexo
    )
  })
  
  
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de año
  observeEvent(input$deselectAll_dcr_dcrCasosInv_year, {
    updateCheckboxGroup(session, "checkGroup_dcr_dcrCasosInv_year", input, dcrCasosInv$Año)
  })
  
  observe({
    inputId <- "checkGroup_dcr_dcrCasosInv_year"
    buttonId <- "deselectAll_dcr_dcrCasosInv_year"
    all_choices <- levels(dcrCasosInv$Año)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  # ### funcion para el boton de deseleccionar/seleccionar del botón del estado de investigación
  # observeEvent(input$deselectAll_dcr_dcrCasosInv_tipo, {
  #   updateCheckboxGroup(session, "checkGroup_dcr_dcrCasosInv_tipo", input, dcrCasosInv$Estado)
  # })
  # 
  # observe({
  #   inputId <- "checkGroup_dcr_dcrCasosInv_tipo"
  #   buttonId <- "deselectAll_dcr_dcrCasosInv_tipo"
  #   all_choices <- levels(dcrCasosInv$Estado)
  #   selected <- input[[inputId]]
  #   
  #   is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
  #   
  #   updateActionButton(
  #     session,
  #     inputId = buttonId,
  #     label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
  #   )
  # })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón de sexo
  observeEvent(input$deselectAll_dcr_dcrCasosInv_sexo, {
    updateCheckboxGroup(session, "checkGroup_dcr_dcrCasosInv_sexo", input, dcrCasosInv$Sexo)
  })
  
  observe({
    inputId <- "checkGroup_dcr_dcrCasosInv_sexo"
    buttonId <- "deselectAll_dcr_dcrCasosInv_sexo"
    all_choices <- levels(dcrCasosInv$Sexo)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  # Colores del status
  dcrCasosInv_fill_tipo <- setColorFill(dcrCasosInv, "Estado")
  
  # Grafico de barras
  output$barPlot_dcr_dcrCasosInv <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_dcr_dcrCasosInv_year) > 0
    # has_tipo <- length(input$checkGroup_dcr_dcrCasosInv_tipo) > 0
    has_sexo <- length(input$checkGroup_dcr_dcrCasosInv_sexo) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_año || !has_sexo) {
      message <- HTML("Seleccione Estado de la investigación, \n Sexo y Año(s) a visualizar")
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot_facets(dcrCasosInv_filt, x = "Año", y = "Cantidad", fill = "Estado",
                         xlab = "Año", ylab = "Promedio de casos sentenciados", fillLab = "Estado de Investigación",
                         colorFill = dcrCasosInv_fill_tipo,
                         emptyMessage = HTML("Seleccione Estado de la investigación, \n Sexo y Año(s) a visualizar"))
      #Altura predeterminada para la grafica.
      plot_height = 500
      numPlots = length(input$checkGroup_dcr_dcrSentenciadas_tipo)
      #Llamado a la funcion calcPlotHeight para calcular la altura basado en el numero de filas.
      total_height = plotHeight(plot_height, numPlots)
      p <- p + facet_wrap(~Sexo, ncol = 2) +
        theme(panel.spacing.x = unit(0.4, "lines"), #Espacio entre las facetas en x.
              panel.spacing.y = unit(1.75, "lines")) #Espacio entre las facetas en y.
      
      p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(data = dcrCasosInv_filt, x = "Año", y = "Cantidad", fill = "Estado",
                                                 xlab = "Año", ylab = "Promedio de casos sentenciados", message)
    convert_to_plotly(empty_plot, tooltip = "text")
  })
  
  #Titulo de la Grafica
  output$plot_title_dcrCasosInv <- renderUI({
    HTML(paste0(
      "Promedio de casos sentenciados bajo el programa de supervisión electrónica por<br>",
      "Ley 54 dentro del Programa de la comunidad"
    ))
  })
  
  dcrCasosInv_filt_rename <- reactive({
    dcrCasosInv_filt() %>% 
      rename(`Estado de Investigación` = Estado) 
  })
  
  # Data Table para dcrCasosInv
  # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
  output$dataTable_dcr_dcrCasosInv <- renderDT(server = FALSE, {
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
  
  observe({
    inputId <- "checkGroup_dcr_dcrSentenciadas_year"
    buttonId <- "deselectAll_dcr_dcrSentenciadas_year"
    all_choices <- levels(dcrSentenciadas$Año)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el boton de deseleccionar/seleccionar del botón del estado de caso 
  observeEvent(input$deselectAll_dcr_dcrSentenciadas_tipo, {
    updateCheckboxGroup(session, "checkGroup_dcr_dcrSentenciadas_tipo", input, dcrSentenciadas$Estado)
  })
  
  observe({
    inputId <- "checkGroup_dcr_dcrSentenciadas_tipo"
    buttonId <- "deselectAll_dcr_dcrSentenciadas_tipo"
    all_choices <- levels(dcrSentenciadas$Estado)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  # Colores del status
  dcrSentenciadas_fill_tipo <- setColorFill(dcrSentenciadas, "Mes")
  #dcrSentenciadas_fill_tipo <- setColorFill(dcrSentenciadas, "Estado")
  
  # # Grafico de barras
  
  output$barPlot_dcr_dcrSentenciadas <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_dcr_dcrSentenciadas_year) > 0
    has_tipo <- length(input$checkGroup_dcr_dcrSentenciadas_tipo) > 0
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_año || !has_tipo) {
      message <- "Seleccione Estado del caso y Año(s) a visualizar"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot_facets(dcrSentenciadas_filt, x = "Año", y = "Cantidad", fill = "Mes",
                         xlab = "Año", ylab = "Cantidad de Personas Sentenciadas", fillLab = "Mes", 
                         colorFill = dcrSentenciadas_fill_tipo, 
                         emptyMessage = "Seleccione Estado del caso y Año(s) a visualizar")
      #Altura predeterminada para la grafica.
      plot_height = 500
      numPlots = length(input$checkGroup_dcr_dcrSentenciadas_tipo)
      #Llamado a la funcion calcPlotHeight para calcular la altura basado en el numero de filas.
      total_height = plotHeight(plot_height, numPlots)
      p <- p + facet_wrap(~Estado, ncol = 1) +
        theme(panel.spacing.x = unit(0.4, "lines"), #Espacio entre las facetas en x.
              panel.spacing.y = unit(1.75, "lines")) #Espacio entre las facetas en y.
      
      p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(data = dcrSentenciadas_filt, x = "Año", y = "Cantidad", fill = "Mes",
                                                 xlab = "Año", ylab = "Cantidad de Personas Sentenciadas", message)
    #ggplotly(empty_plot)
    convert_to_plotly(empty_plot, tooltip = "text")
  })
  
  #Titulo de la Grafica
  output$plot_title_dcrSentenciadas <- renderUI({
    title <- "Sentenciados por violencia doméstica bajo supervisión electrónica del programa de la comunidad"
  })
  
  dcrSentenciadas_filt_rename <- reactive({
    dcrSentenciadas_filt() %>% 
      rename(`Estado del Caso` = Estado) 
  })

  
  # Data Table para dcrSentenciadas
  # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
  output$dataTable_dcr_dcrSentenciadas <- renderDT(server = FALSE, {
    renderDataTable(dcrSentenciadas_filt_rename(), "Datos: Programa de supervisión <br>electrónica por delitos de <br>violencia doméstica")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_dcr_dcrSentenciadas  <- renderUI({
    if (input$showTable_dcr_dcrSentenciadas) {
      hyperlinks <- c("https://dcr.pr.gov/")
      texts <- c("Departamento de Corrección y Rehabilitación")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 90%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 90%; display: flex; justify-content: center;",  
          div(
            style = "width: 90%; max-width: 750px; overflow-x: auto;",  
            DTOutput("dataTable_dcr_dcrSentenciadas")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
  })
  
  
  #### Tab de Publicaciones ####
  # PDF 1
  # publicationCardPDFServer("dcr_pdf1", "dcr/boletinVS2023.pdf")
  # # PDF 2
  # publicationCardPDFServer("dcr_pdf2", "dcr/reporte2022.pdf")
  # 
  # output$dcr_view_content <- renderUI({
  #   if (input$dcr_view_option == "Publicaciones") {
  #     tagList(
  #       tags$ul(
  #         style = "list-style-type: none; text-align: center;",
  #         sectionTitle("Publicaciones y Recursos", "24px")
  #       ),
  #       fluidRow(
  #         column(12, publicationCardPDF("dcr_pdf1", "Boletín Estadístico de Violencia Sexual, Año 2023", "dcr/boletinVS2023.pdf"))
  #       )
  #       # ,
  #       # fluidRow(
  #       #   column(12, publicationCardPDF("dcr_pdf2", "Reporte Estadístico 2022", "dcr/reporte2022.pdf"))
  #       # )
  #     )
  #   } else {
  #     # Dashboard de la Agencia (embed link)
  #     tags$div(
  #       fluidRow(
  #         column(12, publicationCardWeb("Dashboard de la Agencia:",
  #                                       "https://safekits.pr.gov/?fbclid=IwAR3ooo0EgJS_8MEVB3Z5IettOyplWfi3sYQU18AZlC5yieAIMhIYOuHjHZs"))
  #       )
  #     )
  #   }
  # })
  #### Tab de Definiciones ####
  definitions_dcr <- list(
    list(word = "Investigaciones realizadas", definition = "Proceso sistemático y metódico de recopilación, análisis y evaluación de información con el objetivo de obtener conclusiones, resolver problemas o generar conocimiento en un campo específico."),
    list(word = "Ley 54", definition = "Ley Núm. 54-1989, conocida como la “Ley para la Prevención e Intervención con la Violencia Doméstica”, según enmendada, establece la violencia doméstica como delito y lo define como el empleo de fuerza física o violencia psicológica, intimidación o persecución en contra de su pareja o expareja. Esto, para causarle daño físico a su persona, a sus bienes o a otra persona o para causarle grave daño emocional."),
    list(word = "Personas sentenciadas", definition = "Pronunciamiento que hace el juez o la jueza sobre la pena que se le impone a una persona acusada luego de que se determina que es culpable de cometer un delito."),
    list(word = "Programas de Comunidad", definition = "Son programas de tratamientos establecidos para que las personas convictas cumplan parte de su sentencia fuera de la institución penal. Su finalidad es promover que los convictos que estén capacitados para reintegrarse a la sociedad puedan hacerlo como parte de su rehabilitación moral y social."),
    list(word = "Programa de Supervisión Electrónica", definition = "El Programa de Monitoreo Electrónico cuenta con la Unidad Especializada de Monitoreo Electrónico (Unidad) compuesta por Oficiales Correccionales, la cual tiene la responsabilidad de supervisar y monitorear a los participantes pertenecientes al programa. Esta supervisión conlleva el verificar y atender las alertas que se activan a través del sistema de transmisión electrónica, activar el protocolo, solicitar apoyo interagencial, avisar a la víctima y administrar pruebas toxicológicas, entre otras.")
  )
  
  # Convertir lista a dataframe
  definitions_df_dcr <- do.call(rbind, lapply(definitions_dcr, as.data.frame))
  
  # Renombrar columnas
  colnames(definitions_df_dcr) <- c("Concepto", "Definición")
  
  # Usar funcion para presentar tabla con definiciones
  output$dataTable_Def_dcr <- renderDT({
    renderDataTable_Definitions(definitions_df_dcr, "Departamento de Corrección y Rehabilitación")
  })
  
  
  
  ############################################################
  ########## Tab de la Administración de Tribunales ##########
  ############################################################
  #### tab con datos de Ley 148 - Violencia Sexual por grupo de edad (OP_148_SoliGrupEdad Responsive) ####
  
  # Filtrar el conjunto de datos según los valores seleccionados del año fiscal, el grupo de edad y el distrito fiscal
  OP_148_SoliGrupEdad_filt <- reactive({
    filter(OP_148_SoliGrupEdad,
           AñoFiscal %in% input$checkGroup_trib_OP_148_SoliGrupEdad_AñoFiscal,
           Edad %in% input$checkGroup_trib_OP_148_SoliGrupEdad_Edad,
           Región %in% input$checkGroup_trib_OP_148_SoliGrupEdad_Región
    )
  })
  
  OP_148_SoliGrupEdad_total <- OP_148_SoliGrupEdad %>%
    group_by(AñoFiscal, Edad) %>%
    summarise(Solicitudes = sum(Solicitudes, na.rm = TRUE)) %>%
    ungroup()
  
  
  OP_148_SoliGrupEdad_filt_total <- reactive({
    filter(OP_148_SoliGrupEdad_total,
           AñoFiscal %in% input$checkGroup_trib_OP_148_SoliGrupEdad_AñoFiscal,
           Edad %in% input$checkGroup_trib_OP_148_SoliGrupEdad_Edad
    )
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el año fiscal
  observeEvent(input$deselectAll_trib_OP_148_SoliGrupEdad_AñoFiscal, {
    updateCheckboxGroup_trib(session, "checkGroup_trib_OP_148_SoliGrupEdad_AñoFiscal", input, OP_148_SoliGrupEdad$AñoFiscal)
  })
  
  observe({
    inputId <- "checkGroup_trib_OP_148_SoliGrupEdad_AñoFiscal"
    buttonId <- "deselectAll_trib_OP_148_SoliGrupEdad_AñoFiscal"
    all_choices <- levels(OP_148_SoliGrupEdad$AñoFiscal)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  
  ### funcion para el botón de deseleccionar/seleccionar el grupo de edad
  observeEvent(input$deselectAll_trib_OP_148_SoliGrupEdad_Edad, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_148_SoliGrupEdad_Edad", input, OP_148_SoliGrupEdad$Edad)
  })
  
  observe({
    inputId <- "checkGroup_trib_OP_148_SoliGrupEdad_Edad"
    buttonId <- "deselectAll_trib_OP_148_SoliGrupEdad_Edad"
    all_choices <- levels(OP_148_SoliGrupEdad$Edad)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el botón de deseleccionar/seleccionar la región fiscal
  observeEvent(input$deselectAll_trib_OP_148_SoliGrupEdad_Región, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_148_SoliGrupEdad_Región", input, OP_148_SoliGrupEdad$Región)
  })

  observe({
    inputId <- "checkGroup_trib_OP_148_SoliGrupEdad_Región"
    buttonId <- "deselectAll_trib_OP_148_SoliGrupEdad_Región"
    all_choices <- levels(OP_148_SoliGrupEdad$Región)
    selected <- input[[inputId]]

    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)

    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  # Colores del status
  OP_148_SoliGrupEdad_fill_edad <- setColorFill(OP_148_SoliGrupEdad, "Edad")

  # Grafico de Barras
  output$barPlot_OP_148_SoliGrupEdad <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_trib_OP_148_SoliGrupEdad_AñoFiscal) > 0
    has_edad <- length(input$checkGroup_trib_OP_148_SoliGrupEdad_Edad) > 0
    has_region <- length(input$checkGroup_trib_OP_148_SoliGrupEdad_Región) > 0

    # Si faltan selecciones necesarias
    if (!has_año || !has_edad) {
      message <- HTML("Seleccione Grupo de Edad y Año(s) a visualizar")
      empty_plot <- create_empty_plot_with_message(data = OP_148_SoliGrupEdad_filt, x = "AñoFiscal", y = "Solicitudes", fill = "Edad",
                                                   xlab = "Año Fiscal", ylab = "Órdenes de Protección Solicitadas", message)
     
      #Titulo de la Grafica
      output$plot_title_OP_148_SoliGrupEdad <- renderUI({
        title <- "Solicitudes de órdenes de protección bajo Ley 148"
      })
      
      return(convert_to_plotly(empty_plot, tooltip = "text"))
    }
    
    # Si NO hay región seleccionada, usar el dataset agregado
    if (!has_region) {
      p <- renderBarPlot_facets(OP_148_SoliGrupEdad_filt_total, x = "AñoFiscal", y = "Solicitudes", fill = "Edad",
                                xlab = "Año Fiscal", ylab = "Órdenes de Protección Solicitadas", fillLab = "Grupo de Edad",
                                colorFill = OP_148_SoliGrupEdad_fill_edad,
                                emptyMessage = HTML("Seleccione Grupo(s) de Edad y Año(s) a visualizar"))
      #Titulo de la Grafica
      output$plot_title_OP_148_SoliGrupEdad <- renderUI({
        title <- "Total de solicitudes de órdenes de protección bajo Ley 148 según grupo de edad"
      })
      
      OP_148_SoliGrupEdad_filt_rename <- reactive({
        OP_148_SoliGrupEdad_filt_total() %>%
          rename(`Año Fiscal` = AñoFiscal)  
      })
      
      
      # Data Table para dcrCasosInv
      # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
      output$dataTable_OP_148_SoliGrupEdad <- renderDT(server = FALSE, {
        renderDataTable(OP_148_SoliGrupEdad_filt_rename(), "Datos: Órdenes de protección solicitadas por violencia sexual")
      })
      
      return(convert_to_plotly(p, tooltip = "text")%>% layout(height = 450))
    }
    
    #Titulo de la Grafica
    output$plot_title_OP_148_SoliGrupEdad <- renderUI({
      title <- "Solicitudes de órdenes de protección bajo Ley 148 según región judicial y edad"
    })
    
    p <- renderBarPlot_facets(OP_148_SoliGrupEdad_filt, x = "AñoFiscal", y = "Solicitudes", fill = "Edad",
                              xlab = "Año Fiscal", ylab = "Órdenes de Protección Solicitadas", fillLab = "Grupo de Edad",
                              colorFill = OP_148_SoliGrupEdad_fill_edad,
                              emptyMessage = HTML("Seleccione Grupo(s) de Edad, Región Judicial \n y Año(s) a visualizar"))
    #Altura predeterminada para la grafica.
    plot_height = 500
    numPlots = length(input$checkGroup_trib_OP_148_SoliGrupEdad_Región)
    #Llamado a la funcion calcPlotHeight para calcular la altura basado en el numero de filas.
    total_height = plotHeight(plot_height, numPlots)
    p <- p + facet_wrap(~Región, ncol = 2) +
      theme(panel.spacing.x = unit(0.2, "lines"), #Espacio entre las facetas en x.
            panel.spacing.y = unit(-0.02, "lines")) #Espacio entre las facetas en y.
    p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)
    
    OP_148_SoliGrupEdad_filt_rename <- reactive({
      OP_148_SoliGrupEdad_filt() %>%
        rename(`Año Fiscal` = AñoFiscal)  %>%
        rename(`Región Judicial` = Región)
    })
    
    
    # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
    output$dataTable_OP_148_SoliGrupEdad <- renderDT(server = FALSE, {
      renderDataTable(OP_148_SoliGrupEdad_filt_rename(), "Datos: Órdenes de protección solicitadas por violencia sexual")
    })
    
    return(p)
  })
  
  
  # Texto explicativo dinámico
  output$texto_Solicitadas <- renderUI({
    regiones <- input$checkGroup_trib_OP_148_SoliGrupEdad_Región
    if (is.null(regiones) || length(regiones) == 0) {
      HTML(
        "<p style='font-size: 16px;padding: 0px;'>
        Los datos representados en esta gráfica corresponden a
        las órdenes de protección solicitadas por violencia sexual
        bajo la Ley 148, según grupo de edad y año 
        fiscal para todas las regiones judiciales de Puerto Rico.
        Los datos para cada año fiscal se identifican con el
        año en que finaliza el mismo, por ejemplo, para los datos
        del año fiscal 2020-2021, los datos son presentados como 
        año fiscal 2021. Los datos del 2025 son preliminares.
      </p>"
      )
    } else {
      HTML(
        "<p style='font-size: 16px;padding: 0px;'>
        Los datos representados en esta gráfica corresponden a
        las órdenes de protección solicitadas por violencia sexual
        bajo la Ley 148, según grupo de edad, región judicial y año fiscal.
        Los datos para cada año fiscal se identifican con el
        año en que finaliza el mismo, por ejemplo, para los datos
        del año fiscal 2020-2021, los datos son presentados como 
        año fiscal 2021. Los datos del 2025 son preliminares.
      </p>"
      )
    }
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
  
    
  #### (OP_Ley148_ex_parteEmitidas Responsive) ####
  
  # Filtrar el conjunto de datos según los valores seleccionados del año fiscal, el delito cometido y la región fiscal
  OP_Ley148_ex_parteEmitidas_filt <- reactive({
    filter(OP_Ley148_ex_parteEmitidas,
           AñoFiscal %in% input$checkGroup_trib_OP_Ley148_ex_parteEmitidas_AñoFiscal,
           Delito %in% input$checkGroup_trib_OP_Ley148_ex_parteEmitidas_Delito,
           Región %in% input$checkGroup_trib_OP_Ley148_ex_parteEmitidas_Región
    )
  })

  OP_Ley148_ex_parteEmitidas_total <- OP_Ley148_ex_parteEmitidas %>%
    group_by(AñoFiscal, Delito) %>%
    summarise(ÓrdenesEmitidas = sum(ÓrdenesEmitidas, na.rm = TRUE)) %>%
    ungroup()
  
  
  OP_Ley148_ex_parteEmitidas_filt_total <- reactive({
    filter(OP_Ley148_ex_parteEmitidas_total,
           AñoFiscal %in% input$checkGroup_trib_OP_Ley148_ex_parteEmitidas_AñoFiscal,
           Delito %in% input$checkGroup_trib_OP_Ley148_ex_parteEmitidas_Delito
    )
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el año fiscal
  observeEvent(input$deselectAll_trib_OP_Ley148_ex_parteEmitidas_AñoFiscal, {
    updateCheckboxGroup_trib(session, "checkGroup_trib_OP_Ley148_ex_parteEmitidas_AñoFiscal", input, OP_Ley148_ex_parteEmitidas$AñoFiscal)
  })
  
  observe({
    inputId <- "checkGroup_trib_OP_Ley148_ex_parteEmitidas_AñoFiscal"
    buttonId <- "deselectAll_trib_OP_Ley148_ex_parteEmitidas_AñoFiscal"
    all_choices <- levels(OP_Ley148_ex_parteEmitidas$AñoFiscal)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  
  ### funcion para el botón de deseleccionar/seleccionar el delito
  observeEvent(input$deselectAll_trib_OP_Ley148_ex_parteEmitidas_Delito, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_Ley148_ex_parteEmitidas_Delito", input, OP_Ley148_ex_parteEmitidas$Delito)
  })
  
  observe({
    inputId <- "checkGroup_trib_OP_Ley148_ex_parteEmitidas_Delito"
    buttonId <- "deselectAll_trib_OP_Ley148_ex_parteEmitidas_Delito"
    all_choices <- levels(OP_Ley148_ex_parteEmitidas$Delito)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ## funcion para el botón de deseleccionar/seleccionar el Región
  observeEvent(input$deselectAll_trib_OP_Ley148_ex_parteEmitidas_Región, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_Ley148_ex_parteEmitidas_Región", input, OP_Ley148_ex_parteEmitidas$Región)
  })

  observe({
    inputId <- "checkGroup_trib_OP_Ley148_ex_parteEmitidas_Región"
    buttonId <- "deselectAll_trib_OP_Ley148_ex_parteEmitidas_Región"
    all_choices <- levels(OP_Ley148_ex_parteEmitidas$Región)
    selected <- input[[inputId]]

    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)

    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  # Colores de los Delitos
  OP_Ley148_ex_parteEmitidas_fill_delito <- setColorFill(OP_Ley148_ex_parteEmitidas, "Delito")
  
  # Grafico de barras
  output$barPlot_OP_Ley148_ex_parteEmitidas <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_trib_OP_Ley148_ex_parteEmitidas_AñoFiscal) > 0
    has_delito <- length(input$checkGroup_trib_OP_Ley148_ex_parteEmitidas_Delito) > 0
    has_region <- length(input$checkGroup_trib_OP_Ley148_ex_parteEmitidas_Región) > 0
    

    # Si faltan selecciones necesarias
    if (!has_año || !has_delito) {
      message <- HTML("Seleccione Delito y Año(s) a visualizar")
      empty_plot <- create_empty_plot_with_message(OP_Ley148_ex_parteEmitidas_filt, x = "AñoFiscal", y = "ÓrdenesEmitidas", fill = "Delito",
                                                   xlab = "Año fiscal", ylab = "Órdenes de Protección Emitidas", message)
      #Titulo de la Grafica
      output$plot_title_OP_Ley148_ex_parteEmitidas <- renderUI({
        title <- "Órdenes de protección ex parte emitidas bajo Ley 148"
      })
      
      return(convert_to_plotly(empty_plot, tooltip = "text"))
    }
    
    # Si NO hay región seleccionada, usar el dataset agregado
    if (!has_region) {
      p <- renderBarPlot_facets(OP_Ley148_ex_parteEmitidas_filt_total, x = "AñoFiscal", y = "ÓrdenesEmitidas", fill = "Delito",
                                xlab = "Año fiscal", ylab = "Órdenes de Protección Emitidas", fillLab = "Delito Cometido",
                                colorFill = OP_Ley148_ex_parteEmitidas_fill_delito,
                                emptyMessage = HTML("Seleccione Delito(s), Región Judicial \n y Año(s) a visualizar"))
      #Titulo de la Grafica
      output$plot_title_OP_Ley148_ex_parteEmitidas <- renderUI({
        title <- "Total de órdenes de protección ex parte emitidas bajo Ley 148, según delito cometido"
      })
      
      OP_Ley148_ex_parteEmitidas_filt_rename <- reactive({
        OP_Ley148_ex_parteEmitidas_filt_total() %>%  
          rename(`Año Fiscal` = AñoFiscal)  %>% 
          rename(`Órdenes Emitidas` = ÓrdenesEmitidas) %>% 
          rename(`Delito Cometido` = Delito)
      })
      
      
      # Data Table para dcrCasosInv
      # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
      output$dataTable_OP_Ley148_ex_parteEmitidas <- renderDT(server = FALSE, {
        renderDataTable(OP_Ley148_ex_parteEmitidas_filt_rename(), "Datos: Órdenes de protección ex parte emitidas bajo Ley 148")
      })
      
      return(convert_to_plotly(p, tooltip = "text")%>% layout(height = 450))
    }
    
    #Titulo de la Grafica
    output$plot_title_OP_Ley148_ex_parteEmitidas <- renderUI({
      title <- "Órdenes de protección ex parte emitidas bajo Ley 148, según región judicial y delito cometido"
    })
    
    p <- renderBarPlot_facets(OP_Ley148_ex_parteEmitidas_filt, x = "AñoFiscal", y = "ÓrdenesEmitidas", fill = "Delito",
                              xlab = "Año fiscal", ylab = "Órdenes de Protección Emitidas", fillLab = "Delito Cometido",
                              colorFill = OP_Ley148_ex_parteEmitidas_fill_delito,
                              emptyMessage = HTML("Seleccione Delito(s), Región Judicial \n y Año(s) a visualizar"))
    #Altura predeterminada para la grafica.
    plot_height = 500
    numPlots = length(input$checkGroup_trib_OP_Ley148_ex_parteEmitidas_Región)
    #Llamado a la funcion calcPlotHeight para calcular la altura basado en el numero de filas.
    total_height = plotHeight(plot_height, numPlots)
    p <- p + facet_wrap(~Región, ncol = 2) +
      theme(panel.spacing.x = unit(0.2, "lines"), #Espacio entre las facetas en x.
            panel.spacing.y = unit(-0.02, "lines")) #Espacio entre las facetas en y.
    p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)
  
    
    OP_Ley148_ex_parteEmitidas_filt_rename <- reactive({
      OP_Ley148_ex_parteEmitidas_filt() %>%
        rename(`Año Fiscal` = AñoFiscal)  %>%
        rename(`Órdenes Emitidas` = ÓrdenesEmitidas) %>%
        rename(`Región Judicial` = Región) %>%
        rename(`Delito Cometido` = Delito)
    })
    
    
    # Data Table para dcrCasosInv
    # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
    output$dataTable_OP_Ley148_ex_parteEmitidas <- renderDT(server = FALSE, {
      renderDataTable(OP_Ley148_ex_parteEmitidas_filt_rename(), "Datos: Órdenes de protección ex parte emitidas bajo Ley 148")
    })
    
    
    return(p)
  })
  
  
  # Texto explicativo dinámico
  output$texto_exparteEmitidas <- renderUI({
    regiones <- input$checkGroup_trib_OP_Ley148_ex_parteEmitidas_Región
    if (is.null(regiones) || length(regiones) == 0) {
      HTML(
        "<p style='font-size: 16px;padding: 0px;'>
        Los datos representados en esta gráfica corresponden a
        las órdenes de protección ex parte emitidas bajo la Ley 148
        según delito cometido y año fiscal para todas las regiones
        judiciales de Puerto Rico.
        Los datos para cada año fiscal se identifican con el
        año en que finaliza el mismo, por ejemplo, para los datos
        del año fiscal 2020-2021, los datos son presentados como 
        año fiscal 2021. Los datos del 2025 son preliminares.
      </p>"
      )
    } else {
      HTML(
        "<p style='font-size: 16px;padding: 0px;'>
        Los datos representados en esta gráfica corresponden a
        las órdenes de protección ex parte emitidas bajo la Ley 148
        según delito cometido, region judicial y año fiscal.
        Los datos para cada año fiscal se identifican con el
        año en que finaliza el mismo, por ejemplo, para los datos
        del año fiscal 2020-2021, los datos son presentados como 
        año fiscal 2021. Los datos del 2025 son preliminares.
      </p>"
      )
    }
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
  
    
  #### (OP_LEY148Archivadas Responsive) ####
  
  # Filtrar el conjunto de datos según los valores seleccionados del año fiscal, la razón de archivado y la región fiscal
  OP_LEY148Archivadas_filt <- reactive({
    filter(OP_LEY148Archivadas,
           AñoFiscal %in% input$checkGroup_trib_OP_LEY148Archivadas_AñoFiscal,
           Razón %in% input$checkGroup_trib_OP_LEY148Archivadas_Razón,
           Región %in% input$checkGroup_trib_OP_LEY148Archivadas_Región
    )
  })
  
  OP_LEY148Archivadas_total <- OP_LEY148Archivadas %>%
    group_by(AñoFiscal, Razón) %>%
    summarise(ÓrdenesArchivadas = sum(ÓrdenesArchivadas, na.rm = TRUE)) %>%
    ungroup()
  
  OP_LEY148Archivadas_filt_total <- reactive({
    filter(OP_LEY148Archivadas_total,
           AñoFiscal %in% input$checkGroup_trib_OP_LEY148Archivadas_AñoFiscal,
           Razón %in% input$checkGroup_trib_OP_LEY148Archivadas_Razón
    )
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el año fiscal
  observeEvent(input$deselectAll_trib_OP_LEY148Archivadas_AñoFiscal, {
    updateCheckboxGroup_trib(session, "checkGroup_trib_OP_LEY148Archivadas_AñoFiscal", input, OP_LEY148Archivadas$AñoFiscal)
  })
  
  observe({
    inputId <- "checkGroup_trib_OP_LEY148Archivadas_AñoFiscal"
    buttonId <- "deselectAll_trib_OP_LEY148Archivadas_AñoFiscal"
    all_choices <- levels(OP_LEY148Archivadas$AñoFiscal)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el botón de deseleccionar/seleccionar la razón de archivado
  observeEvent(input$deselectAll_trib_OP_LEY148Archivadas_Razón, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_LEY148Archivadas_Razón", input, OP_LEY148Archivadas$Razón)
  })
  
  observe({
    inputId <- "checkGroup_trib_OP_LEY148Archivadas_Razón"
    buttonId <- "deselectAll_trib_OP_LEY148Archivadas_Razón"
    all_choices <- levels(OP_LEY148Archivadas$Razón)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el Región
  observeEvent(input$deselectAll_trib_OP_LEY148Archivadas_Región, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_LEY148Archivadas_Región", input, OP_LEY148Archivadas$Región)
  })
  
  observe({
    inputId <- "checkGroup_trib_OP_LEY148Archivadas_Región"
    buttonId <- "deselectAll_trib_OP_LEY148Archivadas_Región"
    all_choices <- levels(OP_LEY148Archivadas$Región)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  # Colores de las razones
  OP_LEY148Archivadas_fill_Razón <- setColorFill(OP_LEY148Archivadas, "Razón")
  
  # Grafico de Barras
  output$barPlot_OP_LEY148Archivadas <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_trib_OP_LEY148Archivadas_AñoFiscal) > 0
    has_razon <- length(input$checkGroup_trib_OP_LEY148Archivadas_Razón) > 0
    has_region <- length(input$checkGroup_trib_OP_LEY148Archivadas_Región) > 0
    
    # Si faltan selecciones necesarias
    if (!has_año || !has_razon) {
      message <- HTML("Seleccione Razón y Año(s) a visualizar")
      empty_plot <- create_empty_plot_with_message(OP_LEY148Archivadas_filt, x = "AñoFiscal", y = "ÓrdenesArchivadas", fill = "Razón",
                                                   xlab = "Año fiscal", ylab = "Órdenes de Protección Archivadas", message)
      #Titulo de la Grafica
      output$plot_title_OP_LEY148Archivadas <- renderUI({
        title <- "Órdenes de protección ex parte archivadas bajo Ley 148"
      })
      
      return(convert_to_plotly(empty_plot, tooltip = "text"))
    }
    
    # Si NO hay región seleccionada, usar el dataset agregado
    if (!has_region) {
      p <- renderBarPlot_facets(OP_LEY148Archivadas_filt_total, x = "AñoFiscal", y = "ÓrdenesArchivadas", fill = "Razón",
                           xlab = "Año Fiscal", ylab = "Órdenes de Protección Archivadas", fillLab = "Razón de Archivo",
                           colorFill = OP_LEY148Archivadas_fill_Razón,
                           emptyMessage = HTML("Seleccione Razón y Año(s) a visualizar"))
      
      #Titulo de la Grafica
      output$plot_title_OP_LEY148Archivadas <- renderUI({
        title <- "Total de órdenes de protección ex parte archivadas bajo Ley 148"
      })
      
      OP_LEY148Archivadas_filt_rename <- reactive({
        OP_LEY148Archivadas_filt_total() %>% 
          rename(`Año Fiscal` = AñoFiscal)  %>% 
          rename(`Órdenes Archivadas` = ÓrdenesArchivadas)%>% 
          rename(`Razón de Archivo` = Razón)
      })
      
      # Data Table 
      # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
      output$dataTable_OP_LEY148Archivadas <- renderDT(server = FALSE, {
        renderDataTable(OP_LEY148Archivadas_filt_rename(), "Datos: Órdenes de protección ex parte archivadas bajo Ley 148")
      })
      
      return(convert_to_plotly(p, tooltip = "text")%>% layout(height = 450))
    }
    
    #Titulo de la Grafica
    output$plot_title_OP_LEY148Archivadas <- renderUI({
      title <- "Órdenes de protección ex parte archivadas bajo Ley 148 según región judicial"
    })
    
    # Si hay selección en todos los filtros, usar dataset completo con facetas
    p <- p <- renderBarPlot_facets(OP_LEY148Archivadas_filt, x = "AñoFiscal", y = "ÓrdenesArchivadas", fill = "Razón",
                                   xlab = "Año Fiscal", ylab = "Órdenes de Protección Archivadas", fillLab = "Razón de Archivo",
                                   colorFill = OP_LEY148Archivadas_fill_Razón,
                                   emptyMessage = HTML("Seleccione Razón y Año(s) a visualizar"))
    #Altura predeterminada para la grafica.
    plot_height = 500
    numPlots = length(input$checkGroup_trib_OP_LEY148Archivadas_Región)
    #Llamado a la funcion calcPlotHeight para calcular la altura basado en el numero de filas.
    total_height = plotHeight(plot_height, numPlots)
    p <- p + facet_wrap(~Región, ncol = 2) +
      theme(panel.spacing.x = unit(0.2, "lines"), #Espacio entre las facetas en x.
            panel.spacing.y = unit(-0.02, "lines")) #Espacio entre las facetas en y.
    p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)
    
    OP_LEY148Archivadas_filt_rename <- reactive({
      OP_LEY148Archivadas_filt() %>% 
        rename(`Año Fiscal` = AñoFiscal)  %>% 
        rename(`Distrito Fiscal` = Región) %>% 
        rename(`Órdenes Archivadas` = ÓrdenesArchivadas)%>% 
        rename(`Razón de Archivo` = Razón)
    })
    
    # Data Table 
    # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
    output$dataTable_OP_LEY148Archivadas <- renderDT(server = FALSE, {
      renderDataTable(OP_LEY148Archivadas_filt_rename(), "Datos: Órdenes de protección ex parte archivadas bajo Ley 148")
    })

    return(p)
  })
  
  
  # Texto explicativo dinámico
  output$texto_exparteArchivadas <- renderUI({
    regiones <- input$checkGroup_trib_OP_LEY148Archivadas_Región
    if (is.null(regiones) || length(regiones) == 0) {
      HTML(
        "<p style='font-size: 16px;padding: 0px;'>
        Los datos representados en esta gráfica corresponden a
        las órdenes de protección ex parte bajo la Ley 148 archivadas
        por razón y año fiscal para todas las regiones judiciales
        a nivel de Puerto Rico. Los datos para cada año fiscal se
        identifican con el año en que finaliza el mismo, por 
        ejemplo, para los datos del año fiscal 2020-2021, los datos
        son presentados como año fiscal 2021. Los datos del 2025 son preliminares.
      </p>"
      )
    } else {
      HTML(
        "<p style='font-size: 16px;padding: 0px;'>
        Los datos representados en esta gráfica corresponden a
        las órdenes de protección ex parte bajo la Ley 148 archivadas
        por razón, región judicial y año fiscal.
        Los datos para cada año fiscal se identifican con el
        año en que finaliza el mismo, por ejemplo, para los datos
        del año fiscal 2020-2021, los datos son presentados como 
        año fiscal 2021. Los datos del 2025 son preliminares.
      </p>"
      )
    }
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
  
    
  #### (OP_LEY148Denegadas Responsive) ####
  
  # Filtrar el conjunto de datos según los valores seleccionados del año fiscal, la razón de archivado y la región fiscal
  OP_LEY148Denegadas_filt <- reactive({
    filter(OP_LEY148Denegadas,
           AñoFiscal %in% input$checkGroup_trib_OP_LEY148Denegadas_AñoFiscal,
           Razón %in% input$checkGroup_trib_OP_LEY148Denegadas_Razón,
           Región %in% input$checkGroup_trib_OP_LEY148Denegadas_Región
    )
  })
  
  OP_LEY148Denegadas_total <- OP_LEY148Denegadas %>%
    group_by(AñoFiscal, Razón) %>%
    summarise(ÓrdenesDenegadas = sum(ÓrdenesDenegadas, na.rm = TRUE)) %>%
    ungroup()
  
  OP_LEY148Denegadas_filt_total <- reactive({
    filter(OP_LEY148Denegadas_total,
           AñoFiscal %in% input$checkGroup_trib_OP_LEY148Denegadas_AñoFiscal,
           Razón %in% input$checkGroup_trib_OP_LEY148Denegadas_Razón
    )
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el año fiscal
  observeEvent(input$deselectAll_trib_OP_LEY148Denegadas_AñoFiscal, {
    updateCheckboxGroup_trib(session, "checkGroup_trib_OP_LEY148Denegadas_AñoFiscal", input, OP_LEY148Denegadas$AñoFiscal)
  })
  
  observe({
    inputId <- "checkGroup_trib_OP_LEY148Denegadas_AñoFiscal"
    buttonId <- "deselectAll_trib_OP_LEY148Denegadas_AñoFiscal"
    all_choices <- levels(OP_LEY148Denegadas$AñoFiscal)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el botón de deseleccionar/seleccionar la razón de archivado
  observeEvent(input$deselectAll_trib_OP_LEY148Denegadas_Razón, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_LEY148Denegadas_Razón", input, OP_LEY148Denegadas$Razón)
  })
  
  observe({
    inputId <- "checkGroup_trib_OP_LEY148Denegadas_Razón"
    buttonId <- "deselectAll_trib_OP_LEY148Denegadas_Razón"
    all_choices <- levels(OP_LEY148Denegadas$Razón)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el Región
  observeEvent(input$deselectAll_trib_OP_LEY148Denegadas_Región, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_LEY148Denegadas_Región", input, OP_LEY148Denegadas$Región)
  })
  
  observe({
    inputId <- "checkGroup_trib_OP_LEY148Denegadas_Región"
    buttonId <- "deselectAll_trib_OP_LEY148Denegadas_Región"
    all_choices <- levels(OP_LEY148Denegadas$Región)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  # Colores de las razones
  OP_LEY148Denegadas_fill_Razón <- setColorFill(OP_LEY148Denegadas, "Razón")
  
  
  # Grafico de Barras
  output$barPlot_OP_LEY148Denegadas <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_trib_OP_LEY148Denegadas_AñoFiscal) > 0
    has_razon <- length(input$checkGroup_trib_OP_LEY148Denegadas_Razón) > 0
    has_region <- length(input$checkGroup_trib_OP_LEY148Denegadas_Región) > 0
    
    # Si faltan selecciones necesarias
    if (!has_año || !has_razon) {
      message <- HTML("Seleccione Razón y Año(s) a visualizar")
      empty_plot <- create_empty_plot_with_message(OP_LEY148Denegadas_filt, x = "AñoFiscal", y = "ÓrdenesDenegadas", fill = "Razón",
                                                   xlab = "Año fiscal", ylab = "Órdenes de Protección Denegadas", message)
      #Titulo de la Grafica
      output$plot_title_OP_LEY148Denegadas <- renderUI({
        title <- "Órdenes de protección denegadas bajo Ley 148 por razón de denegación"
      })
      
      return(convert_to_plotly(empty_plot, tooltip = "text"))
    }
    
    # Si NO hay región seleccionada, usar el dataset agregado
    if (!has_region) {
      p <- renderBarPlot_facets(OP_LEY148Denegadas_filt_total, x = "AñoFiscal", y = "ÓrdenesDenegadas", fill = "Razón",
                                xlab = "Año Fiscal", ylab = "Órdenes de Protección Denegadas", fillLab = "Razón de Denegación",
                                colorFill = OP_LEY148Denegadas_fill_Razón,
                                emptyMessage = HTML("Seleccione Razón y Año(s) a visualizar"))
      
      #Titulo de la Grafica
      output$plot_title_OP_LEY148Denegadas <- renderUI({
        title <- "Total de órdenes de protección denegadas bajo Ley 148 por razón de denegación"
      })
      
      OP_LEY148Denegadas_filt_rename <- reactive({
        OP_LEY148Denegadas_filt_total() %>% 
          rename(`Año Fiscal` = AñoFiscal)  %>% 
          rename(`Órdenes Denegadas` = ÓrdenesDenegadas)%>% 
          rename(`Razón de Archivo` = Razón)
      })
      
      
      # Data Table 
      # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
      output$dataTable_OP_LEY148Denegadas <- renderDT(server = FALSE, {
        renderDataTable(OP_LEY148Denegadas_filt_rename(), "Datos: Ordenes de protección denegadas por violencia sexual bajo Ley 148")
      })
      
      return(convert_to_plotly(p, tooltip = "text")%>% layout(height = 450))
    }
    
    #Titulo de la Grafica
    output$plot_title_OP_LEY148Denegadas <- renderUI({
      title <- "Órdenes de protección denegadas bajo Ley 148 por razón de denegación según región judicial"
    })
    
    # Si hay selección en todos los filtros, usar dataset completo con facetas
    p <- renderBarPlot_facets(OP_LEY148Denegadas_filt, x = "AñoFiscal", y = "ÓrdenesDenegadas", fill = "Razón",
                              xlab = "Año Fiscal", ylab = "Órdenes de Protección Denegadas", fillLab = "Razón de Denegación",
                              colorFill = OP_LEY148Denegadas_fill_Razón,
                              emptyMessage = HTML("Seleccione Razón y Año(s) a visualizar"))
  
    #Altura predeterminada para la grafica.
    plot_height = 500
    numPlots = length(input$checkGroup_trib_OP_LEY148Denegadas_Región)
    #Llamado a la funcion calcPlotHeight para calcular la altura basado en el numero de filas.
    total_height = plotHeight(plot_height, numPlots)
    p <- p + facet_wrap(~Región, ncol = 2) +
      theme(panel.spacing.x = unit(0.2, "lines"), #Espacio entre las facetas en x.
            panel.spacing.y = unit(-0.02, "lines")) #Espacio entre las facetas en y.
    p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)
    
    
    OP_LEY148Denegadas_filt_rename <- reactive({
      OP_LEY148Denegadas_filt() %>% 
        rename(`Año Fiscal` = AñoFiscal)  %>% 
        rename(`Región Judicial` = Región) %>% 
        rename(`Órdenes Denegadas` = ÓrdenesDenegadas)%>% 
        rename(`Razón de Archivo` = Razón)
    })
    
    
    # Data Table 
    # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
    output$dataTable_OP_LEY148Denegadas <- renderDT(server = FALSE, {
      renderDataTable(OP_LEY148Denegadas_filt_rename(), "Datos: Ordenes de protección denegadas por violencia sexual bajo Ley 148")
    })
    
    return(p)
  })
  

  # Texto explicativo dinámico
  output$texto_Denegadas <- renderUI({
    regiones <- input$checkGroup_trib_OP_LEY148Denegadas_Región
    if (is.null(regiones) || length(regiones) == 0) {
      HTML(
        "<p style='font-size: 16px;padding: 0px;'>
        Los datos representados en esta gráfica corresponden a
        las órdenes de protección por violencia sexual denegadas
        bajo la Ley 148 por razón de denegación y año fiscal para todas
        las regiones judiciales de Puerto Rico.
        Los datos para cada año fiscal se identifican con el
        año en que finaliza el mismo, por ejemplo, para los datos
        del año fiscal 2020-2021, los datos son presentados como 
        año fiscal 2021. Los datos del 2025 son preliminares.
      </p>"
      )
    } else {
      HTML(
        "<p style='font-size: 16px;padding: 0px;'>
        Los datos representados en esta gráfica corresponden a
        las órdenes de protección por violencia sexual denegadas
        bajo la Ley 148 por razón de denegación, región judicial y año
        fiscal. Los datos para cada año fiscal se identifican con el
        año en que finaliza el mismo, por ejemplo, para los datos
        del año fiscal 2020-2021, los datos son presentados como 
        año fiscal 2021. Los datos del 2025 son preliminares.
      </p>"
      )
    }
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
  
  
    
  #### (OP_LEY148FinalEmitidas Responsive) ####

  # Filtrar el conjunto de datos según los valores seleccionados del año fiscal, el delito cometido y la región fiscal
  OP_LEY148FinalEmitidas_filt <- reactive({
    filter(OP_LEY148FinalEmitidas,
           AñoFiscal %in% input$checkGroup_trib_OP_LEY148FinalEmitidas_AñoFiscal,
           Delito %in% input$checkGroup_trib_OP_LEY148FinalEmitidas_Delito,
           Región %in% input$checkGroup_trib_OP_LEY148FinalEmitidas_Región
    )
  })
  
  OP_LEY148FinalEmitidas_total <- OP_LEY148FinalEmitidas %>%
    group_by(AñoFiscal, Delito) %>%
    summarise(ÓrdenesEmitidas = sum(ÓrdenesEmitidas, na.rm = TRUE)) %>%
    ungroup()
  
  OP_LEY148FinalEmitidas_filt_total <- reactive({
    filter(OP_LEY148FinalEmitidas_total,
           AñoFiscal %in% input$checkGroup_trib_OP_LEY148FinalEmitidas_AñoFiscal,
           Delito %in% input$checkGroup_trib_OP_LEY148FinalEmitidas_Delito,
    )
  })

  ### funcion para el botón de deseleccionar/seleccionar el año fiscal
  observeEvent(input$deselectAll_trib_OP_LEY148FinalEmitidas_AñoFiscal, {
    updateCheckboxGroup_trib(session, "checkGroup_trib_OP_LEY148FinalEmitidas_AñoFiscal", input, OP_LEY148FinalEmitidas$AñoFiscal)
  })

  observe({
    inputId <- "checkGroup_trib_OP_LEY148FinalEmitidas_AñoFiscal"
    buttonId <- "deselectAll_trib_OP_LEY148FinalEmitidas_AñoFiscal"
    all_choices <- levels(OP_LEY148FinalEmitidas$AñoFiscal)
    selected <- input[[inputId]]

    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)

    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })


  ### funcion para el botón de deseleccionar/seleccionar delito cometido
  observeEvent(input$deselectAll_trib_OP_LEY148FinalEmitidas_Delito, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_LEY148FinalEmitidas_Delito", input, OP_LEY148FinalEmitidas$Delito)
  })

  observe({
    inputId <- "checkGroup_trib_OP_LEY148FinalEmitidas_Delito"
    buttonId <- "deselectAll_trib_OP_LEY148FinalEmitidas_Delito"
    all_choices <- levels(OP_LEY148FinalEmitidas$Delito)
    selected <- input[[inputId]]

    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)

    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })

  ### funcion para el botón de deseleccionar/seleccionar el Región
  observeEvent(input$deselectAll_trib_OP_LEY148FinalEmitidas_Región, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_LEY148FinalEmitidas_Región", input, OP_LEY148FinalEmitidas$Región)
  })

  observe({
    inputId <- "checkGroup_trib_OP_LEY148FinalEmitidas_Región"
    buttonId <- "deselectAll_trib_OP_LEY148FinalEmitidas_Región"
    all_choices <- levels(OP_LEY148FinalEmitidas$Región)
    selected <- input[[inputId]]

    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)

    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })

  # Colores de las razones
  OP_LEY148FinalEmitidas_fill_Delito <- setColorFill(OP_LEY148FinalEmitidas, "Delito")

  # Grafico de barras
  output$barPlot_OP_LEY148FinalEmitidas <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_trib_OP_LEY148FinalEmitidas_AñoFiscal) > 0
    has_delito <- length(input$checkGroup_trib_OP_LEY148FinalEmitidas_Delito) > 0
    has_region <- length(input$checkGroup_trib_OP_LEY148FinalEmitidas_Región) > 0
    
    
    # Si faltan selecciones necesarias
    if (!has_año || !has_delito) {
      message <- HTML("Seleccione Delito y Año(s) a visualizar")
      empty_plot <- create_empty_plot_with_message(OP_LEY148FinalEmitidas_filt, x = "AñoFiscal", y = "ÓrdenesEmitidas", fill = "Delito",
                                                   xlab = "Año fiscal", ylab = "Órdenes de Protección Emitidas", message)
      #Titulo de la Grafica
      output$plot_title_OP_LEY148FinalEmitidas <- renderUI({
        title <- "Órdenes de protección emitidas bajo Ley 148"
      })
      
      return(convert_to_plotly(empty_plot, tooltip = "text"))
    }
    
    # Si NO hay región seleccionada, usar el dataset agregado
    if (!has_region) {
      p <- renderBarPlot_facets(OP_LEY148FinalEmitidas_filt_total, x = "AñoFiscal", y = "ÓrdenesEmitidas", fill = "Delito",
                                xlab = "Año Fiscal", ylab = "Órdenes de Protección Emitidas", fillLab = "Delito Cometido",
                                colorFill = OP_LEY148FinalEmitidas_fill_Delito,
                                emptyMessage = HTML("Seleccione Delito(s), Región Judicial \n y Año(s) a visualizar"))
      
      #Titulo de la Grafica
      output$plot_title_OP_LEY148FinalEmitidas <- renderUI({
        title <- "Total de órdenes de protección emitidas bajo Ley 148, según delito cometido"
      })
      
      OP_LEY148FinalEmitidas_filt_rename <- reactive({
        OP_LEY148FinalEmitidas_filt_total() %>%
          rename(`Año Fiscal` = AñoFiscal)  %>%
          rename(`Órdenes Emitidas` = ÓrdenesEmitidas)%>%
          rename(`Delito Cometido` = Delito)
      })
      
      
      # Data Table
      # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
      output$dataTable_OP_LEY148FinalEmitidas <- renderDT(server = FALSE, {
        renderDataTable(OP_LEY148FinalEmitidas_filt_rename(), "Datos: Órdenes de protección emitidas bajo ley 148")
      })
      
      return(convert_to_plotly(p, tooltip = "text")%>% layout(height = 450))
    }
    
    #Titulo de la Grafica
    output$plot_title_OP_LEY148FinalEmitidas <- renderUI({
      title <- "Órdenes de protección emitidas bajo Ley 148, según región judicial y tipo de delito"
    })
    
    p <- renderBarPlot_facets(OP_LEY148FinalEmitidas_filt, x = "AñoFiscal", y = "ÓrdenesEmitidas", fill = "Delito",
                              xlab = "Año Fiscal", ylab = "Órdenes de Protección Emitidas", fillLab = "Delito Cometido",
                              colorFill = OP_LEY148FinalEmitidas_fill_Delito,
                              emptyMessage = HTML("Seleccione Delito(s), Región Judicial \n y Año(s) a visualizar"))
    #Altura predeterminada para la grafica.
    plot_height = 500
    numPlots = length(input$checkGroup_trib_OP_LEY148FinalEmitidas_Región)
    #Llamado a la funcion calcPlotHeight para calcular la altura basado en el numero de filas.
    total_height = plotHeight(plot_height, numPlots)
    p <- p + facet_wrap(~Región, ncol = 2) +
      theme(panel.spacing.x = unit(0.2, "lines"), #Espacio entre las facetas en x.
            panel.spacing.y = unit(-0.02, "lines")) #Espacio entre las facetas en y.
    p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)
    
    
    OP_LEY148FinalEmitidas_filt_rename <- reactive({
      OP_LEY148FinalEmitidas_filt() %>%
        rename(`Año Fiscal` = AñoFiscal)  %>%
        rename(`Región Judicial` = Región) %>%
        rename(`Órdenes Emitidas` = ÓrdenesEmitidas)%>%
        rename(`Delito Cometido` = Delito)
    })
    
    
    # Data Table
    # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
    output$dataTable_OP_LEY148FinalEmitidas <- renderDT(server = FALSE, {
      renderDataTable(OP_LEY148FinalEmitidas_filt_rename(), "Datos: Órdenes de protección emitidas bajo ley 148")
    })

    return(p)
  })
  
  
  # Texto explicativo dinámico
  output$texto_FinalEmitidas <- renderUI({
    regiones <- input$checkGroup_trib_OP_LEY148FinalEmitidas_Región
    if (is.null(regiones) || length(regiones) == 0) {
      HTML(
        "<p style='font-size: 16px;padding: 0px;'>
        Los datos representados en esta gráfica corresponden a
        las órdenes de protección emitidas bajo la Ley 148 por delito
        cometido y año fiscal para todas las regiones judiciales
        de Puerto Rico. Los datos para cada
        año fiscal se identifican con el año en que finaliza
        el mismo, por ejemplo, para los datos del año fiscal 2020-2021,
        los datos son presentados como año fiscal 2021. Los datos del 2025 son preliminares.
      </p>"
      )
    } else {
      HTML(
        "<p style='font-size: 16px;padding: 0px;'>
        Los datos representados en esta gráfica corresponden a
        las órdenes de protección emitidas bajo la Ley 148 por delito
        cometido, región judicial y año fiscal. Los datos para cada
        año fiscal se identifican con el año en que finaliza
        el mismo, por ejemplo, para los datos del año fiscal 2020-2021,
        los datos son presentados como año fiscal 2021. Los datos del 2025 son preliminares.
      </p>"
      )
    }
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
    updateCheckboxGroup_trib(session, "checkGroup_trib_OP_LEY148Genero_AñoFiscal", input, OP_LEY148Genero$AñoFiscal)
  })
  
  observe({
    inputId <- "checkGroup_trib_OP_LEY148Genero_AñoFiscal"
    buttonId <- "deselectAll_trib_OP_LEY148Genero_AñoFiscal"
    all_choices <- levels(OP_LEY148Genero$AñoFiscal)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el botón de deseleccionar/seleccionar la parte peticionaria
  observeEvent(input$deselectAll_trib_OP_LEY148Genero_Parte, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_LEY148Genero_Parte", input, OP_LEY148Genero$Parte)
  })
  
  observe({
    inputId <- "checkGroup_trib_OP_LEY148Genero_Parte"
    buttonId <- "deselectAll_trib_OP_LEY148Genero_Parte"
    all_choices <- levels(OP_LEY148Genero$Parte)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el sexo de la parte
  observeEvent(input$deselectAll_trib_OP_LEY148Genero_Sexo, {
    updateCheckboxGroup(session, "checkGroup_trib_OP_LEY148Genero_Sexo", input, OP_LEY148Genero$Sexo)
  })
  
  observe({
    inputId <- "checkGroup_trib_OP_LEY148Genero_Sexo"
    buttonId <- "deselectAll_trib_OP_LEY148Genero_Sexo"
    all_choices <- levels(OP_LEY148Genero$Sexo)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
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
      message <- HTML("Seleccione Parte(s), Sexo y Año(s) a visualizar")
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot_facets(OP_LEY148Genero_filt, x = "AñoFiscal", y = "Solicitudes", fill = "Parte",
                         xlab = "Año fiscal", ylab = "Solicitudes de Ordenes de Protección", fillLab = "Parte",
                         colorFill = OP_LEY148Genero_fill_Parte,
                         emptyMessage = "Seleccione Parte(s), Sexo y Año(s) a visualizar")
      #Altura predeterminada para la grafica.
      plot_height = 500
      numPlots = length(input$checkGroup_trib_OP_LEY148Genero_Sexo)
      #Llamado a la funcion calcPlotHeight para calcular la altura basado en el numero de filas.
      total_height = plotHeight(plot_height, numPlots)
      p <- p + facet_wrap(~Sexo, ncol = 2) +
        theme(panel.spacing.x = unit(1, "lines"), #Espacio entre las facetas en x.
              panel.spacing.y = unit(1, "lines"),#Espacio entre las facetas en y.
              #plot.margin = margin(t = 150, r = 10, b = 150, l = 10)
              ) 
      p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(OP_LEY148Genero_filt, x = "AñoFiscal", y = "Solicitudes", fill = "Parte",
                                                 xlab = "Año fiscal", ylab = "Solicitudes de Ordenes de Protección", message)
    convert_to_plotly(empty_plot, tooltip = "text")
  })
  
  #Titulo de la Grafica
  output$plot_title_OP_LEY148Genero <- renderUI({
    title <- "Órdenes de protección emitidas bajo Ley 148, según la parte"
  })
  
  OP_LEY148Genero_filt_rename <- reactive({
    OP_LEY148Genero_filt() %>% 
      rename(`Año Fiscal` = AñoFiscal) 
  })
  
  # Data Table
  # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
  output$dataTable_OP_LEY148Genero <- renderDT(server = FALSE, {
    renderDataTable(OP_LEY148Genero_filt_rename(), "Datos: Órdenes de protección emitidas bajo Ley 148")
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
  observeEvent(input$deselectAll_trib_tribCasosCrim_AñoFiscal, {
    updateCheckboxGroup_trib(session, "checkGroup_trib_tribCasosCrim_AñoFiscal", input, tribCasosCrim$AñoFiscal)
  })
  
  observe({
    inputId <- "checkGroup_trib_tribCasosCrim_AñoFiscal"
    buttonId <- "deselectAll_trib_tribCasosCrim_AñoFiscal"
    all_choices <- levels(tribCasosCrim$AñoFiscal)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el botón de deseleccionar/seleccionar la parte peticionaria
  observeEvent(input$deselectAll_trib_tribCasosCrim_Delito, {
    updateCheckboxGroup(session, "checkGroup_trib_tribCasosCrim_Delito", input, tribCasosCrim$Delito)
  })
  
  observe({
    inputId <- "checkGroup_trib_tribCasosCrim_Delito"
    buttonId <- "deselectAll_trib_tribCasosCrim_Delito"
    all_choices <- levels(tribCasosCrim$Delito)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el sexo de la parte
  observeEvent(input$deselectAll_trib_tribCasosCrim_Casos, {
    updateCheckboxGroup(session, "checkGroup_trib_tribCasosCrim_Casos", input, tribCasosCrim$Casos)
  })
  
  observe({
    inputId <- "checkGroup_trib_tribCasosCrim_Casos"
    buttonId <- "deselectAll_trib_tribCasosCrim_Casos"
    all_choices <- levels(tribCasosCrim$Casos)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
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
      message <- HTML("Seleccione Delito(s), Estado del caso \n y Año(s) a visualizar")
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot_facets(tribCasosCrim_filt, x = "AñoFiscal", y = "Cantidad", fill = "Delito",
                                xlab = "Año Fiscal", ylab = "Casos", fillLab = "Delito Cometido",
                                colorFill = tribCasosCrim_fill_Delito,
                                emptyMessage = HTML("Seleccione Delito(s), Estado del caso \n y Año(s) a visualizar"), barWidth = 0, xGap = 0)
      #Altura predeterminada para la grafica.
      plot_height = 500
      numPlots = length(input$checkGroup_trib_tribCasosCrim_Casos)
      #Llamado a la funcion calcPlotHeight para calcular la altura basado en el numero de filas.
      total_height = plotHeight(plot_height, numPlots)
      p <- p + facet_wrap(~Casos, ncol = 2) +
        theme(panel.spacing.x = unit(0.2, "lines"), #Espacio entre las facetas en x.
              panel.spacing.y = unit(-0.05, "lines")) #Espacio entre las facetas en y.
      p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height,
                                                                             legend = list(
                                                                               font = list(size = 8.6) #Tamaño de letra para los elementos de la legenda.
                                                                             ))
      
      return(p)
    }
    
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(tribCasosCrim_filt, x = "AñoFiscal", y = "Cantidad", fill = "Delito",
                                                 xlab = "Año Fiscal", ylab = "Casos", message)
    convert_to_plotly(empty_plot, tooltip = "text")
  })
  
  #Titulo de la Grafica
  output$plot_title_tribCasosCrim <- renderUI({
    title <- "Movimiento anual de casos criminales de violencia doméstica en el Tribunal según Ley 54"
  })
  
  tribCasosCrim_filt_rename <- reactive({
    tribCasosCrim_filt() %>%
      rename(`Año Fiscal` = AñoFiscal) %>%
      rename(`Estado del Caso` = Casos)%>%
      rename(`Delito Cometido` = Delito)
  })
  
  # Data Table
  # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
  output$dataTable_tribCasosCrim <- renderDT(server = FALSE, {
    renderDataTable(tribCasosCrim_filt_rename(), "Datos: Ordenes de protección según delito cometido")
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
  #### Tab de Publicaciones ####
  # PDF 1
  # publicationCardPDFServer("trib_pdf1", "trib/boletinVS2023.pdf")
  # # PDF 2
  # publicationCardPDFServer("trib_pdf2", "trib/reporte2022.pdf")
  # 
  # output$trib_view_content <- renderUI({
  #   if (input$trib_view_option == "Publicaciones") {
  #     tagList(
  #       tags$ul(
  #         style = "list-style-type: none; text-align: center;",
  #         sectionTitle("Publicaciones y Recursos", "24px")
  #       ),
  #       fluidRow(
  #         column(12, publicationCardPDF("trib_pdf1", "Boletín Estadístico de Violencia Sexual, Año 2023", "trib/boletinVS2023.pdf"))
  #       )
  #       # ,
  #       # fluidRow(
  #       #   column(12, publicationCardPDF("trib_pdf2", "Reporte Estadístico 2022", "trib/reporte2022.pdf"))
  #       # )
  #     )
  #   } else {
  #     # Dashboard de la Agencia (embed link)
  #     tags$div(
  #       fluidRow(
  #         column(12, publicationCardWeb("Dashboard de la Agencia:",
  #                                       "https://safekits.pr.gov/?fbclid=IwAR3ooo0EgJS_8MEVB3Z5IettOyplWfi3sYQU18AZlC5yieAIMhIYOuHjHZs"))
  #       )
  #     )
  #   }
  # })
  #### Tab de Definiciones ####
  definitions_trib <- list(
    list(word = "Año Fiscal", definition = "Período de 12 meses comprendido entre el 1ro de julio de un año y el 30
         de junio del año siguiente, y que se usa como el calendario presupuestario de las agencias públicas. Los datos 
         para cada año fiscal se identifican con el año en que finaliza el mismo, por ejemplo, para los datos del año 
         fiscal 2020-2021, los datos son presentados como año fiscal 2021."),
    list(word = "Ley 148", definition = "Conocida como la “Ley para la Protección de las Víctimas de Violencia Sexual
         en Puerto Rico”, según enmendada, establece los mecanismos para la expedición de órdenes de protección para
         víctimas de los delitos de agresión sexual, actos lascivos, acoso sexual e incesto."),
    list(word = "Ley 54", definition = HTML("La Ley Núm. 54-1989, conocida como la “Ley para la Prevención e Intervención
         con la Violencia Doméstica”, según enmendada, establece la violencia doméstica como delito y lo define como
         el empleo de fuerza física o violencia psicológica, intimidación o persecución en contra de su pareja o expareja.
         Esto, para causarle daño físico a su persona, a sus bienes o a otra persona o para causarle grave daño emocional.
         Para leer y descargar la Ley 54 en su totalidad, puede utilizar el siguiente enlace: 
         <a href='https://www.justicia.pr.gov/wp-content/uploads/2021/07/Ley-para-la-Prevenci%C3%B3n-e-Intervenci%C3%B3n-con-la-Violencia-Dom%C3%A9stica.pdf'
                                            target='_blank' style='color: #884e9f;'>Descargar Ley 54</a>."
           )
         ),
    list(word = "Organización", definition = "Poder Judicial de Puerto Rico, Oficina de Administración de los Tribunales,
         Directoria de Operaciones, Oficina de Estadísticas."),
    list(word = "Orden de Protección", definition = "Es un remedio civil expedido por escrito bajo el sello de un Tribunal,
         en la cual se dictan las medidas a una persona agresora para que ésta se abstenga de incurrir o llevar a cabo 
         determinados actos o conducta constitutivos de violencia doméstica."),
    list(word = "Órdenes de protección ex parte", definition = "Es una orden emitida por el Tribunal de Primera Instancia 
         luego de escuchar a la parte peticionaria (persona que solicita la orden) y hacer una determinación provisional 
         sobre los hechos."),
    list(word = "Peticionaria", definition = "Persona que solicita una orden de protección."),
    list(word = "Región Judicial", definition = "Se refiere a la región judicial a la que corresponden los datos informados.
         El Tribunal de Primera Instancia se distribuye territorialmente en trece regiones judiciales. Cada región judicial
         está compuesta por un centro judicial y sus respectivas salas superiores y municipales."),
    list(word = "Sexo", definition = "Indica si la persona que solicita la orden de protección, en el periodo de tiempo de
         interés en la región judicial especificada, se identifica como hombre o mujer."),
    list(word = "Solicitudes de órdenes de protección", definition = "Se define como todas las peticiones de orden de protección
         realizadas en el periodo de tiempo de interés en la región judicial especificada."),
    list(word = "Violencia Sexual", definition = "Cualquier acto que degrada o daña el cuerpo y/o la sexualidad de la víctima
         y que por tanto atenta contra su libertad, dignidad e integridad física. Es una expresión de abuso de poder que implica
         la supremacía masculina sobre la mujer, al denigrar y concebirla como objeto.")
  )
  
  # Convertir lista a dataframe
  definitions_df_trib <- do.call(rbind, lapply(definitions_trib, as.data.frame))
  
  # Renombrar columnas
  colnames(definitions_df_trib) <- c("Concepto", "Definición")
  
  # Usar funcion para presentar tabla con definiciones
  output$dataTable_Def_trib <- renderDT({
    renderDataTable_Definitions(definitions_df_trib, "Administración de Tribunales")
  })
  
    
  
  
  #### Codigo Muerto en Administracion de Tribunales ####
  # #### tab con datos de Ley 148 - Violencia Sexual por grupo de edad (OP_148_SoliGrupEdad regiones) ####
  # 
  # # Filtrar el conjunto de datos según los valores seleccionados del año fiscal, el grupo de edad y el distrito fiscal
  # OP_148_SoliGrupEdad_filt <- reactive({
  #   filter(OP_148_SoliGrupEdad,
  #          AñoFiscal %in% input$checkGroup_trib_OP_148_SoliGrupEdad_AñoFiscal,
  #          Edad %in% input$checkGroup_trib_OP_148_SoliGrupEdad_Edad,
  #          Región %in% input$checkGroup_trib_OP_148_SoliGrupEdad_Región
  #   )
  # })
  # 
  # ### funcion para el botón de deseleccionar/seleccionar el año fiscal
  # observeEvent(input$deselectAll_trib_OP_148_SoliGrupEdad_AñoFiscal, {
  #   updateCheckboxGroup_trib(session, "checkGroup_trib_OP_148_SoliGrupEdad_AñoFiscal", input, OP_148_SoliGrupEdad$AñoFiscal)
  # })
  # 
  # observe({
  #   inputId <- "checkGroup_trib_OP_148_SoliGrupEdad_AñoFiscal"
  #   buttonId <- "deselectAll_trib_OP_148_SoliGrupEdad_AñoFiscal"
  #   all_choices <- levels(OP_148_SoliGrupEdad$AñoFiscal)
  #   selected <- input[[inputId]]
  #   
  #   is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
  #   
  #   updateActionButton(
  #     session,
  #     inputId = buttonId,
  #     label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
  #   )
  # })
  # 
  # 
  # ### funcion para el botón de deseleccionar/seleccionar el grupo de edad
  # observeEvent(input$deselectAll_trib_OP_148_SoliGrupEdad_Edad, {
  #   updateCheckboxGroup(session, "checkGroup_trib_OP_148_SoliGrupEdad_Edad", input, OP_148_SoliGrupEdad$Edad)
  # })
  # 
  # observe({
  #   inputId <- "checkGroup_trib_OP_148_SoliGrupEdad_Edad"
  #   buttonId <- "deselectAll_trib_OP_148_SoliGrupEdad_Edad"
  #   all_choices <- levels(OP_148_SoliGrupEdad$Edad)
  #   selected <- input[[inputId]]
  #   
  #   is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
  #   
  #   updateActionButton(
  #     session,
  #     inputId = buttonId,
  #     label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
  #   )
  # })
  # 
  # ### funcion para el botón de deseleccionar/seleccionar la región fiscal
  # observeEvent(input$deselectAll_trib_OP_148_SoliGrupEdad_Región, {
  #   updateCheckboxGroup(session, "checkGroup_trib_OP_148_SoliGrupEdad_Región", input, OP_148_SoliGrupEdad$Región)
  # })
  # 
  # observe({
  #   inputId <- "checkGroup_trib_OP_148_SoliGrupEdad_Región"
  #   buttonId <- "deselectAll_trib_OP_148_SoliGrupEdad_Región"
  #   all_choices <- levels(OP_148_SoliGrupEdad$Región)
  #   selected <- input[[inputId]]
  #   
  #   is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
  #   
  #   updateActionButton(
  #     session,
  #     inputId = buttonId,
  #     label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
  #   )
  # })
  # 
  # # Colores del status
  # OP_148_SoliGrupEdad_fill_edad <- setColorFill(OP_148_SoliGrupEdad, "Edad")
  # 
  # # Grafico de barras
  # output$barPlot_OP_148_SoliGrupEdad <- renderPlotly({
  #   # Verificar si hay opciones seleccionadas en cada grupo
  #   has_año <- length(input$checkGroup_trib_OP_148_SoliGrupEdad_AñoFiscal) > 0
  #   has_edad <- length(input$checkGroup_trib_OP_148_SoliGrupEdad_Edad) > 0
  #   has_region <- length(input$checkGroup_trib_OP_148_SoliGrupEdad_Región) > 0
  # 
  #   # Crear mensaje si faltan opciones seleccionadas
  #   if (!has_año || !has_edad || !has_region) {
  #     message <- HTML("Seleccione Grupo(s) de Edad, Región Judicial \n y Año(s) a visualizar")
  #   } else {
  #     # Si todas las opciones están seleccionadas, crear la gráfica
  #     p <- renderBarPlot_facets(OP_148_SoliGrupEdad_filt, x = "AñoFiscal", y = "Solicitudes", fill = "Edad",
  #                        title = "Solicitudes de órdenes de protección \nbajo Ley 148 según región judicial y edad",
  #                        xlab = "Año Fiscal", ylab = "Órdenes de Protección Solicitadas", fillLab = "Grupo de Edad",
  #                        colorFill = OP_148_SoliGrupEdad_fill_edad,
  #                        emptyMessage = HTML("Seleccione Grupo(s) de Edad, Región Judicial \n y Año(s) a visualizar"))
  #     #Altura predeterminada para la grafica.
  #     plot_height = 500
  #     numPlots = length(input$checkGroup_trib_OP_148_SoliGrupEdad_Región)
  #     #Llamado a la funcion calcPlotHeight para calcular la altura basado en el numero de filas.
  #     total_height = plotHeight(plot_height, numPlots)
  #     p <- p + facet_wrap(~Región, ncol = 2) +
  #       theme(panel.spacing.x = unit(0.2, "lines"), #Espacio entre las facetas en x.
  #             panel.spacing.y = unit(-0.02, "lines")) #Espacio entre las facetas en y.
  #     p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)
  # 
  #     return(p)
  #   }
  # 
  #   # Crear la gráfica vacía con mensaje
  #   empty_plot <- create_empty_plot_with_message(data = OP_148_SoliGrupEdad_filt, x = "AñoFiscal", y = "Solicitudes", fill = "Edad",
  #                                                xlab = "Año Fiscal", ylab = "Órdenes de Protección Solicitadas", message)
  #   convert_to_plotly(empty_plot, tooltip = "text")
  # })
  # 
  # #Titulo de la Grafica
  # output$plot_title_OP_148_SoliGrupEdad <- renderUI({
  #   title <- "Solicitudes de órdenes de protección bajo Ley 148 según región judicial y edad"
  # })
  # 
  # 
  # OP_148_SoliGrupEdad_filt_rename <- reactive({
  #   OP_148_SoliGrupEdad_filt() %>%
  #     rename(`Año Fiscal` = AñoFiscal)  %>%
  #     rename(`Región Judicial` = Región)
  # })
  # 
  # 
  # # Data Table para dcrCasosInv
  # # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
  # output$dataTable_OP_148_SoliGrupEdad <- renderDT(server = FALSE, {
  #   renderDataTable(OP_148_SoliGrupEdad_filt_rename(), "Datos: Órdenes de protección solicitadas por violencia sexual")
  # })
  # 
  # # Crear Card con Fuentes
  # output$dataTableUI_OP_148_SoliGrupEdad  <- renderUI({
  #   if (input$showTable_OP_148_SoliGrupEdad) {
  #     hyperlinks <- c("https://poderjudicial.pr/mision-y-vision-de-la-rama-judicial/")
  #     texts <- c("Oficina de Administración de los Tribunales, Directoría de Operaciones, Oficina de Estadísticas")
  #     
  #     tags$div(
  #       class = "card",
  #       style = "padding: 10px; width: 98%; margin-top: 10px; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",
  #      
  #       # Contenedor centrado para la tabla
  #       div(
  #         style = "padding: 5px; width: 98%; display: flex; justify-content: center;",  
  #         div(
  #           style = "width: 98%; max-width: 800px; overflow-x: auto;",  
  #           DTOutput("dataTable_OP_148_SoliGrupEdad")
  #         )
  #       ),
  #       
  #       createFuenteDiv(hyperlinks, texts)
  #     )
  #   }
  # })
  
  # #### (OP_Ley148_ex_parteEmitidas regiones) ####
  # 
  # # Filtrar el conjunto de datos según los valores seleccionados del año fiscal, el delito cometido y la región fiscal
  # OP_Ley148_ex_parteEmitidas_filt <- reactive({
  #   filter(OP_Ley148_ex_parteEmitidas,
  #          AñoFiscal %in% input$checkGroup_trib_OP_Ley148_ex_parteEmitidas_AñoFiscal,
  #          Delito %in% input$checkGroup_trib_OP_Ley148_ex_parteEmitidas_Delito,
  #          Región %in% input$checkGroup_trib_OP_Ley148_ex_parteEmitidas_Región
  #   )
  # })
  # 
  # ### funcion para el botón de deseleccionar/seleccionar el año fiscal
  # observeEvent(input$deselectAll_trib_OP_Ley148_ex_parteEmitidas_AñoFiscal, {
  #   updateCheckboxGroup_trib(session, "checkGroup_trib_OP_Ley148_ex_parteEmitidas_AñoFiscal", input, OP_Ley148_ex_parteEmitidas$AñoFiscal)
  # })
  # 
  # observe({
  #   inputId <- "checkGroup_trib_OP_Ley148_ex_parteEmitidas_AñoFiscal"
  #   buttonId <- "deselectAll_trib_OP_Ley148_ex_parteEmitidas_AñoFiscal"
  #   all_choices <- levels(OP_Ley148_ex_parteEmitidas$AñoFiscal)
  #   selected <- input[[inputId]]
  #   
  #   is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
  #   
  #   updateActionButton(
  #     session,
  #     inputId = buttonId,
  #     label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
  #   )
  # })
  # 
  # 
  # ### funcion para el botón de deseleccionar/seleccionar el delito
  # observeEvent(input$deselectAll_trib_OP_Ley148_ex_parteEmitidas_Delito, {
  #   updateCheckboxGroup(session, "checkGroup_trib_OP_Ley148_ex_parteEmitidas_Delito", input, OP_Ley148_ex_parteEmitidas$Delito)
  # })
  # 
  # observe({
  #   inputId <- "checkGroup_trib_OP_Ley148_ex_parteEmitidas_Delito"
  #   buttonId <- "deselectAll_trib_OP_Ley148_ex_parteEmitidas_Delito"
  #   all_choices <- levels(OP_Ley148_ex_parteEmitidas$Delito)
  #   selected <- input[[inputId]]
  #   
  #   is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
  #   
  #   updateActionButton(
  #     session,
  #     inputId = buttonId,
  #     label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
  #   )
  # })
  # 
  # ### funcion para el botón de deseleccionar/seleccionar el Región
  # observeEvent(input$deselectAll_trib_OP_Ley148_ex_parteEmitidas_Región, {
  #   updateCheckboxGroup(session, "checkGroup_trib_OP_Ley148_ex_parteEmitidas_Región", input, OP_Ley148_ex_parteEmitidas$Región)
  # })
  # 
  # observe({
  #   inputId <- "checkGroup_trib_OP_Ley148_ex_parteEmitidas_Región"
  #   buttonId <- "deselectAll_trib_OP_Ley148_ex_parteEmitidas_Región"
  #   all_choices <- levels(OP_Ley148_ex_parteEmitidas$Región)
  #   selected <- input[[inputId]]
  #   
  #   is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
  #   
  #   updateActionButton(
  #     session,
  #     inputId = buttonId,
  #     label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
  #   )
  # })
  # 
  # # Colores de los Delitos
  # OP_Ley148_ex_parteEmitidas_fill_delito <- setColorFill(OP_Ley148_ex_parteEmitidas, "Delito")
  # 
  # # Grafico de barras
  # output$barPlot_OP_Ley148_ex_parteEmitidas <- renderPlotly({
  #   # Verificar si hay opciones seleccionadas en cada grupo
  #   has_año <- length(input$checkGroup_trib_OP_Ley148_ex_parteEmitidas_AñoFiscal) > 0
  #   has_delito <- length(input$checkGroup_trib_OP_Ley148_ex_parteEmitidas_Delito) > 0
  #   has_region <- length(input$checkGroup_trib_OP_Ley148_ex_parteEmitidas_Región) > 0
  #   
  #   # Crear mensaje si faltan opciones seleccionadas
  #   if (!has_año || !has_delito || !has_region) {
  #     message <- HTML("Seleccione Delito(s), Región Judicial \n y Año(s) a visualizar")
  #   } else {
  #     # Si todas las opciones están seleccionadas, crear la gráfica
      # p <- renderBarPlot_facets(OP_Ley148_ex_parteEmitidas_filt, x = "AñoFiscal", y = "ÓrdenesEmitidas", fill = "Delito",
      #                    title = "Órdenes de protección ex parte emitidas \nbajo Ley 148, según región judicial y delito cometido",
      #                    xlab = "Año fiscal", ylab = "Órdenes de Protección Emitidas", fillLab = "Delito Cometido",
      #                    colorFill = OP_Ley148_ex_parteEmitidas_fill_delito,
      #                    emptyMessage = HTML("Seleccione Delito(s), Región Judicial \n y Año(s) a visualizar"))
      # #Altura predeterminada para la grafica.
      # plot_height = 500
      # numPlots = length(input$checkGroup_trib_OP_Ley148_ex_parteEmitidas_Región)
      # #Llamado a la funcion calcPlotHeight para calcular la altura basado en el numero de filas.
      # total_height = plotHeight(plot_height, numPlots)
      # p <- p + facet_wrap(~Región, ncol = 2) +
      #   theme(panel.spacing.x = unit(0.2, "lines"), #Espacio entre las facetas en x.
      #         panel.spacing.y = unit(-0.02, "lines")) #Espacio entre las facetas en y.
      # p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)
      # 
      # return(p)
  #   }
  #   
  #   # Crear la gráfica vacía con mensaje
  #   empty_plot <- create_empty_plot_with_message(OP_Ley148_ex_parteEmitidas_filt, x = "AñoFiscal", y = "ÓrdenesEmitidas", fill = "Delito",
  #                                                xlab = "Año fiscal", ylab = "Órdenes de Protección Emitidas", message)
  #   convert_to_plotly(empty_plot, tooltip = "text")
  # })
  # 
  # #Titulo de la Grafica
  # output$plot_title_OP_Ley148_ex_parteEmitidas <- renderUI({
  #   title <- "Órdenes de protección ex parte emitidas bajo Ley 148, según región judicial y delito cometido"
  # })
  # 
  # OP_Ley148_ex_parteEmitidas_filt_rename <- reactive({
  #   OP_Ley148_ex_parteEmitidas_filt() %>%
  #     rename(`Año Fiscal` = AñoFiscal)  %>%
  #     rename(`Órdenes Emitidas` = ÓrdenesEmitidas) %>%
  #     rename(`Región Judicial` = Región) %>%
  #     rename(`Delito Cometido` = Delito)
  # })
  # 
  # 
  # # Data Table para dcrCasosInv
  # # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
  # output$dataTable_OP_Ley148_ex_parteEmitidas <- renderDT(server = FALSE, {
  #   renderDataTable(OP_Ley148_ex_parteEmitidas_filt_rename(), "Datos: Órdenes de protección ex parte emitidas bajo Ley 148")
  # })

  # # Crear Card con Fuentes
  # output$dataTableUI_OP_Ley148_ex_parteEmitidas <- renderUI({
  #   if (input$showTable_OP_Ley148_ex_parteEmitidas) {
  #     hyperlinks <- c("https://poderjudicial.pr/mision-y-vision-de-la-rama-judicial/")
  #     texts <- c("Oficina de Administración de los Tribunales, Directoría de Operaciones, Oficina de Estadísticas")
  #     
  #     tags$div(
  #       class = "card",
  #       style = "padding: 10px; width: 90%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
  #       
  #       # Contenedor centrado para la tabla
  #       div(
  #         style = "padding: 5px; width: 98%; display: flex; justify-content: center;", 
  #         div(
  #           style = "width: 98%; max-width: 800px; overflow-x: auto;", 
  #           DTOutput("dataTable_OP_Ley148_ex_parteEmitidas")
  #         )
  #       ),
  #       
  #       createFuenteDiv(hyperlinks, texts)
  #     )
  #   }
  # })
  # 
  
  # #### (OP_LEY148Archivadas regiones) ####
  # 
  # # Filtrar el conjunto de datos según los valores seleccionados del año fiscal, la razón de archivado y la región fiscal
  # OP_LEY148Archivadas_filt <- reactive({
  #   filter(OP_LEY148Archivadas,
  #          AñoFiscal %in% input$checkGroup_trib_OP_LEY148Archivadas_AñoFiscal,
  #          Razón %in% input$checkGroup_trib_OP_LEY148Archivadas_Razón,
  #          Región %in% input$checkGroup_trib_OP_LEY148Archivadas_Región
  #   )
  # })
  # 
  # ### funcion para el botón de deseleccionar/seleccionar el año fiscal
  # observeEvent(input$deselectAll_trib_OP_LEY148Archivadas_AñoFiscal, {
  #   updateCheckboxGroup_trib(session, "checkGroup_trib_OP_LEY148Archivadas_AñoFiscal", input, OP_LEY148Archivadas$AñoFiscal)
  # })
  # 
  # observe({
  #   inputId <- "checkGroup_trib_OP_LEY148Archivadas_AñoFiscal"
  #   buttonId <- "deselectAll_trib_OP_LEY148Archivadas_AñoFiscal"
  #   all_choices <- levels(OP_LEY148Archivadas$AñoFiscal)
  #   selected <- input[[inputId]]
  #   
  #   is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
  #   
  #   updateActionButton(
  #     session,
  #     inputId = buttonId,
  #     label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
  #   )
  # })
  # 
  # ### funcion para el botón de deseleccionar/seleccionar la razón de archivado
  # observeEvent(input$deselectAll_trib_OP_LEY148Archivadas_Razón, {
  #   updateCheckboxGroup(session, "checkGroup_trib_OP_LEY148Archivadas_Razón", input, OP_LEY148Archivadas$Razón)
  # })
  # 
  # observe({
  #   inputId <- "checkGroup_trib_OP_LEY148Archivadas_Razón"
  #   buttonId <- "deselectAll_trib_OP_LEY148Archivadas_Razón"
  #   all_choices <- levels(OP_LEY148Archivadas$Razón)
  #   selected <- input[[inputId]]
  #   
  #   is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
  #   
  #   updateActionButton(
  #     session,
  #     inputId = buttonId,
  #     label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
  #   )
  # })
  # 
  # ### funcion para el botón de deseleccionar/seleccionar el Región
  # observeEvent(input$deselectAll_trib_OP_LEY148Archivadas_Región, {
  #   updateCheckboxGroup(session, "checkGroup_trib_OP_LEY148Archivadas_Región", input, OP_LEY148Archivadas$Región)
  # })
  # 
  # observe({
  #   inputId <- "checkGroup_trib_OP_LEY148Archivadas_Región"
  #   buttonId <- "deselectAll_trib_OP_LEY148Archivadas_Región"
  #   all_choices <- levels(OP_LEY148Archivadas$Región)
  #   selected <- input[[inputId]]
  #   
  #   is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
  #   
  #   updateActionButton(
  #     session,
  #     inputId = buttonId,
  #     label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
  #   )
  # })
  # 
  # # Colores de las razones
  # OP_LEY148Archivadas_fill_Razón <- setColorFill(OP_LEY148Archivadas, "Razón")
  # 
  # 
  # # Grafico de barras
  # output$barPlot_OP_LEY148Archivadas <- renderPlotly({
  #   # Verificar si hay opciones seleccionadas en cada grupo
  #   has_año <- length(input$checkGroup_trib_OP_LEY148Archivadas_AñoFiscal) > 0
  #   has_razon <- length(input$checkGroup_trib_OP_LEY148Archivadas_Razón) > 0
  #   has_region <- length(input$checkGroup_trib_OP_LEY148Archivadas_Región) > 0
  #   
  #   # Crear mensaje si faltan opciones seleccionadas
  #   if (!has_año || !has_razon || !has_region) {
  #     message <- HTML("Seleccione Razón, Distrito Fiscal \n y Año(s) a visualizar")
  #   } else {
  #     # Si todas las opciones están seleccionadas, crear la gráfica
  #     p <- renderBarPlot_facets(OP_LEY148Archivadas_filt, x = "AñoFiscal", y = "ÓrdenesArchivadas", fill = "Razón",
  #                        title = "Órdenes de protección ex parte \narchivadas bajo Ley 148 según región judicial",
  #                        xlab = "Año fiscal", ylab = "Órdenes de Protección Archivadas", fillLab = "Razón de Archivo",
  #                        colorFill = OP_LEY148Archivadas_fill_Razón,
  #                        emptyMessage = HTML("Seleccione Razón, Distrito Fiscal \n y Año(s) a visualizar"))
  #     #Altura predeterminada para la grafica.
  #     plot_height = 500
  #     numPlots = length(input$checkGroup_trib_OP_LEY148Archivadas_Región)
  #     #Llamado a la funcion calcPlotHeight para calcular la altura basado en el numero de filas.
  #     total_height = plotHeight(plot_height, numPlots)
  #     p <- p + facet_wrap(~Región, ncol = 2) +
  #       theme(panel.spacing.x = unit(0.2, "lines"), #Espacio entre las facetas en x.
  #             panel.spacing.y = unit(-0.02, "lines")) #Espacio entre las facetas en y.
  #     p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)
  #     
  #     return(p)
  #   }
  #   
  #   # Crear la gráfica vacía con mensaje
  #   empty_plot <- create_empty_plot_with_message(OP_LEY148Archivadas_filt, x = "AñoFiscal", y = "ÓrdenesArchivadas", fill = "Razón",
  #                                                xlab = "Año fiscal", ylab = "Órdenes de Protección Archivadas", message)
  #   convert_to_plotly(empty_plot, tooltip = "text")
  # })
  # 
  # #Titulo de la Grafica
  # output$plot_title_OP_LEY148Archivadas <- renderUI({
  #   title <- "Órdenes de protección ex parte archivadas bajo Ley 148 según región judicial"
  # })
  # 
  # 
  # OP_LEY148Archivadas_filt_rename <- reactive({
  #   OP_LEY148Archivadas_filt() %>% 
  #     rename(`Año Fiscal` = AñoFiscal)  %>% 
  #     rename(`Distrito Fiscal` = Región) %>% 
  #     rename(`Órdenes Archivadas` = ÓrdenesArchivadas)%>% 
  #     rename(`Razón de Archivo` = Razón)
  # })
  # 
  # # Data Table 
  # # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
  # output$dataTable_OP_LEY148Archivadas <- renderDT(server = FALSE, {
  #   renderDataTable(OP_LEY148Archivadas_filt_rename(), "Datos: Órdenes de protección ex parte archivadas bajo Ley 148")
  # })
  # 
  # # Crear Card con Fuentes
  # output$dataTableUI_OP_LEY148Archivadas  <- renderUI({
  #   if (input$showTable_OP_LEY148Archivadas) {
  #     hyperlinks <- c("https://poderjudicial.pr/mision-y-vision-de-la-rama-judicial/")
  #     texts <- c("Oficina de Administración de los Tribunales, Directoría de Operaciones, Oficina de Estadísticas")
  #     
  #     tags$div(
  #       class = "card",
  #       style = "padding: 10px; width: 75%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
  #       
  #       # Contenedor centrado para la tabla
  #       div(
  #         style = "padding: 5px; width: 98%; display: flex; justify-content: center;",  
  #         div(
  #           style = "width: 98%; max-width: 800px; overflow-x: auto;",  
  #           DTOutput("dataTable_OP_LEY148Archivadas")
  #         )
  #       ),
  #    
  #       createFuenteDiv(hyperlinks, texts)
  #     )
  #   }
  # })
  
  # #### (OP_LEY148Denegadas regiones) ####
  # 
  # # Filtrar el conjunto de datos según los valores seleccionados del año fiscal, la razón de archivado y la región fiscal
  # OP_LEY148Denegadas_filt <- reactive({
  #   filter(OP_LEY148Denegadas,
  #          AñoFiscal %in% input$checkGroup_trib_OP_LEY148Denegadas_AñoFiscal,
  #          Razón %in% input$checkGroup_trib_OP_LEY148Denegadas_Razón,
  #          Región %in% input$checkGroup_trib_OP_LEY148Denegadas_Región
  #   )
  # })
  # 
  # ### funcion para el botón de deseleccionar/seleccionar el año fiscal
  # observeEvent(input$deselectAll_trib_OP_LEY148Denegadas_AñoFiscal, {
  #   updateCheckboxGroup_trib(session, "checkGroup_trib_OP_LEY148Denegadas_AñoFiscal", input, OP_LEY148Denegadas$AñoFiscal)
  # })
  # 
  # observe({
  #   inputId <- "checkGroup_trib_OP_LEY148Denegadas_AñoFiscal"
  #   buttonId <- "deselectAll_trib_OP_LEY148Denegadas_AñoFiscal"
  #   all_choices <- levels(OP_LEY148Denegadas$AñoFiscal)
  #   selected <- input[[inputId]]
  #   
  #   is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
  #   
  #   updateActionButton(
  #     session,
  #     inputId = buttonId,
  #     label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
  #   )
  # })
  # 
  # ### funcion para el botón de deseleccionar/seleccionar la razón de archivado
  # observeEvent(input$deselectAll_trib_OP_LEY148Denegadas_Razón, {
  #   updateCheckboxGroup(session, "checkGroup_trib_OP_LEY148Denegadas_Razón", input, OP_LEY148Denegadas$Razón)
  # })
  # 
  # observe({
  #   inputId <- "checkGroup_trib_OP_LEY148Denegadas_Razón"
  #   buttonId <- "deselectAll_trib_OP_LEY148Denegadas_Razón"
  #   all_choices <- levels(OP_LEY148Denegadas$Razón)
  #   selected <- input[[inputId]]
  #   
  #   is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
  #   
  #   updateActionButton(
  #     session,
  #     inputId = buttonId,
  #     label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
  #   )
  # })
  # 
  # ### funcion para el botón de deseleccionar/seleccionar el Región
  # observeEvent(input$deselectAll_trib_OP_LEY148Denegadas_Región, {
  #   updateCheckboxGroup(session, "checkGroup_trib_OP_LEY148Denegadas_Región", input, OP_LEY148Denegadas$Región)
  # })
  # 
  # observe({
  #   inputId <- "checkGroup_trib_OP_LEY148Denegadas_Región"
  #   buttonId <- "deselectAll_trib_OP_LEY148Denegadas_Región"
  #   all_choices <- levels(OP_LEY148Denegadas$Región)
  #   selected <- input[[inputId]]
  #   
  #   is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
  #   
  #   updateActionButton(
  #     session,
  #     inputId = buttonId,
  #     label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
  #   )
  # })
  # 
  # # Colores de las razones
  # OP_LEY148Denegadas_fill_Razón <- setColorFill(OP_LEY148Denegadas, "Razón")
  # 
  # 
  # # Grafico de barras
  # output$barPlot_OP_LEY148Denegadas <- renderPlotly({
  #   # Verificar si hay opciones seleccionadas en cada grupo
  #   has_año <- length(input$checkGroup_trib_OP_LEY148Denegadas_AñoFiscal) > 0
  #   has_razon <- length(input$checkGroup_trib_OP_LEY148Denegadas_Razón) > 0
  #   has_region <- length(input$checkGroup_trib_OP_LEY148Denegadas_Región) > 0
  #   
  #   # Crear mensaje si faltan opciones seleccionadas
  #   if (!has_año || !has_razon || !has_region) {
  #     message <- HTML("Seleccione Razón, Región Judicial \n y Año(s) a visualizar")
  #   } else {
  #     # Si todas las opciones están seleccionadas, crear la gráfica
  #     p <- renderBarPlot_facets(OP_LEY148Denegadas_filt, x = "AñoFiscal", y = "ÓrdenesDenegadas", fill = "Razón",
  #                        title = "Órdenes de protección denegadas bajo \nLey 148 por razón de archivo según región judicial",
  #                        xlab = "Año fiscal", ylab = "Órdenes de Protección Denegadas", fillLab = "Razón de Archivo",
  #                        colorFill = OP_LEY148Denegadas_fill_Razón,
  #                        emptyMessage = HTML("Seleccione Razón, Región Judicial \n y Año(s) a visualizar"))
      # #Altura predeterminada para la grafica.
      # plot_height = 500
      # numPlots = length(input$checkGroup_trib_OP_LEY148Denegadas_Región)
      # #Llamado a la funcion calcPlotHeight para calcular la altura basado en el numero de filas.
      # total_height = plotHeight(plot_height, numPlots)
      # p <- p + facet_wrap(~Región, ncol = 2) +
      #   theme(panel.spacing.x = unit(0.2, "lines"), #Espacio entre las facetas en x.
      #         panel.spacing.y = unit(-0.02, "lines")) #Espacio entre las facetas en y.
      # p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)

  #     return(p)
  #   }
  #   
  #   # Crear la gráfica vacía con mensaje
  #   empty_plot <- create_empty_plot_with_message(OP_LEY148Denegadas_filt, x = "AñoFiscal", y = "ÓrdenesDenegadas", fill = "Razón",
  #                                                xlab = "Año fiscal", ylab = "Órdenes de Protección Denegadas", message)
  #   convert_to_plotly(empty_plot, tooltip = "text")
  # })
  # 
  # #Titulo de la Grafica
  # output$plot_title_OP_LEY148Denegadas <- renderUI({
  #   title <- "Órdenes de protección denegadas bajo Ley 148 por razón de archivo según región judicial"
  # })
  # 
  # OP_LEY148Denegadas_filt_rename <- reactive({
  #   OP_LEY148Denegadas_filt() %>% 
  #     rename(`Año Fiscal` = AñoFiscal)  %>% 
  #     rename(`Región Judicial` = Región) %>% 
  #     rename(`Órdenes Denegadas` = ÓrdenesDenegadas)%>% 
  #     rename(`Razón de Archivo` = Razón)
  # })
  # 
  # 
  # # Data Table 
  # # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
  # output$dataTable_OP_LEY148Denegadas <- renderDT(server = FALSE, {
  #   renderDataTable(OP_LEY148Denegadas_filt_rename(), "Datos: Ordenes de protección denegadas por violencia sexual bajo Ley 148")
  # })
  # 
  # # Crear Card con Fuentes
  # output$dataTableUI_OP_LEY148Denegadas  <- renderUI({
  #   if (input$showTable_OP_LEY148Denegadas) {
  #     hyperlinks <- c("https://poderjudicial.pr/mision-y-vision-de-la-rama-judicial/")
  #     texts <- c("Oficina de Administración de los Tribunales, Directoría de Operaciones, Oficina de Estadísticas")
  #     
  #     tags$div(
  #       class = "card",
  #       style = "padding: 10px; width: 85%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
  #       
  #       # Contenedor centrado para la tabla
  #       div(
  #         style = "padding: 5px; width: 98%; display: flex; justify-content: center;", 
  #         div(
  #           style = "width: 98%; max-width: 800px; overflow-x: auto;",  
  #           DTOutput("dataTable_OP_LEY148Denegadas")
  #         )
  #       ),
  #       
  #       createFuenteDiv(hyperlinks, texts)
  #     )
  #   }
  # })
  # 
  # #### (OP_LEY148FinalEmitidas Datos por region) ####
# 
#   # Filtrar el conjunto de datos según los valores seleccionados del año fiscal, el delito cometido y la región fiscal
#   OP_LEY148FinalEmitidas_filt_region <- reactive({
#     filter(OP_LEY148FinalEmitidas,
#            AñoFiscal %in% input$checkGroup_trib_OP_LEY148FinalEmitidas_AñoFiscal,
#            Delito %in% input$checkGroup_trib_OP_LEY148FinalEmitidas_Delito,
#            Región %in% input$checkGroup_trib_OP_LEY148FinalEmitidas_Región
#     )
#   })
# 
#   ### funcion para el botón de deseleccionar/seleccionar el año fiscal
#   observeEvent(input$deselectAll_trib_OP_LEY148FinalEmitidas_AñoFiscal, {
#     updateCheckboxGroup_trib(session, "checkGroup_trib_OP_LEY148FinalEmitidas_AñoFiscal", input, OP_LEY148FinalEmitidas$AñoFiscal)
#   })
# 
#   observe({
#     inputId <- "checkGroup_trib_OP_LEY148FinalEmitidas_AñoFiscal"
#     buttonId <- "deselectAll_trib_OP_LEY148FinalEmitidas_AñoFiscal"
#     all_choices <- levels(OP_LEY148FinalEmitidas$AñoFiscal)
#     selected <- input[[inputId]]
# 
#     is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
# 
#     updateActionButton(
#       session,
#       inputId = buttonId,
#       label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
#     )
#   })
# 
# 
#   ### funcion para el botón de deseleccionar/seleccionar delito cometido
#   observeEvent(input$deselectAll_trib_OP_LEY148FinalEmitidas_Delito, {
#     updateCheckboxGroup(session, "checkGroup_trib_OP_LEY148FinalEmitidas_Delito", input, OP_LEY148FinalEmitidas$Delito)
#   })
# 
#   observe({
#     inputId <- "checkGroup_trib_OP_LEY148FinalEmitidas_Delito"
#     buttonId <- "deselectAll_trib_OP_LEY148FinalEmitidas_Delito"
#     all_choices <- levels(OP_LEY148FinalEmitidas$Delito)
#     selected <- input[[inputId]]
# 
#     is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
# 
#     updateActionButton(
#       session,
#       inputId = buttonId,
#       label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
#     )
#   })
# 
#   ### funcion para el botón de deseleccionar/seleccionar el Región
#   observeEvent(input$deselectAll_trib_OP_LEY148FinalEmitidas_Región, {
#     updateCheckboxGroup(session, "checkGroup_trib_OP_LEY148FinalEmitidas_Región", input, OP_LEY148FinalEmitidas$Región)
#   })
# 
#   observe({
#     inputId <- "checkGroup_trib_OP_LEY148FinalEmitidas_Región"
#     buttonId <- "deselectAll_trib_OP_LEY148FinalEmitidas_Región"
#     all_choices <- levels(OP_LEY148FinalEmitidas$Región)
#     selected <- input[[inputId]]
# 
#     is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
# 
#     updateActionButton(
#       session,
#       inputId = buttonId,
#       label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
#     )
#   })
# 
#   # Colores de las razones
#   OP_LEY148FinalEmitidas_fill_Delito <- setColorFill(OP_LEY148FinalEmitidas, "Delito")
# 
#   # Grafico de barras
#   output$barPlot_OP_LEY148FinalEmitidas_region <- renderPlotly({
#     # Verificar si hay opciones seleccionadas en cada grupo
#     has_año <- length(input$checkGroup_trib_OP_LEY148FinalEmitidas_AñoFiscal) > 0
#     has_delito <- length(input$checkGroup_trib_OP_LEY148FinalEmitidas_Delito) > 0
#     has_region <- length(input$checkGroup_trib_OP_LEY148FinalEmitidas_Región) > 0
# 
#     # Crear mensaje si faltan opciones seleccionadas
#     if (!has_año || !has_delito || !has_region) {
#       message <- HTML("Seleccione Delito(s), Región Judicial \n y Año(s) a visualizar")
#     } else {
#       # Si todas las opciones están seleccionadas, crear la gráfica
      # p <- renderBarPlot_facets(OP_LEY148FinalEmitidas_filt_region, x = "AñoFiscal", y = "ÓrdenesEmitidas", fill = "Delito",
      #                    title = "Órdenes de protección emitidas bajo Ley 148, \nsegún región judicial y tipo de delito",
      #                    xlab = "Año Fiscal", ylab = "Órdenes de Protección Emitidas", fillLab = "Delito Cometido",
      #                    colorFill = OP_LEY148FinalEmitidas_fill_Delito,
      #                    emptyMessage = HTML("Seleccione Delito(s), Región Judicial \n y Año(s) a visualizar"))
      # #Altura predeterminada para la grafica.
      # plot_height = 500
      # numPlots = length(input$checkGroup_trib_OP_LEY148FinalEmitidas_Región)
      # #Llamado a la funcion calcPlotHeight para calcular la altura basado en el numero de filas.
      # total_height = plotHeight(plot_height, numPlots)
      # p <- p + facet_wrap(~Región, ncol = 2) +
      # theme(panel.spacing.x = unit(0.2, "lines"), #Espacio entre las facetas en x.
      #       panel.spacing.y = unit(-0.02, "lines")) #Espacio entre las facetas en y.
      # p <- convert_to_plotly(p, tooltip = "text", TRUE, numPlots) %>% layout(height = total_height)

#       return(p)
#     }
# 
#     # Crear la gráfica vacía con mensaje
#     empty_plot <- create_empty_plot_with_message(OP_LEY148FinalEmitidas_filt_region, x = "AñoFiscal", y = "ÓrdenesEmitidas", fill = "Delito",
#                                                  xlab = "Año Fiscal", ylab = "Órdenes de Protección Emitidas", message)
#     convert_to_plotly(empty_plot, tooltip = "text")
#   })
# 
  # #Titulo de la Grafica
  # output$plot_title_OP_LEY148FinalEmitidas <- renderUI({
  #   title <- "Órdenes de protección emitidas bajo Ley 148, según región judicial y tipo de delito"
  # })
  # 
  # OP_LEY148FinalEmitidas_filt_rename <- reactive({
  #   OP_LEY148FinalEmitidas_filt_region() %>%
  #     rename(`Año Fiscal` = AñoFiscal)  %>%
  #     rename(`Región Judicial` = Región) %>%
  #     rename(`Órdenes Emitidas` = ÓrdenesEmitidas)%>%
  #     rename(`Delito Cometido` = Delito)
  # })
  # 
  # 
  # # Data Table
  # # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
  # output$dataTable_OP_LEY148FinalEmitidas <- renderDT(server = FALSE, {
  #   renderDataTable(OP_LEY148FinalEmitidas_filt_rename(), "Datos: Órdenes de protección emitidas bajo ley 148")
  # })

#   # Crear Card con Fuentes
#   output$dataTableUI_OP_LEY148FinalEmitidas  <- renderUI({
#     if (input$showTable_OP_LEY148FinalEmitidas) {
#       hyperlinks <- c("https://poderjudicial.pr/mision-y-vision-de-la-rama-judicial/")
#       texts <- c("Oficina de Administración de los Tribunales, Directoría de Operaciones, Oficina de Estadísticas")
# 
#       tags$div(
#         class = "card",
#         style = "padding: 10px; width: 87%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
# 
#         # Contenedor centrado para la tabla
#         div(
#           style = "padding: 5px; width: 98%; display: flex; justify-content: center;",
#           div(
#             style = "width: 98%; max-width: 800px; overflow-x: auto;",
#             DTOutput("dataTable_OP_LEY148FinalEmitidas")
#           )
#         ),
# 
#         createFuenteDiv(hyperlinks, texts)
#       )
#     }
#   })
# 
  

  
  
  
  
  
  
  #####################################################################
  ########## Tab del Centro de Ayuda a Víctimas de Violación ##########
  #####################################################################
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
  
  observe({
    inputId <- "checkGroup_cavv_safekitsDF_Año"
    buttonId <- "deselectAll_cavv_safekitsDF_Año"
    all_choices <- levels(safekitsDF$Año)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el estado de querella
  observeEvent(input$deselectAll_cavv_safekitsDF_Kits, {
    updateCheckboxGroup(session, "checkGroup_cavv_safekitsDF_Kits", input, safekitsDF$Kits)
  })
  
  observe({
    inputId <- "checkGroup_cavv_safekitsDF_Kits"
    buttonId <- "deselectAll_cavv_safekitsDF_Kits"
    all_choices <- levels(safekitsDF$Kits)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
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
      p <- renderBarPlot_stack(safekitsDF_filt, x = "Año", y = "Total", fill = "Kits",
                               title = HTML(""),
                               xlab = "Año", ylab = "Total de kits distribuidos", fillLab = "Estado de Querella",
                               colorFill = safekitsDF_fill_Kits,
                               emptyMessage = "Seleccione Estado de querella y Año(s) a visualizar")
      
      p <- convert_to_plotly(p, tooltip = "text") %>% layout(height = 450)
      
      return(p)
    }
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(safekitsDF_filt, x = "Año", y = "Total", fill = "Kits", 
                                                 xlab = "Año", ylab = "Total de Kits Distribuidos", message)
    
    convert_to_plotly(empty_plot, tooltip = "text")
  })
  
  #Titulo de la Grafica
  output$plot_title_safekitsDF <- renderUI({
    title <- HTML("Tendencia anual de <i>SAKs Kits</i> por estado de querella")
  })
 
  safekitsDF_filt_rename <- reactive({
    safekitsDF_filt() %>%  
      rename(`SAFE Kits` = Kits) %>%  
      rename(`Total Distribuidos` = Total)
  })
  
  
  # Data Table
  # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
  output$dataTable_safekitsDF <- renderDT(server = FALSE, {
    renderDataTable(safekitsDF_filt_rename(), "Datos: Tendencia anual del equipo de recolecta de evidencia de SAKs Kits en casos de violencia sexual")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_safekitsDF <- renderUI({
    if (input$showTable_safekitsDF) {
      hyperlinks <- c("https://www.icf.pr.gov/")
      texts <- c("Instituto de Ciencias Forenses")
      
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
  
  #### (safekitsDF_edades) ####
  
  # Filtrar el conjunto de datos según los valores seleccionados del año y el estado de la querella
  safekitsDF_edades_filt <- reactive({
    filter(safekitsDF_edades,
           Año %in% input$checkGroup_cavv_safekitsDF_edades_Año,
           Categoria %in% input$checkGroup_cavv_safekitsDF_edades_Categoria
    )
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el año
  observeEvent(input$deselectAll_cavv_safekitsDF_edades_Año, {
    updateCheckboxGroup(session, "checkGroup_cavv_safekitsDF_edades_Año", input, safekitsDF_edades$Año)
  })
  
  observe({
    inputId <- "checkGroup_cavv_safekitsDF_edades_Año"
    buttonId <- "deselectAll_cavv_safekitsDF_edades_Año"
    all_choices <- levels(safekitsDF_edades$Año)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el estado de querella
  observeEvent(input$deselectAll_cavv_safekitsDF_edades_Categoria, {
    updateCheckboxGroup(session, "checkGroup_cavv_safekitsDF_edades_Categoria", input, safekitsDF_edades$Categoria)
  })
  
  observe({
    inputId <- "checkGroup_cavv_safekitsDF_edades_Categoria"
    buttonId <- "deselectAll_cavv_safekitsDF_edades_Categoria"
    all_choices <- levels(safekitsDF_edades$Categoria)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  # Colores de los estados de Querella
  safekitsDF_fill_Categoria <- setColorFill(safekitsDF_edades, "Categoria")
  
  # Descartar fila con Total de Kits para omitir su representacion en la grafica
  safekitsDF_edades_filt_noTotal <- reactive({
    safekitsDF_edades_filt() %>%  
      filter(Categoria != "Total de Kits con querella")
  })
  
  
  # Grafico de barras
  output$barPlot_safekitsDF_edades <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_cavv_safekitsDF_edades_Año) > 0
    has_categoria <- length(input$checkGroup_cavv_safekitsDF_edades_Categoria) > 0
    
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_año || !has_categoria) {
      message <- "Seleccione Categoría y Año(s) a visualizar"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot_stack(safekitsDF_edades_filt, x = "Año", y = "Total", fill = "Categoria",
                               title = HTML(""),
                               xlab = "Año", ylab = "Total de kits distribuidos", fillLab = "Categoria",
                               colorFill = safekitsDF_fill_Categoria,
                               emptyMessage = "Seleccione Categoría y Año(s) a visualizar")
      
      p <- convert_to_plotly(p, tooltip = "text") %>% layout(height = 450)
      
      return(p)
    }
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(safekitsDF_edades_filt, x = "Año", y = "Total", fill = "Categoria", 
                                                 xlab = "Año", ylab = "Total de Kits Distribuidos", message)
    
    convert_to_plotly(empty_plot, tooltip = "text")
  })
  
  #Titulo de la Grafica
  output$plot_title_safekitsDF_edades <- renderUI({
    title <- HTML("Tendencia anual de <i>SAKs Kits</i> con querella en menores y mayores de edad")
  })
  
  safekitsDF_edades_filt_rename <- reactive({
    safekitsDF_edades_filt() %>%  
      rename(`Total Distribuidos` = Total)
  })
  
  
  # Data Table
  # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
  output$dataTable_safekitsDF_edades <- renderDT(server = FALSE, {
    renderDataTable(safekitsDF_edades_filt_rename(), "Datos: Tendencia anual del equipo de recolecta de evidencia de SAKs Kits en casos de violencia sexual")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_safekitsDF_edades  <- renderUI({
    if (input$showTable_safekitsDF_edades) {
      hyperlinks <- c("https://www.icf.pr.gov/")
      texts <- c("Instituto de Ciencias Forenses")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 98%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;",  
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;", 
            DTOutput("dataTable_safekitsDF_edades")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
  })
  
  
  
  ####  (safekitsDF_analizados) ####
  # Filtrar el conjunto de datos según los valores seleccionados del año y el estado de la querella
  safekitsDF_analizados_filt <- reactive({
    filter(safekitsDF_analizados,
           Año %in% input$checkGroup_cavv_safekitsDF_analizados_Año,
           Laboratorio %in% input$checkGroup_cavv_safekitsDF_analizados_Laboratorio
    )
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el año
  observeEvent(input$deselectAll_cavv_safekitsDF_analizados_Año, {
    updateCheckboxGroup(session, "checkGroup_cavv_safekitsDF_analizados_Año", input, safekitsDF_analizados$Año)
  })
  
  observe({
    inputId <- "checkGroup_cavv_safekitsDF_analizados_Año"
    buttonId <- "deselectAll_cavv_safekitsDF_analizados_Año"
    all_choices <- levels(safekitsDF_analizados$Año)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  ### funcion para el botón de deseleccionar/seleccionar el estado de querella
  observeEvent(input$deselectAll_cavv_safekitsDF_analizados_Laboratorio, {
    updateCheckboxGroup(session, "checkGroup_cavv_safekitsDF_analizados_Laboratorio", input, safekitsDF_analizados$Laboratorio)
  })
  
  observe({
    inputId <- "checkGroup_cavv_safekitsDF_analizados_Laboratorio"
    buttonId <- "deselectAll_cavv_safekitsDF_analizados_Laboratorio"
    all_choices <- levels(safekitsDF_analizados$Laboratorio)
    selected <- input[[inputId]]
    
    is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
    
    updateActionButton(
      session,
      inputId = buttonId,
      label = if (is_all_selected) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
    )
  })
  
  # Colores de los estados de Querella
  safekitsDF_analizados_fill_Laboratorio <- setColorFill(safekitsDF_analizados, "Laboratorio")
  
  
  # Grafico de barras
  output$barPlot_safekitsDF_analizados <- renderPlotly({
    # Verificar si hay opciones seleccionadas en cada grupo
    has_año <- length(input$checkGroup_cavv_safekitsDF_analizados_Año) > 0
    has_lab <- length(input$checkGroup_cavv_safekitsDF_analizados_Laboratorio) > 0
    
    
    # Crear mensaje si faltan opciones seleccionadas
    if (!has_año || !has_lab) {
      message <- "Seleccione Laboratorio(s) y Año(s) a visualizar"
    } else {
      # Si todas las opciones están seleccionadas, crear la gráfica
      p <- renderBarPlot_facets(safekitsDF_analizados_filt, "Año", "Total", "Laboratorio",
                                "Año", "Total de Kits Analizados", fillLab = "Laboratorio", colorFill = safekitsDF_analizados_fill_Laboratorio, 
                                emptyMessage = "Seleccione Laboratorio(s) y Año(s) a visualizar",barWidth = 0, xGap = 0)
      
      p <- convert_to_plotly(p, tooltip = "text") %>% layout(height = 450)
      
      return(p)
    }
    # Crear la gráfica vacía con mensaje
    empty_plot <- create_empty_plot_with_message(safekitsDF_analizados_filt, x = "Año", y = "Total", fill = "Laboratorio", 
                                                 xlab = "Año", ylab = "Total de Kits Analizados", message)
    
    convert_to_plotly(empty_plot, tooltip = "text")
  })
  
  #Titulo de la Grafica
  output$plot_title_safekitsDF_analizados <- renderUI({
    title <- HTML("Total de kits de agresiones sexuales analizados")
  })
  
  safekitsDF_analizados_filt_rename <- reactive({
    safekitsDF_analizados_filt() %>%  
      rename(`Total de Kits Analizados` = Total)
  })
  
  
  # Data Table
  # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
  output$dataTable_safekitsDF_analizados <- renderDT(server = FALSE, {
    renderDataTable(safekitsDF_analizados_filt_rename(), "Datos: Total de kits de agresiones sexuales analizados")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_safekitsDF_analizados  <- renderUI({
    if (input$showTable_safekitsDF_analizados) {
      hyperlinks <- c("https://www.icf.pr.gov/")
      texts <- c("Instituto de Ciencias Forenses")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 98%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;",  
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;", 
            DTOutput("dataTable_safekitsDF_analizados")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
  })
  
  #### Tab del Mapa de SAFE Kits recibidos por region policiaca (mapa_cavv) ####
  
  # Filtrar el conjunto de datos según el año, delito o distrito seleccionado
  mapaCAVV_filt <- reactive({
    filter(mapa_cavv, 
           Año %in% input$select_mapaCAVV_año)
  })
  
  # funcion para el boton de deseleccionar/seleccionar el año
  observeEvent(input$deselectAll_mapaCAVV_año, {
    updateCheckboxGroup(session, "checkGroup_mapaCAVV_año", input, mapa_cavv$Año)
  })
  

  # Renderizar el mapa con Leaflet
  output$map_mapaCAVV <- renderLeaflet({
    data <- mapaCAVV_filt()
    renderMap(data, value_col = "Cantidad",
              value_col_region = "Región",
              map_zoom = 8.5,
              provider = providers$CartoDB.Positron,
              municipios_geo = municipios_geo)
  })
  
  #Titulo de la Grafica
  output$plot_title_mapaCAVV <- renderUI({
    title <- HTML("<i>SAKs Kits</i> recibidos por región policiaca")
  })
    
  mapaCAVV_filt_rename <- reactive({
    st_drop_geometry(mapaCAVV_filt())%>%
      rename(`Cantidad de Kits recibidos` = Cantidad)
  })
  
  # Data Table
  # Con Server = FALSE, todos los datos se envían al cliente, mientras que solo los datos mostrados se envían al navegador con server = TRUE.
  output$dataTable_mapaCAVV <- renderDT(server = FALSE, {
    renderDataTable(mapaCAVV_filt_rename(), "Datos: Cantidad de kits recibidos por region policiaca")
  })
  
  # Crear Card con Fuentes
  output$dataTableUI_mapaCAVV  <- renderUI({
    if (input$showTable_mapaCAVV) {
      hyperlinks <- c("https://www.icf.pr.gov/")
      texts <- c("Instituto de Ciencias Forenses")
      
      tags$div(
        class = "card",
        style = "padding: 10px; width: 98%; margin: 10px auto; border: 1px solid #ddd; border-radius: 5px; box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);",  # Usar margin: 10px auto para centrar el card
        
        # Contenedor centrado para la tabla
        div(
          style = "padding: 5px; width: 98%; display: flex; justify-content: center;",  
          div(
            style = "width: 98%; max-width: 800px; overflow-x: auto;", 
            DTOutput("dataTable_mapaCAVV")
          )
        ),
        
        createFuenteDiv(hyperlinks, texts)
      )
    }
  })
  
  #### Tab de Publicaciones ####
  # PDF 1
  publicationCardPDFServer("cavv_pdf1", "cavv/boletinVS2023.pdf")
  # # PDF 2
  # publicationCardPDFServer("pdf2", "snmv/reporte2022.pdf")
  # 
  output$cavv_view_content <- renderUI({
    if (input$cavv_view_option == "Publicaciones") {
      tagList(
        tags$ul(
          style = "list-style-type: none; text-align: center;",
          sectionTitle("Publicaciones y Recursos", "24px")
        ),
        fluidRow(
          column(12, publicationCardPDF("cavv_pdf1", "Boletín Estadístico de Violencia Sexual, Año 2023", "cavv/boletinVS2023.pdf"))
        )
        # ,
        # fluidRow(
        #   column(12, publicationCardPDF("pdf2", "Reporte Estadístico 2022", "snmv/reporte2022.pdf"))
        # )
      )
    } else {
      # Dashboard de la Agencia (embed link)
      tags$div(
        fluidRow(
          column(12, publicationCardWeb("Dashboard de la Agencia:", 
                                        "https://safekits.pr.gov/?fbclid=IwAR3ooo0EgJS_8MEVB3Z5IettOyplWfi3sYQU18AZlC5yieAIMhIYOuHjHZs"))
        )
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
    list(word = "Kits", definition = "Equipo utilizado por profesionales de la salud para la recuperación de evidencia forense en casos de violencia sexual. Consiste en una caja que contiene instrucciones y materiales que facilitan la recolección de evidencia. También conocido como <i>Sexual Assault Kit</i> o, por sus siglas en inglés, <i>SAK</i>."),
    list(word = "Kit con Querella", definition = "Kit de recolección de evidencia forense que se acompaña con la querella radicada por la víctima sobreviviente de violencia sexual. Cuenta con un número otorgado por el NPPR."),
    list(word = "Kit sin Querella", definition = "Kit de recolección de evidencia forense que no se acompaña con una querella, pues la víctima sobreviviente de violencia sexual no ha radicado querella en el NPPR."),
    list(word = "Mayor de Edad", definition = "Toda persona con 18 años o más."),
    list(word = "Menor de Edad", definition = "Toda persona entre las edades de 0 a 18 años."),
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
    renderDataTable_Definitions(definitions_df_cavv, "Centro de Ayuda a Víctimas de Violación")
  })
  
  
  
  #### Tab de Publicaciones ####
  # PDF 1
  publicationCardPDFServer("cavv_pdf1", "cavv/boletinVS2023.pdf")
  # # PDF 2
  # publicationCardPDFServer("pdf2", "snmv/reporte2022.pdf")
  # 
  output$cavv_view_content <- renderUI({
    if (input$cavv_view_option == "Publicaciones") {
      tagList(
        tags$ul(
          style = "list-style-type: none; text-align: center;",
          sectionTitle("Publicaciones y Recursos", "24px")
        ),
        fluidRow(
          column(12, publicationCardPDF("cavv_pdf1", "Boletín Estadístico de Violencia Sexual, Año 2023", "cavv/boletinVS2023.pdf"))
        )
        # ,
        # fluidRow(
        #   column(12, publicationCardPDF("pdf2", "Reporte Estadístico 2022", "snmv/reporte2022.pdf"))
        # )
      )
    } else {
      # Dashboard de la Agencia (embed link)
      tags$div(
        fluidRow(
          column(12, publicationCardWeb("Dashboard de la Agencia:", 
                                        "https://safekits.pr.gov/?fbclid=IwAR3ooo0EgJS_8MEVB3Z5IettOyplWfi3sYQU18AZlC5yieAIMhIYOuHjHZs"))
        )
      )
    }
  })
  
  ##############################
  #### Tab de Publicaciones Generales ####
  ##############################
  
  # # PDF 1
  # publicationCardPDFServer("cavv_pdf1", "cavv/boletinVS2023.pdf")
  # 
  # output$indicadores_view_content <- renderUI({
  #   tags$div(
  #     fluidRow(
  #       column(12, publicationCardWeb("Indicadores:", 
  #                                     "https://infogram.com/1p0yk2e59wrpqkuek2l20dx0w1unj70evey"))
  #     )
  #   )
  # })
  
  # output$indicadores_view_content <- renderUI({
  #   tags$div(
  #     style = "
  #     width: 100%;
  #     overflow: hidden;
  #     position: relative;
  #     padding-top: 56.25%; /* Relación 16:9 */
  #   ",
  #     tags$iframe(
  #       src = "https://infogram.com/1p0yk2e59wrpqkuek2l20dx0w1unj70evey",
  #       style = "
  #       position: absolute;
  #       top: 0;
  #       left: 0;
  #       width: 100%;
  #       height: 100%;
  #       border: none;
  #       transform: scale(0.8);
  #       transform-origin: 0 0;
  #     "
  #     )
  #   )
  # })
  
  output$publicaciones_view_content <- renderUI({
    tags$div(
      style = "
      width: 100%;
      height: auto;
      overflow: hidden; /* ocultar scroll interno */
      display: flex;
      justify-content: center;
      padding-bottom: 40px; /* margen inferior opcional */
    ",
      tags$iframe(
        src = "https://infogram.com/1p0yk2e59wrpqkuek2l20dx0w1unj70evey",
        style = "
        width: 100%;
        height: 2500px; /* agranda para cubrir toda la infografía */
        border: none;
        overflow: hidden; /* elimina barras internas */
        transform: scale(0.9); /* reduce zoom */
        transform-origin: top center;
        pointer-events: auto; /* mantiene clics funcionales */
      "
      )
    )
  })
  
  
  
  
  
  #########################################
  #### tab con informacion de Contacto ####
  #########################################
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






