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
                       "Año", "Cantidad de víctimas", fillLab = "Grupo de edad", colorFill = homiEdad_fill_edad)
    
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
  
  #### Tab de Definiciones ####
  definitions_snmv <- list(
    list(word = "Violencia", definition = "El uso intencional de la fuerza o el poder físico, de hecho, o como amenaza, contra uno mismo, otra persona o un grupo o comunidad, que cause o tenga gran probabilidad de causar lesiones, muerte, daños psicológicos, trastornos del desarrollo o privaciones."),
    list(word = "Muertes violentas", definition = " PRVDRS define una muerte violenta como: Suicidio (incluyendo terrorismo), Homicidio (incluyendo terrorismo), Intervención legal (excluyendo terrorismo, ejecuciones por pena de muerte, o actos de guerra), Eventos con intención no Determinada, Heridas por arma de fuego no intencional."),
    list(word = "Homicidio", definition = " Es un delito de resultado por el cual la conducta intencionada de un sujeto provoca la muerte de otro."),
    list(word = "Suicidio Único", definition = "Se refiere a un acto en el cual una persona termina deliberadamente con su propia vida, sin la participación o implicación de otras personas en el proceso. Este término implica que el individuo toma la decisión y ejecuta el acto suicida de manera independiente, sin ningún intento de causar la muerte de otras personas o de involucrar a terceros en el evento."),
    list(word = "Homicidio múltiple:", definition = "Se refiere a un acto en el cual una persona causa la muerte de múltiples individuos en un solo incidente."),
    list(word = "Homicidio seguido de suicidio", definition = "Se refiere a un acto en el cual una persona causa la muerte de otra(s) y luego se quita la vida. Este tipo de incidente implica dos acciones distintas pero relacionadas: primero, el homicida comete el acto de matar a otra persona, y luego, la misma persona toma su propia vida."),
    list(word = "Homicidio único", definition = "Se refiere a un acto en el cual una persona causa la muerte de otra en un evento específico."),
    list(word = "Homicidio(s) seguido de suicidio(s), (más de dos víctimas)", definition = "Homicidio(s) seguido de suicidio(s), (más de dos víctimas):</b> Se refiere a un acto extremadamente trágico en el cual una o más personas causan la muerte de múltiples individuos antes de acabar con sus propias vidas en un solo evento"),
    list(word = "Muerte accidental por arma de fuego", definition = "Evento en el cual una persona pierde la vida como resultado involuntario de la manipulación, manejo o uso incorrecto de un arma de fuego. Este tipo de incidente ocurre cuando un individuo dispara un arma de manera accidental, ya sea debido a un descuido, falta de conocimiento sobre el funcionamiento seguro del arma, o por la manipulación indebida de la misma."),
    list(word = "Muerte no determinada", definition = "Caso en el cual las circunstancias que rodean el fallecimiento de una persona no pueden ser claramente establecidas o comprendidas mediante la evidencia disponible en el momento de la investigación. Este término se utiliza cuando no hay suficiente información o evidencia forense para determinar con certeza si la muerte fue el resultado de causas naturales, accidentales, suicidas u homicidas.")
  )
  
  output$definitionCards_snmv <- renderUI({
    definitionCards(definitions_snmv)
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
  
  # I was here
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
  #### Tab de Definiciones ####
  
  definitions_fam <- list(
    list(word = "Menores", definition = "Individuos que se encuentran bajo la edad legal de mayoría de edad en un contexto específico, lo que generalmente implica que aún no han alcanzado la edad en la que se les considera plenamente responsables de sus acciones según la ley."),
    list(word = "Maltrato a Menores", definition = "Toda acción u omisión intencional del padre, de la madre o persona responsable del o de la menor que ocasione o ponga en riesgo de sufrir un daño o perjuicio a su salud e integridad física, mental o emocional."),
    list(word = "Abuso Sexual", definition = "Incurrir en conducta sexual en presencia de un o una menor o que se utilice, voluntaria o involuntariamente, para ejecutar conducta sexual dirigida a satisfacer los deseos sexuales. También se considera cualquier acto que, de procesarse por la vía criminal, configuraría cualesquiera de varios delitos de índole sexual, tales como agresión sexual, actos lascivos, comercio de personas para actos sexuales, exposiciones obscenas, proposición obscena, producción de pornografía infantil, entre otros delitos reconocidos en el Código Penal de Puerto Rico."),
    list(word = "Explotación", definition = "Conducta obscena o utilización de una persona menor de edad para ejecutar conducta obscena. explotación de una persona menor de edad o que se permita que otra persona lo explote, incluyendo –pero sin limitarse– a utilizarla para ejecutar conducta obscena, con el fin de lucrarse o de recibir algún otro beneficio."),
    list(word = "Maltrato Físico", definition = "Se refiere a cualquier trauma, lesión o condición no accidental, causada en un episodio o varios, incluyendo la falta de alimentos que, de no ser atendida, pone en riesgo la vida y salud de la persona menor de edad."),
    list(word = "Negligencia", definition = "Es un tipo de maltrato que consiste en no cumplir con la obligación de proveer a las personas menores de edad de manera adecuada los alimentos, ropa, albergue, educación, atención a su salud, poca supervisión, no visitar, ni mantener contacto con el o la menor o incurrir en alguna de las razones reconocidas en el Código Civil de Puerto Rico para que una persona sea privada de patria potestad entre otros."),
    list(word = "Negligencia Educativa", definition = HTML("La negligencia institucional es cuando a una persona menor de edad, que está en un hogar de crianza, centro de cuidado sustituto o en una institución pública o privada, de cuido, educación, tratamiento o detención, se le cause daño o se ponga en riesgo de sufrir daño a su salud e integridad física, mental o emocional, incluyendo –pero sin limitarse– a abuso sexual. La negligencia institucional, ya sea conocida o que se sospeche que ocurre, o que ocurre como resultado de la política, prácticas y condiciones imperantes en la institución, la puede cometer:<ul>          
            <li> Operador u operadora de un hogar de crianza; 
            <li> Cualquier empleado, empleada, funcionario o funcionaria que ofrezca servicios de cuido o que tenga bajo su control o custodia a una persona menor de edad para su cuido, educación, tratamiento o detención;
            </ul>")),
    list(word = "Negligencia Emocional", definition = "Se define como causar menoscabo o afectar la capacidad intelectual o emocional de la persona menor de edad dentro de lo que se considera normal para su edad y entorno cultural."),
    list(word = "Negligencia Médica", definition = "Situaciones en las que los proveedores de atención médica, como médicos, enfermeras u otros profesionales de la salud, no brindan el nivel adecuado de atención y cuidado a pacientes menores de edad, lo que resulta en daños físicos, emocionales o psicológicos para el paciente. Esto puede incluir errores en el diagnóstico, tratamiento inapropiado, falta de seguimiento adecuado, o cualquier otro acto u omisión que pueda considerarse una violación del estándar de cuidado aceptado en la práctica médica."),
    list(word = "Trata Humana", definition = "Se define como la captación, traslado, transporte, acogida o recepción de una persona utilizando la violencia, amenaza, engaño, rapto, fuerza, abuso de poder, abuso de una situación de vulnerabilidad u otros elementos de coacción, con el fin de someterla a explotación y lucrarse con su actividad.")
  )
  
  output$definitionCards_fam <- renderUI({
    definitionCards(definitions_fam)
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
                       xlab = "Año", ylab = "Cantidad de víctimas",
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
  
  #### Tab de Definiciones ####
  
  definitions_just <- list(
    list(word = "Incumplimiento de órdenes de protección (Artículo 2.8)", definition = "Cualquier violación a sabiendas de una orden de protección expedida."),
    list(word = "Maltrato (Artículo 3.1)", definition = "Toda persona que empleare fuerza física o violencia psicológica,intimidación o persecución en la persona de su cónyuge, ex cónyuge, o la persona con quien cohabita o haya cohabitado, o la persona con quien sostuviere o haya sostenido una 
    relación consensual, o la persona con quien haya procreado un hijo o hija, independientemente del sexo, estado civil, orientación sexual, identidad de género o estatus migratorio de cualquiera de las personas involucradas en la relación, para causarle daño físico a su persona, a los bienes
    apreciados por ésta, excepto aquéllos que pertenecen privativamente al ofensor, o a la persona de otro o para causarle grave daño emocional, incurrirá en delito grave de cuarto grado en su mitad superior."),
    list(word = "Maltrato Agravado (Artículo 3.2)", definition = HTML("Se impondrá pena correspondiente a delito grave de tercer grado en su mitad inferior cuando en la persona del cónyuge, ex cónyuge o de la persona con quien se cohabita o se haya cohabitado, o con quien se sostiene o haya sostenido una relación consensual, o con quien se haya procreado un hijo o hija, independientemente del sexo, estado civil, orientación sexual, identidad de género o estatus migratorio de cualquiera de las personas involucradas en la relación, se incurriere en maltrato según tipificado en esta Ley, mediando una o más de las circunstancias siguientes:<br>
    <ul>
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
    list(word = "Maltrato Por Amenaza (Artículo 3.3)", definition = "Toda persona que amenaza con causarle daño a su cónyuge, ex cónyuge, a la persona con quien cohabita o con quien haya cohabitado o con quien sostiene o haya 
    sostenido una relación consensual, o la persona con quien haya procreado un hijo o hija, independientemente del sexo, estado civil, orientación sexual, identidad de género o 
    estatus migratorio de cualquiera de las personas involucradas en la relación, a los bienes apreciados por ésta, excepto aquéllos que pertenecen privativamente al ofensor, o a la persona de otro, incurrirá en delito grave de cuarto grado en su mitad superior."),
    list(word = "Maltrato Mediante Restricción de la Libertad (Artículo 3.4)", definition = "Maltrato mediante restricción de la libertad. Toda persona que utilice violencia o intimidación en la persona de su cónyuge, ex cónyuge, de la persona con 
    quien cohabita o haya cohabitado, o con quien sostiene o haya sostenido una relación consensual, o la persona con quien haya procreado un hijo o hija, independientemente del 
    sexo, estado civil, orientación sexual, identidad de género o estatus migratorio de cualquiera de las personas involucradas en la relación, o que utilice pretexto de que 
    padece o de que una de las personas antes mencionadas padece de enfermedad o defecto mental, para restringir su libertad con el conocimiento de la víctima, incurrirá en delito grave de tercer grado en su mitad inferior."),
    list(word = "Agresión Sexual Conyugal (Artículo 3.5)", definition = HTML("Se impondrá pena de reclusión, según se dispone más adelante, a toda persona que incurra en una relación sexual no consentida 
    con su cónyuge o ex cónyuge, o con la persona con quien cohabite o haya cohabitado, o con quien sostuviere o haya sostenido una relación consensual, o la persona con quien 
    haya procreado un hijo o hija, independientemente del sexo, estado civil, orientación sexual, identidad de género o estatus migratorio de cualquiera de las personas 
    involucradas en la relación, en cualesquiera de las circunstancias siguientes:
              <ul>          
                <li> Si se ha compelido a incurrir en relación sexual mediante el empleo de fuerza, violencia, intimidación o amenaza de grave e inmediato daño corporal; o
                <li> Si se ha anulado o disminuido sustancialmente, sin su conocimiento o sin su consentimiento, su capacidad de consentir, a través de medios hipnóticos, narcóticos, deprimentes o estimulantes o sustancias o medios similares; o
                <li> Si por enfermedad o incapacidad mental, temporal o permanente, la víctima está incapacitada para comprender la naturaleza del acto en el momento de su realización;
                <li> Si se le obliga o induce mediante maltrato, violencia física o psicológica a participar o involucrarse en una relación sexual no deseada con terceras personas.
              </ul>"))
  )
  
  output$definitionCards_just <- renderUI({
    definitionCards(definitions_just)
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
  
  
  #### Tab de Definiciones ####
  
  definitions_avp <- list(
    list(word = "Vivienda Pública", definition = "Vivienda que es proporcionada, administrada o subsidiada por el gobierno o entidades gubernamentales con el objetivo de brindar alojamiento a personas o familias que tienen dificultades para acceder a una vivienda adecuada en el mercado privado debido a limitaciones económicas o sociales. Estas viviendas suelen estar dirigidas a personas de bajos ingresos, familias en situación de pobreza, personas sin hogar, o aquellos que enfrentan otras formas de vulnerabilidad social."),
    list(word = "Violencia Doméstica", definition = "Cuando una persona emplea fuerza física o violencia psicológica, intimidación o persecución en contra de su pareja o expareja. Esto, para causarle daño físico a su persona, a sus bienes, a otra persona o a un animal de servicio o mascota o para causarle grave daño emocional. Para que se considere violencia doméstica es necesario que exista o haya existido una relación afectiva entre las partes. Es decir, se da cuando la persona agresora es cónyuge, excónyuge, una persona con quien vive o ha vivido, con quien sostiene o haya sostenido una relación consensual o una persona con quien se haya procreado una hija o un hijo."),
    list(word = "Región", definition = "Se refiere a una división geográfica o área delimitada que comparte características similares, ya sea geográficas, culturales, económicas, políticas o administrativas. Subdivisión territorial establecida por las autoridades gubernamentales para propósitos de administración y gestión local. Estas divisiones pueden variar en tamaño y alcance dependiendo del país y su estructura administrativa.")
  )
  
  output$definitionCards_avp <- renderUI({
    definitionCards(definitions_avp)
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
                       paste("Cantidad de mujeres desaparecidas por estatus (localizadas y por localizar)"),
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
  #### Tab de Definiciones ####
  
  definitions_poli <- list(
    list(word = "Adultas Desaparecidas", definition = "Mujeres adultas cuya ubicación y paradero son desconocidos y no pueden ser determinados por sus familiares, amigos, o autoridades competentes. Esta situación puede surgir por una variedad de razones, que van desde accidentes, secuestros, desastres naturales, hasta decisiones voluntarias de la persona de abandonar su entorno sin dejar rastro."),
    list(word = "Adultas Localizadas", definition = "Mujeres adultas cuyo paradero ha sido identificado y confirmado después de que se les haya reportado como desaparecidas."),
    list(word = "Adultas Sin Localizar", definition = "Mujeres adultas cuyo paradero no ha sido identificado ni confirmado después de haber sido reportadas como desaparecidas. Estas personas pueden haber sido vistas por última vez en circunstancias desconocidas, y su ubicación actual sigue siendo un misterio."),
    list(word = "Menores Desaparecidas", definition = "Menores femeninas cuya ubicación y paradero son desconocidos y no pueden ser determinados por sus familiares, amigos, o autoridades competentes. Esta situación puede surgir por una variedad de razones, que van desde accidentes, secuestros, desastres naturales, hasta decisiones voluntarias de la persona de abandonar su entorno sin dejar rastro."),
    list(word = "Menores Localizadas", definition = "Menores femeninas cuyo paradero ha sido identificado y confirmado después de que se les haya reportado como desaparecidos."),
    list(word = "Menores sin Localizar", definition = "Menores femeninas cuyo paradero no ha sido identificado ni confirmado después de haber sido reportados como desaparecidos. Estas personas pueden haber sido vistas por última vez en circunstancias desconocidas, y su ubicación actual sigue siendo un misterio."),
    list(word = "Incidencia", definition = "Se refiere al número de incidentes o delitos que han sido reportados o registrados por las fuerzas policiales durante un período de tiempo específico en una determinada área geográfica. Esta métrica es utilizada por las autoridades policiales y otros organismos encargados de hacer cumplir la ley para medir y analizar la cantidad y el tipo de delitos que ocurren en una comunidad o jurisdicción particular."),
    list(word = "Violencia doméstica", definition = "Cuando una persona emplea fuerza física o violencia psicológica, intimidación o persecución en contra de su pareja o expareja. Esto, para causarle daño físico a su persona, a sus bienes, a otra persona o a un animal de servicio o mascota o para causarle grave daño emocional. Para que se considere violencia doméstica es necesario que exista o haya existido una relación afectiva entre las partes. Es decir, se da cuando la persona agresora es cónyuge, ex cónyuge, una persona con quien vive o ha vivido, con quien sostiene o haya sostenido una relación consensual o una persona con quien se haya procreado una hija o un hijo."),
    list(word = "Víctima", definition = "Persona que ha sufrido daño físico, emocional, psicológico o financiero como resultado de un acto delictivo, un accidente, un desastre natural, o cualquier otro evento traumático."),
    list(word = "Región Policiaca", definition = "Región o zona geográfica específica que está asignada a un cuerpo de policía o agencia de aplicación de la ley para llevar a cabo funciones de vigilancia, patrullaje y protección del orden público. Estas áreas son delimitadas y definidas por las autoridades policiales con el objetivo de organizar y distribuir eficazmente los recursos policiales para atender las necesidades de seguridad y aplicación de la ley en una comunidad o jurisdicción determinada.")
  )
  
  output$definitionCards_poli <- renderUI({
    definitionCards(definitions_poli)
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
  
  # Colores del status
  #despDF_fill_categoria <- setColorFill(despDF, "Categoria")
  # Grafico de barras
  output$barPlot_opm_opmFemiVD <- renderPlotly({
    p <- renderLinePlot(data = opmFemiVD_filt, x = "Año", y = "`Tasa (x100,000 mujeres)`", group = "1",
                        color = "1", title = "Tasa de Asesinatos a lo largo de los Años",
                        xlab = "Año", ylab = "Tasa de Asesinatos por 100,000 mujeres")
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
  
  #### Tab de Definiciones ####
  
  definitions_opm <- list(
    list(word = "Feminicidios", definition = HTML("Es el crimen que consiste en matar intencionalmente a mujeres por el hecho de ser mujeres o de identificarse como tales. Las definiciones más amplias incluyen cualquier asesinato de mujeres o niñas, o el asesinato involuntario o indirecto de mujeres o niñas, «tal como demuestran algunos casos de violencia doméstica que podrían provocar la muerte de mujeres». El concepto «adquirió importancia en el marco del movimiento feminista de la década de 1970 cuando surge la expresión ‘femicidio’ como alternativa al término neutro ‘homicidio’, con el fin de reconocer y visibilizar la opresión, la desigualdad y la violencia sistemática» contra las mujeres que, en su forma más extrema, culmina en la muerte. El femicidio puede presentarse bajo diferentes formas e incluye los siguientes actos:
            <ul>          
            <li> Femicidio íntimo, perpetrado por una pareja actual o anterior, generalmente durante o después de una relación ya violenta (por ejemplo, de violencia doméstica o violencia sexual);
            <li> El llamado crimen de honor (o asesinato de o por honor); 
            <li> El femicidio relacionado con la dote, que ocurre en un contexto de conflicto entre las familias de dos cónyuges recién casados, y es generalmente cometido por la familia política que cuestiona sobre todo el importe de la dote;
            <li> El femicidio no íntimo, cometido por un agresor que no tiene una relación íntima con la víctima, que está muy difundido en algunas partes de América Latina y que, por lo general, está precedido de actos de violencia sexual o tortura.;
            </ul>")),
    list(word = "Tendencia", definition = "Dirección o patrón observado en datos o eventos que muestra una inclinación hacia cierto resultado o comportamiento a lo largo del tiempo."),
    list(word = "Acecho (A)", definition = "Es una persona, en la mayoría de las ocasiones mujer que sufre o es sometida a un patrón o la repetición de una conducta mediante la cual se mantiene de manera constante o repetida una vigilancia, proximidad física o visual sobre una persona específica."),
    list(word = "Agresión sexual (AS)", definition = "Cualquier acto que degrada o daña el cuerpo y/o la sexualidad de la víctima y que por tanto atenta contra su libertad, dignidad e integridad física. Es una expresión de abuso de poder que implica la supremacía masculina sobre la mujer, al denigrar y concebirla como objeto."),
    list(word = "Discrimen de género (DG)", definition = "Hace referencia a «toda distinción, exclusión o restricción» que se realice en función del género de una persona con el objetivo o resultado de menoscabar o anular el reconocimiento, goce o ejercicio de sus derechos humanos. A menudo es consecuencia de los mitos y estereotipos de género tales como: las mujeres son las más aptas para ocuparse de la educación de los hijos, cocinar o limpiar, o para realizar trabajos de secretaría, enseñanza o enfermería, mientras que los hombres son líderes, buenos en economía y comercio. Esto ha dado lugar a un acceso desigual al mercado laboral, así como a un salario desigual para puestos similares, al sostenerse que las mujeres tienen peores resultados que los hombres en determinados ámbitos y, con ello, a una discriminación por género."),
    list(word = "Violencia doméstica (VD)", definition = "Definición que ofrece la Ley Núm. 54 de 1989 que sigue vigente en Puerto Rico. Violencia doméstica significa un patrón constante de empleo de fuerza física o violencia psicológica, intimidación o persecución contra una persona por parte de su cónyuge, ex cónyuge, una persona con quien cohabita o haya cohabitado, con quien sostiene o haya sostenido una relación consensual o una persona con quien se haya procreado una hija o hijo, para causarle daño físico a su persona, sus bienes u otra persona o para causarle grave daño emocional. La Ley Núm. 54 incluyó además como delito la agresión sexual entre personas que cohabitan o matrimonios como violencia doméstica."),
    list(word = "Violencia en cita (VC)", definition = "Violencia cometida por una persona que está o ha estado en una relación social de carácter romántico o íntimo con la víctima. La existencia de dicha relación se determinará con base en la declaración de la parte informante y teniendo en cuenta la duración de la relación, el tipo de relación y la frecuencia de interacción entre las personas involucradas en la relación. A los efectos de esta definición: La violencia en el noviazgo incluye, pero no se limita a, abuso sexual o físico o la amenaza de tal abuso. La violencia en el noviazgo no incluye actos cubiertos por la definición de violencia doméstica."),
    list(word = "Orientaciones", definition = "Direcciones o inclinaciones hacia las que se dirige o enfoca algo."),
    list(word = "CRIAS", definition = "Centro de Respuesta Integrada de Apoyo y Servicios para la Mujer. La misma surgió de la necesidad imperante de trabajar con el problema de la desigualdad que existe contra las mujeres y trabajar particularmente con la violencia doméstica desde una perspectiva dirigida hacia la validación, orientación y coordinación de servicios de apoyo. El Centro CRIAS establece las bases para un modelo de prevención, intervención y fiscalización de los diferentes tipos de violencia que nos permite levantar información de las víctimas sobrevivientes participantes, obtener análisis de experiencias personales y manejo de actitudes ante el problema. En el mismo, se ofrecen servicios de orientación, coordinación de servicios y referidos a mujeres víctimas/sobrevivientes de violencia doméstica, agresión sexual, acecho y otras manifestaciones de violencia por razón de género."),
    list(word = "Identidad de género", definition = "Se refiere a la manera en que una persona se identifica, cómo se reconoce o se expresa sobre sí misma, en cuanto al género que puede corresponder o no a su sexo biológico o asignado en su nacimiento."),
    list(word = ":", definition = "Término utilizado para describir características, atributos o cualidades asociadas tradicionalmente con los hombres o lo que se considera típicamente propio del género masculino."),
    list(word = "Masculino", definition = "Término utilizado para describir características, atributos o cualidades asociadas tradicionalmente con los hombres o lo que se considera típicamente propio del género masculino."),
    list(word = "Femenino", definition = "Se refiere a características, atributos o cualidades asociadas tradicionalmente con las mujeres o lo que se considera típicamente propio del género femenino."),
    list(word = "Trans", definition = "Abreviatura comúnmente utilizada para referirse a personas que son transgénero o que tienen una identidad de género diferente de aquella que se les asignó al nacer. Las personas transgénero pueden identificarse como hombre, mujer, ambos, ninguno o con un género diferente al binario tradicional de hombre y mujer.")
    )
  
  output$definitionCards_opm <- renderUI({
    definitionCards(definitions_opm)
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
                       title = "Estado de investigación de casos de supervisión de Ley 54: Programas Alternos de Comunidad",
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
    p <- ggplot(dcrSentenciadas_filt(), aes(x = fecha, y = cantidad, fill = tipo)) +
      geom_bar(stat = "identity", position = position_dodge2(width = 1, padding = 0.1)) +
      scale_fill_manual(values = dcrSentenciadas_fill_tipo) +
      scale_y_continuous(labels = scales::comma_format(big.mark = ",", decimal.mark = ".")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, margin = margin(t = 10))) + 
      labs(title = "Personas sentenciadas en programa de supervisión Electrónica por delitos de violencia doméstica por estado del caso",
           x = "Año", y = "Cantidad de personas sentenciadas", fill = "Estado del Caso")
    
    ggplotly(p, 
             tooltip = c("fill", "x", "y"))
  })
  
  
  # Data Table para dcrSentenciadas
  output$dataTable_dcr_dcrSentenciadas <- renderDT({
    renderDataTable(dcrSentenciadas_filt())
  })
  #### Tab de Definiciones ####
  
  definitions_dcr <- list(
    list(word = "Ley 54", definition = "Ley Núm. 54-1989, conocida como la “Ley para la Prevención e Intervención con la Violencia Doméstica”, según enmendada, establece la violencia doméstica como delito y lo define como el empleo de fuerza física o violencia psicológica, intimidación o persecución en contra de su pareja o expareja. Esto, para causarle daño físico a su persona, a sus bienes o a otra persona o para causarle grave daño emocional."),
    list(word = "Programas de Comunidad", definition = "Son programas de tratamientos establecidos para que las personas convictas cumplan parte de su sentencia fuera de la institución penal. Su finalidad es promover que los convictos que estén capacitados para reintegrarse a la sociedad puedan hacerlo como parte de su rehabilitación moral y socia"),
    list(word = "Investigaciones realizadas", definition = "Proceso sistemático y metódico de recopilación, análisis y evaluación de información con el objetivo de obtener conclusiones, resolver problemas o generar conocimiento en un campo específico."),
    list(word = "Personas sentenciadas", definition = "Pronunciamiento que hace el juez o la jueza sobre la pena que se le impone a una persona acusada luego de que se determina que es culpable de cometer un delito."),
    list(word = "Programa de Supervisión Electrónica", definition = "El Programa de Monitoreo Electrónico cuenta con la Unidad Especializada de Monitoreo Electrónico (Unidad) compuesta por Oficiales Correccionales, la cual tiene la responsabilidad de supervisar y monitorear a los participantes pertenecientes al programa. Esta supervisión conlleva el verificar y atender las alertas que se activan a través del sistema de transmisión electrónica, activar el protocolo, solicitar apoyo inter agencial, avisar a la víctima y administrar pruebas toxicológicas, entre otras.")
  )
  
  
  output$definitionCards_dcr <- renderUI({
    definitionCards(definitions_dcr)
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
                       title = "Órdenes de protección emitidas según la Ley 148 en Casos de Violencia Sexual, por Región Judicial y Tipo de Delito",
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
  
  #### Tab de Definiciones ####
  
  definitions_trib <- list(
    list(word = "Organización", definition = "Poder Judicial de Puerto Rico, Oficina de Administración de los Tribunales, Directoria de Operaciones, Oficina de Estadísticas."),
    list(word = "Orden de Protección", definition = "Es un remedio civil expedido por escrito bajo el sello de un Tribunal, en la cual se dictan las medidas a una persona agresora para que ésta se abstenga de incurrir o llevar a cabo determinados actos o conducta constitutivos de violencia doméstica."),
    list(word = "Solicitudes de órdenes de protección", definition = "Se define como todas las peticiones de orden de protección realizadas en el periodo de tiempo de interés en la región judicial especificada."),
    list(word = "Ley 148", definition = "Conocida como la “Ley para la Protección de las Víctimas de Violencia Sexual en Puerto Rico”, según enmendada, establece los mecanismos para la expedición de órdenes de protección para víctimas de los delitos de agresión sexual, actos lascivos, acoso sexual e incesto."),
    list(word = "Ley 54", definition = "La Ley Núm. 54-1989, conocida como la “Ley para la Prevención e Intervención con la Violencia Doméstica”, según enmendada, establece la violencia doméstica como delito y lo define como el empleo de fuerza física o violencia psicológica, intimidación o persecución en contra de su pareja o expareja. Esto, para causarle daño físico a su persona, a sus bienes o a otra persona o para causarle grave daño emocional"),
    list(word = "Violencia Sexual", definition = "Cualquier acto que degrada o daña el cuerpo y/o la sexualidad de la víctima y que por tanto atenta contra su libertad, dignidad e integridad física. Es una expresión de abuso de poder que implica la supremacía masculina sobre la mujer, al denigrar y concebirla como objeto."),
    list(word = "Región Judicial", definition = "Se refiere a la región judicial a la que corresponden los datos informados. El Tribunal de Primera Instancia se distribuye territorialmente en trece regiones judiciales. Cada región judicial está compuesta por un centro judicial y sus respectivas salas superiores y municipales."),
    list(word = "Peticionaria", definition = "Persona que solicita una orden de protección."),
    list(word = "Sexo", definition = "Indica si la persona que solicita la orden de protección, en el periodo de tiempo de interés en la región judicial especificada, se identifica como hombre o mujer."),
    list(word = "Año Fiscal", definition = "Período de 12 meses comprendido entre el 1ro de julio de un año y el 30 de junio del año siguiente, y que se usa como el calendario presupuestario de las agencias públicas."),
    list(word = "Órdenes de protección ex parte", definition = "Es una orden emitida por el Tribunal de Primera Instancia luego de escuchar a la parte peticionaria (persona que solicita la orden) y hacer una determinación provisional sobre los hechos.")
  )
  
  output$definitionCards_trib <- renderUI({
    definitionCards(definitions_trib)
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
                       title = HTML("Tendencia anual del equipo de <i>SAFE Kits</i> en casos de violencia sexual por estado de querella"), 
                       xlab = "Año", ylab = "Total de kits distribuidos", fillLab = "Estado de querella", 
                       colorFill = safekitsDF_fill_Kits)
    ggplotly(p, 
             tooltip = c("fill", "x", "y"))
  })
  
  
  # Data Table 
  output$dataTable_safekitsDF <- renderDT({
    renderDataTable(safekitsDF_filt())
  })
  
  #### Tab de Definiciones ####
  
  definitions_cavv <- list(
    list(word = "Violencia Sexual", definition = "Cualquier acto que degrada o daña el cuerpo y/o la sexualidad de la víctima y que por tanto atenta contra su libertad, dignidad e integridad física. Es una expresión de abuso de poder que implica la supremacía masculina sobre la mujer, al denigrar y concebirla como objeto."),
    list(word = "Agresión Sexual", definition = HTML("El Código Penal de Puerto Rico define el delito de agresión sexual como llevar a cabo, o provocar que otra persona lleve a cabo, un acto orogenital o una penetración sexual (vaginal o anal, ya sea esta genital, digital o instrumental) en cualquiera de estas circunstancias:
          <ul>          
          <li> Si a la víctima se le disminuyó, sin esta consentir o sin saberlo, su capacidad de consentir mediante algún medio hipnótico, narcótico, deprimente o estimulante.; 
          <li> Si a la víctima se le obligó al acto por medio de fuerza física, violencia o intimidación.; 
          <li> Si al momento del acto la víctima no tenía capacidad para consentir y la persona agresora lo sabía.; 
          <li> Si la víctima consintió porque se le engañó sobre la identidad de la persona agresora y creía que era otra persona.; 
          <li> Si la víctima no ha cumplido 16 años de edad.; 
          <li> Si por enfermedad o incapacidad mental la víctima no puede comprender el acto en el momento en que ocurre.;
          </ul>")),
    list(word = "Actos Lascivos", definition = "El Código Penal de Puerto Rico define el delito de actos lascivos como aquel en el cual, sin intentar consumar el delito de agresión sexual, se someta a otra persona a un acto que tienda a despertar, excitar o satisfacer la pasión o deseos sexuales de la persona imputada."),
    list(word = "Kits", definition = "Equipo utilizado por profesionales de la salud para la recuperación de evidencia forense en casos de violencia sexual. Consiste en una caja que contiene instrucciones y materiales que facilitan la recoleccion de evidencia. También conocido como <i>Sexual Assault Kit</i> o, por sus siglas en inglés, <i>SAK</i>. "),
    list(word = "Kit con Querella", definition = "Kit de recolección de evidencia forense que se acompaña con la querella radicada por la víctima sobreviviente de violencia sexual. Cuenta con un número otorgado por el NPPR."),
    list(word = "Kit sin Querella", definition = "Kit de recolección de evidencia forense que no se acompaña con una querella, pues la víctima sobreviviente de violencia sexual no ha radicado querella en el NPPR."),
    list(word = "Querella", definition = "Mecanismo que tiene disponible una persona para reportar a la policía un incidente que entiende debe ser investigado por ésta. El incidente puede tratarse de uno que involucre un delito, una infracción, una persona desaparecida, entre otros."),
    list(word = "Tendencia", definition = "Dirección o patrón observado en datos o eventos que muestra una inclinación hacia cierto resultado o comportamiento a lo largo del tiempo."),
    list(word = "Evidencia", definition = "Se refiere a cualquier información, datos, pruebas o testimonios que respaldan una afirmación, teoría o argumento. En diferentes contextos, la evidencia puede ser utilizada para respaldar conclusiones científicas, legales, filosóficas o incluso personales. La calidad y fiabilidad de la evidencia pueden variar según la fuente, el método de recopilación y el contexto en el que se utilice. En general, se busca que la evidencia sea objetiva, verificable y relevante para el tema en cuestión.")
  )
  
  output$definitionCards_cavv <- renderUI({
    definitionCards(definitions_cavv)
  })

  
  #### Lógica para el web hosting en Shiny.io ####
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderText({
    reactiveValuesToList(res_auth)
  })
}
