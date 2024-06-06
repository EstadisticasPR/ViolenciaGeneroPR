# User Interface 
cat("Loading User Interface from ui.R...\n")
source("global.R")
# Importar funciones auxiliares de ui_helpers.R
#addResourcePath("www", "R/www/")
ui <- 
  #secure_app(
  fluidPage(
  ### El theme (colores) de la app ###
  theme = shinytheme("sandstone"),
  
  #### Encabezado ####
  div(
    tags$div(
      tags$title('Instituto de Estadísticas de Puerto Rico'),
      
      tags$ul(
        ### Foto con enlace a la página de IEPR ###
        embedImage("logo_IEPR", "iepr_logo.png", "https://estadisticas.pr/", "estadisticas.pr"),
        
        ### Foto con enlace a la página de PARE ###
        embedImage("logo_PARE", "logo_PARE.png", "https://parelaviolencia.pr.gov/", "PARE.gov"),
        
        ### Título de la App ###
        tags$li(
          style = 'display: inline-block;margin-bottom: 12px;margin-right: 25px;',
          div(
            tags$h3(
              'Estadísticas de Violencia de Género en Puerto Rico',
              style = 'font-family:"Arial Black",sans-serif;'
            )
          )
        ),
        
        style = 'list-style-type: none;
      display: flex;
      flex-wrap: wrap;
      margin: 0px;
      justify-content: space-between;
      background: pink;
      margin: 0px;
      padding-bottom: 10;
      border-radius: 0;'
      )
    ),
    style = "top: 1px;"
  ),
  
  # Titulo de la app
  navbarPage(
    
    # elimina el espacio vacío del título
    tags$style(HTML(".navbar-brand { display: none; } .navbar { min-height: 0; padding-top: 0; padding-bottom: 0; }")),
    
    #### Tab del Sistema de Notificación de Muertes Violentas ####
    tabPanel(
      lowercaseTitle("Sistema de Notificación de Muertes Violentas"),
      icon = icon("exclamation-triangle"),
      tabsetPanel(
        
        #### tab con datos de Homicidios por grupo de Edad ####
        tabPanel(
          lowercaseTitle("Homicidios de mujeres por edad"),
          # Título del Tab
          titlePanel("Homicidios de mujeres por grupo de edad según el año"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Instituto de Estadísticas, Sistema de Notificación de Muertes Violentas"), tags$br(),

          #tags$span(paste0("Actualización Datos: ", actualizacion_snmvB)),
          #tags$span(paste0("Actualización Sistema: ", actualizacion_sistema)),
          

          tags$span(paste0("Actualizado: ", actualizacion_snmvB)),

          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              
              # botón checkbox para seleccionar grupos de edad 
              createDropdownCheckbox(
                label = "Seleccione Grupo(s) de Edad:",
                choices = homiEdad$edad,
                selected = 8,
                id = "snmv_homiEdad_edad"
              ),
              # botón checkbox para seleccionar año
              createDropdownCheckbox(
                label = "Seleccione Año(s):",
                choices = homiEdad$año,
                selected = NULL,
                id = "snmv_homiEdad_año"
              ),
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              #plotlyOutput("linePlot_snmv"),
              plotlyOutput("barPlot_snmv"),
              DTOutput("dataTable_snmv")
            )
          )
        ),
        
        #### tab con datos de Incidentes segun el año ####
        tabPanel(
          lowercaseTitle("Tipos de incidentes violentos"),
          
          # Título del Tab
          titlePanel("Número de incidentes violentos por tipo para ambos sexos"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Instituto de Estadísticas, Sistema de Notificación de Muertes Violentas"), tags$br(),

          #tags$span(paste0("Actualización Datos: ", actualizacion_snmvA)),
          #tags$span(paste0("Actualización Sistema: ", actualizacion_sistema)),
          
          

          tags$span(paste0("Actualizado: ", actualizacion_snmvA)),

          
          sidebarLayout(
            sidebarPanel(
              
              # botón para seleccionar tipo de incidente
              createDropdownCheckbox(
                label = "Seleccione Tipo(s) de Incidente:",
                choices = inci$tipo,
                selected = 7,
                id = "snmv_inci_tipo"
              ),
              
              # botón para seleccionar año
              createDropdownCheckbox(
                label = "Seleccione Año(s):",
                choices = inci$año,
                selected = NULL,
                id = "snmv_inci_año"
              ),
              
              hr()
            ),
            
            # Sección principal con los gráficos y datatable
            mainPanel(
              plotlyOutput("barPlot_snmv_inci"),
              DTOutput("dataTable_snmv_inci")
            )
          ),
        ), 
        
        #### tab con datos de Tasas ####
        # tabPanel(
        #   "Tasas_snmv"
        # ), 
        
        #### tab de Definiciones y Metadatos ####
        tabPanel(
          lowercaseTitle("Definiciones y Metadatos"),
                    br(),
                    tags$ul(sectionTitle("Sistema de Notificación de Muertes Violentas:", "20px")),
                    tags$ul(
                      tags$li(HTML("<b>Violencia:</b> El uso intencional de la fuerza o el poder físico, de hecho, o como amenaza, contra uno mismo, otra persona o un grupo o comunidad, que cause o tenga gran probabilidad de causar lesiones, muerte, daños psicológicos, trastornos del desarrollo o privaciones.")),
                      tags$li(HTML("<b>Muertes violentas :</b> PRVDRS define una muerte violenta como: Suicidio (incluyendo terrorismo), Homicidio (incluyendo terrorismo), Intervención legal (excluyendo terrorismo, ejecuciones por pena de muerte, o actos de guerra), Eventos con intención no Determinada, Heridas por arma de fuego no intencional.")), 
                      tags$li(HTML("<b>Homicidio:</b> Es un delito de resultado por el cual la conducta intencionada de un sujeto provoca la muerte de otro.")), 
                      tags$li(HTML("<b>Suicidio Único:</b> Se refiere a un acto en el cual una persona termina deliberadamente con su propia vida, sin la participación o implicación de otras personas en el proceso. Este término implica que el individuo toma la decisión y ejecuta el acto suicida de manera independiente, sin ningún intento de causar la muerte de otras personas o de involucrar a terceros en el evento. ")), 
                      tags$li(HTML("<b>Homicidio múltiple:</b>Se refiere a un acto en el cual una persona causa la muerte de múltiples individuos en un solo incidente.")), 
                      tags$li(HTML("<b>Homicidio seguido de suicidio:</b> Se refiere a un acto en el cual una persona causa la muerte de otra(s) y luego se quita la vida. Este tipo de incidente implica dos acciones distintas pero relacionadas: primero, el homicida comete el acto de matar a otra persona, y luego, la misma persona toma su propia vida.")), 
                      tags$li(HTML("<b>Homicidio único:</b> Se refiere a un acto en el cual una persona causa la muerte de otra en un evento específico. ")), 
                      tags$li(HTML("<b>Homicidio(s) seguido de suicidio(s), (más de dos víctimas):</b> Se refiere a un acto extremadamente trágico en el cual una o más personas causan la muerte de múltiples individuos antes de acabar con sus propias vidas en un solo evento")), 
                      tags$li(HTML("<b>Muerte accidental por arma de fuego:</b> Evento en el cual una persona pierde la vida como resultado involuntario de la manipulación, manejo o uso incorrecto de un arma de fuego. Este tipo de incidente ocurre cuando un individuo dispara un arma de manera accidental, ya sea debido a un descuido, falta de conocimiento sobre el funcionamiento seguro del arma, o por la manipulación indebida de la misma. ")), 
                      tags$li(HTML("<b>Muerte no determinada:</b> Caso en el cual las circunstancias que rodean el fallecimiento de una persona no pueden ser claramente establecidas o comprendidas mediante la evidencia disponible en el momento de la investigación. Este término se utiliza cuando no hay suficiente información o evidencia forense para determinar con certeza si la muerte fue el resultado de causas naturales, accidentales, suicidas u homicidas."))
                    )
        ),
        
        # ### xiomy
        # tabPanel(
        #   "Xiomy",  # Cambiar por el nombre del tercer dfDeli
        #   # ... Estructura similar a la anterior para el tercer dfDeli
        #   tags$div(
        #     tags$h2("xiomara"),
        #     tags$img(src = "xiomy.jpg", height = 1000, width = 1000)
        #   )
        # ),
      )
    ),
    
    #### Tab del Departamento de la Familia ####
    tabPanel(
      lowercaseTitle("Departamento de la Familia"),
      icon = icon("users"),
      tabsetPanel(
        #### tab con datos de menores víctimas de maltrato (dfMalt)  ####
        tabPanel(
          #HTML("<span style='text-transform: none; font-size: 14px;'>Maltrato de Menores por Sexo</span>"), 
          lowercaseTitle("Maltrato de menores por sexo"),
          # Título del Tab
          titlePanel("Cantidad de menores que fueron víctimas de maltrato por sexo y tipo de maltrato"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Departamento de la Familia"), tags$br(),
          #tags$span("Actualización Datos: ", actualizacion_familia), tags$br(),
          #tags$span(paste0("Actualización Sistema: ", actualizacion_sistema)),
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              
              # botón para seleccionar tipo de maltrato
              createDropdownCheckbox(
                label = "Seleccione Tipo(s) de Maltrato:",
                choices = dfMalt$Maltrato,
                selected = 1,
                id = "fam_dfMalt_tipo"
              ),
              
              # botón para seleccionar año
              createDropdownCheckbox(
                label = "Seleccione Año(s):",
                choices = dfMalt$Año,
                selected = NULL,
                id = "fam_dfMalt_año"
              ),
              customSeparator(),
              # botón para seleccionar sexo
              createDropdownCheckbox(
                label = "Seleccione sexo de las víctimas:",
                choices = dfMalt$Sexo,
                selected = 1,
                id = "fam_dfMalt_sexo"
              ),
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              # aglomerar masculino y femenino
              #plotlyOutput("linePlot_fam"),
              plotlyOutput("barPlot_fam"),
              DTOutput("dataTable_fam")
            )
          ),
          tags$span("NOTA IMPORTANTE: Las cantidades podrían representar conteos duplicados del o la menor. Esto es, debido a que el menor se cuenta cada vez que él o ella son parte de uno o múltiples referidos. Este conteo es conocido por el Departamento de la Familia como el pareo de menores-reportado. Estos datos están en proceso de revisión por el Instituto de Estadísticas de Puerto Rico. *Nota: Datos parciales y preliminares del año 2022. Están disponibles hasta noviembre de 2022."),
        ),
        
        #### tab de Definiciones y Metadatos ####
        tabPanel(
          lowercaseTitle("Definiciones y Metadatos"),
          br(),
          tags$ul(sectionTitle("Departamento de la Familia:", "20px")),
          tags$ul(
            tags$li(HTML("<b>Menores:</b> Individuos que se encuentran bajo la edad legal de mayoría de edad en un contexto específico, lo que generalmente implica que aún no han alcanzado la edad en la que se les considera plenamente responsables de sus acciones según la ley.")),
            tags$li(HTML("<b>Maltrato a Menores:</b> Toda acción u omisión intencional del padre, de la madre o persona responsable del o de la menor que ocasione o ponga en riesgo de sufrir un daño o perjuicio a su salud e integridad física, mental o emocional.")), 
            tags$li(HTML("<b>Abuso Sexual:</b> Incurrir en conducta sexual en presencia de un o una menor o que se utilice, voluntaria o involuntariamente, para ejecutar conducta sexual dirigida a satisfacer los deseos sexuales. También se considera cualquier acto que, de procesarse por la vía criminal, configuraría cualesquiera de varios delitos de índole sexual, tales como agresión sexual, actos lascivos, comercio de personas para actos sexuales, exposiciones obscenas, proposición obscena, producción de pornografía infantil, entre otros delitos reconocidos en el Código Penal de Puerto Rico.")), 
            tags$li(HTML("<b>Explotación:</b> Conducta obscena o utilización de una persona menor de edad para ejecutar conducta obscena. explotación de una persona menor de edad o que se permita que otra persona lo explote, incluyendo –pero sin limitarse– a utilizarla para ejecutar conducta obscena, con el fin de lucrarse o de recibir algún otro beneficio.")), 
            tags$li(HTML("<b>Maltrato Físico:</b> Se refiere a cualquier trauma, lesión o condición no accidental, causada en un episodio o varios, incluyendo la falta de alimentos que, de no ser atendida, pone en riesgo la vida y salud de la persona menor de edad.")), 
            tags$li(HTML("<b>Negligencia:</b> Es un tipo de maltrato que consiste en no cumplir con la obligación de proveer a las personas menores de edad de manera adecuada los alimentos, ropa, albergue, educación, atención a su salud, poca supervisión, no visitar, ni mantener contacto con el o la menor o incurrir en alguna de las razones reconocidas en el Código Civil de Puerto Rico para que una persona sea privada de patria potestad entre otros.")), 
            tags$li(HTML("<b>Negligencia Educativa: </b> La negligencia institucional es cuando a una persona menor de edad, que está en un hogar de crianza, centro de cuidado sustituto o en una institución pública o privada, de cuido, educación, tratamiento o detención, se le cause daño o se ponga en riesgo de sufrir daño a su salud e integridad física, mental o emocional, incluyendo –pero sin limitarse– a abuso sexual. La negligencia institucional, ya sea conocida o que se sospeche que ocurre, o que ocurre como resultado de la política, prácticas y condiciones imperantes en la institución, la puede cometer:
            <ul>          
            <li>a. operador u operadora de un hogar de crianza; 
            <li>b. cualquier empleado, empleada, funcionario o funcionaria que ofrezca servicios de cuido o que tenga bajo su control o custodia a una persona menor de edad para su cuido, educación, tratamiento o detención;
            </ul>"
            )),
            tags$li(HTML("<b>Negligencia Emocional:</b> Se define como causar menoscabo o afectar la capacidad intelectual o emocional de la persona menor de edad dentro de lo que se considera normal para su edad y entorno cultural.")), 
            tags$li(HTML("<b>Negligencia Médica:</b> Situaciones en las que los proveedores de atención médica, como médicos, enfermeras u otros profesionales de la salud, no brindan el nivel adecuado de atención y cuidado a pacientes menores de edad, lo que resulta en daños físicos, emocionales o psicológicos para el paciente. Esto puede incluir errores en el diagnóstico, tratamiento inapropiado, falta de seguimiento adecuado, o cualquier otro acto u omisión que pueda considerarse una violación del estándar de cuidado aceptado en la práctica médica.")), 
            tags$li(HTML("<b>Trata Humana:</b> Se define como la captación, traslado, transporte, acogida o recepción de una persona utilizando la violencia, amenaza, engaño, rapto, fuerza, abuso de poder, abuso de una situación de vulnerabilidad u otros elementos de coacción, con el fin de someterla a explotación y lucrarse con su actividad. ")), 
            
          )
        )
      )
    ),
    
    #### Tab del Departamento de Justicia ####
    tabPanel(
      lowercaseTitle("Departamento de Justicia"),
      icon = icon("balance-scale"),
      tabsetPanel(
        #### tab con datos de delitos de violencia doméstica (dfDeli) ####
        tabPanel(
          lowercaseTitle("Delitos Ley 54 por Distrito"),
          # Título del Tab
          # pregunta si es Distrito Fiscal se llama Jurisdicción Fiscal y si los Casos se le llaman Delitos
          titlePanel("Número de casos radicados por Distrito Fiscal y Artículo de la Ley 54"),  # Cambiar por el título adecuado
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Departamento de Justicia" ),
          #tags$span(paste0("Actualización Datos:   ", actualizacion_justiciaA)), 
          #tags$span(paste0("Actualización Sistema: ", actualizacion_sistema)),
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              
              # botón para seleccionar delito
              createDropdownCheckbox(
                label = "Seleccione Artículo(s) de ley 54:",
                choices = dfDeli$Delito,
                selected = 2,
                id = "just_dfDeli_delito"
              ),
              
              # botón para seleccionar año
              createDropdownCheckbox(
                label = "Seleccionar Año:",
                choices = dfDeli$Año,
                selected = NULL,
                id = "just_dfDeli_año"
              ),
              customSeparator(),
              # botón para seleccionar distrito
              createDropdownCheckbox(
                label = "Seleccionar Distrito(s):",
                choices = dfDeli$`FISCALIA DISTRITO`,
                selected = NULL,
                id = "just_dfDeli_distrito"
              ),
              
            ),
            # Sección principal con los gráficos
            mainPanel(
              #plotlyOutput("boxPlot_just"),
              plotlyOutput("barPlot_just"),
              #plotlyOutput("map_just"),
              #plotlyOutput("facet_bartest"),
              # plotOutput("deliPlot_just"),
              DTOutput("dataTable_just")
            )
          )
        ),
        
        
        #### tab con datos del mapa delitos de violencia doméstica (mapaDeli) ####
        tabPanel(
          lowercaseTitle("Mapa de delitos Ley 54 por Distrito"),
          
          # Título del Tab 
          # pregunta si es Distrito Fiscal se llama Jurisdicción Fiscal y si los Casos se le llaman Delitos
          titlePanel("Número de casos radicados por Distrito Fiscal y Artículo de la Ley 54"),  # Cambiar por el título adecuado
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Departamento de Justicia" ), 
          #tags$span(paste0("Actualización Datos: ", actualizacion_justiciaA)),
          #tags$span(paste0("Actualización Sistema: ", actualizacion_sistema)), 
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              
              # botón para seleccionar delito
              # createDropdownCheckbox(
              #   label = "Seleccione el/los Delitos(s):",
              #   choices = mapaDeli$Delito,
              #   selected = 2,
              #   id = "just_mapaDeli_delito"
              # ),
              
              # botón para seleccionar delito
              selectInput("select_just_mapaDeli_delito", "Seleccione Artículo de la Ley 54:",
                          choices = levels(mapaDeli$Delito),
                          selected = 2),
              
              # createDropdownCheckbox(
              #   label = "Seleccionar Año:",
              #   choices = mapaDeli$Año,
              #   selected = NULL,
              #   id = "just_mapaDeli_año"
              # ),
              
              # botón para seleccionar año
              selectInput("select_just_mapaDeli_año", "Seleccione Año:",
                          choices = levels(mapaDeli$Año),
                          selected = 1)
              
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              plotlyOutput("map_just_mapaDeli"),
              DTOutput("dataTable_just_mapaDeli")
            )
          )
        ), 
        
        
        # Subtab con datos específicos para el segundo dfDeli de la agencia
        # tabPanel(
        #   "convic"  # Cambiar por el nombre del segundo dfDeli
        #   # ... Estructura similar a la anterior para el segundo dfDeli
        # ),
        
        #### tab de Definiciones y Metadatos ####
        tabPanel(
          lowercaseTitle("Definiciones y Metadatos"),
          br(),
          tags$ul(sectionTitle("Artículos de la Ley 54:", "20px")),
          tags$ul(
            tags$li(HTML("<b>Incumplimiento de órdenes de protección (Artículo 2.8):</b> Cualquier violación a sabiendas de una orden de protección expedida.")),
            tags$li(HTML("<b>Maltrato (Artículo 3.1):</b> Toda persona que empleare fuerza física o violencia psicológica,
intimidación o persecución en la persona de su cónyuge, ex cónyuge, o la persona con
quien cohabita o haya cohabitado, o la persona con quien sostuviere o haya sostenido una
relación consensual, o la persona con quien haya procreado un hijo o hija,
independientemente del sexo, estado civil, orientación sexual, identidad de género o
estatus migratorio de cualquiera de las personas involucradas en la relación, para causarle
daño físico a su persona, a los bienes apreciados por ésta, excepto aquéllos que
pertenecen privativamente al ofensor, o a la persona de otro o para causarle grave daño
emocional, incurrirá en delito grave de cuarto grado en su mitad superior.")),
            tags$li(HTML("
<b>Maltrato Agravado (Artículo 3.2):</b><br>
Se impondrá pena correspondiente a delito grave de tercer grado en su mitad inferior cuando en la persona del cónyuge, ex cónyuge o de la persona con quien se cohabita o se haya cohabitado, o con quien se sostiene o haya sostenido una relación consensual, o con quien se haya procreado un hijo o hija, independientemente del sexo, estado civil, orientación sexual, identidad de género o estatus migratorio de cualquiera de las personas involucradas en la relación, se incurriere en maltrato según tipificado en esta Ley, mediando una o más de las circunstancias siguientes:<br>
<ul>
  <li>a. Se penetrare en la morada de la persona o en el lugar donde esté albergada y se cometiere allí maltrato, en el caso de cónyuges o cohabitantes, independientemente del sexo, estado civil, orientación sexual, identidad de género o estatus migratorio de cualquiera de las personas involucradas en la relación, cuando éstos estuvieren separados o mediare una orden de protección ordenando el desalojo de la residencia a una de las partes; o</li>
  <li>b. cuando se infiriere grave daño corporal a la persona; o</li>
  <li>c. cuando se cometiere con arma mortífera en circunstancias que no revistiesen la intención de matar o mutilar; o</li>
  <li>d. cuando se cometiere en la presencia de menores de edad; o</li>
  <li>e. cuando se cometiere luego de mediar una orden de protección o resolución contra la persona acusada expedida en auxilio de la víctima del maltrato; o</li>
  <li>f. se indujere, incitare u obligare a la persona a drogarse con sustancias controladas, o cualquier otra sustancia o medio que altere la voluntad de la persona o a intoxicarse con bebidas embriagantes; o</li>
  <li>g. Cuando se cometiere y simultáneamente se incurriere en maltrato de un menor según definido en la Ley Núm. 177 de 1 de agosto de 2003.</li>
  <li>h. Si a la víctima se le obliga o induce mediante maltrato, violencia física o sicológica a participar o Involucrarse en una relación sexual no deseada con terceras personas.</li>
  <li>i. Cuando se cometiere contra una mujer embarazada.</li>
  <li>j. Cuando se cometiere contra una persona menor de dieciséis (16) años y la persona agresora sea de dieciocho (18) años o más.</li>
</ul>
"))
            
            ,
            tags$li(HTML("<b>Maltrato Por Amenaza (Artículo 3.3):</b> Toda persona que amenaza con causarle daño a su cónyuge, ex cónyuge, a
la persona con quien cohabita o con quien haya cohabitado o con quien sostiene o haya
sostenido una relación consensual, o la persona con quien haya procreado un hijo o hija,
independientemente del sexo, estado civil, orientación sexual, identidad de género o
estatus migratorio de cualquiera de las personas involucradas en la relación, a los bienes
apreciados por ésta, excepto aquéllos que pertenecen privativamente al ofensor, o a la
persona de otro, incurrirá en delito grave de cuarto grado en su mitad superior.")),
            tags$li(HTML("<b>Maltrato Mediante Restricción de la Libertad (Artículo 3.4):</b> Maltrato mediante restricción de la libertad. Toda persona que utilice
violencia o intimidación en la persona de su cónyuge, ex cónyuge, de la persona con
quien cohabita o haya cohabitado, o con quien sostiene o haya sostenido una relación
consensual, o la persona con quien haya procreado un hijo o hija, independientemente del
sexo, estado civil, orientación sexual, identidad de género o estatus migratorio de
cualquiera de las personas involucradas en la relación, o que utilice pretexto de que
padece o de que una de las personas antes mencionadas padece de enfermedad o defecto
mental, para restringir su libertad con el conocimiento de la víctima, incurrirá en delito
grave de tercer grado en su mitad inferior.")),
            
            tags$li(HTML(
              "<b>Agresión Sexual Conyugal (Artículo 3.5): </b> Se impondrá pena de reclusión, según se
dispone más adelante, a toda persona que incurra en una relación sexual no consentida
con su cónyuge o ex cónyuge, o con la persona con quien cohabite o haya cohabitado, o
con quien sostuviere o haya sostenido una relación consensual, o la persona con quien
haya procreado un hijo o hija, independientemente del sexo, estado civil, orientación
sexual, identidad de género o estatus migratorio de cualquiera de las personas
involucradas en la relación, en cualesquiera de las circunstancias siguientes:
<ul>          
  <li>a. Si se ha compelido a incurrir en relación sexual mediante el empleo de fuerza,
violencia, intimidación o amenaza de grave e inmediato daño corporal; o
<li>b. Si se ha anulado o disminuido sustancialmente, sin su conocimiento o sin su
consentimiento, su capacidad de consentir, a través de medios hipnóticos,
narcóticos, deprimentes o estimulantes o sustancias o medios similares; o
<li>c. Si por enfermedad o incapacidad mental, temporal o permanente, la víctima está
incapacitada para comprender la naturaleza del acto en el momento de su
realización;
<li>d. Si se le obliga o induce mediante maltrato, violencia física o psicológica a
participar o involucrarse en una relación sexual no deseada con terceras personas.
              </ul>")),
            
          ),
br(),
tags$ul(sectionTitle("Fiscalías de Puerto Rico:")),
embedImage("Fiscalias_PR", "Fiscalias_PR.png", 
           "https://www.justicia.pr.gov/secretarias-y-oficinas/oficina-del-jefe-de-los-fiscales/listado-de-fiscalias/",
           "https://www.justicia.pr.gov/", size = "250"
           ),
# Sección principal con los gráficos
mainPanel(
  plotlyOutput("map_just_mapaFisc"),
  #DTOutput("dataTable_just_mapaFisc")
)
        ),
        
        # Subtab con datos específicos para el tercer dfDeli de la agencia
        # ### yeiza
        # tabPanel(
        #   "Yeiza",  # Cambiar por el nombre del tercer dfDeli
        #   # ... Estructura similar a la anterior para el tercer dfDeli
        #   tags$div(
        #     tags$h2("yeyeyey"),
        #     tags$img(src = "yeiza.jpg", height = 1000, width = 1000)
        #   )
        # ),
      )
    ),
    
    #### Tab del Departamento del Trabajo y Recursos Humanos ####
    # tabPanel(
    #   lowercaseTitle("Departamento del Trabajo y Recursos Humanos"),
    #   icon = icon("briefcase"),
    #   tabsetPanel(
    #     #### tab con datos de participación laboral (dtParlab) ####
    #     tabPanel(
    #       lowercaseTitle("Participación Laboral por Sexo"), 
    #       # Título del Tab
    #       titlePanel("Tasa de participación laboral por sexo"),
    #       
    #       # Fuente de Datos, Actualización
    #       tags$span("Fuente: Departamento del Trabajo y Recursos Humanos"), tags$br(),
    #       #tags$span("Actualización Datos:  ", actualizacion_trabajo), tags$br(),
    #       
    #       # Menu sidebar con widgets
    #       sidebarLayout(
    #         sidebarPanel(
    #           
    #           # botón para seleccionar el año
    #           createDropdownCheckbox(
    #             label = "Seleccione Año(s):",
    #             choices = parLab$Año,
    #             selected = parLab$Año,
    #             id = "trab_parLab_año"
    #           ),
    #           
    #           # botón para seleccionar el sexo
    #           createDropdownCheckbox(
    #             label = "Seleccione Sexo(s):",
    #             choices = parLab$Sexo,
    #             selected = parLab$Sexo,
    #             id = "trab_parLab_sexo"
    #           ),
    #         ),
    #         
    #         # Sección principal con los gráficos
    #         mainPanel(
    #           plotlyOutput("barPlot_trab_parLab"),
    #           DTOutput("dataTable_trab_parLab")
    #         )
    #       ),
    #     ),
    #     
    #     #### tab de Definiciones y Metadatos ####
    #     tabPanel(
    #       lowercaseTitle("Definiciones y Metadatos"),
    #       
    #     )
    #   )
    # ),
    
    #### Tab de la Administración de Vivienda Pública ####
    tabPanel(
      lowercaseTitle("Administración de Vivienda Pública"),
      icon = icon("house"),
      tabsetPanel(
        
        #### tab con datos de Adminsitración de Vivienda Públicas(dfAvp) ####
        tabPanel(
          lowercaseTitle("Viviendas Públicas Solicitadas y Asignadas"), 
          # Título del Tab
          titlePanel("Total de viviendas públicas solicitadas y asignadas por violencia doméstica por región de la Administración de Vivienda Pública"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Administración de Vivienda Pública"), tags$br(),
          #tags$span("Actualización Datos: ", actualizacion_vivienda), tags$br(),
          #tags$span(paste0("Actualización Sistema:   ", actualizacion_sistema)), 
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              # botón para seleccionar la región
              createDropdownCheckbox(
                label = "Seleccione Región de Vivienda:",
                choices = dfAvp$región,
                selected = dfAvp$región,
                id = "avp_dfAvp_región"
              ),
              
              # botón para seleccionar el año
              createDropdownCheckbox(
                label = "Seleccione Año(s):",
                choices = dfAvp$año,
                selected = dfAvp$año,
                id = "avp_dfAvp_año"
              ),
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              plotlyOutput("barPlot_avp_dfAvp"),
              DTOutput("dataTable_avp_dfAvp")
            )
          ),
        ),
        
        
        #### tab para el mapa de Adminsitración de Vivienda Públicas (mapaAvp) ####
        tabPanel(
          lowercaseTitle("Mapa de Viviendas Públicas Solicitadas y Asignadas"),
          
          # Título del Tab
          titlePanel("Total de viviendas públicas solicitadas y asignadas por violencia doméstica por región de la Administración de Vivienda Pública"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Administración de Vivienda Pública"),
          #tags$span("Actualización Datos:  ", actualizacion_vivienda), 
          #tags$span("Actualización Sistema:  ", actualizacion_sistema), 
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              
              # botón para seleccionar el año
              selectInput("select_avp_mapaAvp_año", "Seleccione Año:",
                          choices = levels(mapaAvp$año),
                          selected = 1),
              
              # createDropdownCheckbox(
              #   label = "Seleccionar Año:",
              #   choices = mapaDeli$Año,
              #   selected = NULL,
              #   id = "just_mapaDeli_año"
              # ),
              
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              plotlyOutput("map_avp_mapaAvp"),
              DTOutput("dataTable_avp_mapaAvp")
            )
          )
        ), 
        
        #### tab para el Definiciones y Metadatos ####
        tabPanel(
          lowercaseTitle("Definiciones y Metadatos"),
          
          # Título del Tab 
          titlePanel("Regiones de la Adminsitración de Vivienda Públicas"),
          
          
          tags$li(HTML("<b>Vivienda Pública:</b> Vivienda que es proporcionada, administrada o subsidiada por el gobierno o entidades gubernamentales con el objetivo de brindar alojamiento a personas o familias que tienen dificultades para acceder a una vivienda adecuada en el mercado privado debido a limitaciones económicas o sociales. Estas viviendas suelen estar dirigidas a personas de bajos ingresos, familias en situación de pobreza, personas sin hogar, o aquellos que enfrentan otras formas de vulnerabilidad social.")),
          tags$li(HTML("<b>Violencia Doméstica:</b> Cuando una persona emplea fuerza física o violencia psicológica, intimidación o persecución en contra de su pareja o expareja. Esto, para causarle daño físico a su persona, a sus bienes, a otra persona o a un animal de servicio o mascota o para causarle grave daño emocional. Para que se considere violencia doméstica es necesario que exista o haya existido una relación afectiva entre las partes. Es decir, se da cuando la persona agresora es cónyuge, excónyuge, una persona con quien vive o ha vivido, con quien sostiene o haya sostenido una relación consensual o una persona con quien se haya procreado una hija o un hijo. ")), 
          tags$li(HTML("<b>Región:</b> Se refiere a una división geográfica o área delimitada que comparte características similares, ya sea geográficas, culturales, económicas, políticas o administrativas. Subdivisión territorial establecida por las autoridades gubernamentales para propósitos de administración y gestión local. Estas divisiones pueden variar en tamaño y alcance dependiendo del país y su estructura administrativa.")), 
          
          tags$ul(sectionTitle("Regiones de Vivienda Pública:")),
          embedImage("RegionesVivienda", "RegionesVivienda.png", 
                     "https://www.avp.pr.gov/regiones-directorio.aspx",
                     "https://www.avp.pr.gov/regiones-directorio.aspx", size = "250"
          ),
          # Sección principal con los gráficos
          mainPanel(
            plotlyOutput("map_avp_mapaRegi")
          )
        )
      )
    ),
    
    #### Tab del Negociado de Policía de Puerto Rico ####
    tabPanel(
      lowercaseTitle("Negociado de Policía"),
      icon = icon("building-shield"),
      tabsetPanel(
        
        #### tab con datos de mujeres desaparecidas (despDF) ####
        tabPanel(
          lowercaseTitle("Mujeres desaparecidas y localizadas"), 
          # Título del Tab
          titlePanel("Cantidad de mujeres desaparecidas y localizadas - adultas y menores"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Negociado de Policía de Puerto Rico"), tags$br(),
          #tags$span("Actualización Datos:  ", actualizacion_policiaB), tags$br(),
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              
              # botón para seleccionar la Categoría
              createDropdownCheckbox(
                label = "Seleccione el estado de la víctima:",
                choices = despDF$Categoria,
                selected = despDF$Categoria,
                id = "poli_despDF_categoría"
              ),
              
              # botón para seleccionar el año
              createDropdownCheckbox(
                label = "Seleccione año(s):",
                choices = despDF$Año,
                selected = despDF$Año,
                id = "poli_despDF_año"
              ),
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              plotlyOutput("barPlot_poli_despDF"),
              DTOutput("dataTable_poli_despDF")
            )
          ),
        ),
        
        #### tab para el barplot de incidentes de violencia doméstica por edad de la víctima (vEdad) ####
        tabPanel(
          lowercaseTitle("Violencia Doméstica por Edad"),
          
          # Título del Tab
          titlePanel("Incidentes de violencia doméstica por edad de la víctima"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Negociado de Policía de Puerto Rico"), tags$br(),
          #tags$span("Actualización Datos:  ", actualizacion_policiaA), tags$br(),
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              
              # botón para seleccionar el año
              createDropdownCheckbox(
                label = "Seleccione Grupo(s) de Edad:",
                choices = vEdad$Edad,
                selected = vEdad$Edad,
                id = "poli_vEdad_edad"
              ),
              customSeparator(),
              # botón para seleccionar el sexo
              createDropdownCheckbox(
                label = "Seleccione sexo de las víctimas:",
                choices = vEdad$Sexo,
                selected = vEdad$Sexo[1],
                id = "poli_vEdad_sexo"
              ),
              customSeparator(),
              # botón para seleccionar el año
              createDropdownCheckbox(
                label = "Seleccione Año(s):",
                choices = vEdad$Año,
                selected = vEdad$Año,
                id = "poli_vEdad_año"
              ),
              
              # createDropdownCheckbox(
              #   label = "Seleccionar Año:",
              #   choices = mapaDeli$Año,
              #   selected = NULL,
              #   id = "just_mapaDeli_año"
              # ),
              
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              plotlyOutput("barPlot_poli_vEdad"),
              DTOutput("dataTable_poli_vEdad")
            )
          )
        ), 
        
        #### tab para el mapa de incidentes de violencia doméstica por área policiaca (inciMapa) ####
        # tabPanel(
        #   lowercaseTitle("inciMapa"),
        #   
        #   # Título del Tab
        #   titlePanel("Incidentes de violencia doméstica por área policíaca (desde enero de 2021 a abril de 2023)"),
        #   
        #   # Fuente de Datos, Actualización
        #   tags$span("Fuente: Negociado de Policía de Puerto Rico"), tags$br(),
        #   #tags$span("Actualización Datos:  ", actualizacion_policiaA), tags$br(),
        #   
        #   # Menu sidebar con widgets
        #   sidebarLayout(
        #     sidebarPanel(
        #       
        #       # botón para seleccionar año
        #       selectInput("select_poli_inciMapa_año", "Seleccione Año:",
        #                   choices = levels(inciMapa$Año),
        #                   selected = 1),
        #       
        #       # createDropdownCheckbox(
        #       #   label = "Seleccionar Año:",
        #       #   choices = mapaDeli$Año,
        #       #   selected = NULL,
        #       #   id = "just_mapaDeli_año"
        #       # ),
        #       
        #     ),
        #     
        #     # Sección principal con los gráficos
        #     mainPanel(
        #       plotlyOutput("map_poli_inciMapa")
        #       #DTOutput("dataTable_poli_inciMapa")
        #     )
        #   )
        # ), 
        
        #### tab de Definiciones y Metadatos ####
        tabPanel(
          lowercaseTitle("Definiciones y Metadatos"),
          br(),
          tags$ul(sectionTitle("Negociado de Policía:", "20px")),
          tags$li(HTML("<b>Adultas Desaparecidas:</b> Mujeres adultas cuya ubicación y paradero son desconocidos y no pueden ser determinados por sus familiares, amigos, o autoridades competentes. Esta situación puede surgir por una variedad de razones, que van desde accidentes, secuestros, desastres naturales, hasta decisiones voluntarias de la persona de abandonar su entorno sin dejar rastro.")), 
          tags$li(HTML("<b>Adultas Localizadas:</b> Mujeres adultas cuyo paradero ha sido identificado y confirmado después de que se les haya reportado como desaparecidas. ")), 
          tags$li(HTML("<b>Adultas Sin Localizar:</b> Mujeres adultas cuyo paradero no ha sido identificado ni confirmado después de haber sido reportadas como desaparecidas. Estas personas pueden haber sido vistas por última vez en circunstancias desconocidas, y su ubicación actual sigue siendo un misterio.")), 
          tags$li(HTML("<b>Menores Desaparecidas:</b> Menores femeninas cuya ubicación y paradero son desconocidos y no pueden ser determinados por sus familiares, amigos, o autoridades competentes. Esta situación puede surgir por una variedad de razones, que van desde accidentes, secuestros, desastres naturales, hasta decisiones voluntarias de la persona de abandonar su entorno sin dejar rastro.  ")), 
          tags$li(HTML("<b>Menores Localizadas:</b> Menores femeninas cuyo paradero ha sido identificado y confirmado después de que se les haya reportado como desaparecidos. ")), 
          tags$li(HTML("<b>Menores sin Localizar:</b> Menores femeninas cuyo paradero no ha sido identificado ni confirmado después de haber sido reportados como desaparecidos. Estas personas pueden haber sido vistas por última vez en circunstancias desconocidas, y su ubicación actual sigue siendo un misterio.")), 
          tags$li(HTML("<b>Incidencia:</b> Se refiere al número de incidentes o delitos que han sido reportados o registrados por las fuerzas policiales durante un período de tiempo específico en una determinada área geográfica. Esta métrica es utilizada por las autoridades policiales y otros organismos encargados de hacer cumplir la ley para medir y analizar la cantidad y el tipo de delitos que ocurren en una comunidad o jurisdicción particular. ")), 
          tags$li(HTML("<b>Violencia doméstica:</b> Cuando una persona emplea fuerza física o violencia psicológica, intimidación o persecución en contra de su pareja o expareja. Esto, para causarle daño físico a su persona, a sus bienes, a otra persona o a un animal de servicio o mascota o para causarle grave daño emocional. Para que se considere violencia doméstica es necesario que exista o haya existido una relación afectiva entre las partes. Es decir, se da cuando la persona agresora es cónyuge, ex cónyuge, una persona con quien vive o ha vivido, con quien sostiene o haya sostenido una relación consensual o una persona con quien se haya procreado una hija o un hijo. ")), 
          tags$li(HTML("<b>Víctima:</b> Persona que ha sufrido daño físico, emocional, psicológico o financiero como resultado de un acto delictivo, un accidente, un desastre natural, o cualquier otro evento traumático.")), 
          tags$li(HTML("<b>Región Policiaca:</b> Región o zona geográfica específica que está asignada a un cuerpo de policía o agencia de aplicación de la ley para llevar a cabo funciones de vigilancia, patrullaje y protección del orden público. Estas áreas son delimitadas y definidas por las autoridades policiales con el objetivo de organizar y distribuir eficazmente los recursos policiales para atender las necesidades de seguridad y aplicación de la ley en una comunidad o jurisdicción determinada.")), 
          
          tags$ul(sectionTitle("Regiones Policíacas:", "20px")),
          embedImage("RegionesPoliciácas", "Regiones_Policiacas.png", 
                     "https://www.policia.pr.gov/",
                     "https://www.policia.pr.gov/", size = "250"
          )
        )
      )
    ),
    
    #### Tab de la Oficina de la Procuradora de las Mujeres ####
    tabPanel(
      lowercaseTitle("Oficina de la Procuradora de la Mujer"),
      icon = icon("person-dress"),
      tabsetPanel(
        
        #### tab con datos de violencia domestica (opmFemiVD) ####
        tabPanel(
          lowercaseTitle("Feminicidios por Violencia Doméstica"), 
          # Título del Tab
          titlePanel("Tasa de asesinatos de mujeres por violencia doméstica, desde 1990 a 2021"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Oficina de la Procuradora de las Mujeres"), tags$br(),
          #tags$span("Actualización Datos:  ", actualizacion_opmA), tags$br(),
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              
              # botón para seleccionar el año
              createDropdownCheckbox(
                label = "Seleccione Año(s):",
                choices = opmFemiVD$Año,
                selected = opmFemiVD$Año,
                id = "opm_opmFemiVD_año"
              ),
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              plotlyOutput("barPlot_opm_opmFemiVD"),
              DTOutput("dataTable_opm_opmFemiVD")
            )
          ),
        ),
        
        #### tab con datos de violencia domestica (opmCasos) ####
        tabPanel(
          lowercaseTitle("Casos Según Razón para Consulta"), 
          # Título del Tab
          titlePanel("Población atendida mediante el programa CRIAS según razón para consulta"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Oficina de la Procuradora de las Mujeres"), tags$br(),
          #tags$span("Actualización Datos:  ", actualizacion_opmA), tags$br(),
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              
              # botón para seleccionar el año
              createDropdownCheckbox(
                label = "Seleccione Año(s):",
                choices = opmCasos$year,
                selected = opmCasos$year,
                id = "opm_opmCasos_año"
              ),
              customSeparator(),
              # botón para seleccionar el tipo de violencia
              createDropdownCheckbox(
                label = "Seleccione Razón para Consulta:",
                choices = opmCasos$tipo,
                selected = opmCasos$tipo,
                id = "opm_opmCasos_tipo"
              ),
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              plotlyOutput("barPlot_opm_opmCasos"),
              DTOutput("dataTable_opm_opmCasos")
            )
          ),
        ),
        
        #### tab con datos del género de las víctimas (opmVic) ####
        tabPanel(
          lowercaseTitle("Género de las Víctimas Atendidas"), 
          # Título del Tab
          titlePanel("Identidad de género de víctimas asistidas por el programa CRIAS"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Oficina de la Procuradora de las Mujeres"), tags$br(),
          #tags$span("Actualización Datos:  ", actualizacion_opmB), tags$br(),
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              
              # botón para seleccionar el año
              createDropdownCheckbox(
                label = "Seleccione Año(s):",
                choices = opmVic$año,
                selected = opmVic$año,
                id = "opm_opmVic_año"
              ),
              customSeparator(),
              # botón para seleccionar el género de las victimas
              createDropdownCheckbox(
                label = "Seleccione Género(s):",
                choices = opmVic$género,
                selected = opmVic$género,
                id = "opm_opmVic_género"
              ),
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              plotlyOutput("barPlot_opm_opmVic"),
              DTOutput("dataTable_opm_opmVic")
            )
          ),
        ),
        
        #### tab con datos de orientaciones medio de comunicación (opmMedio) ####
        tabPanel(
          lowercaseTitle("Medio de Orientación a las Víctimas"), 
          # Título del Tab
          titlePanel("Orientaciones ofrecidas mediante el programa CRIAS"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Oficina de la Procuradora de las Mujeres"), tags$br(),
          #tags$span("Actualización Datos:  ", actualizacion_opmB), tags$br(),
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              
              # botón para seleccionar el año
              createDropdownCheckbox(
                label = "Seleccione Año(s):",
                choices = opmMedio$año,
                selected = opmMedio$año,
                id = "opm_opmMedio_año"
              ),
              customSeparator(),
              # botón para seleccionar el medio de orientación
              createDropdownCheckbox(
                label = "Seleccione el Medio de Orientación:",
                choices = opmMedio$`Medio de orientación`,
                selected = opmMedio$`Medio de orientación`,
                id = "opm_opmMedio_medio"
              ),
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              plotlyOutput("barPlot_opm_opmMedio"),
              DTOutput("dataTable_opm_opmMedio")
            )
          ),
        ),
        
        #### tab con datos de los servicios ofrecidos por mes (opmServiciosMes) ####
        tabPanel(
          lowercaseTitle("Servicios y Alcanze de la OPM"), 
          # Título del Tab
          titlePanel("Población atendida, servicios ofrecidos y seguimientos mediante el programa CRIAS"),
          
          # Fuente de Datos, Actualización
          tags$span("Fuente: Oficina de la Procuradora de las Mujeres"), tags$br(),
          #tags$span("Actualización Datos:  ", actualizacion_opmB), tags$br(),
          
          # Menu sidebar con widgets
          sidebarLayout(
            sidebarPanel(
              
              # botón para seleccionar el año
              createDropdownCheckbox(
                label = "Seleccione Año(s):",
                choices = opmServiciosMes$year,
                selected = opmServiciosMes$year,
                id = "opm_opmServiciosMes_año"
              ),
              customSeparator(),
              # botón para seleccionar el género de las victimas
              createDropdownCheckbox(
                label = "Seleccione el tipo de servicio:",
                choices = opmServiciosMes$tipo,
                selected = opmServiciosMes$tipo,
                id = "opm_opmServiciosMes_tipo"
              ),
            ),
            
            # Sección principal con los gráficos
            mainPanel(
              plotlyOutput("barPlot_opm_opmServiciosMes"),
              DTOutput("dataTable_opm_opmServiciosMes")
            )
          ),
        ),
        
        #### tab de Definiciones y Metadatos ####
        tabPanel(
          lowercaseTitle("Definiciones y Metadatos"),
          br(),
          tags$ul(sectionTitle("Oficina de la Procuradora de la Mujer:", "20px")),
          
          tags$li(HTML(
            "<b>Feminicidios: </b> Es el crimen que consiste en matar intencionalmente a mujeres por el hecho de ser mujeres o de identificarse como tales. Las definiciones más amplias incluyen cualquier asesinato de mujeres o niñas, o el asesinato involuntario o indirecto de mujeres o niñas, «tal como demuestran algunos casos de violencia doméstica que podrían provocar la muerte de mujeres». El concepto «adquirió importancia en el marco del movimiento feminista de la década de 1970 cuando surge la expresión ‘femicidio’ como alternativa al término neutro ‘homicidio’, con el fin de reconocer y visibilizar la opresión, la desigualdad y la violencia sistemática» contra las mujeres que, en su forma más extrema, culmina en la muerte. El femicidio puede presentarse bajo diferentes formas e incluye los siguientes actos:
            <ul>          
            <li>a. Femicidio íntimo, perpetrado por una pareja actual o anterior, generalmente durante o después de una relación ya violenta (por ejemplo, de violencia doméstica o violencia sexual);
            <li>b. el llamado crimen de honor (o asesinato de o por honor); 
            <li>c. el femicidio relacionado con la dote, que ocurre en un contexto de conflicto entre las familias de dos cónyuges recién casados, y es generalmente cometido por la familia política que cuestiona sobre todo el importe de la dote;
            <li>d. el femicidio no íntimo, cometido por un agresor que no tiene una relación íntima con la víctima, que está muy difundido en algunas partes de América Latina y que, por lo general, está precedido de actos de violencia sexual o tortura.;
            </ul>
            ")),
                    
          tags$li(HTML("<b>Tendencia:</b> Dirección o patrón observado en datos o eventos que muestra una inclinación hacia cierto resultado o comportamiento a lo largo del tiempo.")), 
          tags$li(HTML("<b>Acecho (A):</b> Es una persona, en la mayoría de las ocasiones mujer que sufre o es sometida a un patrón o la repetición de una conducta mediante la cual se mantiene de manera constante o repetida una vigilancia, proximidad física o visual sobre una persona específica. ")), 
          tags$li(HTML("<b>Agresión sexual (AS):</b> Cualquier acto que degrada o daña el cuerpo y/o la sexualidad de la víctima y que por tanto atenta contra su libertad, dignidad e integridad física. Es una expresión de abuso de poder que implica la supremacía masculina sobre la mujer, al denigrar y concebirla como objeto.")), 
          tags$li(HTML("<b>Discrimen de género (DG):</b> Hace referencia a «toda distinción, exclusión o restricción» que se realice en función del género de una persona con el objetivo o resultado de menoscabar o anular el reconocimiento, goce o ejercicio de sus derechos humanos. A menudo es consecuencia de los mitos y estereotipos de género tales como: las mujeres son las más aptas para ocuparse de la educación de los hijos, cocinar o limpiar, o para realizar trabajos de secretaría, enseñanza o enfermería, mientras que los hombres son líderes, buenos en economía y comercio. Esto ha dado lugar a un acceso desigual al mercado laboral, así como a un salario desigual para puestos similares, al sostenerse que las mujeres tienen peores resultados que los hombres en determinados ámbitos y, con ello, a una discriminación por género. ")), 
          tags$li(HTML("<b>Violencia doméstica (VD):</b> Definición que ofrece la Ley Núm. 54 de 1989 que sigue vigente en Puerto Rico. Violencia doméstica significa un patrón constante de empleo de fuerza física o violencia psicológica, intimidación o persecución contra una persona por parte de su cónyuge, ex cónyuge, una persona con quien cohabita o haya cohabitado, con quien sostiene o haya sostenido una relación consensual o una persona con quien se haya procreado una hija o hijo, para causarle daño físico a su persona, sus bienes u otra persona o para causarle grave daño emocional. La Ley Núm. 54 incluyó además como delito la agresión sexual entre personas que cohabitan o matrimonios como violencia doméstica. ")), 
          tags$li(HTML("<b>Violencia en cita (VC):</b> Violencia cometida por una persona que está o ha estado en una relación social de carácter romántico o íntimo con la víctima. La existencia de dicha relación se determinará con base en la declaración de la parte informante y teniendo en cuenta la duración de la relación, el tipo de relación y la frecuencia de interacción entre las personas involucradas en la relación. A los efectos de esta definición: La violencia en el noviazgo incluye, pero no se limita a, abuso sexual o físico o la amenaza de tal abuso. La violencia en el noviazgo no incluye actos cubiertos por la definición de violencia doméstica.")), 
          tags$li(HTML("<b>Orientaciones:</b> Direcciones o inclinaciones hacia las que se dirige o enfoca algo.")),
          tags$li(HTML("<b>CRIAS:</b> Centro de Respuesta Integrada de Apoyo y Servicios para la Mujer. La misma surgió de la necesidad imperante de trabajar con el problema de la desigualdad que existe contra las mujeres y trabajar particularmente con la violencia doméstica desde una perspectiva dirigida hacia la validación, orientación y coordinación de servicios de apoyo. El Centro CRIAS establece las bases para un modelo de prevención, intervención y fiscalización de los diferentes tipos de violencia que nos permite levantar información de las víctimas sobrevivientes participantes, obtener análisis de experiencias personales y manejo de actitudes ante el problema. En el mismo, se ofrecen servicios de orientación, coordinación de servicios y referidos a mujeres víctimas/sobrevivientes de violencia doméstica, agresión sexual, acecho y otras manifestaciones de violencia por razón de género. ")),
          tags$li(HTML("<b>Identidad de género:</b> Se refiere a la manera en que una persona se identifica, cómo se reconoce o se expresa sobre sí misma, en cuanto al género que puede corresponder o no a su sexo biológico o asignado en su nacimiento.")),
          tags$li(HTML("<b>:</b> Término utilizado para describir características, atributos o cualidades asociadas tradicionalmente con los hombres o lo que se considera típicamente propio del género masculino.")),
          tags$li(HTML("<b>Masculino:</b> Término utilizado para describir características, atributos o cualidades asociadas tradicionalmente con los hombres o lo que se considera típicamente propio del género masculino. ")),
          tags$li(HTML("<b>Femenino:</b> Se refiere a características, atributos o cualidades asociadas tradicionalmente con las mujeres o lo que se considera típicamente propio del género femenino. ")),
          tags$li(HTML("<b>Trans:</b> Abreviatura comúnmente utilizada para referirse a personas que son transgénero o que tienen una identidad de género diferente de aquella que se les asignó al nacer. Las personas transgénero pueden identificarse como hombre, mujer, ambos, ninguno o con un género diferente al binario tradicional de hombre y mujer.")),
          
          )
        
      ),
    ),
    
    
    
    #### Tab del Departamento de Corrección y Rehabilitación ####
    tabPanel(
      lowercaseTitle("Departamento de Corrección y Rehabilitación"),
      icon = icon("door-open"),
      tabsetPanel(
        #### tab con datos del género de las víctimas (dcrCasosInv) ####
        tabPanel(
        lowercaseTitle("Supervisión Ley 54: Programas Comunitarios"), 
        # Título del Tab
        titlePanel("Casos en supervisión de ley 54 en programas alternos al confinamiento por estado de investigación: Programas de Comunidad"),
      
        # Fuente de Datos, Actualización
        tags$span("Fuente: Departamento de Corrección y Rehabilitación"), tags$br(),
        #tags$span("Actualización Datos:  ", actualizacion_correcion), tags$br(),
      
        # Menu sidebar con widgets
        sidebarLayout(
          sidebarPanel(
          
            # botón para seleccionar el año
          createDropdownCheckbox(
            label = "Seleccione Año(s):",
            choices = dcrCasosInv$year,
            selected = dcrCasosInv$year,
            id = "dcr_dcrCasosInv_year"
          ),
          customSeparator(),
          # botón para seleccionar el sexo
          createDropdownCheckbox(
            label = "Seleccione el sexo:",
            choices = dcrCasosInv$sexo,
            selected = 1,
            id = "dcr_dcrCasosInv_sexo"
          ),
          customSeparator(),
          # botón para seleccionar el tipo de investigación 
          createDropdownCheckbox(
            label = "Seleccione el estado de Investigación:",
            choices = dcrCasosInv$tipo,
            selected = dcrCasosInv$tipo,
            id = "dcr_dcrCasosInv_tipo"
          ),
        ),
        
        # Sección principal con los gráficos
        mainPanel(
          plotlyOutput("barPlot_dcr_dcrCasosInv"),
          DTOutput("dataTable_dcr_dcrCasosInv")
        )
      ),
    ),
    
    #### tab con datos personas sentenciadas al Programa de Supervisión Electrónica (dcrSentenciadas) ####
    tabPanel(
      lowercaseTitle("Sentencias por Violencia Doméstica"),
      # Título del Tab
      titlePanel("Personas sentenciadas en programa de supervisión electrónica por delitos de violencia doméstica por estado del caso"),

      # Fuente de Datos, Actualización
      tags$span("Fuente: Departamento de Corrección y Rehabilitación"), tags$br(),
      #tags$span("Actualización Datos:  ", actualizacion_correcion), tags$br(),

      # Menu sidebar con widgets
      sidebarLayout(
        sidebarPanel(

          # botón para seleccionar el año
          createDropdownCheckbox(
            label = "Seleccione Año(s):",
            choices = dcrSentenciadas$year,
            selected = dcrSentenciadas$year,
            id = "dcr_dcrSentenciadas_year"
          ),
          customSeparator(),
          # botón para seleccionar el tipo de investigación
          createDropdownCheckbox(
            label = "Seleccione el estado de Caso:",
            choices = dcrSentenciadas$tipo,
            selected = dcrSentenciadas$tipo,
            id = "dcr_dcrSentenciadas_tipo"
          ),
          customSeparator(),
          # # botón para seleccionar el sexo
          # createDropdownCheckbox(
          #   label = "Seleccione el sexo:",
          #   choices = dcrSentenciadas,
          #   selected = dcrCasosInv$sexo,
          #   id = "dcr_dcrCasosInv_sexo"
          # ),
        ),

        # Sección principal con los gráficos
        mainPanel(
          plotlyOutput("barPlot_dcr_dcrSentenciadas"),
          DTOutput("dataTable_dcr_dcrSentenciadas")
        )
      )
    ),
    #### tab de Definiciones y Metadatos ####
    tabPanel(
      lowercaseTitle("Definiciones y Metadatos"),
      br(),
      tags$ul(sectionTitle("Departamento de Corrección y Rehabilitación:", "20px")),
      
      tags$li(HTML("<b>Ley 54:</b> Ley Núm. 54-1989, conocida como la “Ley para la Prevención e Intervención con la Violencia Doméstica”, según enmendada, establece la violencia doméstica como delito y lo define como el empleo de fuerza física o violencia psicológica, intimidación o persecución en contra de su pareja o expareja. Esto, para causarle daño físico a su persona, a sus bienes o a otra persona o para causarle grave daño emocional.")),
      tags$li(HTML("<b>Programas de Comunidad:</b> Son programas de tratamientos establecidos para que las personas convictas cumplan parte de su sentencia fuera de la institución penal. Su finalidad es promover que los convictos que estén capacitados para reintegrarse a la sociedad puedan hacerlo como parte de su rehabilitación moral y social ")),
      tags$li(HTML("<b>Investigaciones realizadas:</b> Proceso sistemático y metódico de recopilación, análisis y evaluación de información con el objetivo de obtener conclusiones, resolver problemas o generar conocimiento en un campo específico.  ")),
      tags$li(HTML("<b>Personas sentenciadas:</b> Pronunciamiento que hace el juez o la jueza sobre la pena que se le impone a una persona acusada luego de que se determina que es culpable de cometer un delito. ")),
      tags$li(HTML("<b>Programa de Supervisión Electrónica:</b> El Programa de Monitoreo Electrónico cuenta con la Unidad Especializada de Monitoreo Electrónico (Unidad) compuesta por Oficiales Correccionales, la cual tiene la responsabilidad de supervisar y monitorear a los participantes pertenecientes al programa. Esta supervisión conlleva el verificar y atender las alertas que se activan a través del sistema de transmisión electrónica, activar el protocolo, solicitar apoyo inter agencial, avisar a la víctima y administrar pruebas toxicológicas, entre otras. ")),
      
      
      
    )
  )
    ),
  
  #### Tab de la Administración de Tribunales ####
  tabPanel(
    lowercaseTitle("Administración de Tribunales"),
    icon = icon("building-circle-exclamation"),
    tabsetPanel(
      
      #### tab con datos de ley 148 - Violencia Sexual por grupo de edad (OP_148_SoliGrupEdad) ####
      tabPanel(
        lowercaseTitle("Órdenes de Protección Solicitadas por Edad y Región"), 
        # Título del Tab
        titlePanel("Órdenes de Protección Solicitadas por Violencia Sexual bajo Ley 148, según Grupo de Edad, Región Judicial y año fiscal"),
        
        # Fuente de Datos, Actualización
        tags$span("Fuente: Oficina de Administración de los Tribunales, Directoría de Operaciones, Oficina de Estadísticas"), tags$br(),
        #tags$span("Actualización Datos:  ", actualizacion_tribunalesB), tags$br(),
        
        # Menu sidebar con widgets
        sidebarLayout(
          sidebarPanel(
            
            # botón para seleccionar el año
            createDropdownCheckbox(
              label = "Seleccione Año(s) Fiscal:",
              choices = OP_148_SoliGrupEdad$AñoFiscal,
              selected = OP_148_SoliGrupEdad$AñoFiscal,
              id = "trib_OP_148_SoliGrupEdad_AñoFiscal"
            ),
            customSeparator(),
            # botón para seleccionar el grupo de edad
            createDropdownCheckbox(
              label = "Seleccione grupo(s) de edad:",
              choices = OP_148_SoliGrupEdad$Edad,
              selected = OP_148_SoliGrupEdad$Edad,
              id = "trib_OP_148_SoliGrupEdad_Edad"
            ),
            customSeparator(),
            # botón para seleccionar el distrito fiscal
            createDropdownCheckbox(
              label = "Seleccione Región Judicial:",
              choices = OP_148_SoliGrupEdad$Región,
              selected = OP_148_SoliGrupEdad$Región,
              id = "trib_OP_148_SoliGrupEdad_Región"
            ),
          ),
          
          # Sección principal con los gráficos
          mainPanel(
            plotlyOutput("barPlot_OP_148_SoliGrupEdad"),
            DTOutput("dataTable_OP_148_SoliGrupEdad")
          )
        ),
      ),
      
      #### tab con datos de ley 148 - Violencia Sexual por grupo de edad (OP_Ley148_ex_parteEmitidas) ####
      tabPanel(
        lowercaseTitle("Órdenes de Protección Ex Parte Emitidas por Delito Cometido y Región"), 
        # Título del Tab
        titlePanel("Órdenes de Protección Ex Parte Emitidas bajo Ley 148 según Región Judicial, Delito Cometido y año fiscal"),
        
        # Fuente de Datos, Actualización
        tags$span("Fuente: Oficina de Administración de los Tribunales, Directoría de Operaciones, Oficina de Estadísticas"), tags$br(),
        #tags$span("Actualización Datos:  ", actualizacion_tribunalesB), tags$br(),
        
        # Menu sidebar con widgets
        sidebarLayout(
          sidebarPanel(
            
            # botón para seleccionar el año fiscal
            createDropdownCheckbox(
              label = "Seleccione Año(s) Fiscal:",
              choices = OP_Ley148_ex_parteEmitidas$AñoFiscal,
              selected = OP_Ley148_ex_parteEmitidas$AñoFiscal,
              id = "trib_OP_Ley148_ex_parteEmitidas_AñoFiscal"
            ),
            customSeparator(),
            # botón para seleccionar el delito
            createDropdownCheckbox(
              label = "Seleccione Delito(s) cometido:",
              choices = OP_Ley148_ex_parteEmitidas$Delito,
              selected = OP_Ley148_ex_parteEmitidas$Delito,
              id = "trib_OP_Ley148_ex_parteEmitidas_Delito"
            ),
            customSeparator(),
            # botón para seleccionar la región fiscal
            createDropdownCheckbox(
              label = "Seleccione Región Judicial:",
              choices = OP_Ley148_ex_parteEmitidas$Región,
              selected = OP_Ley148_ex_parteEmitidas$Región,
              id = "trib_OP_Ley148_ex_parteEmitidas_Región"
            ),
            
          ),
          
          # Sección principal con los gráficos
          mainPanel(
            plotlyOutput("barPlot_OP_Ley148_ex_parteEmitidas"),
            DTOutput("dataTable_OP_Ley148_ex_parteEmitidas")
          )
        ),
      ),
      
      #### tab con datos de solicitudes de órdenes de protección archivadas (OP_LEY148Archivadas) ####
      tabPanel(
        lowercaseTitle("Órdenes de protección ex parte archivadas por razón y región"), 
        
        # Título del Tab
        titlePanel("Órdenes de protección ex parte bajo Ley 148 archivadas por razón, Región Judicial y año fiscal"),
        
        # Fuente de Datos, Actualización
        tags$span("Fuente: Oficina de Administración de los Tribunales, Directoría de Operaciones, Oficina de Estadísticas"), tags$br(),
        #tags$span("Actualización Datos:  ", actualizacion_tribunalesB), tags$br(),
        
        # Menu sidebar con widgets
        sidebarLayout(
          sidebarPanel(
            
            # botón para seleccionar el año fiscal
            createDropdownCheckbox(
              label = "Seleccione Año(s) Fiscal:",
              choices = OP_LEY148Archivadas$AñoFiscal,
              selected = OP_LEY148Archivadas$AñoFiscal,
              id = "trib_OP_LEY148Archivadas_AñoFiscal"
            ),
            customSeparator(),
            # botón para seleccionar la razón de archivado
            createDropdownCheckbox(
              label = "Seleccione Razón(s):",
              choices = OP_LEY148Archivadas$Razón,
              selected = OP_LEY148Archivadas$Razón,
              id = "trib_OP_LEY148Archivadas_Razón"
            ),
            customSeparator(),
            # botón para seleccionar la región fiscal
            createDropdownCheckbox(
              label = "Seleccione Distrito(s) Fiscal:",
              choices = OP_LEY148Archivadas$Región,
              selected = OP_LEY148Archivadas$Región,
              id = "trib_OP_LEY148Archivadas_Región"
            ),
            
          ),
          
          # Sección principal con los gráficos
          mainPanel(
            plotlyOutput("barPlot_OP_LEY148Archivadas"),
            DTOutput("dataTable_OP_LEY148Archivadas")
          )
        ),
      ),
      
      #### tab con datos de solicitudes de órdenes de protección denegadas (OP_LEY148Denegadas) ####
      tabPanel(
        lowercaseTitle("Órdenes de protección denegadas por razón del archivo y región"), 
        # Órdenes de Protección Ex Parte Archivadas por Razón del Archivo y Región
        # Título del Tab
        titlePanel("Ordenes de protección denegadas por violencia sexual bajo Ley 148 por razón de archivo, Región Judicial y año fiscal"),

        # Fuente de Datos, Actualización
        tags$span("Fuente: Oficina de Administración de los Tribunales, Directoría de Operaciones, Oficina de Estadísticas"), tags$br(),
        #tags$span("Actualización Datos:  ", actualizacion_tribunalesB), tags$br(),
        
        # Menu sidebar con widgets
        sidebarLayout(
          sidebarPanel(
            
            # botón para seleccionar el año fiscal
            createDropdownCheckbox(
              label = "Seleccione Año(s) fiscal:",
              choices = OP_LEY148Denegadas$AñoFiscal,
              selected = OP_LEY148Denegadas$AñoFiscal,
              id = "trib_OP_LEY148Denegadas_AñoFiscal"
            ),
            customSeparator(),
            # botón para seleccionar la razón de archivado
            createDropdownCheckbox(
              label = "Seleccione razón(s):",
              choices = OP_LEY148Denegadas$Razón,
              selected = OP_LEY148Denegadas$Razón,
              id = "trib_OP_LEY148Denegadas_Razón"
            ),
            customSeparator(),
            # botón para seleccionar la región fiscal
            createDropdownCheckbox(
              label = "Seleccione región judicial:",
              choices = OP_LEY148Denegadas$Región,
              selected = OP_LEY148Denegadas$Región,
              id = "trib_OP_LEY148Denegadas_Región"
            ),
          ),
          
          # Sección principal con los gráficos
          mainPanel(
            plotlyOutput("barPlot_OP_LEY148Denegadas"),
            DTOutput("dataTable_OP_LEY148Denegadas")
          )
        ),
      ),
      
      #### tab con datos de solicitudes de órdenes de protección finales emitidas (OP_LEY148FinalEmitidas) ####
      tabPanel(
        lowercaseTitle("Órdenes de Protección Emitidas por Delito Cometido y Región"), 
        # Título del Tab
        titlePanel("Órdenes de protección emitidas bajo ley 148 por delito cometido, Región Judicial y año fiscal"),
        
        # Fuente de Datos, Actualización
        tags$span("Fuente: Oficina de Administración de los Tribunales, Directoría de Operaciones, Oficina de Estadísticas"), tags$br(),
        #tags$span("Actualización Datos:  ", actualizacion_tribunalesB), tags$br(),
        
        # Menu sidebar con widgets
        sidebarLayout(
          sidebarPanel(
            
            # botón para seleccionar el año fiscal
            createDropdownCheckbox(
              label = "Seleccione Año(s) Fiscal:",
              choices = OP_LEY148FinalEmitidas$AñoFiscal,
              selected = OP_LEY148FinalEmitidas$AñoFiscal,
              id = "trib_OP_LEY148FinalEmitidas_AñoFiscal"
            ),
            customSeparator(),
            # botón para seleccionar la razón de archivado
            createDropdownCheckbox(
              label = "Seleccione Delito(s) Cometido:",
              choices = OP_LEY148FinalEmitidas$Delito,
              selected = OP_LEY148FinalEmitidas$Delito,
              id = "trib_OP_LEY148FinalEmitidas_Delito"
            ),
            customSeparator(),
            # botón para seleccionar la región fiscal
            createDropdownCheckbox(
              label = "Seleccione Región Judicial:",
              choices = OP_LEY148FinalEmitidas$Región,
              selected = OP_LEY148FinalEmitidas$Región,
              id = "trib_OP_LEY148FinalEmitidas_Región"
            ),
          ),
          
          # Sección principal con los gráficos
          mainPanel(
            plotlyOutput("barPlot_OP_LEY148FinalEmitidas"),
            DTOutput("dataTable_OP_LEY148FinalEmitidas")
          )
        ),
      ),
      
      #### tab con datos de solicitudes de órdenes de protección finales emitidas (OP_LEY148Genero) ####
      tabPanel(
        lowercaseTitle("Órdenes de Protección Emitidas por Parte y Sexo"), 
        
        # Título del Tab
        titlePanel("Órdenes de Protección Emitidas bajo Ley 148, por Sexo, la Parte y año fiscal"),
        
        # Fuente de Datos, Actualización
        tags$span("Fuente: Oficina de Administración de los Tribunales, Directoría de Operaciones, Oficina de Estadísticas"), tags$br(),
        #tags$span("Actualización Datos:  ", actualizacion_tribunalesB), tags$br(),
        
        # Menu sidebar con widgets
        sidebarLayout(
          sidebarPanel(
            
            # botón para seleccionar el año fiscal
            createDropdownCheckbox(
              label = "Seleccione Año(s) Fiscal:",
              choices = OP_LEY148Genero$AñoFiscal,
              selected = OP_LEY148Genero$AñoFiscal,
              id = "trib_OP_LEY148Genero_AñoFiscal"
            ),
            customSeparator(),
            # botón para seleccionar la parte
            createDropdownCheckbox(
              label = "Seleccione Parte(s):",
              choices = OP_LEY148Genero$Parte,
              selected = OP_LEY148Genero$Parte,
              id = "trib_OP_LEY148Genero_Parte"
            ),
            customSeparator(),
            # botón para seleccionar el sexo de la parte
            createDropdownCheckbox(
              label = "Seleccione Sexo:",
              choices = OP_LEY148Genero$Sexo,
              selected = OP_LEY148Genero$Sexo,
              id = "trib_OP_LEY148Genero_Sexo"
            ),
          ),
          
          # Sección principal con los gráficos
          mainPanel(
            plotlyOutput("barPlot_OP_LEY148Genero"),
            DTOutput("dataTable_OP_LEY148Genero")
          )
        ),
      ),
      
      
      #### tab con datos de Movimiento de Casos Criminales de Violencia Doméstica (tribCasosCrim) ####
      tabPanel(
        lowercaseTitle("Movimiento de Casos en Tribunal de Primera Instancia por Ley 54"), 
        
        # Título del Tab
        titlePanel("Movimiento de casos criminales de violencia doméstica en el tribunal de primera instancia según la ley Núm. 54 por delito cometido y año fiscal"),
        
        # Fuente de Datos, Actualización
        tags$span("Fuente: Oficina de Administración de los Tribunales, Directoría de Operaciones, Oficina de Estadísticas"), tags$br(),
        #tags$span("Actualización Datos:  ", actualizacion_tribunalesB), tags$br(),
        
        # Menu sidebar con widgets
        sidebarLayout(
          sidebarPanel(
            
            # botón para seleccionar el año fiscal
            createDropdownCheckbox(
              label = "Seleccione Año(s) Fiscal:",
              choices = tribCasosCrim$AñoFiscal,
              selected = tribCasosCrim$AñoFiscal,
              id = "trib_tribCasosCrim_AñoFiscal"
            ),
            customSeparator(),
            # botón para seleccionar el Delito
            createDropdownCheckbox(
              label = "Seleccione Delito(s):",
              choices = tribCasosCrim$Delito,
              selected = tribCasosCrim$Delito,
              id = "trib_tribCasosCrim_Delito"
            ),
            customSeparator(),
            # botón para seleccionar el estado del caso
            createDropdownCheckbox(
              label = "Seleccione Estado del Caso:",
              choices = tribCasosCrim$Casos,
              selected = 1,
              id = "trib_tribCasosCrim_Casos"
            ),
          ),
          
          # Sección principal con los gráficos
          mainPanel(
            plotlyOutput("barPlot_tribCasosCrim"),
            DTOutput("dataTable_tribCasosCrim")
          )
        ),
      ),
      
      #### tab de Definiciones y Metadatos ####
      tabPanel(
        lowercaseTitle("Definiciones y Metadatos"),
        br(),
        tags$ul(sectionTitle("Administración de Tribunales:", "20px")),
        tags$li(HTML("<b>Organización:</b> Poder Judicial de Puerto Rico, Oficina de Administración de los Tribunales, Directoria de Operaciones, Oficina de Estadísticas. ")),
        tags$li(HTML("<b>Orden de Protección:</b> Es un remedio civil expedido por escrito bajo el sello de un Tribunal, en la cual se dictan las medidas a una persona agresora para que ésta se abstenga de incurrir o llevar a cabo determinados actos o conducta constitutivos de violencia doméstica. ")),
        tags$li(HTML("<b>Solicitudes de órdenes de protección:</b> Se define como todas las peticiones de orden de protección realizadas en el periodo de tiempo de interés en la región judicial especificada.")),
        tags$li(HTML("<b>Ley 148:</b> Conocida como la “Ley para la Protección de las Víctimas de Violencia Sexual en Puerto Rico”, según enmendada, establece los mecanismos para la expedición de órdenes de protección para víctimas de los delitos de agresión sexual, actos lascivos, acoso sexual e incesto. ")),
        tags$li(HTML("<b>Ley 54:</b> La Ley Núm. 54-1989, conocida como la “Ley para la Prevención e Intervención con la Violencia Doméstica”, según enmendada, establece la violencia doméstica como delito y lo define como el empleo de fuerza física o violencia psicológica, intimidación o persecución en contra de su pareja o expareja. Esto, para causarle daño físico a su persona, a sus bienes o a otra persona o para causarle grave daño emocional")),
        tags$li(HTML("<b>Violencia Sexual:</b> Cualquier acto que degrada o daña el cuerpo y/o la sexualidad de la víctima y que por tanto atenta contra su libertad, dignidad e integridad física. Es una expresión de abuso de poder que implica la supremacía masculina sobre la mujer, al denigrar y concebirla como objeto.")),
        tags$li(HTML("<b>Región Judicial:</b> Se refiere a la región judicial a la que corresponden los datos informados. El Tribunal de Primera Instancia se distribuye territorialmente en trece regiones judiciales. Cada región judicial está compuesta por un centro judicial y sus respectivas salas superiores y municipales. ")),
        tags$li(HTML("<b>Peticionaria:</b> Persona que solicita una orden de protección. ")),
        tags$li(HTML("<b>Sexo:</b> Indica si la persona que solicita la orden de protección, en el periodo de tiempo de interés en la región judicial especificada, se identifica como hombre o mujer. ")),
        tags$li(HTML("<b>Año Fiscal:</b> Período de 12 meses comprendido entre el 1ro de julio de un año y el 30 de junio del año siguiente, y que se usa como el calendario presupuestario de las agencias públicas. ")),
        tags$li(HTML("<b>Órdenes de protección ex parte:</b> Es una orden emitida por el Tribunal de Primera Instancia luego de escuchar a la parte peticionaria (persona que solicita la orden) y hacer una determinación provisional sobre los hechos. ")),
        
      )
    )
  ),
  #### Tab del Centro de Ayuda a Víctimas de Violación ####
  tabPanel(
    lowercaseTitle("Centro Ayuda a Víctimas de Violación"),
    icon = icon("building-circle-exclamation"),
    tabsetPanel(
      
      tabPanel(
        lowercaseTitle("Tendencia Anual del Equipo Recolecta de Violencia Sexual"), 
        # Título del Tab
        titlePanel(HTML("Tendencia anual del equipo de recolecta de evidencia de <i>Rape Kits</i> en casos de violencia sexual por estado de querella")),
        
        # Fuente de Datos, Actualización
        tags$span("Fuente: Centro de Ayuda a Victimas de Violación, Departamento de Salud"), tags$br(),
        #tags$span("Actualización Datos:  ", actualizacion_caav), tags$br(),
        
        # Menu sidebar con widgets
        sidebarLayout(
          sidebarPanel(
            
            # botón para seleccionar el año
            createDropdownCheckbox(
              label = "Seleccione Año(s):",
              choices = safekitsDF$Año,
              selected = safekitsDF$Año,
              id = "cavv_safekitsDF_Año"
            ),
            customSeparator(),
            # botón para seleccionar el estado de querella
            createDropdownCheckbox(
              label = "Seleccione Estado de Querella:",
              choices = safekitsDF$Kits,
              selected = safekitsDF$Kits,
              id = "cavv_safekitsDF_Kits"
            )
          ),
          
          # Sección principal con los gráficos
          mainPanel(
            plotlyOutput("barPlot_safekitsDF"),
            DTOutput("dataTable_safekitsDF")
          )
        )
      ),
      
      #### tab de Definiciones y Metadatos ####
      tabPanel(
        lowercaseTitle("Definiciones y Metadatos"),
        br(),
        tags$ul(sectionTitle("Centro de Ayuda a Víctimas de Violación:", "20px")),
        tags$li(HTML("<b>Violencia Sexual:</b> Cualquier acto que degrada o daña el cuerpo y/o la sexualidad de la víctima y que por tanto atenta contra su libertad, dignidad e integridad física. Es una expresión de abuso de poder que implica la supremacía masculina sobre la mujer, al denigrar y concebirla como objeto.")),
        tags$li(HTML(
          "<b>Agresión Sexual: </b> El Código Penal de Puerto Rico define el delito de agresión sexual como llevar a cabo, o provocar que otra persona lleve a cabo, un acto orogenital o una penetración sexual (vaginal o anal, ya sea esta genital, digital o instrumental) en cualquiera de estas circunstancias:
          <ul>          
          <li>a. si a la víctima se le disminuyó, sin esta consentir o sin saberlo, su capacidad de consentir mediante algún medio hipnótico, narcótico, deprimente o estimulante.; 
          <li>b. si a la víctima se le obligó al acto por medio de fuerza física, violencia o intimidación.; 
          <li>c. si al momento del acto la víctima no tenía capacidad para consentir y la persona agresora lo sabía.; 
          <li>d. si la víctima consintió porque se le engañó sobre la identidad de la persona agresora y creía que era otra persona.; 
          <li>e. si la víctima no ha cumplido 16 años de edad.; 
          <li>f. si por enfermedad o incapacidad mental la víctima no puede comprender el acto en el momento en que ocurre.;
          </ul>
          ")),
        tags$li(HTML("<b>Actos Lascivos:</b> El Código Penal de Puerto Rico define el delito de actos lascivos como aquel en el cual, sin intentar consumar el delito de agresión sexual, se someta a otra persona a un acto que tienda a despertar, excitar o satisfacer la pasión o deseos sexuales de la persona imputada.")),
        tags$li(HTML("<b>Kits:</b> Equipo utilizado por profesionales de la salud para la recuperación de evidencia forense en casos de violencia sexual. Consiste en una caja que contiene instrucciones y materiales que facilitan la recoleccion de evidencia. También conocido como <i>Sexual Assault Kit</i> o, por sus siglas en inglés, <i>SAK</i>. ")),
        tags$li(HTML("<b>Kit con Querella:</b> Kit de recolección de evidencia forense que se acompaña con la querella radicada por la víctima sobreviviente de violencia sexual. Cuenta con un número otorgado por el NPPR.")),
        tags$li(HTML("<b>Kit sin Querella:</b> Kit de recolección de evidencia forense que no se acompaña con una querella, pues la víctima sobreviviente de violencia sexual no ha radicado querella en el NPPR.")),
        tags$li(HTML("<b>Querella:</b> Mecanismo que tiene disponible una persona para reportar a la policía un incidente que entiende debe ser investigado por ésta. El incidente puede tratarse de uno que involucre un delito, una infracción, una persona desaparecida, entre otros.")), 
        tags$li(HTML("<b>Tendencia:</b> Dirección o patrón observado en datos o eventos que muestra una inclinación hacia cierto resultado o comportamiento a lo largo del tiempo.")),
        tags$li(HTML("<b>Evidencia:</b> Se refiere a cualquier información, datos, pruebas o testimonios que respaldan una afirmación, teoría o argumento. En diferentes contextos, la evidencia puede ser utilizada para respaldar conclusiones científicas, legales, filosóficas o incluso personales. La calidad y fiabilidad de la evidencia pueden variar según la fuente, el método de recopilación y el contexto en el que se utilice. En general, se busca que la evidencia sea objetiva, verificable y relevante para el tema en cuestión."))
        )
        
        
        
    
    )    
  ),
      
  
    #### Tab Acerca del Dashboard ####
    tabPanel(
      lowercaseTitle("Acerca del Dashboard"),
      icon = icon("info-circle"),
      tabsetPanel(
        
        # # tab del transfondo del proyecto
        # tabPanel(
        #   lowercaseTitle("Transfondo del Proyecto")
        #   ),
        # 
        # # tab de las fuentes usadas en el proyecto
        # tabPanel(
        #   lowercaseTitle("Fuentes")
        #   ), 
        
        # tab de los autores del proyecto
        tabPanel(
          lowercaseTitle("Autores"),
          div(
            authorTag(
              nombre = 'Félix A. Báez Santiago',
              email = 'felix.baez@estadisticas.pr',
              puesto = 'Programador Estadístico',
              grados = 'BS Ciencia de Datos, Iowa State University'
            ),
            
            authorTag(
              nombre = 'Manuel Mangual Martínez',
              email = 'manuel.mangual@estadisticas.pr',
              puesto = 'Supervisor del Proyecto Prevención, Apoyo, Rescate y Educación de la Violencia de Género',
              grados = c('MS Investigación y Evaluación de la Salud, Universidad de Puerto Rico',
                         'BS Biología, Universidad de Puerto Rico')
            ),
            
            authorTag(
              nombre = 'Frankie Rodríguez Rivera',
              email = 'frankie.rodriguez@estadisticas.pr',
              puesto = 'Analista de Datos',
              grados = c('MS Administración de Agencias Públicas, Universidad del Turabo',
                         'BS Trabajo Social, Universidad Interamericana')
            ),
            
            authorTag(
              nombre = 'Mariluz Bezares Salinas',
              email = 'mariluz.bezares@estadisticas.pr',
              puesto = 'Gerente de Proyectos Estadísticos',
              grados = c('MS Ciencias en Demografía, Universidad de Puerto Rico',
                         'BA Ciencias Sociales - Geografía, Universidad de Puerto Rico')
            ),
            
            authorTag(
              nombre = 'Jacobo Orenstein Cardona',
              email = 'jacobo.orenstein@estadisticas.pr',
              puesto = 'Ayudante Ejecutivo',
              grados = c(
                'BA Filosofía, Centre Sèvres',
                'BS Química, Massachusetts Institute of Technology',
                'BA Historia, Massachusetts Institute of Technology'
              )
            ),
            
            authorTag(
              nombre = 'Idania R. Rodriguez-Ayuso',
              email = 'idania.ayuso@estadisticas.pr',
              puesto = 'Especialista en Proyectos Estadísticos y Epidemiológicos',
              grados = c(
                'PhD Salud Pública con concentración en Epidemiología, Walden University',
                "MS Epidemiología, Universidad de Puerto Rico",
                'BS Biología, Universidad de Puerto Rico'
              )
            ),
            
            authorTag(
              nombre = 'Raúl Figueroa Rodríguez',
              email = 'raul.figueroa@estadisticas.pr',
              puesto = 'Gerente de Proyectos Estadísticos NVDRS',
              grados = c('MS Demografía, Universidad de Puerto Rico',
                         'BS Ciencias Naturales, Universidad de Puerto Rico')
            ),
            
            authorTag(
              nombre = 'Dr. Orville M. Disdier Flores',
              email = 'orville.disdier@estadisticas.pr',
              puesto = 'Director Ejecutivo',
              grados = c(
                'Ed.D Liderazgo Educativo, Universidad del Turabo',
                'MS Epidemiología, Universidad de Puerto Rico',
                'BS Ciencias Naturales, Universidad de Puerto Rico'
              )
            ), 
            
            authorTag(
              nombre = 'Mario Font Martín',
              email = 'mario.font@estadisticas.pr',
              puesto = 'Gerente de Proyectos Estadísticos',
              grados = c('MS Planificación, Universidad de Puerto Rico',
                         'BA Administración de Empresas, Universidad de Puerto Rico')
            ),
          )
          ),

        
        # tab de la información de contacto del Proyecto
        tabPanel(
          lowercaseTitle("Contacto"),
          mainPanel(
            fluidRow(
              
              column(6, 
                     h1("Instituto De Estadísticas De Puerto Rico"),
                     h4('Postal: P.O. Box 195484 | San Juan, PR 00919-5484'),
                     h4( a(href="tel:787-819-0730", 'Tel: (787) 819-0730')),
                     h4('Email: preguntas@estadisticas.pr'),
                     h4('Horario de Oficina: lunes a viernes, 8:00 am a 4:30 pm'),
                     a(href='https://www.facebook.com/estadisticas.pr',
                       icon("facebook","fa-2x"),) ,
                     a(href='https://www.instagram.com/institutodeestadisticas/' ,
                       icon('instagram','fa-2x')),
                     a(href="https://twitter.com/EstadisticasPR",
                       icon('twitter','fa-2x')),
                     a(href="https://www.youtube.com/channel/UCIZggRtE5KK0z9D39FGZtyQ",
                       icon('youtube','fa-2x'))),
              
              column(3),
              column(3,
                     br(),
                     a(img(src='IEPRlocal.png',height=200),
                       href="https://www.google.com/maps/place/Instituto+de+Estad%C3%ADsticas+de+Puerto+Rico/@18.4279999,-66.056444,17.5z/data=!4m5!3m4!1s0x8c0368a432af0b3d:0x274ac1c656b89f89!8m2!3d18.4275192!4d-66.0562994"),
                     a(img(src='IEPRmap.png',height=200),
                       href='https://www.google.com/maps/place/Instituto+de+Estad%C3%ADsticas+de+Puerto+Rico/@18.4279999,-66.056444,17.5z/data=!4m5!3m4!1s0x8c0368a432af0b3d:0x274ac1c656b89f89!8m2!3d18.4275192!4d-66.0562994'))
            )
            
          )
        )
      )
    ),
    
    #### Tab de prueba ####
    # tabPanel(
    #   "test",
    #   icon = icon("exclamation-triangle"),
    #   sidebarLayout(
    #     div(sidebarPanel(
    #       selectInput("yearInput_xiom", "Seleccionar xiom:", choices = unique(homiEdad$año)),
    #       selectInput("yearInput_yeiz", "Seleccionar yeiza:", choices = unique(homiEdad$año))
    #     )
    #     ),
    #     
    #     mainPanel(
    #       tabsetPanel(
    #         tabPanel(
    #           "Homicidios por grupo de Edad",
    #           uiOutput("yearInput_xiom"),
    #           uiOutput("yearInput_yeiz")
    #         ),
    #         
    #         # tab con datos de Incidentes
    #         # tabPanel("Incidentes_snmv",
    #         #          uiOutput("yearInput_yeiz")),
    #         # 
    #         # # tab con datos de Tasas
    #         # tabPanel("Tasas_snmv",
    #         #          uiOutput("yearInput_xiom")
    #         #          )
    #       )
    #     )
    #   )
    # )
  
  )
)
# )