#' Renderiza un gráfico de histograma utilizando ggplot2 en el UI de Shiny.
#' 
#' Esta función toma un conjunto de datos `data` y diversos parámetros para generar
#' un gráfico de histograma con ggplot2.
#' 
#' @param data Conjunto de datos a utilizar en el gráfico.
#' @param x Variable para el eje x.
#' @param fill Variable para asignar colores de relleno al histograma.
#' @param title Título del gráfico.
#' @param xlab Etiqueta del eje x.
#' @param ylab Etiqueta del eje y.
#' @param fillLab Etiqueta para la leyenda de colores de relleno.
#' @param binwidth Ancho del bin para el histograma.
#' @param color Color del borde de las barras del histograma.
#' @param alpha Transparencia de las barras del histograma.
#' @param xlim Límites del eje x.
#' @param ylim Límites del eje y.
#' 
#' @return No retorna un valor, pero imprime el gráfico generado en la consola.
#' 
#' @examples
#' # Ejemplo de uso:
#' renderHistogram(femi2023, "edad_v", "Víctimas", "Distribución de edades de las víctimas", "Edad", "Frecuencia de Feminicidios", binwidth = 5, color = "black", alpha = 0.7)
#' renderHistogram(femi2023, "edad_ag", "Agresores", "Distribución de edades de los agresores", "Edad", "Frecuencia de Feminicidios", binwidth = 5, color = "black", alpha = 0.7)
#' 
renderHistogram <- function(data, x, fill, title, xlab, ylab, fillLab = fill, binwidth, color, alpha, xlim = NULL, ylim = NULL) {
  p <- ggplot(data, aes_string(x = x, fill = fill)) +
    geom_histogram(binwidth = binwidth, color = color, alpha = alpha) +
    labs(title = title, x = xlab, y = ylab, fill = fillLab) +
    theme_minimal()
  
  if (!is.null(xlim)) {
    p <- p + scale_x_continuous(limits = xlim)
  }
  
  if (!is.null(ylim)) {
    p <- p + scale_y_continuous(limits = ylim)
  }
  
  print(p)
}

##############################


#############
renderLinePlot <- function(data, x, y, group, color, title, xlab, ylab, colorlab = color, colorLine) {
  p <- ggplot(data, aes_string(x = x, y = y, group = group, color = colorLine)) +
    geom_line(linewidth = 1.3) +  
    geom_point(size = 1.5) +      
    theme_minimal() +
    labs(title = title, x = xlab, y = ylab, color = colorlab) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}

renderLinePlot <- function(data, x, y, group, color, title, xlab, ylab, colorlab = color, colorLine) {
  p <- ggplot(data, aes_string(x = x, y = y, group = group)) +
    geom_line(aes(color = factor(colorLine)), linewidth = 1.3) +  
    geom_point(aes(color = factor(colorLine)), size = 1.5) +      
    theme_minimal() +
    labs(title = title, x = xlab, y = ylab, color = colorlab) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}

renderLinePlot <- function(data, x, y, group, color, title, xlab, ylab, colorlab = color, colorLine) {
  p <- ggplot(data, aes_string(x = x, y = y, group = group)) +
    geom_line(linewidth = 1.3, color = colorLine) +  # Especificar el color directamente
    geom_point(aes(color = factor(colorLine)), size = 1.5) +      
    theme_minimal() +
    labs(title = title, x = xlab, y = ylab, color = colorlab) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}

renderLinePlot <- function(data, x, y, group, color, title, xlab, ylab, colorlab = color, colorLine) {
  p <- ggplot(data, aes_string(x = x, y = y, group = group)) +
    geom_line(linewidth = 1.3, color = colorLine) +  
    geom_point(size = 1.5, color = colorLine) +  # Especificar el color directamente
    theme_minimal() +
    labs(title = title, x = xlab, y = ylab, color = colorlab) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}

homiEdad_fill_edad <- setColorFill(homiEdad, "edad")
renderLinePlot(data = homiEdad, x = "año", y = "casos", group = "edad", color = "edad",
              title = "Evolución de homicidios por Grupo de Edad en el Año",
              xlab = "Grupo de Edad", ylab = "Casos", colorlab = "pepe", colorLine = homiEdad_fill_edad)


############################
renderMap2 <- function(data, fill, title, group, fill_lab = fill, 
                      light_color = "lightblue", dark_color = "darkblue") {
  p <- ggplot(data) +
    geom_sf(aes(fill = {{fill}}, group = {{group}})) +  # Incluye group como aesthetic mapping
    labs(title = title, fill = fill_lab) +
    scale_fill_gradient(name = fill_lab, low = light_color, high = dark_color) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
  print(p)
}

############

ui <- fluidPage(
  titlePanel("Tabs con Dropdown Menu"),
  navbarPage(
    title = "Tabs",
    id = "tabs",
    navbarMenu("PARE",
               tabPanel("Agencia 1", "Contenido del Tab 5", icon = icon("suitcase")),
               tabPanel("Agencia 2", "Contenido del Agencia 6"),
               tabPanel("Agencia 3", "Contenido del Tab 7"),
               tabPanel("Agencia 4", "Contenido del Tab 8"),
               tabPanel("Agencia 5", "Contenido del Tab 9"),
               tabPanel("Agencia 6", "Contenido del Tab 10")
    ),
    navbarMenu("FEMINICIDIOS",
               tabPanel("Agencia 5", "Contenido del Tab 5"),
               tabPanel("Agencia 6", "Contenido del Tab 6"),
               tabPanel("Agencia 7", "Contenido del Tab 7"),
               tabPanel("Tab 8", "Contenido del Tab 8"),
               tabPanel("Tab 9", "Contenido del Tab 9"),
               tabPanel("Tab 10", "Contenido del Tab 10")
    )
  )
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)

##########
# importando la data de relaciones familiares
familiar <- read_excel(paste0(poli, "npprRelFam.xlsx")) %>%
  rename(Valor = Familiar) %>%
  mutate(Tipo = "Familiar")

# importando la data de relaciones no familiares
nofamiliar <- read_excel(paste0(poli, "npprRelNoFam.xlsx")) %>%
  rename(Valor = NoFamiliar) %>%
  mutate(Tipo = "No Familiar")

#
relacion <- bind_rows(familiar, nofamiliar) %>%
  pivot_longer(cols = -c(Relación,Tipo) , names_to = "Eliminar", values_to = "Casos") %>%
  select(-Eliminar)
relacion

##########

### mapa 
output$map_poli_inciDF <- renderPlotly({
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

renderBarPlot <- function(data, x, y, fill, title, xlab, ylab, fillLab = fill, colorFill = "Set1") {
  p <- ggplot(data, aes_string(x = x, y = y, fill = fill)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = colorFill) + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = title, x = xlab, y = ylab, fill = fillLab)
  
  print(p)
}
renderBarPlot(opmServiciosMes, x = "year", y = "cantidad", fill = "tipo",
              title = "Población atendida, servicios ofrecidos y seguimientos",
              xlab = "Año", ylab = "Cantidad de Servicios Ofrecidos", fillLab = "`Medio de Orientación`",
              colorFill = opmServiciosMes_fill_tipo)
###############################################################################
#### Procesamiento de datos de la Oficina de la Procuradora de las Mujeres ####
###############################################################################
opm <- here("data", "Oficina_de_procuradora_de_mujeres", "/")

#### violencia domestica ####
opmFemiVD <- read_excel(paste0(opm, "opmFemiVD.xlsx")) 
opmFemiVD

meses <- c("1" = "enero", "2" = "febrero", "3" = "marzo",  "4" = "abril", "5" = "mayo", "6" = "junio", "7" = "julio", "8" = "agosto", "9" = "septiembre","10" = "octubre", "11" = "noviembre", "12" = "diciembre")

#### Casos en Supervisión de Ley 54 ####
opmCasos <-  read_excel(paste0(path, "opmPartMes.xlsx")) %>%
  mutate(
    # la función as.yearmon convierte el año y mes a una sola fecha para poderla visualizar apropiadamente, la función es parte del paquete zoo
    fecha = as.yearmon(paste(year, month), "%Y %m"),
    month =  factor(month, levels = 1:12, labels = meses), 
    year = factor(year),
    tipo = factor(tipo)
  )
opmCasos

#### Tab de la Oficina de la Procuradora de las Mujeres ####
tabPanel(
  "Oficina de la Procuradora de la Mujer",
  icon = icon("person-dress"),
  tabsetPanel(
    
    #### tab con datos de violencia domestica (opmFemiVD) ####
    tabPanel(
      "opmFemiVD", 
      # Título del Tab
      titlePanel("Feminicidios por violencia doméstica, desde 1990 a 2021"),
      
      # Fuente de Datos, Actualización
      tags$span("Fuente: Oficina de la Procuradora de las Mujeres"), tags$br(),
      tags$span("Actualizado:", actualizacion_opmA), tags$br(),
      
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
      "opmCasos", 
      # Título del Tab
      titlePanel("Feminicidios por violencia doméstica, desde 1990 a 2021"),
      
      # Fuente de Datos, Actualización
      tags$span("Fuente: Oficina de la Procuradora de las Mujeres"), tags$br(),
      tags$span("Actualizado:", actualizacion_opmA), tags$br(),
      
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
          
          # botón para seleccionar el tipo de violencia
          createDropdownCheckbox(
            label = "Seleccione tipo de Violencia:",
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
    )
  )
)

############################################################################
#### Procesamiento de datos Departamento de Corrección y Rehabilitación ####
############################################################################
dcr <- here("data", "Departamento_de_correccion_y_rehabilitacion", "/")

#### dcrCasosInv ####
# importando el dataset de Casos en Supervisión de Ley 54
dcrCasosInv <- read_excel(paste0(dcr, "dcrCasosInv.xlsx")) %>%
  #filter(sexo != "Total") %>%
  mutate(
    year = factor(year),
    sexo = factor(sexo),
    tipo = factor(tipo)
  ) %>%
  select(
    -c(mes)
  )
dcrCasosInv

#### dcrSentenciadas ####
dcrSentenciadas <- read_excel(paste0(dcr, "dcrSentenciadas.xlsx"))  %>%
  mutate(
    # la función as.yearmon convierte el año y mes a una sola fecha para poderla visualizar apropiadamente, la función es parte del paquete zoo
    #fecha = as.yearmon(paste(year, mes), "%Y %m")
    tipo = factor(tipo)
  ) %>%
  select(-c(mes))
dcrSentenciadas




#### ui ####
#### tab con datos del género de las víctimas (dcrCasosInv) ####
tabPanel(
  "dcrCasosInv", 
  # Título del Tab
  titlePanel("Casos en Supervisión de Ley 54 en Programas Alternos al Confinamiento: Programas de Comunidad(desde enero 2021 hasta febrero 2023)"),
  
  # Fuente de Datos, Actualización
  tags$span("Fuente: Departamento de Corrección y Rehabilitación"), tags$br(),
  tags$span("Actualizado:", actualizacion_correcion), tags$br(),
  
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
      
      # botón para seleccionar el tipo de investigación 
      createDropdownCheckbox(
        label = "Seleccione el tipo de Investigación:",
        choices = dcrCasosInv$tipo,
        selected = dcrCasosInv$tipo,
        id = "dcr_dcrCasosInv_tipo"
      ),
      
      # botón para seleccionar el sexo
      createDropdownCheckbox(
        label = "Seleccione el sexo:",
        choices = dcrCasosInv$sexo,
        selected = dcrCasosInv$sexo,
        id = "dcr_dcrCasosInv_sexo"
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
  "dcrCasosInv", 
  # Título del Tab
  titlePanel("Personas sentenciadas integradas al Programa de Supervisión Electrónica por incurrir en delitos de violencia doméstica (desde enero 2020 hasta febrero 2023)"),
  
  # Fuente de Datos, Actualización
  tags$span("Fuente: Departamento de Corrección y Rehabilitación"), tags$br(),
  tags$span("Actualizado:", actualizacion_correcion), tags$br(),
  
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
      
      # botón para seleccionar el tipo de investigación 
      createDropdownCheckbox(
        label = "Seleccione el tipo de Investigación:",
        choices = dcrCasosInv$tipo,
        selected = dcrCasosInv$tipo,
        id = "dcr_dcrCasosInv_tipo"
      ),
      
      # botón para seleccionar el sexo
      createDropdownCheckbox(
        label = "Seleccione el sexo:",
        choices = dcrCasosInv$sexo,
        selected = dcrCasosInv$sexo,
        id = "dcr_dcrCasosInv_sexo"
      ),
    ),
    
    # Sección principal con los gráficos
    mainPanel(
      plotlyOutput("barPlot_dcr_dcrCasosInv"),
      DTOutput("dataTable_dcr_dcrCasosInv")
    )
  ),
)





#### tab con datos de orientaciones medio de comunicación (opmMedio) ####
tabPanel(
  "opmMedio", 
  # Título del Tab
  titlePanel("Orientaciones según el Medio (a través de CRIAS). Años 2020 a *2023."),
  
  # Fuente de Datos, Actualización
  tags$span("Fuente: Oficina de la Procuradora de las Mujeres"), tags$br(),
  tags$span("Actualizado:", actualizacion_opmB), tags$br(),
  
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
      
      # botón para seleccionar el medio de orientación
      createDropdownCheckbox(
        label = "Seleccione el Medio de Orientación:",
        choices = opmMedio$`Medio de orientación`,
        selected = opmMedio$`Medio de orientación`,
        id = "opm_opmVic_medio"
      ),
    ),
    
    # Sección principal con los gráficos
    mainPanel(
      plotlyOutput("barPlot_opm_opmMedio"),
      DTOutput("dataTable_opm_opmMedio")
    )
  ),
)

#### tab con datos de los servicios ofrecidos por mes (opmServiciosMes) ####
tabPanel(
  "opmServiciosMes", 
  # Título del Tab
  titlePanel("Población atendida, servicios ofrecidos y seguimientos por mes. Años 2020 a *2023."),
  
  # Fuente de Datos, Actualización
  tags$span("Fuente: Oficina de la Procuradora de las Mujeres"), tags$br(),
  tags$span("Actualizado:", actualizacion_opmB), tags$br(),
  
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
)


#### tab con datos de los servicios ofrecidos (opmServiciosMes) ####
# Filtrar el conjunto de datos según los valores seleccionados del año y el tipo de servicio
opmServiciosMes_filt <- reactive({
  filter(opmServiciosMes,
         año %in% input$checkGroup_opm_opmServiciosMes_año,
         tipo %in% input$checkGroup_opm_opmServiciosMes_tipo
  )
})

### funcion para el boton de deseleccionar/seleccionar del botón de año
observeEvent(input$deselectAll_opm_opmServiciosMes_año, {
  updateCheckboxGroup(session, "checkGroup_opm_opmServiciosMes_año", input, opmServiciosMes$año)
})

### funcion para el boton de deseleccionar/seleccionar del botón de tipo de servicio
observeEvent(input$deselectAll_opm_opmServiciosMes_tipo, {
  updateCheckboxGroup(session, "checkGroup_opm_opmServiciosMes_medio", input, opmServiciosMes$tipo)
})

# Colores del status
opmServiciosMes_fill_tipo <- setColorFill(opmServiciosMes, "tipo")
# Grafico de barras
output$barPlot_opm_opmServiciosMes <- renderPlotly({
  p <- renderBarPlot(opmServiciosMes_filt, x = "año", y = "cantidad", fill = "tipo",
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

#### tab con datos del género de las víctimas (opmMedio) ####
# Filtrar el conjunto de datos según los valores seleccionados del año y el género de la víctima
opmMedio_filt <- reactive({
  filter(opmMedio,
         año %in% input$checkGroup_opm_opmMedio_año,
         género %in% input$checkGroup_opm_opmVic_género
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
output$barPlot_opm_opmCasos <- renderPlotly({
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







#### tab con datos de orientaciones medio de comunicación (opmMedio) ####
#### tab con datos de los servicios ofrecidos por mes (opmServiciosMes) ####