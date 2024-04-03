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
  updateCheckboxGroup(session, "checkGroup_dcr_dcrSentenciadas_año", input, dcrSentenciadas$year)
})

### funcion para el boton de deseleccionar/seleccionar del botón del estado de caso 
observeEvent(input$deselectAll_dcr_dcrSentenciadas_tipo, {
  updateCheckboxGroup(session, "checkGroup_dcr_dcrSentenciadas_medio", input, dcrSentenciadas$tipo)
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













#### tab con datos de orientaciones medio de comunicación (opmMedio) ####
#### tab con datos de los servicios ofrecidos por mes (opmServiciosMes) ####