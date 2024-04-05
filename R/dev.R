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

######################## 148
## Solicitudes de órdenes de protección al amparo de la Ley 148 - Violencia Sexual, por Región Judicial y grupo de edad de la parte peticionaria
```{r, echo=FALSE, warning=FALSE, message=FALSE}
# importando datos del año fiscal 2020-2021
## faltan datos
casosCrimLey148_20 <- read_excel(paste0(trib, "casosCrimLey148_20.xlsx")) %>%
  rename(
    Delito = `Año fiscal/delitos`
  ) %>%
  pivot_longer(
    !Delito,
    names_to = "Status",
    values_to = "Casos"
  ) %>%
  mutate(AñoFiscal = "2020-2021") %>%
  filter(Delito != "2020-2021")

# importando datos del año fiscal 2021-2022
## faltan datos
casosCrimLey148_21 <- read_excel(paste0(trib, "casosCrimLey148_21.xlsx")) %>%
  rename(
    Delito = `Año fiscal/delitos`
  ) %>%
  pivot_longer(
    !Delito,
    names_to = "Status",
    values_to = "Casos"
  ) %>%
  mutate(AñoFiscal = "2021-2022") %>%
  filter(Delito != "2021-2022*")

## faltan datos
casosCrimLey148_long <- full_join(casosCrimLey148_20, casosCrimLey148_21)
casosCrimLey148_long
summary(casosCrimLey148_long)
```

# Solicitudes de órdenes de protección al amparo de la Ley 148 - Violencia Sexual, por Región Judicial y grupo de edad de la parte peticionaria
```{r, echo=FALSE, warning=FALSE, message=FALSE}
new_names <- c("Total", "<20", "21-29", "30-39", "40-49", "50-59", ">60", "No Indica")

# datos de solicitudes de órdenes de protección en el 2020-2021
OP_148_SoliGrupEdad2020_21 <- read_excel(paste0(trib, "OP_148_SoliGrupEdad2020_21.xlsx")) %>%
  rename_at(vars(2:9), ~ new_names) %>%
  pivot_longer(
    !Región, 
    names_to = "Edad", 
    values_to = "Solicitudes"
  ) %>%
  mutate(
    Edad = factor(Edad, levels = unique(Edad)),
    AñoFiscal = "2020-2021"
  ) %>%
  filter(
    Edad != "Total",
    Región != "Total"
  )

# datos de solicitudes de órdenes de protección en el 2021-2022
OP_148_SoliGrupEdad2021_22 <- read_excel(paste0(trib, "OP_148_SoliGrupEdad2021_22.xlsx")) %>%
  rename_at(vars(2:9), ~ new_names) %>%
  pivot_longer(
    !Región, 
    names_to = "Edad", 
    values_to = "Solicitudes"
  ) %>%
  mutate(
    Edad = factor(Edad, levels = unique(Edad)),
    AñoFiscal = "2021-2022"
  ) %>%
  filter(
    Edad != "Total",
    Región != "Total"
  )

# dataset unido
OP_148_SoliGrupEdad_long <- full_join(
  OP_148_SoliGrupEdad2020_21, OP_148_SoliGrupEdad2021_22) %>%
  mutate(
    AñoFiscal = factor(AñoFiscal, levels = unique(AñoFiscal))
  )
OP_148_SoliGrupEdad_long
```

# Órdenes de protección ex parte emitidas al amparo de la Ley 148 - Violencia Sexual, por Región Judicial y delito
```{r, echo=FALSE, warning=FALSE, message=FALSE}
# lista con nuevos nombres de columnas para mejor interpretación
new_names <- c("Total", "Agresión Sexual", "Acoso Sexual", "Actos Lascivos", "Incesto") 

# datos de solicitudes de órdenes de protección en el 2021-2022
OP_Ley148_ex_parteEmitidas2020_21 <- read_excel(paste0(trib, "OP_Ley148_ex_parteEmitidas2020_21.xlsx")) %>%
  rename_at(vars(2:6), ~ new_names) %>%
  pivot_longer(
    !Región, 
    names_to = "Delito", 
    values_to = "ÓrdenesEmitidas"
  ) %>%
  mutate(
    AñoFiscal = factor("2020-2021")
  ) %>%
  filter(
    Delito != "Total"
  )

# datos de solicitudes de órdenes de protección en el 2021-2022
OP_Ley148_ex_parteEmitidas2021_22 <- read_excel(paste0(trib, "OP_Ley148_ex_parteEmitidas2021_22.xlsx")) %>%
  rename_at(vars(2:6), ~ new_names) %>%
  pivot_longer(
    !Región, 
    names_to = "Delito", 
    values_to = "ÓrdenesEmitidas"
  ) %>%
  mutate(
    AñoFiscal = factor("2021-2022")
  ) %>%
  filter(
    Delito != "Total"
  )

# dataset unido
OP_Ley148_ex_parteEmitidas <- full_join(
  OP_Ley148_ex_parteEmitidas2020_21, OP_Ley148_ex_parteEmitidas2021_22)
OP_Ley148_ex_parteEmitidas
```

# Cantidad de solicitudes de órdenes de protección al amparo de la Ley 148 - Violencia Sexual archivadas por Región Judicial
```{r, echo=FALSE, warning=FALSE, message=FALSE}
# lista con nuevos nombres de columnas para mejor interpretación
new_names <- c("Total", "SolicitudPeticionaria", "Otra Razón")

# datos de solicitudes archivadas de órdenes de protección en 2020-2021
OP_LEY148Archivadas2020_21 <- read_excel(paste0(trib, "OP_LEY148Archivadas2020_21.xlsx")) %>%
  rename_at(vars(2:4), ~ new_names) %>%
  pivot_longer(
    !Región, 
    names_to = "Razón", 
    values_to = "ÓrdenesArchivadas"
  ) %>%
  mutate(
    AñoFiscal = factor("2020-2021")
  ) %>%
  filter(
    Razón != "Total"
  )

# datos de solicitudes archivadas de órdenes de protección en 2021-2022
OP_LEY148Archivadas2021_22 <- read_excel(paste0(trib, "OP_LEY148Archivadas2021_22.xlsx")) %>%
  rename_at(vars(2:4), ~ new_names) %>%
  pivot_longer(
    !Región, 
    names_to = "Razón", 
    values_to = "ÓrdenesArchivadas"
  ) %>%
  mutate(
    AñoFiscal = factor("2021-2022")
  ) %>%
  filter(
    Razón != "Total"
  )

# datos de solicitudes archivadas de órdenes de protección en juntadas
OP_LEY148Archivadas <- full_join(
  OP_LEY148Archivadas2020_21, OP_LEY148Archivadas2021_22)
OP_LEY148Archivadas
```

# Cantidad de solicitudes de órdenes de protección al amparo de la Ley 148 - Violencia Sexual denegadas por Región Judicial
```{r, echo=FALSE, warning=FALSE, message=FALSE}
# lista con nuevos nombres de columnas para mejor interpretación
new_names <- c("No Aplican Disposiciones Ley148", "No Prueban Elementos")

# datos de solicitudes denegadas de órdenes de protección en 2022-2023
OP_LEY148Denegadas2020_2021 <- read_excel(paste0(trib, "OP_LEY148Denegadas2020_21.xlsx")) %>%
  rename_at(vars(3:4), ~ new_names) %>%
  pivot_longer(
    !Región, 
    names_to = "Razón", 
    values_to = "ÓrdenesDenegadas"
  ) %>%
  mutate(
    AñoFiscal = factor("2022-2023")
  ) %>%
  filter(
    Región != "Total",
    Razón != "Total"
  )

# datos de solicitudes denegadas de órdenes de protección en 2021-2022
OP_LEY148Denegadas2021_22 <- read_excel(paste0(trib, "OP_LEY148Denegadas2021_22.xlsx")) %>%
  rename_at(vars(3:4), ~ new_names) %>%
  pivot_longer(
    !Región, 
    names_to = "Razón", 
    values_to = "ÓrdenesDenegadas"
  ) %>%
  mutate(
    AñoFiscal = factor("2021-2022")
  ) %>%
  filter(
    Región != "Total",
    Razón != "Total"
  )

# dataset joined
OP_LEY148Denegadas <- full_join(
  OP_LEY148Denegadas2020_2021, OP_LEY148Denegadas2021_22)
OP_LEY148Denegadas
```

# Órdenes de protección ex parte emitidas al amparo de la Ley 148 - Violencia Sexual, por Región Judicial y delito
```{r, echo=FALSE, warning=FALSE, message=FALSE}
# lista con nuevos nombres de columnas para mejor interpretación
new_names <- c("Total", "Agresión Sexual", "Acoso Sexual", "Actos Lascivos", "Incesto")

# datos de solicitudes ex parte emitidas de la ley 148 en 2020-2021
OP_LEY148FinalEmitidas2020_21 <- read_excel(paste0(trib, "OP_LEY148FinalEmitidas2020_21.xlsx")) %>%
  rename_at(vars(2:6), ~ new_names) %>%
  pivot_longer(
    !Región, 
    names_to = "Delito", 
    values_to = "ÓrdenesEmitidas"
  ) %>%
  mutate(
    AñoFiscal = factor("2020-2021")
  ) %>%
  filter(
    Región != "Total",
    Delito != "Total"
  )

# datos de solicitudes ex parte emitidas de la ley 148 en 2021-2022
OP_LEY148FinalEmitidas2021_22 <- read_excel(paste0(trib, "OP_LEY148FinalEmitidas2021_22.xlsx")) %>%
  rename_at(vars(2:6), ~ new_names) %>%
  pivot_longer(
    !Región, 
    names_to = "Delito", 
    values_to = "ÓrdenesEmitidas"
  ) %>%
  mutate(
    AñoFiscal = factor("2021-2022")
  ) %>%
  filter(
    Región != "Total",
    Delito != "Total"
  )

# dataset joined
OP_LEY148FinalEmitidas <- full_join(
  OP_LEY148FinalEmitidas2020_21, OP_LEY148FinalEmitidas2021_22)
OP_LEY148FinalEmitidas
```

# Solicitudes de órdenes de protección al amparo de la Ley 148 - Violencia Sexual, por sexo de la parte
```{r, echo=FALSE, warning=FALSE, message=FALSE}
OP_LEY148Genero2020_21 <- read_excel(paste0(trib, "OP_LEY148Genero2020_21.xlsx")) %>%
  pivot_longer(
    !Sexo, 
    names_to = "Parte", 
    values_to = "Solicitudes"
  ) %>%
  mutate(
    AñoFiscal = factor("2020-2021")
  ) %>%
  filter(
    Sexo != "Total"
  )
OP_LEY148Genero2021_22 <- read_excel(paste0(trib, "OP_LEY148Genero2021_22.xlsx")) %>%
  pivot_longer(
    !Sexo, 
    names_to = "Parte", 
    values_to = "Solicitudes"
  ) %>%
  mutate(
    AñoFiscal = factor("2021-2022")
  ) %>%
  filter(
    Sexo != "Total"
  )

# dataset joined
OP_LEY148Genero <- full_join(
  OP_LEY148Genero2020_21, OP_LEY148Genero2021_22)
OP_LEY148Genero
```

# Tribunal de Primera Instancia: Movimiento de casos criminales de acoso sexual, actos lascivos, agresión sexual, incesto y ley contra el acecho. Ley Núm. 148-2015
```{r, echo=FALSE, warning=FALSE, message=FALSE}
# lista con nuevos nombres de columnas para mejor interpretación 
new_names <- c("Delito", "Pendiente Inicio", "Presentados", 
               "A Resolver", "Condenas", "Absoluciones", 
               "Archivos", "Traslados", "Otros", "Total", "Pendiente Final")

# datos de movimiento de casos criminales en año fiscal 2019-2020
tribCasosCrim19 <- read_excel(paste0(trib, "tribCasosCrim19.xlsx")) %>%
  rename_at(vars(1:11), ~ new_names) %>%
  pivot_longer(
    !Delito, 
    names_to = "Casos", 
    values_to = "Cantidad"
  ) %>%
  mutate(
    AñoFiscal = factor("2019-2020")
  ) %>%
  filter(
    !(Delito %in% c("2019-2020", "Total")
    )
  )

# datos de movimiento de casos criminales en año fiscal 2020-2021
tribCasosCrim20 <- read_excel(paste0(trib, "tribCasosCrim20.xlsx")) %>%
  rename_at(vars(1:11), ~ new_names) %>%
  pivot_longer(
    !Delito, 
    names_to = "Casos", 
    values_to = "Cantidad"
  ) %>%
  mutate(
    AñoFiscal = factor("2020-2021")
  ) %>%
  filter(
    !(Delito %in% c("2020-2021", "Total")
    )
  )

# datos de movimiento de casos criminales en año fiscal 2021-2022
tribCasosCrim21 <- read_excel(paste0(trib, "tribCasosCrim21.xlsx")) %>%
  rename_at(vars(1:11), ~ new_names) %>%
  pivot_longer(
    !Delito, 
    names_to = "Casos", 
    values_to = "Cantidad"
  ) %>%
  mutate(
    AñoFiscal = factor("2021-2022")
  ) %>%
  filter(
    !(Delito %in% c("2021-2022", "Total")
    )
  )

# dataset joined
tribCasosCrim <- full_join(
  tribCasosCrim19, tribCasosCrim20) %>%
  full_join(tribCasosCrim21)
tribCasosCrim
```