# pepe: traduce a español
print("Sourcing helper functions from utils.R")
###############################
#### Helper Functions: UI  ####
###############################
# Función para crear un checkbox group
createCheckboxGroup <- function(inputId, label, choices, selected) {
  checkboxGroupInput(inputId, label, choices = choices, selected = selected)
}

# Función para crear un select input
createSelectInput <- function(inputId, label, choices, selected) {
  selectInput(inputId, label, choices = choices, selected = selected)
}

# Función para crear un gráfico de línea
createLinePlot <- function(outputId, plot) {
  plotlyOutput(outputId, width = "100%", height = "400px")
}

# Función para crear un gráfico de barras
createBarPlot <- function(outputId, plot) {
  plotlyOutput(outputId, width = "100%", height = "400px")
}

# Función para crear una tabla de datos
createDataTable <- function(outputId, dataTable) {
  DTOutput(outputId)
}


##################################
#### Helper Functions: Server #### 
##################################

# Función para filtrar el conjunto de datos según los valores seleccionados
filterData <- function(data, inputValues, column) {
  filter(data, (!!sym(column)) %in% inputValues)
}

### funcion para el boton de deseleccionar/seleccionar
updateCheckboxGroup <- function(session, inputId, input, data) {
  print(is.null(input[[inputId]]))
  print(input[[inputId]])
  
  if (is.null(input[[inputId]])) {
    # "El checkbox está vacío, llénalo"
    updateCheckboxGroupInput(
      session,
      inputId,
      choices = levels(data),
      selected = levels(data)
    )
  } else {
    # "Vacía el checkbox"
    updateCheckboxGroupInput(
      session,
      inputId,
      selected = character(0)
    )
  }
}

# Función para renderizar gráficos de línea
renderLinePlot <- function(data, x, y, group, color, title, xlab, ylab, colorlab = color, facet = FALSE, facet_var = NULL) {
  p <- ggplot(data(), aes_string(x = x, y = y, group = group, color = color)) +
    geom_line(linewidth = 1.3) +
    geom_point(size = 1.5) +
    theme_minimal() +
    labs(title = title, x = xlab, y = ylab, color = color) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  if (facet && !is.null(facet_var)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_var)), scales = "fixed", drop = FALSE)
  }
  
  print(p)
}

#### cosas pa añadir a renderLinePlot
#scale_fill_manual(values = colores_homiEdad) # añadir funcionalidad para setear manualmente los colores de las graficas

# Función para renderizar gráficos de barras
renderBarPlot <- function(data, x, y, fill, title, xlab, ylab, fillLab = fill, facet = FALSE, facet_var = NULL) {
  p <- ggplot(data(), aes_string(x = x, y = y, fill = fill)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_flip() +
    labs(title = title, x = xlab, y = ylab, fill = fillLab)
  
  if (facet && !is.null(facet_var)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_var)), scales = "fixed", drop = FALSE)
  }
  print(p)
}

# Función para renderizar gráficos de caja
renderBoxPlot <- function(data, x, y, color, title, xlab, ylab, colorlab = color, facet = FALSE, facet_var = NULL) {
  p <- ggplot(data, aes_string(x = x, y = y)) +
    geom_boxplot() +
    geom_point(aes(color = color)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = title, x = xlab, y = ylab, color = colorlab)
  
  if (facet && !is.null(facet_var)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_var)), scales = "fixed", drop = FALSE)
  }
  print(p)
}

# Función para renderizar gráficos de distribución
renderDistributionPlot <- function(data, x, y, fill, title, xlab, ylab) {
  ggplot(data, aes_string(x = x, y = y, fill = fill)) +
    geom_bar(stat = "identity", width = 0.7) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_flip() +
    labs(title = title, x = xlab, y = ylab, fill = fill)
}

# Función para renderizar gráficos de puntos
renderPointPlot <- function(data, x, y, title, xlab, ylab) {
  ggplot(data, aes_string(x = x, y = y)) +
    geom_bar(stat = "identity", width = 0.7) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_flip() +
    labs(title = title, x = xlab, y = ylab)
}

# Función para renderizar tablas de datos
renderDataTable <- function(filtered_data) {
  datatable(
    filtered_data,
    extensions = c('Buttons'), # Asegúrate de incluir la extensión 'Buttons'
    options = list(
      pageLength = 5,
      lengthMenu = c(5, nrow(filtered_data) / 2, nrow(filtered_data)),
      scrollX = TRUE,
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = FALSE,
      ordering = TRUE,
      dom = 'lftpB',
      buttons = c('copy', 'csv', 'excel')
    )
  )
}

# Función para observar cambios en la selección del año
observeYearSelection <- function(inputId, session, selectedYear) {
  observeEvent(input[[inputId]], {
    updateSelectInput(session, inputId, selected = selectedYear)
  })
}