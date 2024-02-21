cat("Loading Server helper functions from server_helpers.R...\n")
##################################
#### Helper Functions: Server #### 
##################################

# Función para filtrar el conjunto de datos según los valores seleccionados
filterData <- function(data, inputValues, column) {
  filter(data, (!!sym(column)) %in% inputValues)
}

#' Actualiza un grupo de checkbox en el UI de Shiny.
#' 
#' Esta función toma como entrada el `session`, el `inputId` correspondiente al grupo
#' de checkbox, el objeto `input` y el conjunto de datos `data`. Dependiendo de si el
#' grupo de checkbox está vacío o no, la función llena o vacía el grupo respectivamente.
#' 
#' @param session Una sesión de Shiny.
#' @param inputId Identificación única del grupo de checkbox en el UI.
#' @param input Objeto `input` de Shiny que contiene los valores actuales del UI.
#' @param data Conjunto de datos que proporciona las opciones para el grupo de checkbox.
#' 
#' @return No retorna un valor, pero actualiza el UI de Shiny según la lógica descrita.
#' 
#' @examples
#' # Ejemplo de uso:
#' updateCheckboxGroup(session, "checkGroup_snmv", input, homiEdad)
#' 
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

#' Renderiza un gráfico de línea utilizando ggplot2 en el UI de Shiny.
#' 
#' Esta función toma un conjunto de datos `data` y diversos parámetros para generar
#' un gráfico de línea con ggplot2. Puede incluir opciones adicionales como facetas.
#' 
#' @param data Conjunto de datos a utilizar en el gráfico.
#' @param x Variable para el eje x.
#' @param y Variable para el eje y.
#' @param group Variable para agrupar los datos.
#' @param color Variable para asignar colores.
#' @param title Título del gráfico.
#' @param xlab Etiqueta del eje x.
#' @param ylab Etiqueta del eje y.
#' @param colorlab Etiqueta para la leyenda de colores.
#' @param facet Booleano que indica si se deben agregar facetas al gráfico.
#' @param facet_var Variable para facetas (si `facet` es TRUE).
#' 
#' @return No retorna un valor, pero imprime el gráfico generado en la consola.
#' 
#' @examples
#' # Ejemplo de uso:
#' renderLinePlot(homiEdad, "x", "y", "group", "color", "Title", "X Label", "Y Label")
#' 
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

#' Renderiza un gráfico de barras utilizando ggplot2 en el UI de Shiny.
#' 
#' Esta función toma un conjunto de datos `data` y diversos parámetros para generar
#' un gráfico de barras con ggplot2. Puede incluir opciones adicionales como facetas.
#' 
#' @param data Conjunto de datos a utilizar en el gráfico.
#' @param x Variable para el eje x.
#' @param y Variable para el eje y.
#' @param fill Variable para asignar colores de relleno a las barras.
#' @param title Título del gráfico.
#' @param xlab Etiqueta del eje x.
#' @param ylab Etiqueta del eje y.
#' @param fillLab Etiqueta para la leyenda de colores de relleno.
#' @param facet Booleano que indica si se deben agregar facetas al gráfico.
#' @param facet_var Variable para facetas (si `facet` es TRUE).
#' 
#' @return No retorna un valor, pero imprime el gráfico generado en la consola.
#' 
#' @examples
#' # Ejemplo de uso:
#' renderBarPlot(homiEdad, "x", "y", "fill", "Title", "X Label", "Y Label")
#' 
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

#' Renderiza un gráfico de caja utilizando ggplot2 en el UI de Shiny.
#' 
#' Esta función toma un conjunto de datos `data` y diversos parámetros para generar
#' un gráfico de caja con ggplot2. Puede incluir opciones adicionales como facetas.
#' 
#' @param data Conjunto de datos a utilizar en el gráfico.
#' @param x Variable para el eje x.
#' @param y Variable para el eje y.
#' @param color Variable para asignar colores a los puntos en el gráfico de caja.
#' @param title Título del gráfico.
#' @param xlab Etiqueta del eje x.
#' @param ylab Etiqueta del eje y.
#' @param colorlab Etiqueta para la leyenda de colores.
#' @param facet Booleano que indica si se deben agregar facetas al gráfico.
#' @param facet_var Variable para facetas (si `facet` es TRUE).
#' 
#' @return No retorna un valor, pero imprime el gráfico generado en la consola.
#' 
#' @examples
#' # Ejemplo de uso:
#' renderBoxPlot(homiEdad, "x", "y", "color", "Title", "X Label", "Y Label")
#' 
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

#' Renderiza una tabla de datos utilizando el paquete DT en el UI de Shiny.
#' 
#' Esta función toma un conjunto de datos `filtered_data` y utiliza la librería DT
#' para renderizar una tabla interactiva en el UI de Shiny.
#' 
#' @param filtered_data Conjunto de datos a utilizar en la tabla.
#' 
#' @return No retorna un valor, pero renderiza la tabla en el UI de Shiny.
#' 
#' @examples
#' # Ejemplo de uso:
#' renderDataTable(filtered_data)
#' 
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

#' Renderiza un mapa utilizando ggplot2 en el UI de Shiny.
#' 
#' Esta función toma un conjunto de datos y diversos parámetros para generar un mapa utilizando ggplot2.
#' 
#' @param data Conjunto de datos a utilizar en el mapa. Debe ser un objeto que contenga datos espaciales,
#' como un objeto de clase 'sf' de la librería sf.
#' @param fill Variable para la escala de colores. Debe ser una columna del conjunto de datos `data` que 
#' contiene los valores numéricos que se utilizarán para determinar la intensidad del color en el mapa.
#' @param title Título del mapa. Debe ser una cadena de caracteres que describa el contenido del mapa.
#' @param fill_lab Etiqueta para la leyenda de colores. Debe ser una cadena de caracteres que describa
#' la variable representada por la escala de colores.
#' @param fill_color Color base para la escala de colores. Debe ser un nombre de color válido en R o un
#' código hexadecimal que represente un color válido.
#' 
#' @return Esta función no retorna un valor, pero imprime el mapa generado en la consola.
#' 
#' @examples
#' # Ejemplo de uso:
#' renderMap(datos_delito_seleccionado, "Casos", "Incidencia de casos de Delito X")
#' 
renderMap <- function(data, fill, title, 
                       fill_lab = fill, fill_color = "blue") {
  p <- ggplot(data) +
    geom_sf(aes(fill = {{fill}})) +
    labs(title = title, fill = fill_lab) +
    scale_fill_gradient(name = fill_lab, low = paste0("light", fill_color), high = paste0("dark", fill_color)) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
  
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

# Función para observar cambios en la selección del año
observeYearSelection <- function(inputId, session, selectedYear) {
  observeEvent(input[[inputId]], {
    updateSelectInput(session, inputId, selected = selectedYear)
  })
}