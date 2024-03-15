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


#' Crear un dropdownButton con checkboxGroupInput
#'
#' Esta función crea un dropdownButton con un checkboxGroupInput dentro,
#' estilizado con inline-block.
#'
#' @param label La etiqueta para el dropdownButton.
#' @param choices Las opciones para el checkboxGroupInput.
#' @param selected Las opciones seleccionadas para el checkboxGroupInput.
#' @param id El identificador a ser usado en el checkboxGroupInput.
#' @param actionButtonId El identificador para el actionButton dentro del dropdownButton.
#' @return Un div conteniendo el dropdownButton con las opciones especificadas.
#' @export
#' @examples
#' createDropdownCheckbox(
#'   label = "Seleccionar Grupos:",
#'   choices = c("A", "B", "C"),
#'   selected = "A",
#'   id = "grupo",
#'   actionButtonId = "deselectAll_grupo"
#' )
createDropdownCheckbox1 <- function(label, choices, selected, id) {
  div(
    dropdownButton(
      circle = FALSE,
      label = label,
      status = "default",
      size = "default",
      checkboxGroupInput(
        paste0("checkGroup_", id),
        label = "",
        choices = levels(choices),
        selected = choices[selected]
      ),
      actionButton(paste0("deselectAll_", id), "(De)seleccionar todo")
    ),
    style = "display: inline-block; padding-right: 20px;"
  )
}

createDropdownCheckbox2 <- function(choices, selected) {
  choices <- levels(choices)
  if (is.null(selected)) {
    selected <- choices  # Seleccionar todos los elementos
  } else {
    selected <- choices[selected]
  }
  print(selected)
}


createDropdownCheckbox2(
  choices = homiEdad$edad,
  selected = 8
)

createDropdownCheckbox2(
  choices = homiEdad$edad,
  selected = homiEdad$edad
)

createDropdownCheckbox2(
  choices = homiEdad$edad,
  selected = NULL
)





















