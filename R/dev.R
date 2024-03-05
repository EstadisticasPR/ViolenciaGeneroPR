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

renderMap <- function(data, fill, title, fill_lab = fill, 
                      light_color = "lightblue", dark_color = "darkblue") {
  p <- ggplot(data) +
    geom_sf(aes(fill = {{fill}})) +
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
