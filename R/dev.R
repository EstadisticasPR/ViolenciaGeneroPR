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


################################
#' Genera una paleta de colores para los niveles de una variable categórica.
#' 
#' Esta función toma un dataframe y el nombre de una variable categórica, 
#' y devuelve una paleta de colores con un color único para cada nivel de la variable.
#' 
#' @param df Un dataframe que contiene la variable categórica.
#' @param variable El nombre de la variable categórica en el dataframe.
#' 
#' @return Un vector con los nombres de los niveles de la variable como claves y colores únicos como valores.
#' 
#' @examples
#' # Ejemplo de uso:
#' generate_color_vector(data, "Sexo")
#' 
generate_color_vector <- function(df, variable) {
  # Obtener los niveles únicos de la variable
  unique_levels <- unique(df[[variable]])
  
  # Generar una paleta de colores basada en el número de niveles únicos
  my_colors <- rainbow(length(unique_levels))
  names(my_colors) <- unique_levels
  
  return(my_colors)
}























