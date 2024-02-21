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
