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

