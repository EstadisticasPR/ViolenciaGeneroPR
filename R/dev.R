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
########### tab template

avp <- here("data", "Administracion_de_viviendas_publicas", "/")

# importando el dataset de Casos en Supervisión de Ley 54
avpAsignadas <- read_excel(paste0(avp, "avpAsignadas2017_23.xlsx")) %>% 
  rename(región = `Región `) %>%
  pivot_longer(!región, names_to = "año", values_to = "asignadas")

avpSolicitadas <- read_excel(paste0(avp, "avpSolicitudes2017_23.xlsx")) %>% 
  rename(región = `Región `) %>%
  pivot_longer(!región, names_to = "año", values_to = "solicitadas")

# Unir los datasets por columna "región" y "año"
dfAvp <- left_join(avpSolicitadas, avpAsignadas, by = c("región", "año")) %>% 
  filter(región != "Total") %>%
  mutate(
    región = factor(región)
  ) %>%
  pivot_longer(
    !c(región, año), names_to = "status", values_to = "cantidad"
  )

# Convertir el año a numérico para eliminar el asterisco y convertirlo a int
dfAvp$año <- as.factor(sub("\\*", "", dfAvp$año))
