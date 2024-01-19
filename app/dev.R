# Función para renderizar gráficos de línea
renderLinePlot <- function(data, x, y, group, color, title, xlab, ylab, colorlab = color, facet = FALSE, facet_var = NULL) {
  p <- ggplot(data, aes_string(x = x, y = y, group = group, color = color)) +
    geom_line(linewidth = 1.3) +
    geom_point(size = 1.5) +
    theme_minimal() +
    labs(title = title, x = xlab, y = ylab, color = colorlab) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  if (facet && !is.null(facet_var)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_var)), scales = "fixed", drop = FALSE)
  }
  
  print(p)
}

# Función para renderizar gráficos de barras
renderBarPlot <- function(data, x, y, fill, title, xlab, ylab, fillLab = fill, facet = FALSE, facet_var = NULL) {
  p <- ggplot(data, aes_string(x = x, y = y, fill = fill)) +
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
    labs(title = title, x = xlab, y = ylab, color = colorlab)
  
  if (facet && !is.null(facet_var)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_var)), scales = "fixed", drop = FALSE)
  }
  print(p)
}
# Visualize data using your functions

# Line plot
renderLinePlot(dfMalt, "Año", "Casos", "Maltrato", "Sexo",
               "Casos de Maltrato por Año y Tipo", "Año", "Casos", facet = TRUE, facet_var = "Sexo")

# Bar plot
renderBarPlot(dfMalt, "Año", "Casos", "Sexo",
              "Casos de Maltrato por Año y Tipo", "Año", "Casos", facet = TRUE, facet_var = "Maltrato")

# Box plot
renderBoxPlot(dfMalt, "Maltrato", "Casos", "Sexo",
              "Casos de Maltrato por Tipo y Sexo", "Tipo de Maltrato", "Casos", facet = TRUE, facet_var = "Año")
