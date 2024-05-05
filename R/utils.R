cat("Loading helper functions from utils.R...\n")
###############################
#### Helper Functions: UI  ####
###############################

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
createDropdownCheckbox <- function(label, choices, selected, id) {
  choices <- levels(choices)
  if (is.null(selected)) {
    selected <- choices  # Seleccionar todos los elementos
  } else {
    selected <- choices[selected]
  }

  div(
    dropdownButton(
      circle = FALSE,
      label = label,
      status = "default",
      size = "default",
      checkboxGroupInput(
        paste0("checkGroup_", id),
        label = "",
        choices = choices,
        selected = selected
      ),
      actionButton(paste0("deselectAll_", id), "(De)seleccionar todo")
    ),
    style = "display: inline-block; padding-right: 20px;",
    tags$style(HTML('
      .btn {
        background-color: lightgrey !important;
        color: black !important;
        border-color: #ccc !important;
      }
      .btn-default:hover, .btn-default:focus, .btn-default:active {
        background-color: #f5f5f5 !important;
        color: black !important;
      }
    '))
  )
}

#' Crea una imagen con enlace a una página web
#'
#' Esta función crea una imagen con un enlace a una página web especificada.
#' 
#' @param ID El identificador del contenedor de la imagen.
#' @param img_src La ruta de la imagen.
#' @param link_href La URL de la página web a la que se enlazará la imagen.
#' @param link_alt El texto alternativo para la imagen.
#' @return Una lista HTML con la imagen enlazada.
#' @examples
#' embedImage("logo_IEPR", "www/iepr_logo.png", "https://estadisticas.pr/", "estadisticas.pr")
#' embedImage("logo_PARE", "www/logo_PARE.png", "https://parelaviolencia.pr.gov/", "PARE.gov")
# embedImage <- function(ID, size = "20",img_src, link_href, link_alt) {
#   tags$li(
#     style = 'display: inline-block; margin-right: 20px; vertical-align: middle;',
#     div(
#       id = ID,
#       tags$a(
#         tags$figure(
#           img(src = img_src, height = 60, alt = link_alt, deleteFile = FALSE)
#         ),
#         href = link_href
#       )
#     )
#   )
# }
embedImage <- function(ID, img_src, link_href, link_alt, size = "60") {
  tags$li(
    style = 'display: inline-block; margin-right: 20px; vertical-align: middle;',
    div(
      id = ID,
      tags$a(
        tags$figure(
          img(src = img_src, height = size, alt = link_alt, deleteFile = FALSE)
        ),
        href = link_href
      )
    )
  )
}

# Función para especificar el tamaño de los titulos de secciones
sectionTitle <- function(title, font_size = "20px") {
  HTML(paste("<b style='font-size:", font_size, ";'>", title, "</b>", sep = ""))
}




# Función para crear separador personalizado
customSeparator <- function() {
  tags$div(
    style = "margin: 5px 0;",
    ""
  )
}

# Función para hacer que los titulos se muestren en letras minúsculas
lowercaseTitle <- function(title) {
  HTML(paste0("<span style='text-transform: none; '>", title, "</span>", sep = ""))
}

##################################
#### Helper Functions: Server #### 
##################################

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
#' un gráfico de línea con ggplot2. 
#' 
#' @param data Conjunto de datos a utilizar en el gráfico.
#' @param x Variable para el eje x.
#' @param y Variable para el eje y.
#' @param group Variable para agrupar los datos.
#' @param color Variable para asignar colores a la línea y los puntos.
#' @param title Título del gráfico.
#' @param xlab Etiqueta del eje x.
#' @param ylab Etiqueta del eje y.
#' @param colorlab Etiqueta para la leyenda de colores.
#' @param colorLine Color de la línea y los puntos en el gráfico.
#' 
#' @return No regresa un valor, pero imprime el gráfico generado en la consola.
#' 
#' @examples
#' # Ejemplo de uso:
#' renderLinePlot(homiEdad, "x", "y", "group", "color", "Title", "X Label", "Y Label")
#' 
# renderLinePlot <- function(data, x, y, group, color, title, xlab, ylab, colorlab = color, colorLine) {
#   p <- ggplot(data(), aes_string(x = x, y = y, group = group, color = color)) +
#     geom_line(linewidth = 1.3, color = colorLine) +  
#     geom_point(size = 1.5, color = "black") +      
#     theme_minimal() +
#     labs(title = title, x = xlab, y = ylab, color = colorlab) +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
#   
#   print(p)
# }
renderLinePlot <- function(data, x, y, group, color, title, xlab, ylab, colorlab = color) {
  p <- ggplot(data(), aes_string(x = x, y = y, group = group, color = color)) +
    geom_line(color = "blue", size = 1) +
    geom_point(color = "red", size = 2) +
    labs(title = title, x = xlab, y = ylab, color = colorlab) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}
# renderLinePlot <- function(data, x, y, group, color, title, xlab, ylab, colorlab = color) {
#   p <- ggplot(data(), aes(x = x, y = y, group = group, color = color)) +
#     geom_line(linewidth = 1.3) +  
#     geom_point(size = 1.5, color = "black") +      
#     theme_minimal() +
#     labs(title = title, x = xlab, y = ylab, color = colorlab) +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
#   
#   print(p)
# }

#' Renderiza un gráfico de barras utilizando ggplot2 en el UI de Shiny.
#' 
#' Esta función toma un conjunto de datos `data` y diversos parámetros para generar
#' un gráfico de barras con ggplot2. 
#' 
#' @param data Conjunto de datos a utilizar en el gráfico.
#' @param x Variable para el eje x.
#' @param y Variable para el eje y.
#' @param fill Variable para asignar colores de relleno a las barras.
#' @param title Título del gráfico.
#' @param xlab Etiqueta del eje x.
#' @param ylab Etiqueta del eje y.
#' @param fillLab Etiqueta para la leyenda de colores de relleno.
#' 
#' @return No retorna un valor, pero imprime el gráfico generado en la consola.
#' 
#' @examples
#' # Ejemplo de uso:
#' renderBarPlot(homiEdad, "x", "y", "fill", "Title", "X Label", "Y Label")
#' 
# renderBarPlot <- function(data, x, y, fill, title, xlab, ylab, fillLab = fill) {
#   p <- ggplot(data(), aes_string(x = x, y = y, fill = fill)) +
#     geom_bar(stat = "identity", position = "dodge") +
#     theme_minimal() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#     labs(title = title, x = xlab, y = ylab, fill = fillLab)
#   
#   print(p)
# }

# renderBarPlot <- function(data, x, y, fill, title, xlab, ylab, fillLab = fill, colorFill = "Set1") {
#   p <- ggplot(data(), aes_string(x = x, y = y, fill = fill)) +
#     geom_bar(stat = "identity", position = "dodge") +
#     scale_fill_manual(values = colorFill) +
#     theme_minimal() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#     labs(title = title, x = xlab, y = ylab, fill = fillLab)
# 
#   print(p)
# }
renderBarPlot <- function(data, x, y, fill, title, xlab, ylab, fillLab = fill, colorFill = "Set1",
                          barWidth = 1, xGap = 0.1) {
  p <- ggplot(data(), aes_string(x = x, y = y, fill = fill)) +
    geom_bar(stat = "identity", position = position_dodge2(width = barWidth, padding = xGap)) +
    scale_fill_manual(values = colorFill) +
    scale_y_continuous(labels = scales::comma_format(big.mark = ",", decimal.mark = ".")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = title, x = xlab, y = ylab, fill = fillLab)

  print(p)
}

renderHistogram <- function(data, x, y, fill, title, xlab, ylab, fillLab = fill, colorFill = "Set1") {
  p <- ggplot(data(), aes_string(x = x, y = y, fill = fill)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = colorFill) + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = title, x = xlab, y = ylab, fill = fillLab)
  
  print(p)
}








#' Renderiza un gráfico de caja utilizando ggplot2 en el UI de Shiny.
#' 
#' Esta función toma un conjunto de datos `data` y diversos parámetros para generar
#' un gráfico de caja con ggplot2. 
#' 
#' @param data Conjunto de datos a utilizar en el gráfico.
#' @param x Variable para el eje x.
#' @param y Variable para el eje y.
#' @param color Variable para asignar colores a los puntos en el gráfico de caja.
#' @param title Título del gráfico.
#' @param xlab Etiqueta del eje x.
#' @param ylab Etiqueta del eje y.
#' @param colorlab Etiqueta para la leyenda de colores.
#' 
#' @return No retorna un valor, pero imprime el gráfico generado en la consola.
#' 
#' @examples
#' # Ejemplo de uso:
#' renderBoxPlot(homiEdad, "x", "y", "color", "Title", "X Label", "Y Label")
#' 
renderBoxPlot <- function(data, x, y, color, title, xlab, ylab, colorlab = color) {
  p <- ggplot(data, aes(x = x, y = y)) +
    geom_boxplot() +
    geom_point(aes(color = color)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = title, x = xlab, y = ylab, color = colorlab)
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

#' Renderiza un mapa utilizando ggplot2 en el UI del ShinyApp
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
#' @export
#' @examples
#' # Ejemplo de uso:
#' renderMap(datos_delito_seleccionado, "Casos", "Incidencia de casos de Delito X")
#' 
# renderMap <- function(data, fill, title, fill_lab = fill, 
#                       light_color = "lightblue", dark_color = "darkblue") {
#   p <- ggplot(data()) +
#     geom_sf(aes(fill = {{fill}})) +
#     labs(title = title, fill = fill_lab) +
#     scale_fill_gradient(name = fill_lab, low = light_color, high = dark_color) +
#     theme_minimal() +
#     theme(
#       legend.position = "bottom",
#       axis.text = element_blank(),
#       axis.ticks = element_blank(),
#       panel.grid = element_blank()
#     )
#   print(p)
# }
renderMap <- function(data, fill, title, group, fill_lab = fill, 
                      light_color = "lightblue", dark_color = "darkblue") {
  p <- ggplot(data()) +
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


renderMapGroup <- function(data, fill, title, fill_lab = fill) {
  p <- ggplot(data) +
    geom_sf(aes(fill = {{fill}})) +
    labs(title = title, fill = fill_lab) +
    #scale_fill_gradient(name = fill_lab, low = light_color, high = dark_color) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
  print(p)
}

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
#' setColorFill(data, "Sexo")
#' 
# setColorFill <- function(df, variable) {
#   # Obtener los niveles únicos de la variable
#   unique_levels <- unique(df[[variable]])
#   
#   # Generar una paleta de colores basada en el número de niveles únicos
#   my_colors <- scales::hue_pal()(length(unique_levels))
#   names(my_colors) <- unique_levels
#   
#   return(my_colors)
# }
library(RColorBrewer)

setColorFill <- function(df, variable) {
  # Obtener los niveles únicos de la variable
  unique_levels <- unique(df[[variable]])
  
  # Elegir una paleta de colores apropiada para el número de niveles únicos
  num_colors <- length(unique_levels)
  if (num_colors <= 8) {
    palette <- brewer.pal(n = num_colors, name = "Set1")
  } else {
    palette <- rainbow(num_colors)
  }
  
  # Generar una lista de patrones para los colores
  patterns <- c("#CC6677", "#E69F00", "#88CCEE", "#B8E186", "#332288", "#D55E00", 
                "#F0E442", "#CC79A7", "#661100", "#888888", "#117733", "#000000")
  # Asignar patrones y colores a cada nivel único
  my_fill <- rep("black", length(unique_levels))
  names(my_fill) <- unique_levels
  for (i in 1:length(unique_levels)) {
    my_fill[i] <- ifelse(i <= length(patterns), patterns[i], palette[i])
  }
  
  return(my_fill)
}



###########################

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