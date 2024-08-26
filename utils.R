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
    style = "display: inline-block; padding: 5px;",
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

# Crear un checkbox para mostrar u ocultar los datos.
showDataCheckbox <- function(inputId, label = lowercaseTitle("Mostrar Datos"), value = FALSE) {
  div(
    style = "margin-left: 10px; background-color: #3e3f3a; border-radius: 5px; width: 150px; height: 48px; display: flex; align-items: center; justify-content: center;",
    tags$style(HTML('
      .custom-checkbox {
        color: white;
        font-size: 13px;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      .custom-checkbox:hover {
        width: 150px; 
        height: 48px;
        border-radius: 5px;
        background-color: #2c2c2a; /* Darker background color on hover */
        color: #b0dce4; /* Change text color on hover */
      }
      .custom-checkbox input[type="checkbox"] {
        padding-top: 0px;
        color: white;
        margin-right: 5px;
      }
      .custom-checkbox label {
        padding-top: 15px;
        margin: 0; /* Ensure no margin around the label */
      }
    ')),
    div(
      class = "custom-checkbox",
      checkboxInput(inputId, tags$b(label), value = value)
    )
  )
}



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

# Función para crear sección de autores
authorTag <- function(nombre, email, puesto, grados){
  tags$html(
    h2(nombre),
    tags$b('Email'),
    p(email),
    tags$b('Puesto'),
    p(puesto),
    tags$b('Grados Académicos'),
    lapply(grados, p),
    tags$hr()
  )
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


# Renderiza un gráfico de barras utilizando ggplot2 en el UI de Shiny.
# Crea un gráfico vacio indicando al usuario que debe seleccionar las variables a visualizar
create_empty_plot_with_message <- function(data, x, y, fill, title, xlab, ylab, emptyMessage) {
  data_df <- data()
  ggplot(data_df, aes_string(x = x, y = y)) +
    geom_blank() +  # Add blank geom to ensure plot structure
    labs(title = title, x = xlab, y = ylab) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),  # Remove axis ticks
      axis.text.y = element_blank(),  # Remove y-axis text
      axis.title.x = element_text(size = 12, margin = margin(t = 10)),  # Keep x-axis label
      axis.title.y = element_text(size = 12, margin = margin(r = 10)),  # Keep y-axis label
      plot.title = element_text(hjust = 0.5, size = 15, colour = "black", face = "bold"),
      panel.border = element_rect(colour = "black", fill = NA, size = 1),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    ) +
    annotate("text", x = 0.5, y = 0.5, label = emptyMessage, size = 6, hjust = 0.5, vjust = 0.5)
}



# Renderiza un gráfico de barras utilizando ggplot2 en el UI de Shiny.
# Crea un gráfico de barras con ggplot2 basado en el conjunto de datos y parámetros proporcionados.
renderBarPlot <- function(data, x, y, fill, title, xlab, ylab, fillLab = fill, colorFill = "Set1",
                          emptyMessage, barWidth = 1, xGap = 0.1) {
  
  data_df <- data()  # Evaluate the reactive data once
  
  if (is.null(data_df) || nrow(data_df) == 0 || is.null(data_df[[x]]) || is.null(data_df[[y]]) || is.null(data_df[[fill]])) {
    # If there are no data or x/y variables are null, display an empty plot with axes and a message
    p <- create_empty_plot_with_message(data, x, y, fill, title, xlab, ylab,emptyMessage)
    
    return(p)
  } else {
    
    upper_y_limit <- ceiling(max(eval(parse(text = paste0("data()$", y))), na.rm = TRUE) * 1.2) 
    
    p <- ggplot(data_df, aes_string(x = x, y = y, fill = fill)) +
      geom_bar(stat = "identity",
               position = position_dodge2(width = barWidth, padding = xGap),
               width = 0.7,
               aes(
                 text = paste(
                   paste0("<b>", ylab, ":</b> ", after_stat(y)), "<br>",
                   paste0("<b>", fillLab, ":</b> ", after_stat(fill)), "<br>"
                 )
               )) +
      scale_fill_manual(values = colorFill) +
      scale_y_continuous(labels = function(x) scales::comma_format(big.mark = ",", decimal.mark = ".")(x) %>% paste0(" "),
                         expand = expansion(mult = c(0, 0.1))) +
      coord_cartesian(ylim = c(0, upper_y_limit)) +
      labs(title = title, x = xlab, y = ylab, fill = fillLab) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 15, colour = "black", face = "bold"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
      )
    
    return(p)
  }
}

renderHistogram <- function(data, x, y, fill, title, xlab, ylab, fillLab = fill, colorFill = "Set1") {
  upper_y_limit <- ceiling(max(eval(parse(text = paste0("data()$", y))), na.rm = TRUE) * 1.2) 
  p <- ggplot(data(), aes_string(x = x, y = y, fill = fill)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.6) +
    scale_fill_manual(values = colorFill) + 
    scale_y_continuous(labels = scales::comma_format(big.mark = ",", decimal.mark = ".")) +
    coord_cartesian(ylim = c(0, upper_y_limit)) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.border = element_rect(colour = "black", fill = NA, size = 1),  # Adds a border around the plot panel
      plot.margin = margin(10, 10, 10, 10)  # Adjusts the margin around the plot
      ) +
    labs(title = title, x = xlab, y = ylab, fill = fillLab)
  
  print(p)
}

# Convertir un objeto ggplot a plotly y ajustar la leyenda.
# Esta función transforma un gráfico ggplot en un gráfico interactivo plotly y configura el tooltip.
convert_to_plotly <- function(p, tooltip_value) {
  # Convert ggplot object to plotly object
  p_plotly <- ggplotly(p, tooltip = tooltip_value)
  
  # Adjust layout to position the legend on the left side and center it vertically
  p_plotly <- p_plotly %>% layout(
    legend = list(
      x = 1.05,  # X position for legend (left side)
      y = 0.5,  # Y position for legend (centered vertically)
      xanchor = "left",  # Anchor the legend to the left
      yanchor = "middle"
    )
  )
  
  return(p_plotly)
}


# Crear un div con enlaces a fuentes y textos asociados.
# Genera un div que muestra una lista de enlaces con textos, precedida por "Fuentes: ".
createFuenteDiv <- function(hyperlinks, fuenteTexts) {
  # Ensure the length of both lists are the same
  if (length(hyperlinks) != length(fuenteTexts)) {
    stop("The length of hyperlinks and fuenteTexts must be the same.")
  }
  
  # Create a single string with hyperlinks and text
  combined <- sapply(seq_along(hyperlinks), function(i) {
    paste0(tags$a(href = hyperlinks[i], fuenteTexts[i], style = "color: white; text-decoration: none;"))
  })
  
  # Join combined strings with commas and a space
  combined_text <- paste(combined, collapse = ", ")
  
  # Create the div card with "Fuentes: " before the references
  tags$div(
    style = "background-color: #3e3f3a; padding: 10px; border-radius: 5px; margin-top: 10px; text-align: center;",
    tags$span(
      "Fuentes: ",
      style = "color: white; font-weight: bold;"
    ),
    tags$span(
      HTML(combined_text),
      style = "color: white;"
    )
  )
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
    extensions = c('Buttons'),
    options = list(
      pageLength = 5,
      lengthMenu = c(5, nrow(filtered_data) / 2, nrow(filtered_data)),
      scrollX = TRUE,
      fixedColumns = list(leftColumns = 1),
      #fixedColumns = TRUE,
      paging = TRUE,
      searching = TRUE,
      autoWidth = FALSE,
      #autoWidth = TRUE,
      destroy = TRUE,
      ordering = TRUE,
      dom = 'lftpB',
      buttons = c('copy', 'csv', 'excel'),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().container()).find('.dt-buttons').css({'padding-top': '10px', 'padding-bottom': '10px'});",
        "$(this.api().table().container()).find('.dataTables_paginate').css('padding-top', '10px');",
        "}"
      )
    )
  )
}


#' Renderizar Tabla de Definiciones con Botones de Exportación
#'
#' Crea una tabla de datos interactiva con botones de exportación para copiar, CSV, Excel y PDF. La tabla permite búsqueda, paginación y incluye opciones para diversas configuraciones de visualización.
#'
#' @param filtered_data Un data frame o tibble que contiene los datos a ser mostrados en la tabla de datos.
#'
#' @return Un objeto de tipo DataTable con opciones de exportación y configuraciones personalizadas.
#'
#' @details
#' La función utiliza la función `datatable` del paquete `DT` para renderizar la tabla de datos con las siguientes características:
#' \itemize{
#'   \item La tabla está paginada con 10 entradas por página, con opciones para ajustar el número de entradas mostradas.
#'   \item Se proporcionan botones de exportación para copiar, CSV, Excel y PDF.
#'   \item El botón PDF exporta la tabla con un título y mensaje personalizados, y el botón Excel incluye un nombre de archivo y título personalizados.
#'   \item La DataTable permite el desplazamiento horizontal, la búsqueda y el ordenamiento.
#'   \item La tabla utiliza JavaScript para callbacks personalizados de dibujo para ajustar la apariencia de los botones de exportación y mantener los números de índice de fila.
#' }
#'
#' @examples
#' # Ejemplo de uso con un data frame de muestra
#' datos_muestra <- data.frame(
#'   Concepto = c("Concepto 1", "Concepto 2"),
#'   Definicion = c("Definición 1", "Definición 2")
#' )
#' renderDataTable_Definitions(datos_muestra)
#'
#' @import DT
#' @importFrom htmltools HTML
#' @importFrom JS JS
#' @export
renderDataTable_Definitions <- function(filtered_data) {
  datatable(
    filtered_data,
    extensions = c('Buttons'),
    options = list(
      pageLength = 10,
      lengthMenu = c(10, nrow(filtered_data) / 2, nrow(filtered_data)),
      scrollX = TRUE,
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = FALSE,
      ordering = TRUE,
      dom = 'Bfrtip', 
      buttons = list(
        list(
          extend = 'copy',
          text = 'Copy'
        ),
        list(
          extend = 'csv',
          text = 'CSV',
          filename = 'Definiciones_y_Metadatos'
        ),
        list(
          extend = 'excel',
          text = 'Excel',
          filename = 'Definiciones_y_Metadatos', # Custom filename
          title = 'Definiciones y Metadatos'    # Title in Excel
        ),
        list(
          extend = 'pdf',
          text = 'PDF',
          title = 'Definiciones y Metadatos',
          messageTop = 'Sistema de Compilación y Manejo de Estadísticas de Violencia de Género',
          orientation = 'portrait',
          pageSize = 'A4'
        )
      ),
      drawCallback = JS(
        "function(settings) {",
        "var api = this.api();",
        "api.rows({ search: 'applied' }).every(function (rowIdx) {",
        "  var dataIndex = rowIdx + 1;",
        "  api.cell(rowIdx, 0).data(dataIndex);", 
        "});",
        "$(this.api().table().container()).find('.dt-buttons').css({'margin-top': '10px', 'padding-bottom': '10px'});",
        "$(this.api().table().container()).find('.dataTables_filter').css('padding-top', '10px');",
        "$(this.api().table().container()).find('.dataTables_paginate').css({'padding-top': '10px', 'padding-bottom': '10px'});",
        "$(this.api().table().container()).find('.dataTables_info').css('padding-top', '20px');",
        "}"
      ),
      columnDefs = list(
        list(targets = 0, orderable = FALSE) # Disable ordering for the index column
      )
    ),
    class = 'display compact' 
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
      panel.grid = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, size = 1),  # Adds a border around the plot panel
      plot.margin = margin(10, 10, 10, 10)  # Adjusts the margin around the plot
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


# Función para crear cards de definiciones y metadatos

#' Generate HTML Definition Cards
#'
#' This function creates HTML definition cards from a list of definitions.
#'
#' @param definitions A list of definitions where each element is a list containing two elements: `word` and `definition`.
#'
#' @return A tagList of HTML div elements styled as definition cards.
#' @import htmltools
#' @export
#'
#' @examples
#' definitions <- list(
#'   list(word = "Cat", definition = "A small domesticated carnivorous mammal."),
#'   list(word = "Dog", definition = "A domesticated carnivorous mammal.")
#' )
#' definitionCards(definitions)
definitionCards <- function(definitions) {
  card_list <- lapply(definitions, function(def) {
    background_color <- "#F2F2F3"
    tags$div(
      style = paste("background-color:", background_color, ";
                    padding: 15px;
                    margin: 10px;
                    border-radius: 10px;"),
      tags$h2(style = "font-weight: bold; margin: 0; padding: 0; font-size: 20px;", def$word),
      tags$p(style = "margin: 0; padding-top: 5px; font-size: 14px;", def$definition)
    )
  })
  do.call(tagList, card_list)
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



