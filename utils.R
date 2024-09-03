cat("Loading helper functions from utils.R...\n")
###############################
#### Helper Functions: UI  ####
###############################

# Crear un dropdownButton con checkboxGroupInput
# Esta función crea un dropdownButton con un checkboxGroupInput dentro,
# estilizado con inline-block.

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

# Crea una imagen con enlace a una página web
# Esta función crea una imagen con un enlace a una página web especificada.
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
# Esta función toma como entrada el `session`, el `inputId` correspondiente al grupo
# de checkbox, el objeto `input` y el conjunto de datos `data`. Dependiendo de si el
# grupo de checkbox está vacío o no, la función llena o vacía el grupo respectivamente.

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

# Renderiza un gráfico de lineas utilizando ggplot2 en el UI de Shiny.
renderLinePlot <- function(data, x, y, group, color, title, xlab, ylab, colorlab = color, emptyMessage) {

  data_df <- data()  # Evaluar los datos reactivos una vez

  if (is.null(data_df) || nrow(data_df) == 0 || is.null(data_df[[x]])) {
    # Si no hay datos o las variables x/y son nulas, mostrar una gráfica vacía con ejes y un mensaje
    p <- ggplot(data_df, aes_string(x = x, y = y, group = group, color = color)) +
      geom_blank() +  # Añadir geom_blank para garantizar la estructura de la gráfica
      labs(title = title, x = xlab, y = ylab) +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),  # Quitar ticks de los ejes
        axis.text.y = element_blank(),  # Quitar texto del eje y
        axis.title.x = element_text(size = 12, margin = margin(t = 10)),  # Mantener etiqueta del eje x
        axis.title.y = element_text(size = 12, margin = margin(r = 10)),  # Mantener etiqueta del eje y
        plot.title = element_text(hjust = 0.5, size = 15, colour = "black", face = "bold"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.margin = margin(t = 45, r = 10, b = 10, l = 10)
      ) +
      annotate("text", x = 0.5, y = 0.5, label = emptyMessage, size = 6, hjust = 0.5, vjust = 0.5)

    return(p)

  } else {

    # Calculate the tick interval based on the range of y values
    y_max <- max(data_df[[y]], na.rm = TRUE)
    y_interval <- pretty(c(0, y_max), n = 5)[2]  # Get the interval size for the y-axis
    upper_y_limit <- ceiling(y_max / y_interval) * y_interval  # Round up to the next interval


    p <- ggplot(data_df, aes_string(x = x, y = y, group = group, color = color)) +
      geom_line(color = "#2f2e7d", size = 1) +  # Línea de color skyblue
      geom_point(color = "#adcc4e", size = 2,
                 aes(
                   text =
                     paste0("<b>", xlab, ":</b> ", .data[[x]],
                            "<br><b>", ylab, ":</b> ", .data[[y]]
                     )
                 )
      ) +
      expand_limits(y = 0) +  # Asegurar que el eje y comience en 0
      labs(title = title, x = xlab, y = ylab, color = colorlab) +
      scale_y_continuous(labels = function(x) scales::comma_format(big.mark = ",", decimal.mark = ".")(x) %>% paste0(" "),
                         expand = expansion(mult = c(0, 0.1))) +
      coord_cartesian(ylim = c(0, upper_y_limit)) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 15, colour = "black", face = "bold"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.margin = margin(t = 45, r = 10, b = 10, l = 10)
      )

    return(p)
  }
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


# Renderiza un gráfico de caja utilizando ggplot2 en el UI de Shiny.
# Esta función toma un conjunto de datos `data` y diversos parámetros para generar
# un gráfico de caja con ggplot2. 

renderBoxPlot <- function(data, x, y, color, title, xlab, ylab, colorlab = color) {
  p <- ggplot(data, aes(x = x, y = y)) +
    geom_boxplot() +
    geom_point(aes(color = color)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = title, x = xlab, y = ylab, color = colorlab)
  print(p)
}

# Renderiza una tabla de datos utilizando el paquete DT en el UI de Shiny.
# Esta función toma un conjunto de datos `filtered_data` y utiliza la librería DT
# para renderizar una tabla interactiva en el UI de Shiny.

renderDataTable <- function(filtered_data, title = " ", font_size = "18px") {
  datatable(
    filtered_data,
    extensions = c('Buttons'),
    options = list(
      pageLength = 5,
      lengthMenu = c(5, nrow(filtered_data) / 2, nrow(filtered_data)),
      scrollX = TRUE,
      fixedColumns = list(leftColumns = 1),
      paging = TRUE,
      searching = TRUE,
      autoWidth = FALSE,
      destroy = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip',
      buttons = list(
        list(
          extend = 'collection',
          buttons = c('copy', 'csv', 'excel'),
          text = 'Descargar Datos'
        )
      ),
      initComplete = JS(
        sprintf(
          "function(settings, json) {
            var container = $(this.api().table().container());
            container.prepend('<h3 class=\"custom-title\">%s</h3>');
            container.find('.dt-buttons').css({'padding-top': '10px', 'padding-bottom': '10px'});
            container.find('.dataTables_paginate').css('padding-top', '10px');
            container.find('.dataTables_filter').css('padding-top', '10px');
            container.find('.custom-title').css({'text-align': 'center', 'margin-bottom': '10px', 'font-size': '%s', 'font-weight': 'bold'});
          }",
          title, font_size
        )
      )
    )
  )
}


# Renderizar Tabla de Definiciones con Botones de Exportación
# Crea una tabla de datos interactiva con botones de exportación para copiar, CSV, Excel y PDF. 
# La tabla permite búsqueda, paginación y incluye opciones para diversas configuraciones de visualización.

renderDataTable_Definitions <- function(filtered_data) {
  datatable(
    filtered_data,
    extensions = c('Buttons'),
    escape = c(FALSE, TRUE),
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
          extend = 'collection',
          buttons = c('copy', 'csv', 'excel', 'pdf'),
          text = 'Descargar Datos'
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



# Renderiza un mapa utilizando ggplot2 en el UI del ShinyApp
# Esta función toma un conjunto de datos y diversos parámetros para generar un mapa utilizando ggplot2.

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
# Generate HTML Definition Cards

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


# Genera una paleta de colores para los niveles de una variable categórica.
# Esta función toma un dataframe y el nombre de una variable categórica, 
# y devuelve una paleta de colores con un color único para cada nivel de la variable.

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



