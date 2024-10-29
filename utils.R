cat("Loading helper functions from utils.R...\n")
###############################
#### Helper Functions: UI  ####
###############################


# Esta función genera un elemento de selección de checkboxes en un botón desplegable dentro de una interfaz de Shiny.
# Permite a los usuarios seleccionar o deseleccionar todas las opciones de la lista de manera interactiva.
# createDropdownCheckbox <- function(label, choices, selected, id) {
#   choices <- levels(choices)
#   if (is.null(selected)) {
#     selected <- choices  
#   } else {
#     selected <- choices[selected]
#   }
# 
#   div(
#     dropdownButton(
#       circle = FALSE,
#       label = label,
#       status = "default",
#       size = "default",
#       checkboxGroupInput(
#         paste0("checkGroup_", id),
#         label = "",
#         choices = choices,
#         selected = selected
#       ),
#       actionButton(paste0("deselectAll_", id), "(De)seleccionar todo")
#     ),
#     style = "display: inline-block; padding: 5px;",
#     tags$style(HTML('
#       .btn {
#         background-color: #3e3f3a !important;
#         color: white !important;
#         border-color: #ccc !important;
#       }
#       .btn-default:hover, .btn-default:focus, .btn-default:active {
#         background-color: #2c2c2a !important;
#         color: #b0dce4 !important;
#       }
#     '))
#   )
# }

createDropdownCheckbox <- function(label, choices, selected, id) {
  choices <- levels(choices)
  if (is.null(selected)) {
    selected <- choices  # Seleccionar todos los elementos
  } else {
    selected <- choices[selected]
  }
  
  div(
    style = "display: flex; justify-content: center; width: 100%;",
    dropdownButton(
      circle = FALSE,
      label = lowercaseTitle(label),
      status = "default",
      size = "default",
      checkboxGroupInput(
        paste0("checkGroup_", id),
        label = "",
        choices = choices,
        selected = selected
      ),
      actionButton(
        paste0("deselectAll_", id), 
        HTML(lowercaseTitle("(De)seleccionar<br>todo")),
        style = "display: block; margin: 10px auto; width: 100%;"  # Centrar el botón dentro del dropdown
      )
    ),
    style = "display: flex; padding-right: 20px; padding-top: 10px; width: 100%;",
    tags$style(HTML('
      .btn {
        font-size: 12px;
        background-color: #3e3f3a !important;
        color: white !important;
        border-color: #ccc !important;
        width: auto; /* Cambiar a auto para que se adapte al contenido */
        max-width: 100%; /* Máximo 100% del contenedor */
        box-sizing: border-box; /* Incluir padding en el tamaño total */
      }
      .btn-default:hover, .btn-default:focus, .btn-default:active {
        background-color: #2c2c2a !important;
        color: #b0dce4 !important;
      }
      .dropdown-menu {
        width: 100% !important;
        font-size: 14px;
        
      }
      .shiny-input-checkboxgroup {
        
        display: flex;
        flex-direction: column;
        
      }
      @media (max-width: 1040px) { /* Adaptación para tabletas con ancho menor a 1040px */
        .btn {
          font-size: 8px !important;
          padding: 6px 4px !important;
          max-width: 100%; /* Asegurar que no se expanda más del contenedor */
        }
        .dropdown-menu {
          font-size: 8px !important;
        }
      }
      @media (max-width: 768px) { /* Adaptación para dispositivos medianos */
        .btn {
          font-size: 8px !important;
          padding: 6px 4px !important;
          max-width: 100%;
        }
        .dropdown-menu {
          font-size: 8px !important;
        }
      }
      @media (max-width: 600px) { /* Adaptación para teléfonos */
        .btn {
          font-size: 8px !important;
          padding: 6px 4px !important;
          max-width: 100%;
        }
        .dropdown-menu {
          font-size: 8px !important;
        }
      }
    '))
  )
}

# Crea un elemento de lista con una imagen que enlaza a una página web especificada.
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
# showDataCheckbox <- function(inputId, label = lowercaseTitle("Mostrar Datos"), value = FALSE) {
#   div(
#     style = "margin-left: 10px; background-color: #3e3f3a; border-radius: 5px; width: 150px; height: 48px; display: flex; align-items: center; justify-content: center;",
#     tags$style(HTML('
#       .custom-checkbox {
#         color: white;
#         font-size: 13px;
#         display: flex;
#         align-items: center;
#         justify-content: center;
#       }
#       .custom-checkbox:hover {
#         width: 150px; 
#         height: 48px;
#         border-radius: 5px;
#         background-color: #2c2c2a; /* Darker background color on hover */
#         color: #b0dce4; /* Change text color on hover */
#       }
#       .custom-checkbox input[type="checkbox"] {
#         padding-top: 0px;
#         color: white;
#         margin-right: 5px;
#       }
#       .custom-checkbox label {
#         padding-top: 15px;
#         margin: 0; /* Ensure no margin around the label */
#       }
#     ')),
#     div(
#       class = "custom-checkbox",
#       checkboxInput(inputId, tags$b(label), value = value)
#     )
#   )
# }

showDataCheckbox <- function(inputId, label = lowercaseTitle("Mostrar Datos"), value = FALSE) {
  div(
    style = "background-color: #3e3f3a; border-radius: 5px; width: 150px; height: 48px; display: flex; align-items: center; justify-content: center; margin-top: 13px;",
    tags$style(HTML('
      .custom-checkbox {
        color: white;
        font-size: 12px; /* Ajustar tamaño de fuente del texto */
        display: flex;
        align-items: center;
        justify-content: center;
        width: 150px;  /* Mantener el mismo tamaño del contenedor */
        height: 48px;  /* Mantener el mismo tamaño del contenedor */
        border-radius: 5px; /* Asegurar que el borde se mantenga */
      }
      .custom-checkbox:hover {
        background-color: #2c2c2a; /* Cambiar color de fondo en hover */
        color: #b0dce4; /* Cambiar color del texto en hover */
        width: 150px;  /* Mantener el tamaño en hover */
        height: 48px;  /* Mantener el tamaño en hover */
      }
      .custom-checkbox input[type="checkbox"] {
        padding-top: 10px; /* Ajustar padding superior del checkbox */
        margin-top: 0px;
        margin-right: 5px; /* Separar el checkbox del texto */
        width: 18px; /* Ajustar tamaño del checkbox */
        height: 18px; /* Ajustar tamaño del checkbox */
      }
      .custom-checkbox label {
        margin: 0; /* Asegurarse de que no haya margen alrededor del label */
        padding-top: 15px; /* Aplicar padding-top al texto */
      }
      .custom-checkbox label::before {
        display: none; /* Eliminar el diseño predeterminado del checkbox */
      }

      /* Estilos para pantallas pequeñas */
      @media (max-width: 768px) {
        .custom-checkbox {
          font-size: 8px; /* Reducir tamaño de fuente en pantallas medianas */
          width: 97%; /* Ajustar el ancho en pantallas medianas */
        }
        .custom-checkbox:hover {
          width: 97%; /* Mantener el mismo tamaño en hover en pantallas medianas */
        }
      }
      @media (max-width: 600px) {
        .custom-checkbox {
          font-size: 8px; /* Reducir tamaño de fuente en pantallas pequeñas */
          padding: 8px 6px;
          width: 97%; /* Ajustar el ancho en pantallas pequeñas */
        }
        .custom-checkbox:hover {
          width: 97%; /* Mantener el mismo tamaño en hover en pantallas pequeñas */
        }
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
      geom_blank() +  
      labs(title = title, x = xlab, y = ylab) +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title.x = element_text(size = 12, margin = margin(t = 10)),  
        axis.title.y = element_text(size = 12, margin = margin(r = 10)),  
        plot.title = element_text(hjust = 0.5, size = 13, colour = "black", face = "bold"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.margin = margin(t = 50, r = 10, b = 10, l = 10)
      ) +
      annotate("text", x = 0.5, y = 0.5, label = emptyMessage, size = 6, hjust = 0.5, vjust = 0.5)

    return(p)

  } else {
    # Calcular rango del eje de y
    y_max <- max(data_df[[y]], na.rm = TRUE)
    y_interval <- pretty(c(0, y_max), n = 5)[2]  
    upper_y_limit <- ceiling(y_max / y_interval) * y_interval  


    p <- ggplot(data_df, aes_string(x = x, y = y, group = group, color = color)) +
      geom_line(color = "#2f2e7d", size = 1) +  
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

# Crea un gráfico vacio indicando al usuario que debe seleccionar las variables a visualizar
create_empty_plot_with_message <- function(data, x, y, fill, title, xlab, ylab, emptyMessage) {
  data_df <- data()
  ggplot(data_df, aes_string(x = x, y = y)) +
    geom_blank() +  
    labs(title = title, x = xlab, y = ylab) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),  
      axis.text.y = element_blank(),
      axis.title.x = element_text(size = 12, margin = margin(t = 10)), 
      axis.title.y = element_text(size = 12, margin = margin(r = 10)),  
      plot.title = element_text(hjust = 0.5, size = 13, colour = "black", face = "bold"),
      panel.border = element_rect(colour = "black", fill = NA, size = 1),
      plot.margin = margin(t = 50, r = 10, b = 10, l = 10)
    ) +
    annotate("text", x = 0.5, y = 0.5, label = emptyMessage, size = 6, hjust = 0.5, vjust = 0.5)
}

# Renderiza un gráfico de barras utilizando ggplot2 en el UI de Shiny.
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
        plot.title = element_text(hjust = 2, size = 13, colour = "black", face = "bold"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.margin = margin(t = 50, r = 10, b = 10, l = 10)
      )
    
    return(p)
  }
}

# Renderiza un gráfico de barras apiladas 
renderBarPlot_stack <- function(data, x, y, fill, title, xlab, ylab, fillLab = fill, colorFill = "Set1",
                          emptyMessage, barWidth = 0.4, xGap = 0.1) {  
  
  data_df <- data()  # Evalúa los datos reactivos una vez
  
  if (is.null(data_df) || nrow(data_df) == 0 || is.null(data_df[[x]]) || is.null(data_df[[y]]) || is.null(data_df[[fill]])) {
    # Si no hay datos o las variables x/y son nulas, muestra un gráfico vacío con un mensaje
    p <- create_empty_plot_with_message(data, x, y, fill, title, xlab, ylab, emptyMessage)
    
    return(p)
  } else {
    
    # Filtrar solo las filas para apilar barras (sin el total de kits)
    apiladas_df <- data_df %>%
      filter(Kits %in% c("Total con querella", "Total sin querella"))
    
    upper_y_limit <- ceiling(max(eval(parse(text = paste0("data()$", y))), na.rm = TRUE) * 1.2)
    
    p <- ggplot(apiladas_df, aes_string(x = x, y = y, fill = fill)) +
      geom_bar(stat = "identity",
               position = position_stack(reverse = TRUE),  # Invierte el apilado
               width = barWidth,  
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
        plot.title = element_text(hjust = 0.5, size = 13, colour = "black", face = "bold"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.margin = margin(t = 50, r = 10, b = 10, l = 10)
      )
    
    return(p)
  }
}

# Renderiza un gráfico de barras apiladas con etiquetas de totales justo encima de las barras
renderBarPlot_stack2 <- function(data, x, y, fill, title, xlab, ylab, fillLab = fill, colorFill = "Set1",
                          emptyMessage, barWidth = 0.4, xGap = 0.1) {
  
  data_df <- data()  # Evalúa los datos reactivos una vez
  
  if (is.null(data_df) || nrow(data_df) == 0 || is.null(data_df[[x]]) || is.null(data_df[[y]]) || is.null(data_df[[fill]])) {
    # Si no hay datos o las variables x/y son nulas, muestra un gráfico vacío con un mensaje
    p <- create_empty_plot_with_message(data, x, y, fill, title, xlab, ylab, emptyMessage)
    return(p)
  } else {
    # Filtrar solo las filas para apilar barras (sin el total de kits)
    apiladas_df <- data_df %>%
      filter(Kits %in% c("Total con querella", "Total sin querella"))
    
    # Filtrar los totales para poner las etiquetas de total encima
    totales_df <- data_df %>%
      filter(Kits == "Total de Kits")
    
    # Crear el gráfico de barras apiladas
    p <- ggplot(apiladas_df, aes_string(x = x, y = y, fill = fill)) +
      geom_bar(stat = "identity",
               position = position_stack(reverse = TRUE),  
               width = barWidth,  
               aes(
                 text = paste(
                   paste0("<b>", ylab, ":</b> ", after_stat(y)), "<br>",
                   paste0("<b>", fillLab, ":</b> ", after_stat(fill)), "<br>"
                 )
               )) +
      # Agregar los totales encima de cada barra
      geom_text(data = totales_df, aes_string(x = x, y = y, label = y),
                position = position_stack(vjust = 1.04),  # Etiquetas justo encima de la última barra
                size = 4, color = "black") +  
      scale_fill_manual(values = colorFill) +
      scale_y_continuous(labels = function(x) scales::comma_format(big.mark = ",", decimal.mark = ".")(x) %>% paste0(" "),
                         expand = expansion(mult = c(0, 0.1))) +
      coord_cartesian(ylim = c(0, ceiling(max(totales_df[[y]], na.rm = TRUE) * 1.2))) +  # Límite superior basado en los totales
      labs(title = title, x = xlab, y = ylab, fill = fillLab) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 13, colour = "black", face = "bold"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.margin = margin(t = 50, r = 10, b = 10, l = 10)
      )
    
    return(p)
  }
}

# Transforma un gráfico ggplot en un gráfico interactivo plotly y configura el tooltip.
convert_to_plotly <- function(p, tooltip_value) {
  # Convertir a grafico de plotly
  p_plotly <- ggplotly(p, tooltip = tooltip_value)
  
  p_plotly <- p_plotly %>% layout(
    legend = list(
      x = 1.05,  
      y = 0.5,  
      xanchor = "left", 
      yanchor = "middle"
    )
  )
  
  return(p_plotly)
}

# Genera un div que muestra una lista de enlaces con las Fuentes
createFuenteDiv <- function(hyperlinks, fuenteTexts) {
  if (length(hyperlinks) != length(fuenteTexts)) {
    stop("The length of hyperlinks and fuenteTexts must be the same.")
  }
  
  # Crear un string con hyperlinks y texto
  combined <- sapply(seq_along(hyperlinks), function(i) {
    paste0(tags$a(href = hyperlinks[i], fuenteTexts[i], style = "color: white; text-decoration: none;"))
  })
  
  # Join de los strings con comas
  combined_text <- paste(combined, collapse = ", ")
  
  # Crear div con "Fuentes: "
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
renderBoxPlot <- function(data, x, y, color, title, xlab, ylab, colorlab = color) {
  p <- ggplot(data, aes(x = x, y = y)) +
    geom_boxplot() +
    geom_point(aes(color = color)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = title, x = xlab, y = ylab, color = colorlab)
  print(p)
}

# Renderiza una tabla de datos utilizando el paquete DT en el UI de Shiny. Utilizada para presentar los datos
renderDataTable <- function(filtered_data, title, font_size = "18px") {
  datatable(
    filtered_data,
    extensions = c('Buttons'),
    options = list(
      pageLength = 5,
      lengthMenu = c(5, nrow(filtered_data) / 2, nrow(filtered_data)),
      scrollX = TRUE,
      paging = TRUE,
      searching = TRUE,
      fixedColumns = list(leftColumns = 1),
      autoWidth = FALSE,
      destroy = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip',
      buttons = list(
        #Boton para copiar la data
        #Para poder utilizar exportOption: modifier la funcion renderDT() debe
        #recibir como argumento server = FALSE. Ya que por defacto siempre es TRUE.
        list(
                extend = 'copy', 
                title = title,        #El argumento que se recibe como titulo de la data.
                exportOptions = list(
                columns = ":visible",     #Solo se va a copiar las columnas visibles de la data.
                modifier = list(page = "all") # ALL descarga toda la data seleccionada por el usuario. 
                )
        ),
        list(
          extend = 'collection',
          buttons = list(
            list(
              extend = 'csv', 
              title = title,
              exportOptions = list(
                columns = ":visible", 
                modifier = list(page = "all") # 'all' descarga toda la data seleccionada por el usuario.
                ),                            # 'current' descarga solo la pagina visible.
              filename = title
              #filename = "datos_ViolenciaGeneroPR" #Nombre del archivo sin extension
            ),
            list(
              extend = 'excel',
              title = title,
              exportOptions = list(
                columns = ":visible", 
                modifier = list(page = "all") # ALL descarga toda la data seleccionada por el usuario.
              ),
              filename = title
              #filename = "datos_ViolenciaGeneroPR" #Nombre del archivo sin extension
            )
          ),
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
renderDataTable_Definitions <- function(filtered_data) {
  datatable(
    filtered_data,
    extensions = c('Buttons'),
    escape = c(FALSE, TRUE),
    options = list(
      pageLength = 10,
      lengthMenu = c(10, nrow(filtered_data) / 2, nrow(filtered_data)),
      scrollX = TRUE,
      paging = FALSE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = FALSE,
      ordering = TRUE,
      dom = 'Bfrtip', 
      buttons = list(
        list(
          extend = 'collection',
          text = 'Descargar Conceptos',
          buttons = list(
            list(extend = 'copy', text = 'Copiar'),
            list(extend = 'csv', text = 'CSV', filename = 'Definiciones_ViolenciaGeneroPR'),
            list(extend = 'excel', text = 'Excel', filename = 'Definiciones_ViolenciaGeneroPR'),
            list(extend = 'pdf', text = 'PDF', filename = 'Definiciones_ViolenciaGeneroPR')
          )
        )
      ),
      drawCallback = JS(
        "function(settings) {",
        "var api = this.api();",
        "api.rows({ search: 'applied' }).every(function (rowIdx) {",
        "var dataIndex = rowIdx + 1;",
        "api.cell(rowIdx, 0).data(dataIndex);", 
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
      panel.border = element_rect(colour = "black", fill = NA, size = 1),  
      plot.margin = margin(60, 10, 10, 10)
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


# Función para crear cards de definiciones y metadatos (Actualmente no se esta utilizando esta funcion)
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
# Esta función toma un dataframe y el nombre de una variable categórica, y devuelve una paleta de colores con un 
# color único para cada nivel de la variable.
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



