cat("Loading helper functions from utils.R...\n")

##################################
#### Helper Functions: Global ####
##################################
#### cleanSheet_npprDesp ####
cleanSheet_npprDesp <- function(data, sheet_name) {
  total_column <- paste0("Total Año ", sheet_name)  # Crear el nombre dinámico
  data %>%
    mutate(Año = sheet_name) %>%
    rename(Categoria = Mes, Total = !!sym(total_column)) # Usar el nombre dinámico
}

#### cleanSheet_npprVDedad ####
cleanSheet_npprVDedad <- function(data, sheet_name) {
  data %>%
    mutate(Año = sheet_name)
}

#### cleanSheet_OP_148_SoliGrupEdad ####
cleanSheet_OP_148_SoliGrupEdad <- function(data, sheet_name, new_names) {
  data %>%
    rename_at(vars(2:9), ~ new_names) %>%       # Renombra las columnas de la 2 a la 9
    pivot_longer(                                # Convierte las columnas a formato largo
      !Región, 
      names_to = "Edad", 
      values_to = "Solicitudes"
    ) %>%
    mutate(
      Edad = as.character(Edad),                # Convierte Edad a carácter para asegurar el filtrado
      AñoFiscal = sheet_name                     # Asigna el año fiscal desde sheet_name
    ) %>%
    filter(
      Edad != "Total",                           # Filtra filas con 'Total' en Edad
      Región != "Total"                          # Filtra filas con 'Total' en Región
    ) %>%
    mutate(
      Edad = factor(Edad, levels = unique(Edad)) # Convierte Edad de nuevo a factor con niveles únicos
    )
}

#### cleanSheet_OP_Ley148_ex_parteEmitidas ####
cleanSheet_OP_Ley148_ex_parteEmitidas <- function(data, sheet_name, new_names) {
  data %>%
    rename_at(vars(2:6), ~ new_names) %>%       # Renombra las columnas de la 2 a la 6
    pivot_longer(                                # Convierte las columnas a formato largo
      !Región, 
      names_to = "Delito", 
      values_to = "ÓrdenesEmitidas"
    ) %>%
    mutate(
      Delito = as.character(Delito),             # Convierte Delito a carácter para asegurar el filtrado
      AñoFiscal = factor(sheet_name)             # Asigna el año fiscal desde sheet_name
    ) %>%
    filter(
      Delito != "Total",                         # Filtra filas con 'Total' en Delito
      Región != "Total"                          # Filtra filas con 'Total' en Región
    ) %>%
    mutate(
      Delito = factor(Delito, levels = unique(Delito)) # Convierte Delito de nuevo a factor con niveles únicos
    )
}

#### cleanSheet_OP_LEY148Archivadas ####
cleanSheet_OP_LEY148Archivadas <- function(data, sheet_name, new_names) {
  data %>%
    rename_at(vars(2:4), ~ new_names) %>%
    pivot_longer(
      !Región, 
      names_to = "Razón", 
      values_to = "ÓrdenesArchivadas"
    ) %>%
    mutate(
      Región = as.character(Región), 
      AñoFiscal = factor(sheet_name)
    ) %>%
    filter(
      Razón != "Total",
      Región != "Total"
    ) %>%
    mutate(
      Región = factor(Región, levels = unique(Región)) # Convierte Región de nuevo a factor con niveles únicos
    )
}

#### cleanSheet_OP_LEY148Denegadas ####
cleanSheet_OP_LEY148Denegadas <- function(data, sheet_name, new_names) {
  data %>%
    rename_at(vars(3:4), ~ new_names) %>%
    pivot_longer(
      !Región, 
      names_to = "Razón", 
      values_to = "ÓrdenesDenegadas"
    ) %>%
    mutate(
      AñoFiscal = factor(sheet_name)
    ) %>%
    filter(
      Región != "Total",
      Razón != "Total"
    )
}

#### cleanSheet_OP_LEY148FinalEmitidas ####
cleanSheet_OP_LEY148FinalEmitidas <- function(data, sheet_name, new_names) {
  data %>%
    rename_at(vars(2:6), ~ new_names) %>%
    pivot_longer(
      !Región, 
      names_to = "Delito", 
      values_to = "ÓrdenesEmitidas"
    ) %>%
    mutate(
      AñoFiscal = factor(sheet_name)
    ) %>%
    filter(
      Región != "Total",
      Delito != "Total"
    )
}

#### cleanSheet_OP_LEY148Genero ####
cleanSheet_OP_LEY148Genero <- function(data, sheet_name) {
  data %>%
    pivot_longer(
      !Sexo, 
      names_to = "Parte", 
      values_to = "Solicitudes"
    ) %>%
    mutate(
      AñoFiscal = factor(sheet_name)
    ) %>%
    filter(
      Sexo != "Total"
    )
}

#### cleanSheet_tribCasosCrim ####
cleanSheet_tribCasosCrim <- function(data, sheet_name, new_names) {
  data %>%
    rename_at(vars(1:11), ~ new_names) %>%      # Renombra las primeras 11 columnas
    pivot_longer(                                # Convierte las columnas a formato largo
      !Delito, 
      names_to = "Casos", 
      values_to = "Cantidad"
    ) %>%
    mutate(
      AñoFiscal = factor(sheet_name)             # Añade el año fiscal como factor
    ) %>%
    filter(
      !(Casos %in% c("Total"))                   # Filtra las filas con 'Total' en Casos
    )
}




###############################
#### Helper Functions: UI  ####
###############################
#### createDropdownCheckbox / createDropdownCheckbox_añoFiscal ####
# Esta función genera un elemento de selección de checkboxes en un botón desplegable dentro de una interfaz de Shiny.
# Permite a los usuarios seleccionar o deseleccionar todas las opciones de la lista de manera interactiva.
createDropdownCheckbox_añoFiscal <- function(label, choices, selected, id) {
  # Step 1: Extract unique years and create custom labels
  unique_years <- unique(as.character(choices))
  formatted_choices <- setNames(
    unique_years,
    paste0("jul ", unique_years, " - jun ", as.numeric(unique_years) + 1)
  )
  
  # Step 2: Set all unique years as selected by default if selected is NULL
  if (is.null(selected)) {
    selected <- unique_years  # Select all unique years
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
        choices = formatted_choices,  # Show custom names for years
        selected = selected
      ),
      actionButton(
        paste0("deselectAll_", id), 
        HTML(lowercaseTitle("(De)seleccionar<br>todo")),
        style = "display: block; margin: 10px auto; width: 100%;"  # Center button in dropdown
      )
    ),
    style = "display: flex; padding-right: 20px; padding-top: 10px; width: 100%;",
    tags$style(HTML('
      .btn {
        font-size: 12px !important; /* Tamaño general del botón */
        background-color: #3e3f3a !important;
        color: white !important;
        border-color: #ccc !important;
        max-width: 100%; /* Máximo 100% del contenedor */
        box-sizing: border-box; /* Incluir padding en el tamaño total */
      }
      .btn > span { 
        font-size: 10px !important; /* Tamaño del título del botón */
      }
      .btn-default:hover, .btn-default:focus, .btn-default:active {
        background-color: #2c2c2a !important;
        color: #b0dce4 !important;
      }
      .dropdown-menu {
        width: 100% !important;
        font-size: 8px !important; /* Tamaño de texto dentro del dropdown */
      }
      .shiny-input-checkboxgroup {
        display: flex;
        flex-direction: column;
      }
      @media (max-width: 1040px) { /* Teléfonos y tabletas medianas */
        .btn {
          font-size: 10px !important; /* Tamaño general para dispositivos pequeños */
          padding: 6px 4px !important;
          max-width: 100%;
        }
        .btn > span { 
          font-size: 10px !important; /* Mantener el título pequeño */
        }
        .dropdown-menu {
          font-size: 10px !important;
        }
      }
      @media (min-width: 1041px) { /* Computadoras (pantallas grandes) */
        .btn {
          font-size: 12px !important; /* Tamaño general para dispositivos grandes */
          padding: 8px 6px !important;
          max-width: 100%;
        }
        .btn > span { 
          font-size: 11px !important; /* Título del botón sigue siendo pequeño */
        }
        .dropdown-menu {
          font-size: 12px !important;
        }
      }
    '))
  )
}

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
        font-size: 12px !important; /* Tamaño general del botón */
        background-color: #3e3f3a !important;
        color: white !important;
        border-color: #ccc !important;
        max-width: 100%; /* Máximo 100% del contenedor */
        box-sizing: border-box; /* Incluir padding en el tamaño total */
      }
      .btn > span { 
        font-size: 10px !important; /* Tamaño del título del botón */
      }
      .btn-default:hover, .btn-default:focus, .btn-default:active {
        background-color: #2c2c2a !important;
        color: #b0dce4 !important;
      }
      .dropdown-menu {
        width: 100% !important;
        font-size: 8px !important; /* Tamaño de texto dentro del dropdown */
      }
      .shiny-input-checkboxgroup {
        display: flex;
        flex-direction: column;
      }
      @media (max-width: 1040px) { /* Teléfonos y tabletas medianas */
        .btn {
          font-size: 10px !important; /* Tamaño general para dispositivos pequeños */
          padding: 6px 4px !important;
          max-width: 100%;
        }
        .btn > span { 
          font-size: 10px !important; /* Mantener el título pequeño */
        }
        .dropdown-menu {
          font-size: 10px !important;
        }
      }
      @media (min-width: 1041px) { /* Computadoras (pantallas grandes) */
        .btn {
          font-size: 12px !important; /* Tamaño general para dispositivos grandes */
          padding: 8px 6px !important;
          max-width: 100%;
        }
        .btn > span { 
          font-size: 11px !important; /* Título del botón sigue siendo pequeño */
        }
        .dropdown-menu {
          font-size: 12px !important;
        }
      }
    '))
  )
}

#### embedImage ####
# Crea un elemento de lista con una imagen que enlaza a una página web especificada.
embedImage <- function(ID, img_src, link_href, link_alt, size = "60", padding_bottom = "0px") {
  tags$li(
    style = 'display: inline-block; margin-right: 20px; vertical-align: middle;',
    div(
      id = ID,
      tags$a(
        tags$figure(
          img(
            src = img_src, 
            height = size, 
            alt = link_alt, 
            deleteFile = FALSE, 
            style = paste0("padding-bottom: ", padding_bottom, ";")
          )
        ),
        href = link_href
      )
    )
  )
}

#### showDataCheckbox ####
# Crear un checkbox para mostrar u ocultar los datos.
showDataCheckbox <- function(inputId, label = lowercaseTitle_mostrarDatos("Mostrar Datos"), value = FALSE) {
  div(
    style = "background-color: #3e3f3a; border-radius: 5px; width: 100px; height: 43px; display: flex; align-items: center; justify-content: center; margin-top: 13px;",
    tags$style(HTML('
      .custom-checkbox {
        color: white;
        font-size: 13px; /* Ajustar tamaño de fuente general */
        display: flex;
        align-items: center;
        justify-content: center;
        width: 100px;  /* Mantener el mismo tamaño del contenedor */
        height: 43px;  /* Mantener el mismo tamaño del contenedor */
        border-radius: 5px; /* Asegurar que el borde se mantenga */
      }
      .custom-checkbox:hover {
        background-color: #2c2c2a; /* Cambiar color de fondo en hover */
        color: #b0dce4; /* Cambiar color del texto en hover */
        width: 100px;  /* Mantener el tamaño en hover */
        height: 43px;  /* Mantener el tamaño en hover */
      }
      .custom-checkbox input[type="checkbox"] {
        padding-top: 0px; /* Ajustar padding superior del checkbox */
        margin-top: 0px;
        margin-right: 5px; /* Separar el checkbox del texto */
        width: 16px; /* Ajustar tamaño del checkbox */
        height: 16px; /* Ajustar tamaño del checkbox */
      }
      .custom-checkbox label {
        margin: 0; /* Asegurarse de que no haya margen alrededor del label */
        padding-top: 15px; /* Aplicar padding-top al texto */
        font-size: 8px; /* Ajustar el tamaño del texto del título */
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
        .custom-checkbox label {
          font-size: 10px !important; /* Mantener el texto pequeño */
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
        .custom-checkbox label {
          font-size: 10px !important; /* Asegurar consistencia en el tamaño del texto */
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

#### sectionTitle ####
# Función para especificar el tamaño de los titulos de secciones
sectionTitle <- function(title, font_size = "20px") {
  HTML(paste("<div style='text-align: center; font-size:", font_size, ";'><b>", title, "</b></div>", sep = ""))
}

#### customSeparator ####
# Función para crear separador personalizado
customSeparator <- function() {
  tags$div(
    style = "margin: 5px 0;",
    ""
  )
}

#### lowercaseTitle ####
# Función para hacer que los titulos se muestren en letras minúsculas
lowercaseTitle <- function(title, font_size = "15px") {
  HTML(paste0(
    "<span style='text-transform: none; font-size: ", font_size, ";'>",
    title,
    "</span>"
  ))
}

#### lowercaseTitle_mostrarDatos ####
# Función para hacer que los titulos se muestren en letras minúsculas
lowercaseTitle_mostrarDatos <- function(title, font_size = "11px") {
  HTML(paste0(
    "<span style='text-transform: none; font-size: ", font_size, ";'>",
    title,
    "</span>"
  ))
}

#### authorTag ####
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

#### updateCheckboxGroup ####

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

#### create_empty_plot_with_message_forLine ####
create_empty_plot_with_message_forLine <- function(data, x, y, fill, title, xlab, ylab, emptyMessage) {
  data_df <- data()
  
  # Crear el gráfico base
  base_plot <- ggplot(data_df, aes_string(x = x, y = y)) +
    geom_blank() +
    labs(title = title, x = xlab, y = ylab) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_text(size = 12, margin = margin(t = 10)),
      axis.title.y = element_text(size = 12, margin = margin(r = 10)),
      plot.title = element_text(hjust = 0.5, size = 15, colour = "black", face = "bold"),
      panel.border = element_rect(colour = "black", fill = NA, size = 1),
      plot.margin = margin(t = 50, r = 10, b = 10, l = 10)
    )
  
  # Obtener las dimensiones de la trama
  plot_data <- ggplot_build(base_plot)
  panel_width <- diff(plot_data$layout$panel_params[[1]]$x.range)
  panel_height <- diff(plot_data$layout$panel_params[[1]]$y.range)
  
  # Calcular el tamaño del texto en función de las dimensiones del panel
  text_size <- min(panel_width, panel_height) * 1.75
  
  # Agregar el texto dinámico al gráfico
  base_plot +
    annotate(
      "text", x = 0.5, y = 0.5, label = emptyMessage,
      size = text_size, hjust = 0.5, vjust = 0.5
    )
}

#### renderLinePlot ####
renderLinePlot <- function(data, x, y, group, color, title, xlab, ylab, colorlab = color, emptyMessage) {
  data_df <- data()  # Evaluar los datos reactivos una vez
  
  if (is.null(data_df) || nrow(data_df) == 0 || is.null(data_df[[x]])) {
    # Mostrar una gráfica vacía con un mensaje si no hay datos
    p <- create_empty_plot_with_message_forLine(data, x, y, fill, title, xlab, ylab, emptyMessage)
    return(p)
  } else {
    # Filtrar los niveles del factor Año para que solo incluya cada 5 años
    x_levels <- levels(data_df[[x]])
    x_labels <- ifelse(as.numeric(x_levels) %% 5 == 0, x_levels, "")  # Mostrar etiquetas solo cada 5 años
    
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
      scale_x_discrete(breaks = x_labels) +  # Usar etiquetas filtradas para el eje x
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

#### create_empty_plot_with_message ####
create_empty_plot_with_message <- function(data, x, y, fill, title, xlab, ylab, emptyMessage) {
  data_df <- data()
  
  # Crear el gráfico base
  base_plot <- ggplot(data_df, aes_string(x = x, y = y)) +
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
      plot.margin = margin(t = 100, r = 50, b = 100, l = 50)
    )
  
  # Obtener las dimensiones de la trama
  plot_data <- ggplot_build(base_plot)
  panel_width <- diff(plot_data$layout$panel_params[[1]]$x.range)
  panel_height <- diff(plot_data$layout$panel_params[[1]]$y.range)
  
  # Calcular el tamaño del texto en función de las dimensiones del panel
  text_size <- min(panel_width, panel_height) * 1.75
  
  # Agregar el texto dinámico al gráfico
  base_plot +
    annotate(
      "text", x = 0.5, y = 0.5, label = emptyMessage,
      size = text_size, hjust = 0.5, vjust = 0.5
    )
}

#### renderBarPlot ####
#Renderiza un gráfico de barras utilizando ggplot2 en el UI de Shiny.
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
        plot.title = element_text(hjust = 0.5, size = 13, colour = "black", face = "bold"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.margin = margin(t = 150, r = 50, b = 100, l = 50)
      )

    return(p)
  }
}

#### renderBarPlot_facets ####
#Funcion para las graficas con FACETAS
#Renderiza un gráfico de barras utilizando ggplot2 en el UI de Shiny.
renderBarPlot_facets <- function(data, x, y, fill, title, xlab, ylab, fillLab = fill, colorFill = "Set1",
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
      labs(x = xlab, y = ylab, fill = fillLab) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.margin = margin(t = 105, r = 50, b = 100, l = 50),
        panel.grid.major.x = element_blank() # Elimina el grid line por defacto en el eje de X
      ) 
      if (length(unique(data_df[[x]])) > 1) {
        p <- p + geom_vline(xintercept = seq(1.5, length(unique(data_df[[x]])) - 0.5, by = 1), #Grid line en x
                            linetype = "dotted", color = "grey", size = 0.3, alpha = 0.5) #Visualización del grid line
      }
    
    return(p)
  }
}

#### renderBarPlot_stack ####
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
        plot.margin = margin(t = 100, r = 50, b = 100, l = 50),
        panel.grid.major.x = element_blank() # Elimina el grid line por defacto en el eje de X
      ) 
    
      if (length(unique(data_df[[x]])) > 1) {
        p <- p + geom_vline(xintercept = seq(1.5, length(unique(data_df[[x]])) - 0.5, by = 1), #Grid line en x
                            linetype = "dotted", color = "grey", size = 0.3, alpha = 0.5) #Visualización del grid line
      }
    
    
    return(p)
  }
}

#### renderBarPlot_stack2 ####
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

#### convert_to_plotly ####
# Transforma un gráfico ggplot en un gráfico interactivo plotly y configura el tooltip.
convert_to_plotly <- function(p, tooltip_value, isFacets = FALSE, numPlots = 1, width= "100%", height= "100%") {
  # Obtener los títulos de los ejes y el título del gráfico desde el objeto ggplot
  x_axis_title <- p$labels$x
  y_axis_title <- p$labels$y
  plot_title <- p$labels$title
  legend_title <- p$labels$colour %||% p$labels$fill  # Usar el nombre de la leyenda de 'colour' o 'fill'
  
  # Extraer los márgenes del objeto ggplot
  margins <- if (!is.null(p$theme$plot.margin)) {
    p$theme$plot.margin
  } else {
    margin(150, 50, 150, 50)  # Márgenes predeterminados si no están definidos en el ggplot
  }

  # Convertir márgenes de ggplot a formato compatible con plotly
  plotly_margins <- list(
    t = as.numeric(margins[1]),  # Top
    r = as.numeric(margins[2]),  # Right
    b = as.numeric(margins[3]),  # Bottom
    l = as.numeric(margins[4])   # Left
  )

  # Ajustar los tamaños basados en el tamaño del contenedor
  title_size <- ifelse(width < 500, 16, ifelse(width < 800, 18, 22))
  axis_text_size <- ifelse(width < 500, 13, ifelse(width < 800, 15, 18))
  tick_size <- ifelse(height < 400, 12, 14)
  legend_font_size <- ifelse(width < 500, 10, 12)  # Ajuste dinámico del tamaño de fuente de la leyenda

  # Definir la posición de la leyenda y tamaño basado en el tamaño del contenedor
  if (width < 800 && isFacets == FALSE) {
    legend_position <- list(
      x = 0.5,
      y = 1.01,  # Ajuste dinámico para evitar solapamiento
      orientation = "h",
      xanchor = "center",
      yanchor = "bottom"
    )
  } else if (width < 800 && isFacets == TRUE) {
    legend_position <- list(
      x = 0.5,
      y = cacl_Y_Axis(numPlots), # Ajuste dinámico para evitar solapamiento
      orientation = "h",
      xanchor = "center",
      yanchor = "bottom"
    ) 
    } else {
    legend_position <- list(
      x = 1.05,
      y = 0.5,
      xanchor = "left",
      yanchor = "middle"
    )
  }

  # Convertir ggplot en un objeto plotly
  p_plotly <- ggplotly(p, tooltip = tooltip_value)

  # Ajustar el diseño para que la leyenda se acople dinámicamente
  p_plotly <- p_plotly %>% layout(
    autosize = TRUE,
    title = list(
      text = plot_title,
      font = list(size = title_size, family = "Arial", color = "black", weight = "bold")
    ),
    legend = list(
      x = legend_position$x,
      y = legend_position$y,
      orientation = legend_position$orientation,
      xanchor = legend_position$xanchor,
      yanchor = legend_position$yanchor,
      font = list(size = legend_font_size, family = "Arial", color = "black"),
      title = list(
        text = paste0(legend_title, "\n"),  # Usar `\n` en lugar de `<br>`
        font = list(size = axis_text_size, family = "Arial", color = "black", weight = "bold")
      )
    ),
    margin = plotly_margins
  ) %>%
    onRender("
      function(el, x) {
        function adjustLegendSpacing() {
          console.log('Adjusting legend spacing');
          var legendTexts = el.querySelectorAll('.legendtext'); // legendtext contiene el texto de los elementos de la leyenda
          console.log('Number of legend texts found:', legendTexts.length);
          if (legendTexts.length > 0) {
            var fixedSpacingValue = 40;  // El spacing para todos los elementos de la leyenda
            legendTexts.forEach(function(textNode) {
              var parentG = textNode.closest('g');
              var colorBox = parentG ? parentG.querySelector('rect') : null;
              if (textNode && colorBox) {
                console.log('Applying fixed spacing value:', fixedSpacingValue);
                textNode.setAttribute('x', fixedSpacingValue);
              }
            });
          }
        }
        
        adjustLegendSpacing();
        
        // Observa si hay cambios en la leyenda(Seleccionar o Deseleccionar)
        var observer = new MutationObserver(function(mutations) {
          mutations.forEach(function(mutation) {
            adjustLegendSpacing();
          });
        });
        
        var legendNode = el.querySelector('.legend');
        if (legendNode) {
          observer.observe(legendNode, { childList: true, subtree: true });
        }
      }
    ")

  # Ajustar las opciones de la leyenda en dispositivos pequeños
  if (width < 800) {
    p_plotly <- p_plotly %>% layout(
      legend = list(
        orientation = "h",
        font = list(size = legend_font_size, family = "Arial", color = "black"),
        title = list(
          text = paste0("     <b>",legend_title, "</b>", "\n"),  # Cambiar a `\n` para salto de línea
          font = list(size = axis_text_size, family = "Arial", color = "black"),
          side = "top"
        ),
        traceorder = "normal",
        itemwidth = 50,  # Ajuste del ancho de cada categoría
        valign = "top"   # Alinear el texto de la leyenda en la parte superior
      )
    ) %>%
      onRender("
      function(el, x) {
        function adjustLegendSpacing() {
          console.log('Adjusting legend spacing');
          var legendTexts = el.querySelectorAll('.legendtext'); // legendtext contiene el texto de los elementos de la leyenda
          console.log('Number of legend texts found:', legendTexts.length);
          if (legendTexts.length > 0) {
            var fixedSpacingValue = 40;  // El spacing para todos los elementos de la leyenda
            legendTexts.forEach(function(textNode) {
              var parentG = textNode.closest('g');
              var colorBox = parentG ? parentG.querySelector('rect') : null;
              if (textNode && colorBox) {
                console.log('Applying fixed spacing value:', fixedSpacingValue);
                textNode.setAttribute('x', fixedSpacingValue);
              }
            });
          }
        }
        
        adjustLegendSpacing();
        
        // Observa si hay cambios en la leyenda(Seleccionar o Deseleccionar)
        var observer = new MutationObserver(function(mutations) {
          mutations.forEach(function(mutation) {
            adjustLegendSpacing();
          });
        });
        
        var legendNode = el.querySelector('.legend');
        if (legendNode) {
          observer.observe(legendNode, { childList: true, subtree: true });
        }
      }
    ")
  }

  return(p_plotly)
}

#### createFuenteDiv ####
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

#### renderBoxPlot ####
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

#### renderDataTable ####
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
                text = 'Copiar',
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

#### renderDataTable_Definitions ####
# Renderizar Tabla de Definiciones con Botones de Exportación
renderDataTable_Definitions <- function(filtered_data, title) {
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
          extend = 'copy',
          text = 'Copiar',
          title = title,        #El argumento que se recibe como titulo de la data.
          exportOptions = list(
            columns = ":visible",     #Solo se va a copiar las columnas visibles de la data.
            modifier = list(page = "all") # ALL descarga toda la data seleccionada por el usuario. 
          )
        ),
        list(
          extend = 'collection',
          text = 'Descargar Conceptos',
          buttons = list(
            list(extend = 'csv', text = 'CSV', title = title, filename = 'Definiciones_ViolenciaGeneroPR'),
            list(extend = 'excel', text = 'Excel', title = title, filename = 'Definiciones_ViolenciaGeneroPR'),
            list(extend = 'pdf', text = 'PDF', title = title, filename = 'Definiciones_ViolenciaGeneroPR')
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



#### renderMap ####
renderMap <- function(data, value_col, value_col_region, map_zoom, provider = providers$CartoDB.Positron, municipios_geo) {
  # Verificar que hay datos disponibles
  if (nrow(data) == 0 || !value_col %in% colnames(data)) {
    return(leaflet() %>% addTiles())
  }
  
  # Obtener valores de la columna dinámica
  values <- data[[value_col]]
  regiones <- data[[value_col_region]]
  
  # Calcular límites para la escala de colores
  min_val <- min(values, na.rm = TRUE)
  max_val <- max(values, na.rm = TRUE)
  rango <- max_val - min_val
  
  # Garantizar al menos 3 rangos
  num_bins <- 3
  if (rango < num_bins) {
    step <- 1
    max_val <- min_val + (num_bins - 1) * step
  } else {
    step <- ceiling(rango / num_bins)
  }
  
  # Crear los límites de los bins
  bins <- seq(
    floor(min_val / step) * step, 
    ceiling(max_val / step) * step, 
    by = step
  )
  
  violet_colors <- c(
    "#f9f5ff", "#e2b3ff", 
    "#cc80ff", "#a64dff", 
    "#8c33ff"
  )
  
  
  # Seleccionar colores según el número de categorías
  num_bins <- length(bins) - 1
  colors <- violet_colors[seq_len(num_bins)]
  
  # Crear la paleta de colores personalizada
  pal <- colorBin(colors, domain = values, bins = bins, na.color = "transparent")

  # Crear el mapa
  leaflet(data) %>%
    setView(lng = -66.5, lat = 18.2, zoom = map_zoom) %>%
    addProviderTiles(provider) %>%
    # Polígonos de las regiones coloreadas
    addPolygons(
      fillColor = ~pal(values), 
      weight = 1, 
      opacity = 1,
      color = "#666", 
      dashArray = "", 
      fillOpacity = 0.7,
      label = ~paste0(
        value_col_region, ": ", regiones, "<br>",
        value_col, ": ", values
      ) %>% lapply(htmltools::HTML), 
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.9,
        bringToFront = TRUE
      )
    ) %>%
    # Fronteras de los municipios
    addPolylines(
      data = municipios_geo,
      weight = 1, # Grosor de las líneas
      color = "#666", # Color de las líneas de los municipios
      opacity = 1
    ) %>%
    addLegend(
      pal = pal,
      values = ~values,
      opacity = 0.7,
      title = value_col,
      position = "bottomright",
      labFormat = labelFormat(digits = 0)
    )
}

#### renderMap_vivienda ####
renderMap_vivienda <- function(data, value_col, value_col_region, map_zoom, provider = providers$CartoDB.Positron, municipios_geo) {
  # Verificar que hay datos disponibles
  if (nrow(data) == 0 || !value_col %in% colnames(data)) {
    return(leaflet() %>% addTiles())
  }
  
  # Obtener valores de la columna dinámica
  values <- data[[value_col]]
  regiones <- data[[value_col_region]]
  
  # Calcular límites para la escala de colores
  min_val <- 0
  max_val <- max(values, na.rm = TRUE)
  rango <- max_val - min_val
  
  # Definir el tamaño de los pasos y los límites de los bins
  step <- ifelse(rango >= 30, 10, 5)
  bins <- seq(
    floor(min_val / step) * step,  # Múltiplo inferior del tamaño del paso
    ceiling(max_val / step) * step,  # Múltiplo superior del tamaño del paso
    by = step  # Incrementos definidos
  )
  
  violet_colors <- c(
    "#f9f5ff", "#f4e3ff", "#edd0ff", "#e2b3ff", 
    "#d89aff", "#cc80ff", "#bf66ff", "#a64dff", 
    "#8c33ff", "#701aff", "#5900f2", "#4400cc", 
    "#3000a6", "#1d007d", "#0d004c"
  )
  
  
  # Seleccionar colores según el número de categorías
  num_bins <- length(bins) - 1
  colors <- violet_colors[seq_len(num_bins)]
  
  # Crear la paleta de colores personalizada
  pal <- colorBin(colors, domain = values, bins = bins, na.color = "transparent")
  
  # Crear el mapa
  leaflet(data) %>%
    setView(lng = -66.5, lat = 18.2, zoom = map_zoom) %>%
    addProviderTiles(provider) %>% # Fondo claro
    addPolygons(
      fillColor = ~pal(values), # Colores según cantidad de casos
      weight = 1, # Líneas divisorias de los polígonos
      opacity = 1,
      color = "#666", # Color de las líneas divisorias
      dashArray = "", 
      fillOpacity = 0.7,
      label = ~paste0(
        value_col_region, ": ", regiones, "<br>",
        value_col, ": ", values
      ) %>% lapply(htmltools::HTML), # Interpretar el HTML
      highlightOptions = highlightOptions(
        weight = 1,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.9,
        bringToFront = TRUE
      )
    ) %>%
    # Fronteras de los municipios
    addPolylines(
      data = municipios_geo,
      weight = 1, # Grosor de las líneas
      color = "#666", # Color de las líneas de los municipios
      opacity = 1
    ) %>%
    addLegend(
      pal = pal,
      values = ~values,
      opacity = 0.7,
      title = value_col,
      position = "bottomright",
      labFormat = labelFormat(digits = 0) # Evitar decimales en la leyenda
    )
}

#### definitionCards ####
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

#### setColorFill ####
setColorFill <- function(df, variable) {
  # Obtener los niveles únicos de la variable
  unique_levels <- unique(df[[variable]])
  num_colors <- length(unique_levels)
  
  # Colores base de la página web
  base_colors <- c("#8bc344", "#884e9f", "#2f2e7d", "#e84924", "#d5dc30", "#5eb7df", "#3e3f3a")
  
  # Expandir la paleta manteniendo armonía con los colores base
  extended_palette <- if (num_colors > length(base_colors)) {
    colorRampPalette(base_colors)(num_colors)
  } else {
    base_colors[1:num_colors]
  }
  
  # Convertir colores a espacio LAB para calcular distancias perceptuales
  colors_rgb <- col2rgb(extended_palette) / 255 # Normalizar a rango 0-1
  colors_lab <- convertColor(t(colors_rgb), from = "sRGB", to = "Lab")
  
  # Función para calcular la distancia entre colores
  color_distance <- function(c1, c2) sqrt(sum((c1 - c2) ^ 2))
  
  # Filtrar colores que sean muy similares (distancia perceptual < 15)
  unique_colors <- list(colors_lab[1, ])
  final_colors <- extended_palette[1]
  
  for (i in 2:nrow(colors_lab)) {
    distances <- sapply(unique_colors, color_distance, c2 = colors_lab[i, ])
    if (all(distances >= 15)) { # Distancia mínima para diferenciación
      unique_colors <- append(unique_colors, list(colors_lab[i, ]))
      final_colors <- c(final_colors, extended_palette[i])
    }
  }
  
  # Asegurar que haya suficientes colores; si no, generar variaciones adicionales
  if (length(final_colors) < num_colors) {
    warning("Algunos colores eran muy similares. Generando colores adicionales.")
    final_colors <- colorRampPalette(final_colors)(num_colors)
  }
  
  # Asignar colores a cada nivel único
  my_fill <- setNames(final_colors, unique_levels)
  return(my_fill)
}

#### plotHeight ####
# Función para calcular el numero de facetas en una grafica basado
# en la cantidad de filas.
plotHeight <- function(plot_height, selected_plots){
  
  num_selected_plots = selected_plots
  num_rows = (num_selected_plots/2)
  
  if(num_selected_plots > 4){
    height = (350*num_rows)
  } else {
    height = plot_height
  }
  print(height)
  
  return(height)
}

#### cacl_Y_Axis ####
#Funcion para calcular la posicion de la leyenda en Y
#Esto cada vez que una fila en las graficas de facetas se elimina

cacl_Y_Axis <- function(current_rows){
  
  num_rows = ceiling(current_rows/2)
  
  if (num_rows == 2 || num_rows == 1) {
    y = 1.05
  } else if (num_rows == 3) {
    y = 1.03
  } else if (num_rows == 4) {
    y = 1.03
  } else if (num_rows == 5) {
    y = 1.02
  }else if (num_rows == 6) {
    y = 1.01
  } else {
    y = 1.01
  }
  
  return(y)
}



#######################
#### CODIGO MUERTO ####
#######################

# create_empty_plot_with_message_forLine <- function(data, x, y, fill, title, xlab, ylab, emptyMessage) {
#   data_df <- data()
#   ggplot(data_df, aes_string(x = x, y = y)) +
#     geom_blank() +  
#     labs(title = title, x = xlab, y = ylab) +
#     theme_minimal() +
#     theme(
#       axis.text.x = element_blank(),
#       axis.ticks = element_blank(),  
#       axis.text.y = element_blank(),
#       axis.title.x = element_text(size = 12, margin = margin(t = 10)), 
#       axis.title.y = element_text(size = 12, margin = margin(r = 10)),  
#       plot.title = element_text(hjust = 0.5, size = 15, colour = "black", face = "bold"),
#       panel.border = element_rect(colour = "black", fill = NA, size = 1),
#       plot.margin = margin(t = 50, r = 10, b = 10, l = 10)
#     ) +
#     annotate("text", x = 0.5, y = 0.5, label = emptyMessage, size = 4, hjust = 0.5, vjust = 0.5)
# }

# Renderiza un gráfico de lineas utilizando ggplot2 en el UI de Shiny.
# renderLinePlot <- function(data, x, y, group, color, title, xlab, ylab, colorlab = color, emptyMessage) {
# 
#   data_df <- data()  # Evaluar los datos reactivos una vez
# 
#   if (is.null(data_df) || nrow(data_df) == 0 || is.null(data_df[[x]])) {
#     # Si no hay datos o las variables x/y son nulas, mostrar una gráfica vacía con ejes y un mensaje
#     p <- create_empty_plot_with_message_forLine(data, x, y, fill, title, xlab, ylab,emptyMessage)
#     
#     return(p)
#     # Si no hay datos o las variables x/y son nulas, mostrar una gráfica vacía con ejes y un mensaje
#     # p <- ggplot(data_df, aes_string(x = x, y = y, group = group, color = color)) +
#     #   geom_blank() +  
#     #   labs(title = title, x = xlab, y = ylab) +
#     #   theme_minimal() +
#     #   theme(
#     #     axis.text.x = element_blank(),
#     #     axis.ticks = element_blank(), 
#     #     axis.text.y = element_blank(), 
#     #     axis.title.x = element_text(size = 12, margin = margin(t = 10)),  
#     #     axis.title.y = element_text(size = 12, margin = margin(r = 10)),  
#     #     plot.title = element_text(hjust = 0.5, size = 13, colour = "black", face = "bold"),
#     #     panel.border = element_rect(colour = "black", fill = NA, size = 1),
#     #     plot.margin = margin(t = 50, r = 10, b = 10, l = 10)
#     #   ) +
#     #   annotate("text", x = 0.5, y = 0.5, label = emptyMessage, size = 6, hjust = 0.5, vjust = 0.5)
#     # 
#     # return(p)
# 
#   } else {
#     # Calcular rango del eje de y
#     y_max <- max(data_df[[y]], na.rm = TRUE)
#     y_interval <- pretty(c(0, y_max), n = 5)[2]  
#     upper_y_limit <- ceiling(y_max / y_interval) * y_interval  
# 
# 
#     p <- ggplot(data_df, aes_string(x = x, y = y, group = group, color = color)) +
#       geom_line(color = "#2f2e7d", size = 1) +  
#       geom_point(color = "#adcc4e", size = 2,
#                  aes(
#                    text =
#                      paste0("<b>", xlab, ":</b> ", .data[[x]],
#                             "<br><b>", ylab, ":</b> ", .data[[y]]
#                      )
#                  )
#       ) +
#       expand_limits(y = 0) +  # Asegurar que el eje y comience en 0
#       labs(title = title, x = xlab, y = ylab, color = colorlab) +
#       scale_y_continuous(labels = function(x) scales::comma_format(big.mark = ",", decimal.mark = ".")(x) %>% paste0(" "),
#                          expand = expansion(mult = c(0, 0.1))) +
#       coord_cartesian(ylim = c(0, upper_y_limit)) +
#       theme_minimal() +
#       theme(
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.title = element_text(hjust = 0.5, size = 15, colour = "black", face = "bold"),
#         panel.border = element_rect(colour = "black", fill = NA, size = 1),
#         plot.margin = margin(t = 45, r = 10, b = 10, l = 10)
#       )
# 
#     return(p)
#   }
# }

# Crea un gráfico vacio indicando al usuario que debe seleccionar las variables a visualizar
# create_empty_plot_with_message <- function(data, x, y, fill, title, xlab, ylab, emptyMessage) {
#   data_df <- data()
#   ggplot(data_df, aes_string(x = x, y = y)) +
#     geom_blank() +  
#     labs(title = title, x = xlab, y = ylab) +
#     theme_minimal() +
#     theme(
#       axis.text.x = element_blank(),
#       axis.ticks = element_blank(),  
#       axis.text.y = element_blank(),
#       axis.title.x = element_text(size = 12, margin = margin(t = 10)), 
#       axis.title.y = element_text(size = 12, margin = margin(r = 10)),  
#       plot.title = element_text(hjust = 0.5, size = 13, colour = "black", face = "bold"),
#       panel.border = element_rect(colour = "black", fill = NA, size = 1),
#       plot.margin = margin(t = 100, r = 50, b = 100, l = 50)
#     ) +
#     annotate("text", x = 0.5, y = 0.5, label = emptyMessage, size = 4, hjust = 0.5, vjust = 0.5)
# }

# renderBarPlot <- function(data, x, y, fill, title, xlab, ylab, fillLab = fill, colorFill = "Set1",
#                           emptyMessage, barWidth = 1, xGap = 0.1) {
#   
#   data_df <- data()  # Evaluate the reactive data once
#   
#   if (is.null(data_df) || nrow(data_df) == 0 || is.null(data_df[[x]]) || is.null(data_df[[y]]) || is.null(data_df[[fill]])) {
#     # If there are no data or x/y variables are null, display an empty plot with axes and a message
#     p <- create_empty_plot_with_message(data, x, y, fill, title, xlab, ylab, emptyMessage)
#     
#     return(p)
#   } else {
#     
#     upper_y_limit <- ceiling(max(eval(parse(text = paste0("data()$", y))), na.rm = TRUE) * 1.2) 
#     
#     p <- ggplot(data_df, aes_string(x = x, y = y, fill = fill)) +
#       geom_bar(stat = "identity",
#                position = position_dodge2(width = barWidth, padding = xGap),
#                width = 0.7,
#                aes(
#                  text = paste(
#                    paste0("<b>", ylab, ":</b> ", after_stat(y)), "<br>",
#                    paste0("<b>", fillLab, ":</b> ", after_stat(fill)), "<br>"
#                  )
#                )) +
#       scale_fill_manual(values = colorFill) +
#       scale_y_continuous(labels = function(x) scales::comma_format(big.mark = ",", decimal.mark = ".")(x) %>% paste0(" "),
#                          expand = expansion(mult = c(0, 0.1))) +
#       coord_cartesian(ylim = c(0, upper_y_limit)) +
#       labs(title = title, x = xlab, y = ylab, fill = fillLab) +
#       theme_minimal()  # Usar tema básico sin estilos específicos
#     
#     return(p)
#   }
# }



# 
# convert_to_plotly <- function(p, tooltip_value, width = "100%", height = "100%") {
#   # Obtener los títulos de los ejes y el título del gráfico desde el objeto ggplot
#   x_axis_title <- p$labels$x
#   y_axis_title <- p$labels$y
#   plot_title <- p$labels$title
#   legend_title <- p$labels$colour %||% p$labels$fill
#   
#   # Convertir ggplot en un objeto plotly
#   p_plotly <- ggplotly(p, tooltip = tooltip_value)
#   
#   # Ajustar el diseño
#   p_plotly <- p_plotly %>% layout(
#     autosize = TRUE,
#     title = list(
#       text = plot_title,
#       font = list(size = 18, family = "Arial", color = "black", weight = "bold")
#     ),
#     legend = list(
#       x = 1.05,
#       y = 0.5,
#       xanchor = "left",
#       yanchor = "middle",
#       font = list(size = 12, family = "Arial", color = "black"),
#       title = list(
#         text = if (!is.null(legend_title)) paste0(legend_title, "\n") else NULL,
#         font = list(size = 14, family = "Arial", color = "black", weight = "bold")
#       )
#     ),
#     margin = list(t = 100, r = 50, b = 100, l = 50),
#     xaxis = list(
#       title = list(text = x_axis_title, standoff = 10),
#       tickfont = list(size = 12, family = "Arial", color = "black"),
#       automargin = TRUE
#     ),
#     yaxis = list(
#       title = list(text = y_axis_title, standoff = 10),
#       tickfont = list(size = 12, family = "Arial", color = "black"),
#       automargin = TRUE
#     )
#   )
#   
#   return(p_plotly)
# }



# # Renderiza un mapa utilizando ggplot2 en el UI del ShinyApp
# renderMap <- function(data, fill, title, group, fill_lab = fill,
#                       light_color = "lightblue", dark_color = "darkblue") {
#   p <- ggplot(data()) +
#     geom_sf(aes(fill = {{fill}}, group = {{group}})) +  # Incluye group como aesthetic mapping
#     # labs(title = title, fill = fill_lab) +
#     labs(fill = fill_lab) +
#     scale_fill_gradient(name = fill_lab, low = light_color, high = dark_color) +
#     theme_minimal() +
#     theme(
#       legend.position = "left",
#       axis.text = element_blank(),
#       axis.ticks = element_blank(),
#       panel.grid = element_blank(),
#       panel.border = element_rect(colour = "black", fill = NA, size = 1),
#       plot.margin = margin(20, 10, 10, 10),
#       # plot.title = element_text(hjust = 0.5)
#     )
#   print(p)
# }

# renderMapGroup <- function(data, fill, title, fill_lab = fill) {
#   p <- ggplot(data) +
#     geom_sf(aes(fill = {{fill}})) +
#     labs(title = title, fill = fill_lab) +
#     #scale_fill_gradient(name = fill_lab, low = light_color, high = dark_color) +
#     theme_minimal() +
#     theme(
#       legend.position = "bottom",
#       axis.text = element_blank(),
#       axis.ticks = element_blank(),
#       panel.grid = element_blank(),
#       plot.title = element_text(hjust = 0.5)
#     )
#   print(p)
# }

# Genera una paleta de colores para los niveles de una variable categórica.
# Esta función toma un dataframe y el nombre de una variable categórica, y devuelve una paleta de colores con un 
# color único para cada nivel de la variable.
# setColorFill <- function(df, variable) {
#   # Obtener los niveles únicos de la variable
#   unique_levels <- unique(df[[variable]])
#   
#   # Elegir una paleta de colores apropiada para el número de niveles únicos
#   num_colors <- length(unique_levels)
#   if (num_colors <= 8) {
#     palette <- brewer.pal(n = num_colors, name = "Set1")
#   } else {
#     palette <- rainbow(num_colors)
#   }
#   
#   # Generar una lista de patrones para los colores
#   patterns <- c("#CC6677", "#E69F00", "#88CCEE", "#B8E186", "#332288", "#D55E00", 
#                 "#F0E442", "#CC79A7", "#661100", "#888888", "#117733", "#000000")
#   # Asignar patrones y colores a cada nivel único
#   my_fill <- rep("black", length(unique_levels))
#   names(my_fill) <- unique_levels
#   for (i in 1:length(unique_levels)) {
#     my_fill[i] <- ifelse(i <= length(patterns), patterns[i], palette[i])
#   }
#   
#   return(my_fill)
# }

# 
# setColorFill <- function(df, variable) {
#   # Obtener los niveles únicos de la variable
#   unique_levels <- unique(df[[variable]])
#   num_colors <- length(unique_levels)
#   
#   # Colores base de la página web
#   base_colors <- c("#8bc344", "#884e9f", "#2f2e7d", "#e84924", "#d5dc30", "#5eb7df", "#3e3f3a")
#   
#   # Expandir la paleta manteniendo armonía con los colores base
#   if (num_colors > length(base_colors)) {
#     extended_palette <- colorRampPalette(base_colors)(num_colors)
#   } else {
#     extended_palette <- base_colors[1:num_colors]
#   }
#   
#   # Convertir colores a LAB para calcular distancias perceptuales
#   colors_rgb <- col2rgb(extended_palette)
#   colors_lab <- convertColor(t(colors_rgb / 255), from = "sRGB", to = "Lab")
#   
#   # Función para calcular la distancia entre colores
#   color_distance <- function(c1, c2) {
#     sqrt(sum((c1 - c2) ^ 2))
#   }
#   
#   # Eliminar colores demasiado similares (distancia perceptual < 10)
#   unique_colors <- list(colors_lab[1, ])
#   final_colors <- extended_palette[1]
#   
#   for (i in 2:nrow(colors_lab)) {
#     distances <- sapply(unique_colors, color_distance, c2 = colors_lab[i, ])
#     if (all(distances >= 15)) { # Distancia mínima para diferenciación
#       unique_colors <- append(unique_colors, list(colors_lab[i, ]))
#       final_colors <- c(final_colors, extended_palette[i])
#     }
#   }
#   
#   # Asegurar que haya suficientes colores; si no, se interpolan nuevos
#   if (length(final_colors) < num_colors) {
#     warning("Algunos colores eran muy similares. Generando colores adicionales.")
#     final_colors <- colorRampPalette(final_colors)(num_colors)
#   }
#   
#   # Asignar colores a cada nivel único
#   my_fill <- setNames(final_colors, unique_levels)
#   return(my_fill)
# }


