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


enumerar_lista_especial <- function(items) {
  lista_html <- "<div class='no-bullet'>"
  for (item in items) {
    lista_html <- paste(lista_html, "<div>", item, "</div>", sep = "")
  }
  lista_html <- paste(lista_html, "</div>", sep = "")
  return(HTML(lista_html))
}

items <- c(
  "Se penetrare en la morada de la persona o en el lugar donde esté albergada y se cometiere allí maltrato, en el caso de cónyuges o cohabitantes, independientemente del sexo, estado civil, orientación sexual, identidad de género o estatus migratorio de cualquiera de las personas involucradas en la relación, cuando éstos estuvieren separados o mediare una orden de protección ordenando el desalojo de la residencia a una de las partes; o",
  "cuando se infiriere grave daño corporal a la persona; o",
  "cuando se cometiere con arma mortífera en circunstancias que no revistiesen la intención de matar o mutilar; o",
  "cuando se cometiere en la presencia de menores de edad; o",
  "cuando se cometiere luego de mediar una orden de protección o resolución contra la persona acusada expedida en auxilio de la víctima del maltrato; o",
  "se indujere, incitare u obligare a la persona a drogarse con sustancias controladas, o cualquier otra sustancia o medio que altere la voluntad de la persona o a intoxicarse con bebidas embriagantes; o",
  "Cuando se cometiere y simultáneamente se incurriere en maltrato de un menor según definido en la Ley Núm. 177 de 1 de agosto de 2003.",
  "Si a la víctima se le obliga o induce mediante maltrato, violencia física o sicológica a participar o Involucrarse en una relación sexual no deseada con terceras personas.",
  "Cuando se cometiere contra una mujer embarazada.",
  "Cuando se cometiere contra una persona menor de dieciséis (16) años y la persona agresora sea de dieciocho (18) años o más."
)

tags$li(
  HTML("<b>Maltrato agravado (Artículo 3.2):</b><br> Se impondrá pena correspondiente a delito grave de tercer grado en su mitad inferior cuando en la persona del cónyuge, ex cónyuge o de la persona con quien se cohabita o se haya cohabitado, o con quien se sostiene o haya sostenido una relación consensual, o con quien se haya procreado un hijo o hija, independientemente del sexo, estado civil, orientación sexual, identidad de género o estatus migratorio de cualquiera de las personas involucradas en la relación, se incurriere en maltrato según tipificado en esta Ley, mediando una o más de las circunstancias siguientes:"),
  
  enumerar_lista_especial(items)
)

tags$ul(
  HTML("<b style='font-size: 20px;'>Artículos de la Ley 54:</b>")
)


sectionTitle <- function(title, font_size = "20px") {
  HTML(paste("<b style='font-size:", font_size, ";'>", title, "</b>", sep = ""))
}

tags$ul(
  sectionTitle("Artículos de la Ley 54:", "20px")
)

<<<<<<< HEAD
###########

tabPanel(
  lowercaseTitle("Definiciones y Metadatos"),
  br(),
  ### FRANKIE: para cada título de sección escríbelo adentro de lo siguiente: tags$ul(sectionTitle("título", "20px")),
  tags$ul(sectionTitle("Artículos de la Ley 54:", "20px")),
  tags$ul(
    ########### FRANKIE cuando algo se escribe adentro de un tags$li(HTML()) eso crea un item aparte
    ### FRANKIE: Cuando algo se pone entre <b> y </b> se renderiza en negritas
    tags$li(HTML("<b>Incumplimiento de órdenes de protección (Artículo 2.8):</b> Cualquier violación a sabiendas de una orden de protección expedida.")),
    tags$li(HTML("<b>Maltrato (Artículo 3.1):</b> Toda persona que empleare fuerza física ...")),
    tags$li(HTML("<b>Maltrato Agravado (Artículo 3.2):</b><br>

######## FRANKIE: aqui hay un <ul> y </ul>. Esto crea una lista de items donde los <li> vienen siendo cada bullet point individualmente
<ul>
  <li>a. bullet 1; o</li>
  <li>b. bullet 2; o</li>
  <li>c. bullet 3; o</li>
</ul>
"))
    
    ,

    
    tags$li(HTML(
      "<b>Agresión Sexual Conyugal (Artículo 3.5): </b> DEFINICION :
<ul>          
  <li>a. bullet 1; 
<li>b. bullet 2; 
<li>c. bullet 3;")),
    
  ),
  br(),
  ### FRANKIE: para cada título de sección escríbelo adentro de lo siguiente: tags$ul(sectionTitle("título", "20px")),
  tags$ul(sectionTitle("Fiscalías de Puerto Rico:")),
  
)


########
renderBarPlot <- function(data, x, y, fill, title, xlab, ylab, fillLab = fill, colorFill = "Set1",
                          barWidth = 1, xGap = 0.1) {
  # Calculate the maximum y-value
  # max_y <- max(data[[y]], na.rm = TRUE)
  # max_y <- max(data %>% pull(!!sym(y)), na.rm = TRUE)
  max_y <- max(eval(parse(text = paste0("data$", y))), na.rm = TRUE)
  print(paste("max y: ", max_y))

  # Set the upper y-limit to be slightly higher than the max value (e.g., 10% higher)
  upper_y_limit <- max_y * 1.1
  print(paste("upper y: ", upper_y_limit))

  p <- ggplot(data, aes_string(x = x, y = y, fill = fill)) +
    geom_bar(stat = "identity", position = position_dodge2(width = barWidth, padding = xGap)) +
    scale_fill_manual(values = colorFill) +
    scale_y_continuous(labels = scales::comma_format(big.mark = ",", decimal.mark = "."),
                       limits = c(0, max_y)) +
    coord_cartesian(ylim = c(0, upper_y_limit), expand = TRUE) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, margin = margin(t = 10))) + # ajuste de posición vertical
    labs(title = title, x = xlab, y = ylab, fill = fillLab)

  print(p)
}




opm_fill_tipo <- setColorFill(opmCasos, "tipo")
renderBarPlot(opmCasos, x = "year", y = "cantidad", fill = "tipo",
              paste("Población atendida mediante el programa CRIAS según razón para consulta"),
              xlab = "Año", ylab = "Cantidad de personas atendidas", fillLab = "Razón para consulta",
              colorFill = opm_fill_tipo)

# Colores del status
opmMedio_fill_medio <- setColorFill(opmMedio, "Medio de orientación")

renderBarPlot(opmMedio, x = "año", y = "personas atendidas", fill = "`Medio de orientación`",
              title = "Población atendida, servicios ofrecidos y seguimientos mediante el programa CRIAS",
              xlab = "Año", ylab = "Cantidad de personas orientadas", fillLab = "Medio de orientación",
              colorFill = opmMedio_fill_medio)


data = opmMedio
x = "año"
y = pepe = "`personas atendidas`"
fill = "`Medio de orientación`"
title = "Población atendida, servicios ofrecidos y seguimientos mediante el programa CRIAS"
xlab = "Año"
ylab = "Cantidad de personas orientadas"
fillLab = "Medio de orientación"
colorFill = opmMedio_fill_medio
barWidth = 1
xGap = 0.1
