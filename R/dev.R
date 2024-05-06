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

