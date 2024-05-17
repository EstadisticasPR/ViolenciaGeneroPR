library(shiny)
library(shinymanager)


#temp<-read_excel("data/db.xlsx",sheet="temp")

# Definir usuarios y contraseñas
credentials <- data.frame(
  user = c("PuertoRico"),
  password = c("123456789"),
  stringsAsFactors = FALSE
)

ui <- secure_app(
  dashboardPage(
    dashboardHeader(title = 'PRVDRS'),
    dashboardSidebar(),
    dashboardBody(
      fluidRow(
        p("HOLA")
      )
    )
  ),
  choose_language = TRUE
)

server <- function(input, output, session) {
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderText({
    reactiveValuesToList(res_auth)
  })
  
  
}

shinyApp(ui, server)