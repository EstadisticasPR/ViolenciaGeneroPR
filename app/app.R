# Importar el contenido de global.R, ui.R y server.R
source("~/Estadisticas-Sobre-Violencia-de-Genero-en-Puerto-Rico/app/global.R")
source("ui.R")
source("server.R")

# Lanzar la app Shiny
shinyApp(ui = ui, server = server)