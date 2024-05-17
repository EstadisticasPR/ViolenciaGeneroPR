# Lanzar la app Shiny
#' Iniciar la aplicación Shiny
#'
#' Esta función inicializa la aplicación Shiny cargando las interfaces de usuario (UI) y
#' los servidores desde archivos separados, y luego inicia la aplicación Shiny.
#'
#' @return No retorna ningún valor, pero inicia la aplicación Shiny.
#' @export
#'
#' @examples
#' myApp()
#'
#' @import shiny
#' @importFrom base local
#' @import tidyverse
#' @import readxl
#' @import kableExtra
#' @import zoo
#' @import here
#' @import viridis
#' @import shinythemes
#' @import plotly
#' @import DT
#' @import shinyWidgets
#' @import sf
#' @import roxygen2
#' @seealso \code{\link{shinyApp}}
#'
#' @keywords internal
myApp <- function() {
  packages <- c(
    "tidyverse",
    "testthat",
    "shiny",
    "readxl",
    "kableExtra",
    "zoo",
    "here",
    "viridis",
    "shinythemes",
    "plotly",
    "DT",
    "shinyWidgets",
    "sf",
    "roxygen2",
    "htmltools",
    "shinymanager",
    "packrat",
    "rsconnect"
  )
  
  # Instalar los paquetes si no están instalados
  not_installed <- setdiff(packages, installed.packages()[,"Package"])
  if (length(not_installed) > 0) {
    install.packages(not_installed)
  }
  
  # Cargar paquetes
  lapply(packages, function(pkg) {
    library(pkg, character.only = TRUE)
  })
  
  source("R/global.R")$value
  source("R/utils.R")$value
  ui <- source("R/ui.R", local = TRUE)$value
  server <- source("R/server.R", local = TRUE)$value
  shinyApp(ui = ui, server = server)
}
