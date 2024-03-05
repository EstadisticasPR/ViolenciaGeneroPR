# En el archivo test-renderLinePlot.R dentro del directorio tests
context("Probando la función renderLinePlot")
# Definir la función que devuelve el tibble como si fuera un dataset reactivo
reactive_sim <- function() {
  tibble(x = 1:10, y = rnorm(10), grupo = rep(1:2, each = 5), color = rep(c("rojo", "azul"), each = 5))
}

reactive_sim_facets <- function(){
  tibble(x = 1:20, y = rnorm(20), grupo = rep(1:2, each = 10), color = rep(c("rojo", "azul"), each = 10), facet_var = rep(c("A", "B"), each = 10))
}

reactive_sim_vacio <- function() {
  tibble(x = numeric(), y = numeric(), grupo = numeric(), color = character(), facet_var = character())
}

reactive_sim_vars_faltantes <- function() {
  tibble(x = numeric(), y = numeric(), grupo = numeric(), color = NA_character_, facet_var = NA_character_)
}

test_that("renderLinePlot renderiza correctamente el gráfico de líneas sin facetas", {
  renderLinePlot(reactive_sim, "x", "y", "grupo", "color", "Título", "Etiqueta X", "Etiqueta Y")
})

test_that("renderLinePlot renderiza correctamente el gráfico de líneas con facetas", {
  renderLinePlot(reactive_sim_facets, "x", "y", "grupo", "color", "Título", "Etiqueta X", "Etiqueta Y", facet = TRUE, facet_var = "facet_var")
})

test_that("renderLinePlot maneja correctamente facet_var NULL", {
  renderLinePlot(reactive_sim, "x", "y", "grupo", "color", "Título", "Etiqueta X", "Etiqueta Y", facet = TRUE, facet_var = NULL)
  # Agregar aserciones específicas para la salida esperada
})

test_that("renderLinePlot maneja correctamente reactive_sim vacíos", {
  renderLinePlot(reactive_sim_vacio, "x", "y", "grupo", "color", "Título", "Etiqueta X", "Etiqueta Y")
  # Agregar aserciones específicas para la salida esperada
})

test_that("renderLinePlot maneja variables faltantes con gracia", {
  renderLinePlot(reactive_sim, "x", "y", "grupo", "color", "Título", "Etiqueta X", "Etiqueta Y")
  # Agregar aserciones específicas para la salida esperada
})
