# Función para crear un checkbox group en el UI
createCheckboxGroup <- function(inputId, label, choices, selected) {
  checkboxGroupInput(inputId, label, choices = choices, selected = selected)
}

# Función para el botón de deselección/selección
updateCheckboxGroup <- function(session, inputId, choices, selected) {
  if (is.null(selected)) {
    print(selected)
    updateCheckboxGroupInput(session, inputId, choices = choices, selected = choices)
  } else {
    updateCheckboxGroupInput(session, inputId, selected = character(0))
  }
}


## esto es el function call del server
observeEvent(input$deselectAll_snmv, {
  updateCheckboxGroup(session, "checkGroup_snmv", levels(homiEdad$edad), levels(homiEdad$edad))
})

#### como funciona presentemente

### funcion para el boton de deseleccionar/seleccionar
observeEvent(input$deselectAll_snmv, {
  
  if (is.null(input$checkGroup_snmv)) { # si el checkbox está vacío, llénalo
    updateCheckboxGroupInput(
      session,
      "checkGroup_snmv",
      choices = levels(homiEdad$edad),
      selected = levels(homiEdad$edad)
    )
  } else {
    updateCheckboxGroupInput( # vacía el checkbox 
      session,
      "checkGroup_snmv",
      selected = character(0)
    )
  }
})








