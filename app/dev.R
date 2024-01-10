# ui
# botón para tener el checkbox en un menu dropdown 
dropdownButton(
  circle = "FALSE",
  label = h3("Seleccione Tipo(s) de Maltrato:", status = "default", width = 350),
  actionButton("deselectAll_just", "Deseleccionar todo"),
  # el checkbox
  checkboxGroupInput(
    "checkGroup_just",
    label = "Seleccione el/los Articulo(s)",
    choices = levels(dfDeli$Delito),
    selected = levels(dfDeli$Delito)
  )
)

# server
### funcion para el boton de deseleccionar/seleccionar
observeEvent(input$deselectAll_just, {
  
  if (is.null(input$checkGroup_just)) {
    updateCheckboxGroupInput(
      session, 
      "checkGroup_just", 
      choices = levels(dfDeli$Delito),
      selected = levels(dfDeli$Delito)
    )
  } else {
    updateCheckboxGroupInput(
      session, 
      "checkGroup_just", 
      selected = character(0)
    )
  }
})

