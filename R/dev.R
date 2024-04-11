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

server <- function(input, output) {}

shinyApp(ui = ui, server = server)

#### Tab de la Administración de Tribunales ####
tabPanel(
  "Oficina de la Procuradora de la Mujer",
  icon = icon("person-dress"),
  tabsetPanel(
    
    #### tab con datos de solicitudes de órdenes de protección al amparo de la Ley 148 (casosCrimLey148) ####
    tabPanel(
      "opmFemiVD", 
      # Título del Tab
      titlePanel("Feminicidios por violencia doméstica, desde 1990 a 2021"),
      
      # Fuente de Datos, Actualización
      tags$span("Fuente: Oficina de la Procuradora de las Mujeres"), tags$br(),
      tags$span("Actualizado:", actualizacion_opmA), tags$br(),
      
      # Menu sidebar con widgets
      sidebarLayout(
        sidebarPanel(
          
          # botón para seleccionar el año
          createDropdownCheckbox(
            label = "Seleccione Año(s):",
            choices = opmFemiVD$Año,
            selected = opmFemiVD$Año,
            id = "opm_opmFemiVD_año"
          ),
        ),
        
        # Sección principal con los gráficos
        mainPanel(
          plotlyOutput("barPlot_opm_opmFemiVD"),
          DTOutput("dataTable_opm_opmFemiVD")
        )
      ),
    ),
    
    #### tab con datos de violencia domestica (OP_148_SoliGrupEdad) ####
    tabPanel(
      "opmCasos", 
      # Título del Tab
      titlePanel("Feminicidios por violencia doméstica, desde 1990 a 2021"),
      
      # Fuente de Datos, Actualización
      tags$span("Fuente: Oficina de la Procuradora de las Mujeres"), tags$br(),
      tags$span("Actualizado:", actualizacion_opmA), tags$br(),
      
      # Menu sidebar con widgets
      sidebarLayout(
        sidebarPanel(
          
          # botón para seleccionar el año
          createDropdownCheckbox(
            label = "Seleccione Año(s):",
            choices = opmCasos$year,
            selected = opmCasos$year,
            id = "opm_opmCasos_año"
          ),
          
          # botón para seleccionar el tipo de violencia
          createDropdownCheckbox(
            label = "Seleccione tipo de Violencia:",
            choices = opmCasos$tipo,
            selected = opmCasos$tipo,
            id = "opm_opmCasos_tipo"
          ),
        ),
        
        # Sección principal con los gráficos
        mainPanel(
          plotlyOutput("barPlot_opm_opmCasos"),
          DTOutput("dataTable_opm_opmCasos")
        )
      ),
    ),
    
    #### tab con datos del género de las víctimas (OP_Ley148_ex_parteEmitidas) ####
    tabPanel(
      "opmVic", 
      # Título del Tab
      titlePanel("Identidad de género de las víctimas (a través de CRIAS). Años 2020 a *2023"),
      
      # Fuente de Datos, Actualización
      tags$span("Fuente: Oficina de la Procuradora de las Mujeres"), tags$br(),
      tags$span("Actualizado:", actualizacion_opmB), tags$br(),
      
      # Menu sidebar con widgets
      sidebarLayout(
        sidebarPanel(
          
          # botón para seleccionar el año
          createDropdownCheckbox(
            label = "Seleccione Año(s):",
            choices = opmVic$año,
            selected = opmVic$año,
            id = "opm_opmVic_año"
          ),
          
          # botón para seleccionar el género de las victimas
          createDropdownCheckbox(
            label = "Seleccione el Género(s):",
            choices = opmVic$género,
            selected = opmVic$género,
            id = "opm_opmVic_género"
          ),
        ),
        
        # Sección principal con los gráficos
        mainPanel(
          plotlyOutput("barPlot_opm_opmVic"),
          DTOutput("dataTable_opm_opmVic")
        )
      ),
    ),
    

    #### (OP_LEY148Archivadas) ####
    #### (OP_LEY148Denegadas) ####
    #### (OP_LEY148FinalEmitidas) ####
    #### (OP_LEY148Genero) ####
    #### (tribCasosCrim) ####
###################################################################
#### Procesamiento de datos de la Administración de Tribunales ####
###################################################################

trib <- here("data", "administracion_de_tribunales", "/")

#### casosCrimLey148 ####
## Solicitudes de órdenes de protección al amparo de la Ley 148 - Violencia Sexual, por Región Judicial y grupo de edad de la parte peticionaria

# importando datos del año fiscal 2020-2021
## faltan datos
casosCrimLey148_20 <- read_excel(paste0(trib, "casosCrimLey148_20.xlsx")) %>%
  rename(
    Delito = `Año fiscal/delitos`
  ) %>%
  pivot_longer(
    !Delito,
    names_to = "Status",
    values_to = "Casos"
  ) %>%
  mutate(AñoFiscal = "2020-2021") %>%
  filter(Delito != "2020-2021")

# importando datos del año fiscal 2021-2022
## faltan datos
casosCrimLey148_21 <- read_excel(paste0(trib, "casosCrimLey148_21.xlsx")) %>%
  rename(
    Delito = `Año fiscal/delitos`
  ) %>%
  pivot_longer(
    !Delito,
    names_to = "Status",
    values_to = "Casos"
  ) %>%
  mutate(AñoFiscal = "2021-2022") %>%
  filter(Delito != "2021-2022*")

## faltan datos
casosCrimLey148 <- full_join(casosCrimLey148_20, casosCrimLey148_21)

#### OP_148_SoliGrupEdad ####

# Solicitudes de órdenes de protección al amparo de la Ley 148 - Violencia Sexual, por Región Judicial y grupo de edad de la parte peticionaria

new_names <- c("Total", "<20", "21-29", "30-39", "40-49", "50-59", ">60", "No Indica")

# datos de solicitudes de órdenes de protección en el 2020-2021
OP_148_SoliGrupEdad2020_21 <- read_excel(paste0(trib, "OP_148_SoliGrupEdad2020_21.xlsx")) %>%
  rename_at(vars(2:9), ~ new_names) %>%
  pivot_longer(
    !Región, 
    names_to = "Edad", 
    values_to = "Solicitudes"
  ) %>%
  mutate(
    Edad = factor(Edad, levels = unique(Edad)),
    AñoFiscal = "2020-2021"
  ) %>%
  filter(
    Edad != "Total",
    Región != "Total"
  )

# datos de solicitudes de órdenes de protección en el 2021-2022
OP_148_SoliGrupEdad2021_22 <- read_excel(paste0(trib, "OP_148_SoliGrupEdad2021_22.xlsx")) %>%
  rename_at(vars(2:9), ~ new_names) %>%
  pivot_longer(
    !Región, 
    names_to = "Edad", 
    values_to = "Solicitudes"
  ) %>%
  mutate(
    Edad = factor(Edad, levels = unique(Edad)),
    AñoFiscal = "2021-2022"
  ) %>%
  filter(
    Edad != "Total",
    Región != "Total"
  )

# dataset unido
OP_148_SoliGrupEdad <- full_join(
  OP_148_SoliGrupEdad2020_21, OP_148_SoliGrupEdad2021_22) %>%
  mutate(
    AñoFiscal = factor(AñoFiscal, levels = unique(AñoFiscal))
  )

#### OP_Ley148_ex_parteEmitidas ####
# Órdenes de protección ex parte emitidas al amparo de la Ley 148 - Violencia Sexual, por Región Judicial y delito

# lista con nuevos nombres de columnas para mejor interpretación
new_names <- c("Total", "Agresión Sexual", "Acoso Sexual", "Actos Lascivos", "Incesto") 

# datos de solicitudes de órdenes de protección en el 2021-2022
OP_Ley148_ex_parteEmitidas2020_21 <- read_excel(paste0(trib, "OP_Ley148_ex_parteEmitidas2020_21.xlsx")) %>%
  rename_at(vars(2:6), ~ new_names) %>%
  pivot_longer(
    !Región, 
    names_to = "Delito", 
    values_to = "ÓrdenesEmitidas"
  ) %>%
  mutate(
    AñoFiscal = factor("2020-2021")
  ) %>%
  filter(
    Delito != "Total"
  )

# datos de solicitudes de órdenes de protección en el 2021-2022
OP_Ley148_ex_parteEmitidas2021_22 <- read_excel(paste0(trib, "OP_Ley148_ex_parteEmitidas2021_22.xlsx")) %>%
  rename_at(vars(2:6), ~ new_names) %>%
  pivot_longer(
    !Región, 
    names_to = "Delito", 
    values_to = "ÓrdenesEmitidas"
  ) %>%
  mutate(
    AñoFiscal = factor("2021-2022")
  ) %>%
  filter(
    Delito != "Total"
  )

# dataset unido
OP_Ley148_ex_parteEmitidas <- full_join(
  OP_Ley148_ex_parteEmitidas2020_21, OP_Ley148_ex_parteEmitidas2021_22)

#### OP_LEY148Archivadas ####

# Cantidad de solicitudes de órdenes de protección al amparo de la Ley 148 - Violencia Sexual archivadas por Región Judicial
# lista con nuevos nombres de columnas para mejor interpretación
new_names <- c("Total", "SolicitudPeticionaria", "Otra Razón")

# datos de solicitudes archivadas de órdenes de protección en 2020-2021
OP_LEY148Archivadas2020_21 <- read_excel(paste0(trib, "OP_LEY148Archivadas2020_21.xlsx")) %>%
  rename_at(vars(2:4), ~ new_names) %>%
  pivot_longer(
    !Región, 
    names_to = "Razón", 
    values_to = "ÓrdenesArchivadas"
  ) %>%
  mutate(
    AñoFiscal = factor("2020-2021")
  ) %>%
  filter(
    Razón != "Total"
  )

# datos de solicitudes archivadas de órdenes de protección en 2021-2022
OP_LEY148Archivadas2021_22 <- read_excel(paste0(trib, "OP_LEY148Archivadas2021_22.xlsx")) %>%
  rename_at(vars(2:4), ~ new_names) %>%
  pivot_longer(
    !Región, 
    names_to = "Razón", 
    values_to = "ÓrdenesArchivadas"
  ) %>%
  mutate(
    AñoFiscal = factor("2021-2022")
  ) %>%
  filter(
    Razón != "Total"
  )

# datos de solicitudes archivadas de órdenes de protección en juntadas
OP_LEY148Archivadas <- full_join(
  OP_LEY148Archivadas2020_21, OP_LEY148Archivadas2021_22)

#### OP_LEY148Denegadas ####

# Cantidad de solicitudes de órdenes de protección al amparo de la Ley 148 - Violencia Sexual denegadas por Región Judicial

# lista con nuevos nombres de columnas para mejor interpretación
new_names <- c("No Aplican Disposiciones Ley148", "No Prueban Elementos")

# datos de solicitudes denegadas de órdenes de protección en 2022-2023
OP_LEY148Denegadas2020_2021 <- read_excel(paste0(trib, "OP_LEY148Denegadas2020_21.xlsx")) %>%
  rename_at(vars(3:4), ~ new_names) %>%
  pivot_longer(
    !Región, 
    names_to = "Razón", 
    values_to = "ÓrdenesDenegadas"
  ) %>%
  mutate(
    AñoFiscal = factor("2022-2023")
  ) %>%
  filter(
    Región != "Total",
    Razón != "Total"
  )

# datos de solicitudes denegadas de órdenes de protección en 2021-2022
OP_LEY148Denegadas2021_22 <- read_excel(paste0(trib, "OP_LEY148Denegadas2021_22.xlsx")) %>%
  rename_at(vars(3:4), ~ new_names) %>%
  pivot_longer(
    !Región, 
    names_to = "Razón", 
    values_to = "ÓrdenesDenegadas"
  ) %>%
  mutate(
    AñoFiscal = factor("2021-2022")
  ) %>%
  filter(
    Región != "Total",
    Razón != "Total"
  )

# dataset joined
OP_LEY148Denegadas <- full_join(
  OP_LEY148Denegadas2020_2021, OP_LEY148Denegadas2021_22)

#### OP_LEY148FinalEmitidas ####

# Órdenes de protección ex parte emitidas al amparo de la Ley 148 - Violencia Sexual, por Región Judicial y delito

# lista con nuevos nombres de columnas para mejor interpretación
new_names <- c("Total", "Agresión Sexual", "Acoso Sexual", "Actos Lascivos", "Incesto")

# datos de solicitudes ex parte emitidas de la ley 148 en 2020-2021
OP_LEY148FinalEmitidas2020_21 <- read_excel(paste0(trib, "OP_LEY148FinalEmitidas2020_21.xlsx")) %>%
  rename_at(vars(2:6), ~ new_names) %>%
  pivot_longer(
    !Región, 
    names_to = "Delito", 
    values_to = "ÓrdenesEmitidas"
  ) %>%
  mutate(
    AñoFiscal = factor("2020-2021")
  ) %>%
  filter(
    Región != "Total",
    Delito != "Total"
  )

# datos de solicitudes ex parte emitidas de la ley 148 en 2021-2022
OP_LEY148FinalEmitidas2021_22 <- read_excel(paste0(trib, "OP_LEY148FinalEmitidas2021_22.xlsx")) %>%
  rename_at(vars(2:6), ~ new_names) %>%
  pivot_longer(
    !Región, 
    names_to = "Delito", 
    values_to = "ÓrdenesEmitidas"
  ) %>%
  mutate(
    AñoFiscal = factor("2021-2022")
  ) %>%
  filter(
    Región != "Total",
    Delito != "Total"
  )

# dataset joined
OP_LEY148FinalEmitidas <- full_join(
  OP_LEY148FinalEmitidas2020_21, OP_LEY148FinalEmitidas2021_22)

#### OP_LEY148Genero ####


# Solicitudes de órdenes de protección al amparo de la Ley 148 - Violencia Sexual, por sexo de la parte

OP_LEY148Genero2020_21 <- read_excel(paste0(trib, "OP_LEY148Genero2020_21.xlsx")) %>%
  pivot_longer(
    !Sexo, 
    names_to = "Parte", 
    values_to = "Solicitudes"
  ) %>%
  mutate(
    AñoFiscal = factor("2020-2021")
  ) %>%
  filter(
    Sexo != "Total"
  )
OP_LEY148Genero2021_22 <- read_excel(paste0(trib, "OP_LEY148Genero2021_22.xlsx")) %>%
  pivot_longer(
    !Sexo, 
    names_to = "Parte", 
    values_to = "Solicitudes"
  ) %>%
  mutate(
    AñoFiscal = factor("2021-2022")
  ) %>%
  filter(
    Sexo != "Total"
  )

# dataset joined
OP_LEY148Genero <- full_join(
  OP_LEY148Genero2020_21, OP_LEY148Genero2021_22)


#### tribCasosCrim ####

# Tribunal de Primera Instancia: Movimiento de casos criminales de acoso sexual, actos lascivos, agresión sexual, incesto y ley contra el acecho. Ley Núm. 148-2015

# lista con nuevos nombres de columnas para mejor interpretación 
new_names <- c("Delito", "Pendiente Inicio", "Presentados", 
               "A Resolver", "Condenas", "Absoluciones", 
               "Archivos", "Traslados", "Otros", "Total", "Pendiente Final")

# datos de movimiento de casos criminales en año fiscal 2019-2020
tribCasosCrim19 <- read_excel(paste0(trib, "tribCasosCrim19.xlsx")) %>%
  rename_at(vars(1:11), ~ new_names) %>%
  pivot_longer(
    !Delito, 
    names_to = "Casos", 
    values_to = "Cantidad"
  ) %>%
  mutate(
    AñoFiscal = factor("2019-2020")
  ) %>%
  filter(
    !(Delito %in% c("2019-2020", "Total")
    )
  )

# datos de movimiento de casos criminales en año fiscal 2020-2021
tribCasosCrim20 <- read_excel(paste0(trib, "tribCasosCrim20.xlsx")) %>%
  rename_at(vars(1:11), ~ new_names) %>%
  pivot_longer(
    !Delito, 
    names_to = "Casos", 
    values_to = "Cantidad"
  ) %>%
  mutate(
    AñoFiscal = factor("2020-2021")
  ) %>%
  filter(
    !(Delito %in% c("2020-2021", "Total")
    )
  )

# datos de movimiento de casos criminales en año fiscal 2021-2022
tribCasosCrim21 <- read_excel(paste0(trib, "tribCasosCrim21.xlsx")) %>%
  rename_at(vars(1:11), ~ new_names) %>%
  pivot_longer(
    !Delito, 
    names_to = "Casos", 
    values_to = "Cantidad"
  ) %>%
  mutate(
    AñoFiscal = factor("2021-2022")
  ) %>%
  filter(
    !(Delito %in% c("2021-2022", "Total")
    )
  )

# dataset joined
tribCasosCrim <- full_join(
  tribCasosCrim19, tribCasosCrim20) %>%
  full_join(tribCasosCrim21)

