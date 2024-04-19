cat("Loading global objects from global.R...\n\n")
########################################
#### Cargar bibliotecas necesarias #####
########################################
# packages <- c(
#   "tidyverse",
#   "readxl",
#   "kableExtra",
#   "zoo",
#   "here",
#   "viridis",
#   "shinythemes",
#   "plotly",
#   "DT",
#   "shinyWidgets",
#   "sf",
#   "roxygen2"
#   #"leaflet"
# )
# 
# # Instalar los paquetes si no están instalados
# not_installed <- setdiff(packages, installed.packages()[,"Package"])
# if (length(not_installed) > 0) {
#   install.packages(not_installed)
# }
# 
# # Cargar paquetes
# lapply(packages, function(pkg) {
#   library(pkg, character.only = TRUE)
# })

# cargando el dataset starwars para testing
data("starwars")

##################################################################
##### convert_mixed_columns se usa para manejar missing data #####
##################################################################
# funcion para convertir columna con varios tipos de datos a numerico
convert_mixed_columns <- function(data) {
  mixed_columns <- sapply(data, function(col) any(is.character(col) & !is.na(as.numeric(col))))
  mixed_columns_names <- names(mixed_columns)[mixed_columns]
  
  for (col in mixed_columns_names) {
    suppressWarnings({
      data[[col]] <- ifelse(data[[col]] == "N/A", NA, as.character(data[[col]]))
      data[[col]] <- as.numeric(data[[col]])
    })
  }
  
  return(data)
}
##################################################################################
#### Procesamiento de datos del Sistema de Notificacion de Muertes Violentas #####
##################################################################################
# path para Sistema_de_Notificacion_de_Muertes_Violentas
snmv <- here("data", "Sistema_de_Notificacion_de_Muertes_Violentas")

# importar los datos de homicidios por grupo de edad; homiEdad
homiEdad <- read_excel(paste0(snmv, "/svmvHomiEdad.xlsx")) %>%
  select(-Total) %>%
  filter(!grepl("Total", `Grupo de edad`) & `Grupo de edad` != "Desconocido") %>%
  pivot_longer(!`Grupo de edad`, names_to = "año", values_to = "casos") %>%
  rename(edad = `Grupo de edad`) %>%
  mutate(
    edad = factor(edad, levels = unique(edad)),
    año = factor(año)
  )

# Definir una paleta de colores personalizada
colores_homiEdad <- setNames(
  unique(homiEdad$edad), 
  scales::hue_pal()(length(unique(homiEdad$edad)))
)

# importar los datos de incidentes violentos segun tipo de muerte según el año, desde 2017-2020
inci <- read_excel(file.path(snmv, "svmvIncidentes.xlsx")) %>%
  # Seleccionar columnas y pivote largo
  select(-total) %>%
  pivot_longer(!`Tipo de Incidente`, names_to = "año", values_to = "casos") %>%
  # Filtrar datos no deseados
  filter(
    !grepl("Total de víctimas mujeres", `Tipo de Incidente`),
    `Tipo de Incidente` != "Total de incidentes"
  ) %>%
  mutate(
    `Tipo de Incidente` = factor(`Tipo de Incidente`),
    año = factor(año)
  ) %>%
  rename(
    tipo = `Tipo de Incidente`
  )

#############################################################
#### Procesamiento de datos del Departamento de Familia #####
#############################################################
# path para los datos del Departamento de Familia
dfam <- here("data", "Departamento_de_Familia", "/")

# importar los datos de maltratos; dfMalt
# importando datos del 2018
dfMalt2018 <- read_excel(paste0(dfam, "dfMalt2018.xlsx")) %>%
  mutate(Año = "2018")

# importando datos del 2019
dfMalt2019 <- read_excel(paste0(dfam, "dfMalt2019.xlsx")) %>%
  mutate(Año = "2019")

# importando datos del 2020
dfMalt2020 <- read_excel(paste0(dfam, "dfMalt2020.xlsx")) %>%
  mutate(Año = "2020")

# importando datos del 2021
dfMalt2021 <- read_excel(paste0(dfam, "dfMalt2021.xlsx")) %>%
  mutate(Año = "2021")

# importando datos del 2022
dfMalt2022 <- read_excel(paste0(dfam, "dfMalt2022.xlsx")) %>%
  mutate(Año = "2022")

dfMalt <- bind_rows(
  list(dfMalt2018, dfMalt2019, dfMalt2020, dfMalt2021, dfMalt2022)
) %>%
  rename(
    Masculino = `Cantidad Masculino`,
    Femenino = `Cantidad Femenino`
  ) %>%
  mutate(
    `Ambos Sexos` = Masculino + Femenino
  ) %>%
  pivot_longer(
    !c(`Tipo de Maltrato`, Año),
    names_to = "Sexo",
    values_to = "Casos"
  ) %>%
  mutate(
    `Tipo de Maltrato` = factor(`Tipo de Maltrato`),
    Año = factor(Año),
    Sexo = factor(Sexo)
  ) %>%
  rename(
     Maltrato = `Tipo de Maltrato`
  )

###############################################################
##### Procesamiento de datos del Departamento de Justicia #####
###############################################################
djus <- here("data", "Departamento_de_Justicia", "/")
maps_fol <- here("data", "mapas/")

# importando delitos del 2020
deli2020 <- read_excel(paste0(djus, "djDelitos2020.xlsx")) %>% 
  convert_mixed_columns() %>%
  mutate(Año = "2020") 

# importando delitos del 2021
deli2021 <- read_excel(paste0(djus, "djDelitos2021.xlsx")) %>%
  convert_mixed_columns() %>%
  mutate(Año = "2021")

# importando delitos del 2022
deli2022 <- read_excel(paste0(djus, "djDelitos2022.xlsx")) %>%
  convert_mixed_columns() %>%
  mutate(Año = "2022")

# importando delitos del 2023
deli2023 <- read_excel(paste0(djus, "djDelitos2023.xlsx")) %>%
  convert_mixed_columns() %>%
  mutate(Año = "2023")

dfDeli <- bind_rows(
  list(deli2020, deli2021, deli2022, deli2023)
) %>%
  filter(!grepl("TOTAL", `FISCALIA DISTRITO`, ignore.case = TRUE)) %>%
  select(-TOTAL) %>%
  pivot_longer(-c(`FISCALIA DISTRITO`, Año), names_to = "Delito", values_to = "Casos") %>%
  mutate(
    Año = factor(Año),
    Delito = factor(Delito),
    `FISCALIA DISTRITO` = factor(str_to_title(tolower(`FISCALIA DISTRITO`))
    ),
    Delito = recode(Delito,
                    "Art3.5" = "Agresión Sexual Conyugal",
                    "Art3.2" = "Maltrato Agravado",
                    "Art3.1" = "Maltrato",
                    "Art3.3" = "Maltrato por Amenaza",
                    "Art3.4" = "Maltrato por Restricción de Libertad",
                    "Art2.8" = "Incumplimiento de la Órden de Protección")
  )

# Crear un dataframe con las coordenadas de las fiscalías policiacas y combinar los datos de delitos con los datos geográficos de los distritos fiscales
mapaDeli <- st_read(paste0(maps_fol, "/distritos_fiscales.shp")) %>%
  merge(dfDeli, by.x = "GROUP", by.y = "FISCALIA DISTRITO")

###############################################################################
#### Procesamiento de datos el Departamento del Trabajo y Recursos Humanos ####
###############################################################################
dtra <- here("data", "departamento_de_trabajo", "/")

# importando el dataset de Casos en Supervisión de Ley 54
parLab <- read_excel(paste0(dtra, "dtpartlab.xlsx")) %>%
  rename(
    Sexo = Género
  ) %>%
  filter(Sexo != "Ambos") %>%
  pivot_longer(!Sexo, names_to = "Año", values_to = "Tasa") %>%
  mutate(
    Año = factor(Año),
    Sexo = factor(Sexo)
  )

##########################################################################
#### Procesamiento de datos de la Administración de Vivienda Pública #####
##########################################################################
avp <- here("data", "Administracion_de_viviendas_publicas/")

avpAsignadas <- read_excel(paste0(avp, "/avpAsignadas2017_23.xlsx")) %>% 
  rename(región = `Región `) %>%
  pivot_longer(!región, names_to = "año", values_to = "asignadas")

avpSolicitadas <- read_excel(paste0(avp, "/avpSolicitudes2017_23.xlsx")) %>% 
  rename(región = `Región `) %>%
  pivot_longer(!región, names_to = "año", values_to = "solicitadas")

# Unir los datasets por columna "región" y "año"
dfAvp <- left_join(avpSolicitadas, avpAsignadas, by = c("región", "año")) %>% 
  filter(región != "Total")  %>%
  pivot_longer(
    !c(región, año), names_to = "status", values_to = "cantidad"
  ) %>%
  mutate(
    región = factor(región),
    status = factor(status, levels = c("solicitadas", "asignadas"))
  )

# Convertir el año a numérico para eliminar el asterisco y convertirlo a int
dfAvp$año <- as.factor(sub("\\*", "", dfAvp$año))

# Crear un dataframe con las coordenadas de las fiscalías policiacas y combinar los datos de delitos con los datos geográficos de los distritos fiscales
mapaAvp <- st_read(paste0(maps_fol, "/regiones_vivienda.shp")) %>%
  merge(dfAvp, by.x = "GROUP", by.y = "región")

############################################################################
#### Procesamiento de datos Departamento de Corrección y Rehabilitación ####
############################################################################
dcr <- here("data", "Departamento_de_correccion_y_rehabilitacion", "/")

#### dcrCasosInv ####
# importando el dataset de Casos en Supervisión de Ley 54
dcrCasosInv <- read_excel(paste0(dcr, "dcrCasosInv.xlsx")) %>%
  #filter(sexo != "Total") %>%
  mutate(
    year = factor(year),
    sexo = factor(sexo),
    tipo = factor(tipo)
  ) %>%
  select(
    -c(mes)
  )
dcrCasosInv

#### dcrSentenciadas ####
dcrSentenciadas <- read_excel(paste0(dcr, "dcrSentenciadas.xlsx"))  %>%
  mutate(
    # la función as.yearmon convierte el año y mes a una sola fecha para poderla visualizar apropiadamente, la función es parte del paquete zoo
    #fecha = as.yearmon(paste(year, mes), "%Y %m")
    tipo = factor(tipo),
    year = factor(year)
  ) %>%
  select(-c(mes))
dcrSentenciadas

#########################################################
#### Procesamiento de datos del Negociado de Policía ####
#########################################################
poli <- here("data", "Negociado_de_Policia", "/")

#### despDF ####
meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# importando data del 2020
desp2020 <- read_excel(paste0(poli, "npprDesp2020.xlsx")) %>% mutate(Año = "2020") %>% rename(Categoria = Mes,Total = `Total Año 2020`)

# importando data del 2021
desp2021 <- read_excel(paste0(poli, "npprDesp2021.xlsx")) %>% mutate(Año = "2021") %>% rename(Categoria = Mes, Total = `Total Año 2021`)

# importando data del 2022
desp2022 <- read_excel(paste0(poli, "npprDesp2022.xlsx")) %>% mutate(Año = "2022") %>% rename(Categoria = Mes, Total = `Total Año 2022`)

#No incluyo 2023 porque faltan datos desde abril
desp2023 <- read_excel("data/Negociado_de_Policia/npprDesp2023.xlsx") %>% mutate(Año = "2023") %>% rename(Categoria = Mes, Total = `Total Año 2023`)

# uniendo los datasets de 2020, 2021 y 2022
despDF <- bind_rows(
  list(desp2020, desp2021, desp2022, desp2023)
) %>%
  pivot_longer(cols = -c(Categoria, Año), names_to = "Meses", values_to = "Casos") %>%
  filter(!grepl("Total", Meses)) %>%
  group_by(Categoria) %>%
  mutate(
    Meses = factor(Meses, levels = meses), 
    Meses_Numéricos = match(Meses, meses) ,
    Fecha = as.yearmon(paste(Año, Meses_Numéricos), "%Y %m"),
    Categoria = factor(Categoria),
    Año = factor(Año)
  ) %>%
  ungroup() %>%
  select(-Meses_Numéricos)

#### vEdad ####
vEdad2021 <- read_excel(paste0(poli, "npprVDedad2021.xlsx")) %>% 
  mutate(Año = "2021")
vEdad2022 <- read_excel(paste0(poli, "npprVDedad2022.xlsx")) %>% 
  mutate(Año = "2022")
vEdad2023 <- read_excel(paste0(poli, "npprVDedad2023.xlsx")) %>% 
  mutate(Año = "2023")
vEdad <- bind_rows(vEdad2021, vEdad2022, vEdad2023) %>%
  rename(
    Edad = `Grupos de Edad`,
    `Ambos Sexos` = Total,
    Mujeres = `Cantidad de mujeres víctimas`,
    Hombres = Masculino,
    Año = Año
  ) %>%
  pivot_longer(
    !c(Edad, Año), names_to = "Sexo", values_to = "Casos"
  ) %>%
  mutate(
    Edad = factor(Edad),
    Año = factor(Año),
    Sexo = factor(Sexo)
  )

#### inciDF ####
inci2021 <- read_excel(paste0(poli, "NPPRincidentes_2021.xlsx")) %>% 
  rename_with(~gsub("2021", "",.), contains("2021")) %>%
  rename_at(vars(2), ~ "Población") %>% 
  mutate(Año = "2021")

# faltan datos para el 2022
inci2022 <- read_excel(paste0(poli, "NPPRincidentes_2022.xlsx")) %>% 
  rename_with(~gsub("2022", "",.), contains("2022")) %>%
  rename_at(vars(2), ~"Población") %>% 
  mutate(Año = "2022")

# faltan datos desde mayo en adelante
inci2023 <- read_excel(paste0(poli,"NPPRincidentes_2023.xlsx")) %>% rename_with(~gsub("2022", "",.), contains("2022")) %>%
  rename_with(~gsub("2023", "",.), contains("2023")) %>%
  rename_at(vars(2), ~ "Población") %>%
  mutate(Año = "2023")

# dataframe con toda la data combinada
inciDF <- bind_rows(inci2021, inci2022, inci2023) %>%
  filter(`Áreas Policiacas` != "Total") %>%
  pivot_longer(cols = -c(`Áreas Policiacas`, Población, Año), names_to = "Mes", values_to = "Casos") %>%
  mutate(
    `Áreas Policiacas` = factor(str_trim(`Áreas Policiacas`)),
    Año = factor(Año),
    Meses = factor(Mes), 
    Meses_Numéricos = match(Meses, Mes),
    Fecha = as.yearmon(paste(Año, Meses_Numéricos), "%Y %m")
  ) %>%
  select(-c(Meses_Numéricos, Mes, Meses))

# Crear un dataframe con las coordenadas de las fiscalías policiacas y combinar los datos de delitos con los datos geográficos de los distritos fiscales
inciMapa <- st_read(paste0(maps_fol, "/distritos_fiscales.shp")) %>%
  merge(inciDF, by.x = "GROUP", by.y = "Áreas Policiacas")

###############################################################################
#### Procesamiento de datos de la Oficina de la Procuradora de las Mujeres ####
###############################################################################
opm <- here("data", "Oficina_de_procuradora_de_mujeres", "/")

#### opmFemiVD ####
opmFemiVD <- read_excel(paste0(opm, "opmFemiVD.xlsx")) %>%
  mutate(
    Año = factor(Año)
  )

#### opmCasos ####
meses <- c("1" = "enero", "2" = "febrero", "3" = "marzo",  "4" = "abril", "5" = "mayo", "6" = "junio", "7" = "julio", "8" = "agosto", "9" = "septiembre","10" = "octubre", "11" = "noviembre", "12" = "diciembre")
opmCasos <-  read_excel(paste0(opm, "opmPartMes.xlsx")) %>%
  mutate(
    # la función as.yearmon convierte el año y mes a una sola fecha para poderla visualizar apropiadamente, la función es parte del paquete zoo
    fecha = as.yearmon(paste(year, month), "%Y %m"),
    month =  factor(month, levels = 1:12, labels = meses), 
    year = factor(year),
    tipo = factor(tipo)
  )

#### opmVic ####
opmVic <- read_excel(paste0(opm, "opmVicGraf.xlsx")) %>% 
  rename_at(vars(1,2,3,4), ~ c("género","2020", "2021", "2022")) %>%
  pivot_longer(!género, names_to = "año", values_to = "víctimas") %>%
  mutate(
    género = factor(género),
    año = factor(año)
  )

#### opmMedio ####
opmMedio <- read_excel(paste0(opm, "opmMedio.xlsx")) %>% 
  rename_at(vars(2,3,4), ~ c("2020", "2021", "2022")) %>%
  pivot_longer(!`Medio de orientación`, names_to = "año", values_to = "personas atendidas") %>% 
  filter(`Medio de orientación` != "Total") %>%
  mutate(
    `Medio de orientación` = factor(`Medio de orientación`),
    año = factor(año)
  )

#### opmServiciosMes ####
opmServiciosMes <-  read_excel(paste0(opm, "opmServiciosMes.xlsx")) %>%
  mutate(
    fecha = as.yearmon(paste(year, month), "%Y %m"),
    tipo = factor(tipo),
    year = factor(year)
  ) %>%
  select(-c(month))


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
    AñoFiscal = factor(AñoFiscal, levels = unique(AñoFiscal)),
    Región = factor(Región)
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
  OP_Ley148_ex_parteEmitidas2020_21, OP_Ley148_ex_parteEmitidas2021_22 
  ) %>%
  mutate(
    Región = factor(Región),
    Delito = factor(Delito)
  )

#### OP_LEY148Archivadas ####

# Cantidad de solicitudes de órdenes de protección al amparo de la Ley 148 - Violencia Sexual archivadas por Región Judicial
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
  OP_LEY148Archivadas2020_21, OP_LEY148Archivadas2021_22) %>%
  mutate(
    Razón = factor(Razón),
    Región = factor(Región)
  )

#### OP_LEY148Denegadas ####

# Cantidad de solicitudes de órdenes de protección denegadas al amparo de la Ley 148 - Violencia Sexual denegadas por Región Judicial
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
  OP_LEY148Denegadas2020_2021, OP_LEY148Denegadas2021_22) %>%
  mutate(
    Región = factor(Región),
    Razón = factor(Razón)
  )

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


########################################
##### Actualizaciones de los Datos #####
########################################
# Fecha actualizacion de los datos de violencia domestica
actualizacion_policiaA <- "06/09/2023"

# Fecha cuando se actualizan los datos de desaparecidas
actualizacion_policiaB <- "05/03/2023"

# Fecha actualizacion delitos Ley 54
actualizacion_justiciaA <- "06/09/2023"

# Fecha actualizacion registro de personas convictas
actualizacion_justiciaB <- "06/09/2023"

# Fecha actualizacion feminicidios
actualizacion_opmA <- "05/17/2022"

# Fecha actualizacion resto de los datos de OPM
actualizacion_opmB <- "04/20/2023"

# Fecha actualizacion datos del SNMV
actualizacion_snmvA <- "05/10/2024"

# Fecha actualizacion tasas ajustadas del SNMV
actualizacion_snmvB <- "05/10/2024"

# Fecha actualizacion datos de Correcion
actualizacion_correcion <- "04/03/2023"

# Fecha actualizacion datos de Vivienda
actualizacion_vivienda <- "07/19/2023"

# Fecha actualizacion datos del Observatorio
actualizacion_observatorio <- ""

# Fecha actualizacion datos del Dept de Trabajo
actualizacion_trabajo <- "07/12/2021"

# Fecha actualizacion datos Dept Familia
actualizacion_familia <- "06/09/2021"

# Fecha actualizacion Ordenes de Proteccion
actualizacion_tribunalesA <- "02/21/2023"

# Fecha actualizacion Movimiento Casos
actualizacion_tribunalesB <- "02/21/2023"
