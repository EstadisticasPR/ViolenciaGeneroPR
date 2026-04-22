cat("Loading global objects from global.R...\n\n")
########################################
#### Cargar bibliotecas necesarias #####
library(shiny)
library(here)
library(dplyr)
library(purrr)
library(readxl)
library(stringr)
library(tidyr)
library(shinythemes)
library(plotly)
library(DT)
library(shinyWidgets)
library(RColorBrewer)
library(rsconnect)
library(sf)
library(zoo)
library(writexl)
library(scales)
library(ggplot2)
library(leaflet)
library(grDevices)
library(htmlwidgets)
library(pdftools)
library(htmltools)
source("utils.R")

# packages <- c(
#   #"tidyverse",
#   #"testthat",
#   "shiny",
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
#   "roxygen2",
#   "htmltools",
#   "shinymanager",
#   "packrat",
#   "rsconnect",
#   "writexl"
# )
# 
# # Instalar los paquetes si no están instalados
# not_installed <- setdiff(packages, installed.packages()[,"Package"])
# if (length(not_installed) > 0) {
#   install.packages(not_installed)
# }

# Cargar paquetes
# lapply(packages, function(pkg) {
#   library(packages)
# })

# cargando el dataset starwars para testing
#data("starwars")

#### Funcion para convertir columna con varios tipos de datos a numerico #####
#### convert_mixed_columns####
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


##################################################################
#### Funcion para guardar dataframes por agencias #####
#### guardarDatos ####
guardarDatos <- function(dataframes, nombres_sheets, nombre_archivo) {
  # Añadir la extensión .xlsx al nombre proporcionado
  nombre_archivo <- paste0(nombre_archivo, ".xlsx")
  
  # Guardar los dataframes en diferentes hojas del mismo archivo Excel
  writexl::write_xlsx(setNames(dataframes, nombres_sheets), nombre_archivo)
  
  # Mensaje de confirmación
  message("Archivo guardado como: ", nombre_archivo, " con ", length(dataframes), " hojas.")
}
#### guardarDatos_Polygons ####
guardarDatos_Polygons <- function(dataframe, nombre_archivo) {
  # Asegurarse de que el dataframe es de tipo 'sf'
  if (!inherits(dataframe, "sf")) {
    stop("El dataframe proporcionado no es un objeto espacial (sf).")
  }
  
  # Añadir la extensión .shp al nombre del archivo
  nombre_shapefile <- paste0(nombre_archivo, ".shp")
  
  # Guardar el dataframe espacial como un Shapefile
  st_write(dataframe, nombre_shapefile, delete_layer = TRUE)
  
  # Mensaje de confirmación
  message("Shapefile guardado como: ", nombre_shapefile)
}








##################################################################################
#### Procesamiento de datos del Sistema de Notificacion de Muertes Violentas #####
##################################################################################
#### Directorio ####
snmv <- here::here("data", "Sistema_de_Notificacion_de_Muertes_Violentas")

#### homiEdad ####
# importar los datos de homicidios por grupo de edad; homiEdad
homiEdad <- read_excel(paste0(snmv, "/svmvHomiEdad.xlsx")) %>%
               cleanSheet_homiEdad()

#### inci ####
# importar los datos de incidentes violentos segun tipo de muerte según el año
inci <- read_excel(file.path(snmv, "svmvIncidentes.xlsx")) %>%
  cleanSheet_inci()


#### Guardar datos procesados de Sistema de Notificacion de Muertes Violentas ####
# dataframes <- list(homiEdad, inci) # Lista de dataframes (por ejemplo: homiEdad y inci)
# 
# # Lista de nombres de hojas correspondientes a los dataframes
# nombres_sheets <- list("homicidios_mujeres_edadV", "incidentes_violentos")
# 
# # Uso de la función
# guardarDatos(dataframes, nombres_sheets, "sistema_notificacion_muertes_violentas")




##################################################################################
#### Procesamiento de datos del Departamento de Familia #####
##################################################################################
#### Directorio ####
dfam <- here::here("data", "Departamento_de_Familia", "/")

#### dfMalt ####
dfMalt <- cleanSheet_dfMalt(
  paste0(dfam, "dfMalt.xlsx"),
  years = 2018:2022
)

#### Guardar datos procesados de Departamento de Familia ####
# dataframes <- list(dfMalt) # Lista de dataframes (por ejemplo: homiEdad y inci)
# 
# # Lista de nombres de hojas correspondientes a los dataframes
# nombres_sheets <- list("maltrato_menores_victimas")
# 
# # Uso de la función
# guardarDatos(dataframes, nombres_sheets, "departamento_familia")






##################################################################################
##### Procesamiento de datos del Departamento de Justicia #####
##################################################################################
#### Directorio ####
djus <- here::here("data", "Departamento_de_Justicia", "/")

#### dfDeli ####
maps_fol <- here::here("data", "mapas/")

dfDeli <- cleanSheet_dfDeli(
  file_path = paste0(djus, "djDelitos.xlsx"),
  years = 2020:2025
)

# Crear un dataframe con las coordenadas de las fiscalías policiacas y combinar 
# los datos de delitos con los datos geográficos de los distritos fiscales
#### mapaDeli ####
maps <- cleanMap_dfDeli(
  shp_distritos = paste0(maps_fol, "/distritos_fiscales.shp"),
  dfDeli = dfDeli,
  shp_municipios = paste0(maps_fol, "/municipios.shp")
)

mapaDeli <- maps$mapaDeli
municipios_geo <- maps$municipios_geo

#### Guardar datos procesados de Departamento de Justicia ####
# dataframes <- list(dfDeli) # Lista de dataframes (por ejemplo: homiEdad y inci)
# 
# # Lista de nombres de hojas correspondientes a los dataframes
# nombres_sheets <- list("delitos_ArticuloLey54")
# 
# # Uso de la función
# guardarDatos(dataframes, nombres_sheets, "departamento_justicia")

# Uso de la función
# guardarDatos_Polygons(mapaDeli, "departamento_justicia_distritosfiscales")








##################################################################################
#### Procesamiento de datos de la Administración de Vivienda Pública #####
##################################################################################
#### Directorio ####
avp <- here::here("data", "Administracion_de_viviendas_publicas/")

#### dfAvp ####
archivo_avp <- paste0(avp, "/administracion_vivienda_publica.xlsx")
dfAvp <- cleanSheet_avp(archivo_avp)

archivo_datos_avp <- paste0(avp, "/datos_vivienda.xlsx")
dfAvp_region_soli <- cleanSheet_avp_region(archivo_datos_avp, "region_soli")
dfAvp_region_asig <- cleanSheet_avp_region(archivo_datos_avp, "region_asig")

dfAvp_municipios_soli <- cleanSheet_avp_municipios(archivo_datos_avp, "municipios_soli")
dfAvp_municipios_asig <- cleanSheet_avp_municipios(archivo_datos_avp, "municipios_asig")

#### mapa_avp ####
mapaAvp_region_soli <- create_map_avp_region(
  paste0(maps_fol, "/regiones_vivienda.shp"),
  dfAvp_region_soli
)

mapaAvp_region_asig <- create_map_avp_region(
  paste0(maps_fol, "/regiones_vivienda.shp"),
  dfAvp_region_asig
)

mapaAvp <- create_map_avp_region(
  paste0(maps_fol, "/regiones_vivienda.shp"),
  dfAvp
)

mapaAvp_asig <- mapaAvp %>%
  filter(Estado == "asignadas")

mapaAvp_sol <- mapaAvp %>%
  filter(Estado == "solicitadas")

#### Guardar datos procesados de Administración de Vivienda Pública ####
# dataframes <- list(dfAvp) # Lista de dataframes (por ejemplo: homiEdad y inci)
# 
# # Lista de nombres de hojas correspondientes a los dataframes
# nombres_sheets <- list("viviendasPublicas_porViolenciaDomestica")
# 
# # Uso de la función
# guardarDatos(dataframes, nombres_sheets, "administracion_vivienda_publica")





##################################################################################
#### Procesamiento de datos del Negociado de Policía ####
##################################################################################
#### Directorio ####
poli <- here::here("data", "Negociado_de_Policia", "/")

#### despDF ####
despDF_Adultas <- cleanSheet_despDF(paste0(poli, "npprDesp.xlsx"), "Adultas")
despDF_Menores <- cleanSheet_despDF(paste0(poli, "npprDesp.xlsx"), "Menores")


#### vEdad ####
vEdad <- cleanSheet_vEdad(paste0(poli, "npprVDedad.xlsx"))

#### maltPoli ####
maltPoli <- cleanSheet_maltPoli(paste0(poli, "nppr_maltrato.xlsx"))

#### npprDS_totales ####
npprDS_totales <- cleanSheet_npprDS_totales(paste0(poli, "npprDS_totales.xlsx"))

#### npprDS_victima ####
npprDS_victima <- cleanSheet_npprDS_victima(paste0(poli, "npprDS_victima.xlsx"))


#### npprDS_ofensores ####
npprDS_ofensores <- cleanSheet_npprDS_ofensores(paste0(poli, "npprDS_ofensores.xlsx"))

# Unir víctimas y ofensores en un solo dataframe para descarga
npprDS_rol <- dplyr::bind_rows(npprDS_victima, npprDS_ofensores)

#### npprDS_region ####
npprDS_region <- cleanSheet_npprDS_region(paste0(poli, "npprDS_region.xlsx"))

# ---- Combinar con shapefile ----
mapa_npprDS_region <- st_read(paste0(maps_fol, "/regiones_vivienda.shp")) %>%
  merge(npprDS_region, by.x = "GROUP", by.y = "Región") %>%
  rename(Región = GROUP) %>%
  relocate(Año, Región, Categoría, geometry, Casos)

# ---- Filtrar según Categoría ----
mapa_npprDS_victimas_mujeres   <- mapa_npprDS_region %>% filter(Categoría == "Víctimas: Mujeres")
mapa_npprDS_victimas_hombres   <- mapa_npprDS_region %>% filter(Categoría == "Víctimas: Hombres")
mapa_npprDS_ofensores_mujeres  <- mapa_npprDS_region %>% filter(Categoría == "Ofensores: Mujeres")
mapa_npprDS_ofensores_hombres  <- mapa_npprDS_region %>% filter(Categoría == "Ofensores: Hombres")

#### npprDS_victimas_agrupados ####
npprDS_victimas_agrupados <- cleanSheet_npprDS_victimas_agrupados(
  paste0(poli, "npprDS_victimas_agrupados.xlsx")
)


#### npprDS_ofensores_agrupados ####
npprDS_ofensores_agrupados <- cleanSheet_npprDS_ofensores_agrupados(
  paste0(poli, "npprDS_ofensores_agrupados.xlsx")
)

#### npprDS_relacion ####
npprDS_relacion <- cleanSheet_npprDS_relacion(
  paste0(poli, "npprDS_relacion.xlsx")
)

#### npprDS_tiposdelitos ####
npprDS_tiposdelitos <- cleanSheet_npprDS_tiposdelitos(
  paste0(poli, "npprDS_tiposdelitos.xlsx")
)


#### inciDF ####
inciDF <- cleanSheet_inciDF(paste0(poli, "NPPRincidentes"))

# Crear un dataframe con las coordenadas de las fiscalías policiacas y 
# combinar los datos de delitos con los datos geográficos de los distritos fiscales
inciMapa <- st_read(paste0(maps_fol, "/distritos_fiscales.shp")) %>%
  merge(inciDF, by.x = "GROUP", by.y = "Áreas Policiacas")

#### Guardar datos procesados de Negociado de Policía ####
# dataframes <- list(despDF, vEdad) # Lista de dataframes (por ejemplo: homiEdad y inci)
# 
# # Lista de nombres de hojas correspondientes a los dataframes
# nombres_sheets <- list("mujeres_desaparecidas_localizadas", "victimas_violencia_domestica")
# 
# # Uso de la función
# guardarDatos(dataframes, nombres_sheets, "negociado_policia")
# 





##################################################################################
#### Procesamiento de datos de la Oficina de la Procuradora de las Mujeres ####
##################################################################################
#### Directorio ####
opm <- here::here("data", "Oficina_de_procuradora_de_mujeres", "/")

#### opmFemiVD ####
opmFemiVD <- cleansheet_opmFemiVD(
  paste0(opm, "opmTasas.xlsx")
)

#### opmAgresores ####
opmAgresores <- cleansheet_opmAgresores(
  paste0(opm, "opmAgresores.xlsx")
)

#### opmCasos ####
opmCasos <- cleansheet_opmCasos(
  paste0(opm, "opmVictimas.xlsx")
)

#### opmVic ####
opmVic <- cleansheet_opmVic(
  paste0(opm, "opmVicGraf.xlsx")
)

#### opmMedio ####
opmMedio <- cleansheet_opmMedio(
  paste0(opm, "opmMedio.xlsx")
)

#### opmServiciosMes ####
opmServiciosMes <- cleansheet_opmServiciosMes(
  paste0(opm, "opmServiciosMes.xlsx")
)

#### Guardar datos procesados de Oficina de la Procuradora de las Mujeres ####
# dataframes <- list(opmFemiVD, opmCasos, opmVic, opmMedio, opmServiciosMes) # Lista de dataframes (por ejemplo: homiEdad y inci)
# 
# # Lista de nombres de hojas correspondientes a los dataframes
# nombres_sheets <- list("tasa_asesinatos_mujeres_violencia_domestica", "personas_atendidas_programaCRIAS",
#                        "identidad_genero_victimas_atendidas_CRIAS", "orientaciones_ofrecidas_programaCRIAS",
#                        "servicio_alcance_programaCRIAS")
# 
# # Uso de la función
# guardarDatos(dataframes, nombres_sheets, "oficina_procuradora_mujeres")
# 





##################################################################################
#### Procesamiento de datos Departamento de Corrección y Rehabilitación ####
##################################################################################
#### Directorio ####
dcr <- here::here("data", "Departamento_de_correccion_y_rehabilitacion", "/")

#### dcrCasosInv ####
archivo_dcrCasos <- paste0(dcr, "dcrCasosInv.xlsx")
dcrCasosInv <- cleansheet_dcrCasosInv(archivo_dcrCasos)
dcrCasosInv_supervision <- cleansheet_dcrCasosInv_supervision(archivo_dcrCasos)


#### dcrSentenciadas ####
archivo_dcrSent <- paste0(dcr, "dcrSentenciadas.xlsx")
dcrSentenciadas <- cleansheet_dcrSentenciadas(archivo_dcrSent)


#### Guardar datos procesados de Departamento de Corrección y Rehabilitación ####
# dataframes <- list(dcrCasosInv, dcrSentenciadas) # Lista de dataframes (por ejemplo: homiEdad y inci)
# 
# # Lista de nombres de hojas correspondientes a los dataframes
# nombres_sheets <- list("casos_supervision_ley54", "casos_sentenciados_violencia_domestica")
# 
# # Uso de la función
# guardarDatos(dataframes, nombres_sheets, "departamento_correcion")









##################################################################################
#### Procesamiento de datos de la Administración de Tribunales ####
##################################################################################
#### Directorio ####
trib <- here::here("data", "administracion_de_tribunales", "/")

#### casosCrimLey148 ####
## Solicitudes de órdenes de protección al amparo de la Ley 148 - Violencia Sexual, por Región Judicial y grupo de edad de la parte peticionaria

#### Leer todos los años fiscales ####
archivo_casosCrimLey148 <- paste0(trib, "casosCrimLey148.xlsx")
casosCrimLey148 <- cleanSheet_casosCrimLey148(archivo_casosCrimLey148)


#### OP_148_SoliGrupEdad ####
# Solicitudes de órdenes de protección al amparo de la Ley 148 - Violencia Sexual, 
# por Región Judicial y grupo de edad de la parte peticionaria
archivo_OP_148_SoliGrupEdad <- paste0(trib, "OP_148_SoliGrupEdad.xlsx")
OP_148_SoliGrupEdad <- cleanSheet_OP_148_SoliGrupEdad(archivo_OP_148_SoliGrupEdad)


#### OP_Ley148_ex_parteEmitidas ####
# Órdenes de protección ex parte emitidas al amparo de la Ley 148 - Violencia Sexual, por Región Judicial y delito
archivo_OP_Ley148_ex_parteEmitidas <- paste0(trib, "OP_Ley148_ex_parteEmitidas.xlsx")
OP_Ley148_ex_parteEmitidas <- cleanSheet_OP_Ley148_ex_parteEmitidas(archivo_OP_Ley148_ex_parteEmitidas)

#### OP_LEY148Archivadas ####
# Cantidad de órdenes de protección archivadas al amparo de la Ley 148 - Violencia Sexual archivadas por Región Judicial
archivo_OP_LEY148Archivadas <- paste0(trib, "OP_LEY148Archivadas.xlsx")
OP_LEY148Archivadas <- cleanSheet_OP_LEY148Archivadas(archivo_OP_LEY148Archivadas)

#### OP_LEY148Denegadas ####
# Cantidad de solicitudes de órdenes de protección denegadas al amparo de la Ley 148 - Violencia Sexual denegadas por Región Judicial
# lista con nuevos nombres de columnas para mejor interpretación
archivo_OP_LEY148Denegadas <- paste0(trib, "OP_LEY148Denegadas.xlsx")
OP_LEY148Denegadas <- cleanSheet_OP_LEY148Denegadas(archivo_OP_LEY148Denegadas)



#### OP_LEY148FinalEmitidas ####
# Órdenes de protección finales emitidas al amparo de la Ley 148 - Violencia Sexual, por Región Judicial y delito
archivo_OP_LEY148FinalEmitidas <- paste0(trib, "OP_LEY148FinalEmitidas.xlsx")
OP_LEY148FinalEmitidas <- cleanSheet_OP_LEY148FinalEmitidas(archivo_OP_LEY148FinalEmitidas)




#### OP_LEY148Genero ####
# Solicitudes de órdenes de protección al amparo de la Ley 148 - Violencia Sexual, por sexo de la parte
archivo_OP_LEY148Genero <- paste0(trib, "OP_LEY148Genero.xlsx")
OP_LEY148Genero <- cleanSheet_OP_LEY148Genero(archivo_OP_LEY148Genero)


#### tribCasosCrim ####

# Tribunal de Primera Instancia: Movimiento de casos criminales al amparo de la Ley Núm. 54-1989 para la prevención e intervención con la violencia doméstica
archivo_tribCasosCrim <- paste0(trib, "tribCasosCrim.xlsx")
tribCasosCrim <- cleanSheet_tribCasosCrim(archivo_tribCasosCrim)

#### Guardar datos procesados de Administración de Tribunales ####
# dataframes <- list(OP_148_SoliGrupEdad, OP_Ley148_ex_parteEmitidas,
#                    OP_LEY148Archivadas, OP_LEY148Denegadas, 
#                    OP_LEY148FinalEmitidas, OP_LEY148Genero, tribCasosCrim) # Lista de dataframes 
# 
# # Lista de nombres de hojas correspondientes a los dataframes
# nombres_sheets <- list("ordenes_proteccion_solicitadas", "ordenes_proteccion_exparteEmitidas",
#                        "ordenes_proteccion_exparteArchivadas", "ordenes_proteccion_denegadas",
#                        "ordenes_proteccion_emitidas", "ordenes_proteccion_emitidas_genero",
#                        "delitos_casos_tribunal")
# 
# # Uso de la función
# guardarDatos(dataframes, nombres_sheets, "administracion_tribunales")











##################################################################################
#### Procesamiento de datos del CAVV ####
##################################################################################
#### Directorio ####
cavv <- here::here("data", "Prevencion_de_violencia_de_genero_CAVV", "/")
#### safekitsDF ####
archivo_safekitsDF <- paste0(cavv, "SAFEkits.xlsx")
safekitsDF <- cleansheet_safekitsDF(archivo_safekitsDF)


#### safekitsDF_edades ####
archivo_safekitsDF_edades <- paste0(cavv, "SAFEkits.xlsx")
safekitsDF_edades <- cleansheet_safekitsDF_edades(archivo_safekitsDF_edades)


#### safekitsDF_analizados ####
archivo_safekitsDF_analizados <- paste0(cavv, "SAFEkits.xlsx")
safekitsDF_analizados <- cleansheet_safekitsDF_analizados(archivo_safekitsDF_analizados)
# 
# safekitsDF_analizados <- read_excel(paste0(cavv, "SAFEkits.xlsx"),
#                                 sheet = "Kit Analizados")  %>%
#   pivot_longer(
#     !Año,
#     names_to = "Laboratorio",
#     values_to = "Total"
#   ) %>%
#   mutate(
#     Laboratorio = factor(Laboratorio, 
#                        levels = c("Laboratorio Forense", "Laboratorios Externos: BODE", "Laboratorios Externos: DNA Solutions")),
#     Año = factor(Año)
#   )

#### safekits_region ####
# kits recibidos por region policiaca
archivo_safekits_region <- paste0(cavv, "SAFEkits.xlsx")
safekits_region <- cleansheet_safekits_region(archivo_safekits_region)

#### kits recibidos por region policiaca ####

# safekits_region <- read_excel(paste0(cavv, "SAFEkits.xlsx"),
#                               sheet = "Región Policiaca") %>%
#   pivot_longer(
#     cols = 2:6,
#     names_to = "Año",
#     values_to = "Cantidad"
#   ) %>%
#   filter(
#     `Región Policiaca` != "Total"
#   ) %>% 
#   rename(Región = `Región Policiaca`) %>%
#   mutate(
#     Región = factor(Región),
#     Año = factor(Año)
#   )

mapa_cavv <- st_read(paste0(maps_fol, "/regiones_vivienda.shp")) %>%
  merge(safekits_region, by.x = "GROUP", by.y = "Región") %>%
  rename(Región = GROUP) %>%
  relocate(
    Año, Región, geometry, Cantidad
  )


#### Guardar datos procesados de CAVV ####
# dataframes <- list(safekitsDF) # Lista de dataframes 
# 
# # Lista de nombres de hojas correspondientes a los dataframes
# nombres_sheets <- list("safe_kits")
# 
# # Uso de la función
# guardarDatos(dataframes, nombres_sheets, "centro_ayuda_victimas_violacion")








##################################################################################
##### Actualizaciones de los Datos #####
##################################################################################
# Fecha actualizacion de los datos de SNMV tab1
actualizacion_snmv1 <- "Última actualización: 31 de diciembre de 2024"

# Fecha actualizacion de los datos de SNMV tab2
actualizacion_snmv2 <- "Última actualización: 31 de diciembre de 2024"

# Fecha actualizacion familia
actualizacion_familia <- "Última actualización: 31 de diciembre de 2022"

# Fecha actualizacion justicia tab1
actualizacion_justicia1 <- "Última actualización: 31 de diciembre de 2025"

# Fecha actualizacion justicia tab2
actualizacion_justicia2 <- "Última actualización: 31 de diciembre de 2025"

# Fecha actualizacion avp tab1
actualizacion_avp1 <- "Última actualización: 31 de diciembre de 2025"

# Fecha actualizacion avp tab2
actualizacion_avp2 <- "Última actualización: 31 de diciembre de 2025"

# Fecha actualizacion avp tab3
actualizacion_avp3 <- "Última actualización: 31 de diciembre de 2025"

# Fecha actualizacion policia tab1
actualizacion_policia1 <- "Última actualización: 31 de diciembre de 2025"

# Fecha actualizacion policia tab2
actualizacion_policia2 <- "Última actualización: 20 de septiembre de 2024"

# Fecha actualizacion policia tab3
actualizacion_policia3 <- "Última actualización: 31 de diciembre de 2025"

# Fecha actualizacion policia tab4
actualizacion_policia4 <- "Última actualización: 31 de diciembre de 2025"

# Fecha actualizacion policia tab5
actualizacion_policia5 <- "Última actualización: 31 de diciembre de 2025"

# Fecha actualizacion policia tab6
actualizacion_policia6 <- "Última actualización: 31 de diciembre de 2025"

# Fecha actualizacion policia tab7
actualizacion_policia7 <- "Última actualización: 31 de diciembre de 2025"

# Fecha actualizacion policia tab8
actualizacion_policia8 <- "Última actualización: 31 de diciembre de 2025"

# Fecha actualizacion opm tab1
actualizacion_opm1 <- "Última actualización: 31 de julio de 2024"

# Fecha actualizacion opm tab2
actualizacion_opm2 <- "Última actualización: 31 de diciembre de 2025"

# Fecha actualizacion opm tab3
actualizacion_opm3 <- "Última actualización: 31 de diciembre de 2025"

# Fecha actualizacion opm tab4
actualizacion_opm4 <- "Última actualización: 31 de diciembre de 2025"

# Fecha actualizacion opm tab5
actualizacion_opm5 <- "Última actualización: 31 de diciembre de 2025"

# Fecha actualizacion opm tab6
actualizacion_opm6 <- "Última actualización: 31 de diciembre de 2025"

# Fecha actualizacion correccion y rehabilitacion tab1
actualizacion_dcr1 <- "Última actualización: 31 de diciembre de 2025"

# Fecha actualizacion correccion y rehabilitacion tab2
actualizacion_dcr2 <- "Última actualización: 28 de febrero de 2023"

# Fecha actualizacion Tribunales tab1
actualizacion_tribunales1 <- "Última actualización: 31 de diciembre de 2025"

# Fecha actualizacion Tribunales tab2
actualizacion_tribunales2 <- "Última actualización: 30 de junio de 2025"

# Fecha actualizacion Tribunales tab3
actualizacion_tribunales3 <- "Última actualización: 30 de junio de 2025"

# Fecha actualizacion Tribunales tab4
actualizacion_tribunales4 <- "Última actualización: 30 de junio de 2025"

# Fecha actualizacion Tribunales tab5
actualizacion_tribunales5 <- "Última actualización: 30 de junio de 2025"

# Fecha actualizacion Tribunales tab6
actualizacion_tribunales6 <- "Última actualización: 15 de octubre de 2024"

# Fecha actualizacion Tribunales tab1
actualizacion_tribunales7 <- "Última actualización: 31 de octubre de 2025"

# Fecha actualizacion datos cavv tab1
actualizacion_cavv1 <- "Última actualización: 31 de diciembre de 2025"

# Fecha actualizacion datos cavv tab2
actualizacion_cavv2 <- "Última actualización: 31 de diciembre de 2025"

# Fecha actualizacion datos cavv tab3
actualizacion_cavv3 <- "Última actualización: 31 de diciembre de 2025"

# Fecha actualizacion datos cavv tab4
actualizacion_cavv4 <- "Última actualización: 31 de diciembre de 2025"

##################################################################################
##### Credenciales para el web hosting #####
##################################################################################
# Definir usuarios y contraseñas
credentials <- data.frame(
  user = c("manu"),
  password = c("mangu"),
  stringsAsFactors = FALSE
)

