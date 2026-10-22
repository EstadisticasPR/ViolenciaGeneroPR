cat("Loading helper functions from utils.R...\n")

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(sf)

##################################
### Helper Functions: Limpieza ###
##################################

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

##################################
#### Helper Functions: Global ####
##################################





######## Sistema de Notificacion de Muertes Violentas ########
#### cleanSheet_homiEdad ####
cleanSheet_homiEdad <- function(data) {
  
  data %>%
    select(-Total) %>%
    filter(!grepl("Total", `Grupo de edad`) & `Grupo de edad` != "Desconocido") %>%
    pivot_longer(
      cols = -`Grupo de edad`,
      names_to = "Año",
      values_to = "Casos"
    ) %>%
    rename(
      Edad = `Grupo de edad`
    ) %>%
    replace_na(list(Casos = 0)) %>%
    mutate(
      Edad = str_replace(Edad, "^Menos de 15 años$", "menos de 15 años"),
      Edad = factor(Edad, levels = unique(Edad)),
      Año = factor(Año)
    ) %>%
    relocate(Año, Edad, Casos)
}

#### cleanSheet_inci ####
cleanSheet_inci <- function(data) {
  
  data %>%
    select(-total) %>%
    pivot_longer(
      cols = -`Tipo de Incidente`,
      names_to = "Año",
      values_to = "Casos"
    ) %>%
    filter(
      !grepl("Total de víctimas mujeres", `Tipo de Incidente`),
      `Tipo de Incidente` != "Total de incidentes"
    ) %>%
    rename(
      Incidente = `Tipo de Incidente`
    ) %>%
    replace_na(list(Casos = 0)) %>%
    mutate(
      Incidente = factor(Incidente),
      Año = factor(Año)
    ) %>%
    relocate(Año, Incidente, Casos)
}








######## Departamento de la Familia ########
#### cleanSheet_dfMalt ####
cleanSheet_dfMalt <- function(data, years) {
  
  dfMalt <- lapply(as.character(years), function(sheet_name) {
    read_excel(data, sheet = sheet_name) %>%
      mutate(Año = sheet_name)
  }) %>%
    bind_rows() %>%
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
      `Tipo de Maltrato` = factor(`Tipo de Maltrato`,
                                  levels = c(
                                    "Abuso Sexual", "Explotación", "Maltrato Físico",
                                    "Negligencia", "Negligencia Educativa", "Negligencia Emocional",
                                    "Negligencia Médica", "Trata Humana", "Otro"
                                  ),
                                  ordered = TRUE
      ),
      Año = factor(Año),
      Sexo = factor(Sexo)
    ) %>%
    rename(
      Maltrato = `Tipo de Maltrato`
    ) %>%
    replace_na(list(Casos = 0)) %>%
    distinct() %>%
    relocate(Año, Maltrato, Sexo, Casos)
  
  # Crear dataset de negligencia agregada
  negligencia_sum <- dfMalt %>%
    filter(grepl("Negligencia", Maltrato, ignore.case = TRUE)) %>%
    group_by(Año, Sexo) %>%
    summarise(Casos = sum(Casos), .groups = "drop") %>%
    mutate(Maltrato = "Negligencia")
  
  dfMalt %>%
    filter(!grepl("Negligencia", Maltrato, ignore.case = TRUE)) %>%
    bind_rows(negligencia_sum) %>%
    mutate(
      Maltrato = factor(
        Maltrato,
        levels = c(
          "Abuso Sexual", "Explotación", "Maltrato Físico",
          "Trata Humana", "Negligencia", "Otro"
        ),
        ordered = TRUE
      )
    )
}








######## Departamento de Justicia ########
#### cleanSheet_dfDeli ####
cleanSheet_dfDeli <- function(file_path, years) {
  
  df <- purrr::map_dfr(as.character(years), function(sheet_name) {
    read_excel(file_path, sheet = sheet_name) %>%
      convert_mixed_columns() %>%       # función que ya usas para normalizar tipos
      mutate(Año = sheet_name)
  }) %>%
    filter(!grepl("TOTAL", `FISCALIA DISTRITO`, ignore.case = TRUE)) %>%
    select(-TOTAL) %>%
    pivot_longer(
      -c(`FISCALIA DISTRITO`, Año),
      names_to = "Delito",
      values_to = "Casos"
    ) %>%
    mutate(
      Año = factor(Año),
      Delito = factor(Delito),
      Distrito = factor(str_to_title(tolower(`FISCALIA DISTRITO`))),
      Delito = recode(Delito,
                      "Art3.5" = "Agresión Sexual Conyugal",
                      "Art3.2" = "Maltrato Agravado",
                      "Art3.1" = "Maltrato",
                      "Art3.3" = "Maltrato por Amenaza",
                      "Art3.4" = "Maltrato por Restricción de Libertad",
                      "Art2.8" = "Incumplimiento de la Órden de Protección"
      )
    ) %>%
    replace_na(list(Casos = 0)) %>%
    select(-`FISCALIA DISTRITO`) %>%
    relocate(Año, Distrito, Delito, Casos)
  
  return(df)
}

#### cleanMap_dfDeli ####
cleanMap_dfDeli <- function(shp_distritos, dfDeli, shp_municipios) {
  
  # Leer shapefile de distritos y unir con los datos
  mapaDeli <- st_read(shp_distritos) %>%
    merge(dfDeli, by.x = "GROUP", by.y = "Distrito") %>%
    rename(`Distrito Fiscal` = GROUP) %>%
    relocate(Año, `Distrito Fiscal`, Delito, geometry, Casos)
  
  # Leer shapefile de municipios y estandarizar CRS
  municipios_geo <- st_read(shp_municipios) %>%
    st_transform(crs = 4326) %>% # WGS84
    mutate(municipio = case_when(
      municipio == "A??asco" ~ "Añasco",
      municipio == "Bayam??n" ~ "Bayamón",
      municipio == "Can??vanas" ~ "Canóvanas",
      municipio == "Cata??o" ~ "Cataño",
      municipio == "Comer??o" ~ "Comerío",
      municipio == "Gu??nica" ~ "Guánica",
      municipio == "Juana D??az" ~ "Juana Díaz",
      municipio == "Las Mar??as" ~ "Las Marias",
      municipio == "Lo??za" ~ "Loíza",
      municipio == "Manat??" ~ "Manatí",
      municipio == "Mayag??ez" ~ "Mayagüez",
      municipio == "Pe??uelas" ~ "Peñuelas",
      municipio == "Rinc??n" ~ "Rincón",
      municipio == "R??o Grande" ~ "Rio Grande",
      municipio == "San Germ??n" ~ "San Germán",
      municipio == "San Sebasti??n" ~ "San Sebastián",
      TRUE ~ municipio
    )) %>%
    dplyr::select(municipio, geometry)
  
  return(list(
    mapaDeli = mapaDeli,
    municipios_geo = municipios_geo
  ))
}









######## Administracion de Vivienda Publica ########
#### cleanSheet_avp ####
cleanSheet_avp <- function(file){
  
  df <- read_excel(file) %>%
    mutate(
      Región = factor(Región),
      Estado = factor(Estado, levels = c("solicitadas","asignadas"))
    ) %>%
    replace_na(list(Cantidad = 0))
  
  df$Año <- as.factor(sub("\\*", "", df$Año))
  
  return(df)
  
}

#### cleanSheet_avp_region ####
cleanSheet_avp_region <- function(file, sheet_name){
  
  df <- read_excel(file, sheet = sheet_name)
  
  # year_cols <- names(df)[grepl("^[0-9]{4}$", names(df))]
  year_cols <- names(df)[2:11]
  
  estado <- ifelse(grepl("soli", sheet_name), "solicitadas", "asignadas")
  
  df_long <- df %>%
    pivot_longer(
      cols = all_of(year_cols),
      names_to = "Año",
      values_to = "Cantidad"
    ) %>%
    mutate(
      Estado = factor(estado),
      Región = factor(Región),
      Año = factor(Año)
    ) %>%
    relocate(Año, .before = everything()) %>%
    relocate(Estado, .before = Cantidad)
  
  return(df_long)
  
}

#### cleanSheet_avp_municipios ####
cleanSheet_avp_municipios <- function(file, sheet_name){
  
  df <- read_excel(file, sheet = sheet_name)
  
  # year_cols <- names(df)[grepl("^[0-9]{4}$", names(df))]
  year_cols <- names(df)[3:12]
  
  estado <- ifelse(grepl("soli", sheet_name), "solicitadas", "asignadas")
  
  df_long <- df %>%
    pivot_longer(
      cols = all_of(year_cols),
      names_to = "Año",
      values_to = "Cantidad"
    ) %>%
    mutate(
      Estado = factor(estado),
      municipio = factor(municipio),
      Región = factor(Región),
      Año = factor(Año)
    ) %>%
    relocate(Año, .before = everything()) %>%
    relocate(Estado, .before = Cantidad)
  
  return(df_long)
  
}

#### create_map_avp_region ####
create_map_avp_region <- function(shapefile, df){
  
  mapa <- st_read(shapefile) %>%
    merge(df, by.x = "GROUP", by.y = "Región") %>%
    rename(Región = GROUP) %>%
    relocate(Año, Región, Estado, geometry, Cantidad)
  
  return(mapa)
  
}

create_map_avp_municipio <- function(shapefile, df){
  
  mapa <- shapefile %>%
    merge(df, by.x = "municipio", by.y = "municipio") %>%
    # rename(Municipio = municipio) %>%
    relocate(Año, municipio, Región, Estado, geometry, Cantidad)
  
  return(mapa)
  
}






#### Policia de Puerto Rico ####
#### cleanSheet_despDF ####
cleanSheet_despDF <- function(file, tipo = c("Adultas", "Menores")) {
  
  tipo <- match.arg(tipo)
  
  df <- read_excel(file) %>%
    # ---- Limpieza base (antes estaba en cleanSheet_npprDesp) ----
  rename(Estado = Año) %>%
    pivot_longer(
      cols = -Estado,
      names_to = "Año",
      values_to = "Casos"
    )
  
  # ---- Filtros según tipo ----
  if (tipo == "Adultas") {
    df <- df %>%
      filter(!grepl("Adultas Desaparecidas", Estado, ignore.case = TRUE)) %>%
      filter(!grepl("Menores Desaparecidas", Estado, ignore.case = TRUE)) %>%
      filter(!grepl("Menores Localizadas", Estado, ignore.case = TRUE)) %>%
      filter(!grepl("Menores sin Localizar", Estado, ignore.case = TRUE))
    
  } else if (tipo == "Menores") {
    df <- df %>%
      filter(!grepl("Adultas Desaparecidas", Estado, ignore.case = TRUE)) %>%
      filter(!grepl("Menores Desaparecidas", Estado, ignore.case = TRUE)) %>%
      filter(!grepl("Adultas Localizadas", Estado, ignore.case = TRUE)) %>%
      filter(!grepl("Adultas Sin Localizar", Estado, ignore.case = TRUE))
  }
  
  # ---- Limpieza final ----
  df <- df %>%
    mutate(
      Año = factor(Año),
      Estado = factor(Estado)
    ) %>%
    replace_na(list(Casos = 0)) %>%
    select(Año, Estado, Casos)
  
  return(df)
}

#### cleanSheet_npprVDedad ####
cleanSheet_vEdad <- function(file, years = 2021:2026) {
  
  
  vEdad_list <- lapply(as.character(years), function(sheet_name) {
    read_excel(file, sheet = sheet_name) %>%
      mutate(Año = sheet_name)
  })
  
  df <- vEdad_list %>%
    reduce(full_join) %>%
    rename(
      Edad = `Grupos de Edad`,
      `Ambos Sexos` = Total,
      Mujeres = `Cantidad de mujeres víctimas`,
      Hombres = Masculino
    ) %>%
    pivot_longer(
      !c(Edad, Año),
      names_to = "Sexo",
      values_to = "Casos"
    ) %>%
    mutate(
      Edad = str_replace_all(Edad, c(
        "^< 16$" = "menos de 16 años",
        ">60$" = "60 años o más"
      )),
      Edad = factor(
        Edad,
        levels = c(
          "menos de 16 años", "16-29 años", "30-44 años", 
          "45-59 años","60 años o más", "Desconocida"
        ),
        ordered = TRUE
      ),
      Año = factor(Año),
      Sexo = factor(
        Sexo,
        levels = c("Hombres", "Mujeres", "Ambos Sexos", "Desconocido"),
        ordered = TRUE
      )
    ) %>%
    replace_na(list(Casos = 0)) %>%
    select(Año, Edad, Sexo, Casos)
  
  return(df)
}


#### cleanSheet_npprMalt ####
cleanSheet_maltPoli <- function(file, years = 2021:2025) {
  
  # ---- Leer y añadir año por hoja ----
  malt_list <- lapply(as.character(years), function(sheet_name) {
    read_excel(file, sheet = sheet_name) %>%
      mutate(Año = sheet_name)
  })
  
  # ---- Unir y transformar ----
  df <- malt_list %>%
    bind_rows() %>%   # mejor que full_join en este caso
    rename(
      Maltrato = `Tipo de Maltrato`,
      Mujeres = Femenino,
      Hombres = Masculino
    ) %>%
    pivot_longer(
      !c(Maltrato, Año),
      names_to = "Sexo",
      values_to = "Casos"
    ) %>%
    mutate(
      Maltrato = factor(
        Maltrato,
        levels = c(
          "Violación Orden de Protección", "Sexual", "Restricción de libertad",
          "Psicológico o emocional", "Físico", "Amenaza", "Otro"
        ),
        ordered = TRUE
      ),
      Año = factor(Año),
      Sexo = factor(
        Sexo,
        levels = c(
          "Hombres", "Mujeres", "Mujer Trans", "Hombre Trans",
          "No Binario", "Desconocido"
        ),
        ordered = TRUE
      )
    ) %>%
    replace_na(list(Casos = 0)) %>%
    select(Año, Maltrato, Sexo, Casos)
  
  return(df)
}

#### cleanSheet_npprDS_totales ####
cleanSheet_npprDS_totales <- function(file, sheets = c("Victimas", "Ofensores")) {
  
  # ---- Leer y añadir Rol por hoja ----
  ds_list <- lapply(sheets, function(sheet_name) {
    read_excel(file, sheet = sheet_name) %>%
      mutate(Rol = sheet_name)
  })
  
  # ---- Unir y transformar ----
  df <- ds_list %>%
    bind_rows() %>%   # mejor que full_join
    pivot_longer(
      cols = c(Mujeres, Hombres),
      names_to = "Sexo",
      values_to = "Casos"
    ) %>%
    mutate(
      Rol = factor(Rol),
      Año = factor(Año),
      Sexo = factor(Sexo)
    ) %>%
    replace_na(list(Casos = 0)) %>%
    select(Año, Sexo, Casos, Rol)
  
  return(df)
}

#### cleanSheet_npprDS_victima ####
cleanSheet_npprDS_victima <- function(file, years = 2019:2026) {
  
  # ---- Leer y añadir Año por hoja ----
  vict_list <- lapply(as.character(years), function(sheet_name) {
    read_excel(file, sheet = sheet_name) %>%
      mutate(Año = sheet_name)
  })
  
  # ---- Unir y transformar ----
  df <- vict_list %>%
    bind_rows() %>%   # apilar todas las hojas
    rename(Edad = `Grupos de Edad`) %>%
    pivot_longer(
      !c(Edad, Año),
      names_to = "Sexo",
      values_to = "Casos"
    ) %>%
    mutate(
      Edad = str_replace(Edad, "^15 años o menos$", "menos de 15 años"),
      Edad = factor(Edad, levels = unique(Edad)),
      Año = factor(Año),
      Sexo = factor(Sexo),
      Rol = "Víctima"
    ) %>%
    replace_na(list(Casos = 0)) %>%
    select(Año, Edad, Sexo, Casos, Rol)
  
  return(df)
}

#### cleanSheet_npprDS_ofensores ####
cleanSheet_npprDS_ofensores <- function(file, years = 2019:2026) {
  
  # ---- Leer y añadir Año por hoja ----
  ofens_list <- lapply(as.character(years), function(sheet_name) {
    read_excel(file, sheet = sheet_name) %>%
      mutate(Año = sheet_name)
  })
  
  # ---- Unir y transformar ----
  df <- ofens_list %>%
    bind_rows() %>%
    rename(Edad = `Grupos de Edad`) %>%
    pivot_longer(
      !c(Edad, Año),
      names_to = "Sexo",
      values_to = "Casos"
    ) %>%
    mutate(
      Edad = str_replace(Edad, "^15 años o menos$", "menos de 15 años"),
      Edad = factor(Edad, levels = unique(Edad)),
      Año = factor(Año),
      Sexo = factor(Sexo),
      Rol = "Ofensor"
    ) %>%
    replace_na(list(Casos = 0)) %>%
    select(Año, Edad, Sexo, Casos, Rol)
  
  return(df)
}

#### cleanSheet_npprDS_region ####
cleanSheet_npprDS_region <- function(file, years = 2019:2026) {
  
  # ---- Leer y añadir Año por hoja ----
  region_list <- lapply(as.character(years), function(sheet_name) {
    read_excel(file, sheet = sheet_name) %>%
      mutate(Año = sheet_name)
  })
  
  # ---- Unir y transformar ----
  df <- region_list %>%
    bind_rows() %>%
    pivot_longer(
      !c(Región, Año),
      names_to = "Categoría",
      values_to = "Casos"
    ) %>%
    mutate(
      Región = factor(Región, levels = unique(Región)),
      Año = factor(Año),
      Categoría = factor(
        Categoría,
        levels = c("Víctimas: Mujeres", "Víctimas: Hombres", "Ofensores: Mujeres", "Ofensores: Hombres"),
        ordered = TRUE
      )
    ) %>%
    replace_na(list(Casos = 0)) %>%
    select(Año, Región, Categoría, Casos)
  
  return(df)
}

#### cleanSheet_npprDS_victimas_agrupados ####
cleanSheet_npprDS_victimas_agrupados <- function(file, years = 2019:2026) {
  
  # ---- Leer y añadir Año por hoja ----
  vict_list <- lapply(as.character(years), function(sheet_name) {
    read_excel(file, sheet = sheet_name) %>%
      mutate(Año = sheet_name)
  })
  
  # ---- Unir y transformar ----
  df <- vict_list %>%
    bind_rows() %>%   # apilar hojas por año
    rename(Edad = `Grupos de Edad`) %>%
    pivot_longer(
      !c(Edad, Año),
      names_to = "Sexo",
      values_to = "Casos"
    ) %>%
    mutate(
      Edad = factor(Edad, levels = unique(Edad)),
      Año = factor(Año),
      Sexo = factor(Sexo)
    ) %>%
    replace_na(list(Casos = 0)) %>%
    select(Año, Edad, Sexo, Casos)
  
  return(df)
}

#### cleanSheet_npprDS_ofensores_agrupados ####
cleanSheet_npprDS_ofensores_agrupados <- function(file, years = 2019:2026) {
  
  # ---- Leer y añadir Año por hoja ----
  ofens_list <- lapply(as.character(years), function(sheet_name) {
    read_excel(file, sheet = sheet_name) %>%
      mutate(Año = sheet_name)
  })
  
  # ---- Unir y transformar ----
  df <- ofens_list %>%
    bind_rows() %>%   # apilar hojas por año
    rename(Edad = `Grupos de Edad`) %>%
    pivot_longer(
      !c(Edad, Año),
      names_to = "Sexo",
      values_to = "Casos"
    ) %>%
    mutate(
      Edad = factor(Edad, levels = unique(Edad)),
      Año = factor(Año),
      Sexo = factor(Sexo)
    ) %>%
    replace_na(list(Casos = 0)) %>%
    select(Año, Edad, Sexo, Casos)
  
  return(df)
}

#### cleanSheet_npprDS_relacion ####
cleanSheet_npprDS_relacion <- function(file, years = 2019:2026) {
  
  # ---- Leer y añadir Año por hoja ----
  relacion_list <- lapply(as.character(years), function(sheet_name) {
    read_excel(file, sheet = sheet_name) %>%
      mutate(Año = sheet_name)
  })
  
  # ---- Unir y transformar ----
  df <- relacion_list %>%
    bind_rows() %>%
    pivot_longer(
      c(Familiar, `No Familiar`),
      names_to = "Relación",
      values_to = "Casos"
    ) %>%
    mutate(
      Relación = factor(Relación),
      Año = factor(Año),
      Región = factor(Región)
    ) %>%
    replace_na(list(Casos = 0)) %>%
    select(Año, Región, Relación, Casos)
  
  return(df)
}

#### cleanSheet_npprDS_tiposdelitos ####
cleanSheet_npprDS_tiposdelitos <- function(file, years = 2019:2026) {
  
  # ---- Leer y añadir Año por hoja ----
  delitos_list <- lapply(as.character(years), function(sheet_name) {
    read_excel(file, sheet = sheet_name) %>%
      mutate(Año = sheet_name)
  })
  
  # ---- Unir y transformar ----
  df <- delitos_list %>%
    bind_rows() %>%
    pivot_longer(
      cols = c(Mujeres, Hombres),
      names_to = "Sexo",
      values_to = "Casos"
    ) %>%
    mutate(
      Delitos = factor(
        Delitos,
        levels = c(
          "Violación", "Sodomia", "Actos Lascivos", "Incesto", "Violación Técnica",
          "Ley 54 (3.5)", "Agresión Sexual", "Maltrato", "Pornografia infantil",
          "Hostigamiento Sexual", "Maltrato Institucional", "Trata Humana", "Agresión",
          "Maltrato a menores"
        ),
        ordered = TRUE
      ),
      Año = factor(Año),
      Sexo = factor(Sexo)
    ) %>%
    replace_na(list(Casos = 0)) %>%
    select(Año, Delitos, Sexo, Casos)
  
  return(df)
}

#### cleanSheet_inciDF ####
cleanSheet_inciDF <- function(file_path, años = c("2021", "2022", "2023")) {
  library(zoo)  # para as.yearmon
  
  # Función interna para leer y limpiar cada archivo
  leer_datos <- function(año) {
    data <- read_excel(paste0(file_path, "_", año, ".xlsx")) %>%
      rename_with(~ gsub(año, "", .), contains(año)) %>%
      rename_at(vars(2), ~ "Población") %>%
      mutate(Año = año)
    
    # Para el año 2023, también renombrar columnas con "2022"
    if (año == "2023") {
      data <- data %>%
        rename_with(~ gsub("2022", "", .), contains("2022"))
    }
    
    return(data)
  }
  
  # Leer todos los años y combinar
  df <- lapply(años, leer_datos) %>%
    bind_rows() %>%
    filter(`Áreas Policiacas` != "Total") %>%
    pivot_longer(
      cols = -c(`Áreas Policiacas`, Población, Año),
      names_to = "Mes",
      values_to = "Casos"
    ) %>%
    mutate(
      `Áreas Policiacas` = factor(str_trim(`Áreas Policiacas`)),
      Año = factor(Año),
      Meses = factor(Mes),
      Meses_Numéricos = match(Meses, Mes),
      Fecha = as.yearmon(paste(Año, Meses_Numéricos), "%Y %m")
    ) %>%
    replace_na(list(Casos = 0)) %>%
    select(-c(Meses_Numéricos, Mes, Meses)) %>%
    relocate(Año, `Áreas Policiacas`, Población, Casos)
  
  return(df)
}







#### Oficina de la Procuradora de la Mujer ####
#### cleansheet_opmFemiVD ####
cleansheet_opmFemiVD <- function(file){
  
  df <- read_excel(file) %>%
    mutate(
      Año = factor(Año),
      Tasa = round(`Tasa_IE`, 2)
    ) %>%
    rename(
      Asesinatos = `Asesinatos`
    ) %>%
    select(Año, Asesinatos, Tasa) %>%
    relocate(Año, Asesinatos, Tasa)
  
  return(df)
  
}

#### cleansheet_opmAgresores ####
cleansheet_opmAgresores <- function(file){
  
  df <- read_excel(file) %>%
    rename(
      Razón = tipo,
      Año = year,
      Cantidad = cantidad
    ) %>%
    mutate(
      Año = factor(Año),
      Razón = factor(
        Razón,
        levels = c(
          "Acecho",
          "Agresión sexual",
          "Discrimen de género",
          "Violencia doméstica",
          "Violencia en cita",
          "Trata Humana",
          "Otras"
        )
      )
    )
  
  return(df)
  
}


#### cleanSheet_OPM ####
cleanSheet_OPM <- function(df, hoja, niveles) {
  df %>%
    mutate(
      Año = factor(Año),
      `Razón para Consulta` = factor(`Razón para Consulta`, levels = niveles)
    ) %>%
    rename(Año = Año, Razón = `Razón para Consulta`, Cantidad = `Personas Atendidas`) %>%
    replace_na(list(Cantidad = 0)) %>%
    relocate(Año, Razón, Cantidad)
}


#### cleansheet_opmCasos ####
cleansheet_opmCasos <- function(file){
  
  niveles_completos <- c(
    "Acecho",
    "Agresión sexual",
    "Discrimen de género",
    "Violencia doméstica",
    "Violencia en cita",
    "Trata Humana",
    "Otras"
  )
  
  opm_anual_df <- read_excel(file, sheet = "anual")
  
  df_list <- list(
    cleanSheet_OPM(opm_anual_df, "anual", niveles_completos)
  )
  
  df <- reduce(df_list, full_join)
  
  return(df)
  
}
#### cleansheet_opmVic ####
cleansheet_opmVic <- function(file){
  
  df <- read_excel(file) %>%
    rename_at(
      vars(1,2,3,4,5,6,7,8),
      ~ c("género","2020","2021","2022","2023","2024","2025","2026")
    ) %>%
    pivot_longer(!género, names_to = "año", values_to = "víctimas") %>%
    mutate(
      género = factor(
        género,
        levels = c("Femenino","Masculino","Trans","No informó")
      ),
      año = factor(año)
    ) %>%
    rename(
      Año = año,
      Género = género,
      Víctimas = víctimas
    ) %>%
    replace_na(list(Víctimas = 0)) %>%
    relocate(Año, Género, Víctimas)
  
  return(df)
  
}
#### cleansheet_opmMedio ####
cleansheet_opmMedio <- function(file){
  
  df <- read_excel(file) %>%
    rename_at(
      vars(2,3,4,5,6,7,8),
      ~ c("2020","2021","2022","2023","2024","2025", "2026")
    ) %>%
    mutate(
      across(
        c("2020","2021","2022","2023","2024","2025", "2026"),
        as.numeric
      )
    ) %>%
    pivot_longer(
      !`Medio de orientación`,
      names_to = "año",
      values_to = "personas atendidas"
    ) %>%
    filter(`Medio de orientación` != "Total") %>%
    mutate(
      `Medio de orientación` = factor(`Medio de orientación`),
      año = factor(año)
    ) %>%
    rename(
      Año = año,
      Orientación = `Medio de orientación`,
      Cantidad = `personas atendidas`
    ) %>%
    replace_na(list(Cantidad = 0)) %>%
    relocate(Año, Orientación, Cantidad)
  
  return(df)
  
}
#### cleansheet_opmServiciosMes ####
cleansheet_opmServiciosMes <- function(file){
  
  df <- read_excel(file) %>%
    mutate(
      `Tipo de Servicio` = factor(`Tipo de Servicio`),
      Año = factor(Año)
    ) %>%
    rename(
      Servicio = `Tipo de Servicio`,
      Cantidad = `Servicios Ofrecidos`
    ) %>%
    replace_na(list(Cantidad = 0)) %>%
    relocate(Año, Servicio, Cantidad)
  
  return(df)
  
}








#### Departamento de Corrección y Rehabilitación ####
#### cleansheet_dcrCasosInv ####
cleansheet_dcrCasosInv <- function(file){
  
  df <- read_excel(file)
  
  df_clean <- df %>%
    group_by(tipo, year, sexo) %>%
    summarise(
      Cantidad = round(sum(cantidad, na.rm = TRUE) / 12),
      .groups = "drop"
    ) %>%
    mutate(
      year = factor(year),
      sexo = factor(sexo),
      tipo = factor(tipo)
    ) %>%
    rename(
      Año = year,
      Sexo = sexo,
      Estado = tipo
    ) %>%
    replace_na(list(Cantidad = 0)) %>%
    relocate(Año, Sexo, Estado, Cantidad)
  
  return(df_clean)
  
}

#### cleansheet_dcrCasosInv_supervision ####
cleansheet_dcrCasosInv_supervision <- function(file){
  
  df <- read_excel(file)
  
  df_clean <- df %>%
    filter(!grepl("Investigaciones", tipo, ignore.case = TRUE)) %>%
    group_by(tipo, year, sexo) %>%
    summarise(
      Cantidad = round(sum(cantidad, na.rm = TRUE) / 12),
      .groups = "drop"
    ) %>%
    mutate(
      year = factor(year),
      sexo = factor(sexo),
      tipo = factor(tipo)
    ) %>%
    rename(
      Año = year,
      Sexo = sexo,
      Estado = tipo
    ) %>%
    replace_na(list(Cantidad = 0)) %>%
    relocate(Año, Sexo, Estado, Cantidad)
  
  return(df_clean)
  
}

#### cleansheet_dcrAlternos ####
cleansheet_dcrAlternos <- function(file){
  
  df <- read_excel(file)
  
  df_clean <- df %>%
    mutate(
      Año = factor(Año),
      Sexo = factor(Sexo),
      Tipo = factor(Tipo)
    ) %>%
    replace_na(list(Promedio = 0)) %>%
    relocate(Año, Sexo, Tipo, Promedio)
  
  return(df_clean)
  
}

#### cleansheet_dcrPSAJ ####
cleansheet_dcrPSAJ <- function(file){
  
  df <- read_excel(file)
  
  df_clean <- df %>%
    mutate(
      Año = factor(Año),
      Sexo = factor(Sexo),
      Tipo = factor(Tipo)
    ) %>%
    replace_na(list(Promedio = 0)) %>%
    relocate(Año, Sexo, Tipo, Promedio)
  
  return(df_clean)
  
}


#### cleansheet_dcrSentenciadas ####
cleansheet_dcrSentenciadas <- function(file){
  
  meses_es <- c(
    "Enero","Febrero","Marzo","Abril","Mayo","Junio",
    "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"
  )
  
  df <- read_excel(file) %>%
    mutate(
      tipo = factor(tipo),
      year = factor(year)
    )
  
  df$fecha <- as.Date(paste0(df$year, "-", df$mes, "-01"))
  
  df_clean <- df %>%
    mutate(
      mes = factor(mes, levels = 1:12, labels = meses_es)
    ) %>%
    rename(
      Año = year,
      Mes = mes,
      Fecha = fecha,
      Estado = tipo,
      Cantidad = cantidad
    ) %>%
    replace_na(list(Cantidad = 0)) %>%
    select(
      Mes, Año, Fecha, Estado, Cantidad
    )
  
  return(df_clean)
  
}

#### cleansheet_dcrPEA_resumen ####
cleansheet_dcrPEA_resumen <- function(file){
  
  df <- read_excel(file, sheet = "resumen_activos")
  
  df_clean <- df %>%
    filter(Programa != "Total") %>%
    pivot_longer(                                # Convierte las columnas a formato largo
      !Programa,
      names_to = "Año", 
      values_to = "Casos"
    ) %>%
    mutate(
      Año = factor(Año),
      Programa = factor(Programa)
    ) %>%
    replace_na(list(Casos = 0)) %>%
    relocate(Año, Programa, Casos)
  
  return(df_clean)
  
}

#### cleansheet_dcrAAVSV####
cleansheet_dcrAAVSV <- function(file){
  
  df <- read_excel(file, sheet = "AAVSV")
  
  df_clean <- df %>%
    pivot_longer(
      cols = starts_with("2"),
      names_to = "Año",
      values_to = "Casos"
    ) %>%
    rename(
      Sexo = Género,
      Delito = `Tipo de delito`
    ) %>%
    mutate(
      Año = factor(Año),
      Sexo = factor(Sexo),
      Región = factor(Región),
      Delito = factor(Delito,
                      levels = c(
                        "Asesinato",
                        "Delitos graves con arma",
                        "Maltrato de menores",
                        "Ofensores Sexuales",
                        "Secuestro",
                        "Violencia de genero",
                        "Otros delitos"
                      ),
                      ordered = TRUE
      ),
    ) %>%
    replace_na(list(Casos = 0)) %>%
    relocate(Año, Delito, Región, Sexo, Casos)
  
  return(df_clean)
  
}

#### cleansheet_dcrCSVC ####
cleansheet_dcrCSVC <- function(file){
  
  df <- read_excel(file, sheet = "CSVC")
  
  df_clean <- df %>%
    pivot_longer(
      cols = starts_with("2"),
      names_to = "Año",
      values_to = "Casos"
    ) %>%
    rename(
      Sexo = Género,
      Delito = `Tipo de delito`
    ) %>%
    mutate(
      Año = factor(Año),
      Sexo = factor(Sexo),
      Región = factor(Región),
      Delito = factor(Delito,
                      levels = c(
                        "Asesinato",
                        "Delitos con armas",
                        "Maltrato de menores",
                        "Ofensores Sexuales",
                        "Violencia de Genero",
                        "Otros delitos"
                      ),
                      ordered = TRUE
      )
    )%>%
    replace_na(list(Casos = 0)) %>%
    relocate(Año, Delito, Región, Sexo, Casos)
  
  return(df_clean)
  
}














#### Administración de Tribunales ####
#### cleanSheet_casosCrimLey148 ####
cleanSheet_casosCrimLey148 <- function(file, sheets = c("2020-2021", "2021-2022", "2022-2023", "2023-2024", "2024-2025")) {
  
  df_list <- lapply(sheets, function(sheet_name) {
    
    read_excel(file, sheet = sheet_name) %>%
      rename(Delito = `Año fiscal/delitos`) %>%
      pivot_longer(!Delito, names_to = "Status", values_to = "Casos") %>%
      mutate(AñoFiscal = sheet_name) %>%
      filter(Delito != sheet_name)
    
  })
  
  df <- df_list %>%
    bind_rows() %>%
    mutate(
      AñoFiscal = recode(AñoFiscal,
                         "2020-2021" = "2021",
                         "2021-2022" = "2022",
                         "2022-2023" = "2023",
                         "2023-2024" = "2024",
                         "2024-2025" = "2025"),
      AñoFiscal = factor(AñoFiscal),
      Status = factor(Status),
      Delito = factor(Delito)
    ) %>%
    replace_na(list(Casos = 0)) %>%
    relocate(AñoFiscal, Status, Delito, Casos)
  
  return(df)
}



#### cleanSheet_OP_148_SoliGrupEdad ####
cleanSheet_OP_148_SoliGrupEdad <- function(file, 
                                           sheets = c("2020-2021", "2021-2022", "2022-2023", 
                                                      "2023-2024", "2024-2025", "2025-2026"),
                                           new_names = c("Total", "20 años o menos", "21-39 años",
                                                         "40-59 años","60 años o más", "No Indica")) {
  
  df_list <- lapply(sheets, function(sheet_name) {
    
    read_excel(file, sheet = sheet_name) %>%
      rename_at(vars(2:7), ~ new_names) %>%       # Renombra las columnas de la 2 a la 9
      pivot_longer(                                # Convierte las columnas a formato largo
        !Región, 
        names_to = "Edad", 
        values_to = "Solicitudes"
      ) %>%
      mutate(
        Edad = as.character(Edad),                # Convierte Edad a carácter para asegurar el filtrado
        AñoFiscal = sheet_name                     # Asigna el año fiscal desde sheet_name
      ) %>%
      filter(
        Edad != "Total",                           # Filtra filas con 'Total' en Edad
        Región != "Total"                          # Filtra filas con 'Total' en Región
      ) %>%
      mutate(
        Edad = factor(Edad, levels = unique(Edad)) # Convierte Edad de nuevo a factor con niveles únicos
      )
    
  })
  
  df <- df_list %>%
    bind_rows() %>%
    mutate(
      AñoFiscal = recode(AñoFiscal,
                         "2020-2021" = "2021",
                         "2021-2022" = "2022",
                         "2022-2023" = "2023",
                         "2023-2024" = "2024",
                         "2024-2025" = "2025",
                         "2025-2026" = "2026"),
      AñoFiscal = factor(AñoFiscal),
      Región = factor(Región)
    ) %>%
    replace_na(list(Solicitudes = 0)) %>%
    relocate(AñoFiscal, Región, Edad, Solicitudes)
  
  return(df)
}

#### cleanSheet_OP_Ley148_ex_parteEmitidas ####
cleanSheet_OP_Ley148_ex_parteEmitidas <- function(file, 
                                                  sheets = c("2020-2021", "2021-2022", "2022-2023", 
                                                             "2023-2024", "2024-2025"),
                                                  new_names = c("Total", "Agresión Sexual", "Acoso Sexual",
                                                                "Actos Lascivos", "Incesto") ) {
  
  df_list <- lapply(sheets, function(sheet_name) {
    
    read_excel(file, sheet = sheet_name) %>%
      rename_at(vars(2:6), ~ new_names) %>%       # Renombra las columnas de la 2 a la 6
      pivot_longer(                                # Convierte las columnas a formato largo
        !Región, 
        names_to = "Delito", 
        values_to = "ÓrdenesEmitidas"
      ) %>%
      mutate(
        Delito = as.character(Delito),             # Convierte Delito a carácter para asegurar el filtrado
        AñoFiscal = factor(sheet_name)             # Asigna el año fiscal desde sheet_name
      ) %>%
      filter(
        Delito != "Total",                         # Filtra filas con 'Total' en Delito
        Región != "Total"                          # Filtra filas con 'Total' en Región
      ) %>%
      mutate(
        Delito = factor(Delito, levels = unique(Delito)) # Convierte Delito de nuevo a factor con niveles únicos
      )
    
  })
  
  df <- df_list %>%
    bind_rows() %>%
    mutate(
      AñoFiscal = recode(AñoFiscal,
                         "2020-2021" = "2021",
                         "2021-2022" = "2022",
                         "2022-2023" = "2023",
                         "2023-2024" = "2024",
                         "2024-2025" = "2025"),
      Región = factor(Región),
      Delito = factor(Delito),
      AñoFiscal = factor(AñoFiscal)
    ) %>%
    filter(Región != "Total") %>%
    replace_na(list(ÓrdenesEmitidas = 0)) %>%
    relocate(AñoFiscal, Delito, Región, ÓrdenesEmitidas)
  
  return(df)
}


#### cleanSheet_OP_LEY148Archivadas ####
cleanSheet_OP_LEY148Archivadas <- function(file, 
                                           sheets = c("2020-2021", "2021-2022", "2022-2023",
                                                      "2023-2024", "2024-2025"),
                                           new_names = c("Total", "Solicitud Peticionaria",
                                                         "Otra Razón")) {
  
  df_list <- lapply(sheets, function(sheet_name) {
    
    read_excel(file, sheet = sheet_name) %>%
      rename_at(vars(2:4), ~ new_names) %>%
      pivot_longer(
        !Región, 
        names_to = "Razón", 
        values_to = "ÓrdenesArchivadas"
      ) %>%
      mutate(
        Región = as.character(Región), 
        AñoFiscal = factor(sheet_name)
      ) %>%
      filter(
        Razón != "Total",
        Región != "Total"
      ) %>%
      mutate(
        Región = factor(Región, levels = unique(Región)) # Convierte Región de nuevo a factor con niveles únicos
      )
    
  })
  
  df <- df_list %>%
    bind_rows() %>%
    mutate(
      Razón = factor(Razón, levels = c("Solicitud Peticionaria", "Otra Razón")),
      Región = factor(Región),
      AñoFiscal = factor(case_when(
        AñoFiscal == "2020-2021" ~ "2021",
        AñoFiscal == "2021-2022" ~ "2022",
        AñoFiscal == "2022-2023" ~ "2023",
        AñoFiscal == "2023-2024" ~ "2024",
        AñoFiscal == "2024-2025" ~ "2025",
        TRUE ~ as.character(AñoFiscal)
      ))
    ) %>%
    filter(Región != "Total") %>%
    replace_na(list(ÓrdenesArchivadas = 0)) %>%
    relocate(AñoFiscal, Razón, Región, ÓrdenesArchivadas)
  return(df)
}


#### cleanSheet_OP_LEY148Denegadas ####
cleanSheet_OP_LEY148Denegadas <- function(file, 
                                          sheets = c("2020-2021", "2021-2022", "2022-2023", 
                                                     "2023-2024", "2024-2025"),
                                          new_names = c("No Aplican Disposiciones Ley148", 
                                                        "No ha lugar archivo de petición",
                                                        "No Prueban Elementos",
                                                        "Otros")) {
  
  df_list <- lapply(sheets, function(sheet_name) {
    read_excel(file, sheet = sheet_name) %>%
      rename_at(vars(3:6), ~ new_names) %>%
      pivot_longer(
        !Región, 
        names_to = "Razón", 
        values_to = "ÓrdenesDenegadas"
      ) %>%
      mutate(
        AñoFiscal = factor(sheet_name)
      ) %>%
      filter(
        Región != "Total",
        Razón != "Total"
      )
    
  })
  
  df <- df_list %>%
    bind_rows() %>%
    mutate(
      Región = factor(Región),
      Razón = factor(Razón),
      AñoFiscal = factor(case_when(
        AñoFiscal == "2020-2021" ~ "2021",
        AñoFiscal == "2021-2022" ~ "2022",
        AñoFiscal == "2022-2023" ~ "2023",
        AñoFiscal == "2023-2024" ~ "2024",
        AñoFiscal == "2024-2025" ~ "2025",
        TRUE ~ as.character(AñoFiscal)
      )),
      ÓrdenesDenegadas = replace_na(ÓrdenesDenegadas, 0)
    ) %>%
    relocate(AñoFiscal, Razón, Región, ÓrdenesDenegadas)
  
  return(df)
}


#### cleanSheet_OP_LEY148FinalEmitidas ####
cleanSheet_OP_LEY148FinalEmitidas <- function(file, 
                                              sheets = c("2020-2021", "2021-2022", "2022-2023",
                                                         "2023-2024", "2024-2025"),
                                              new_names = c("Total", "Agresión Sexual", "Acoso Sexual",
                                                            "Actos Lascivos", "Incesto")) {
  
  df_list <- lapply(sheets, function(sheet_name) {
    read_excel(file, sheet = sheet_name) %>%
      rename_at(vars(2:6), ~ new_names) %>%
      pivot_longer(
        !Región, 
        names_to = "Delito", 
        values_to = "ÓrdenesEmitidas"
      ) %>%
      mutate(
        AñoFiscal = factor(sheet_name)
      ) %>%
      filter(
        Región != "Total",
        Delito != "Total"
      )
    
  })
  
  df <- df_list %>%
    bind_rows() %>%
    mutate(
      Región = factor(Región),
      Delito = factor(Delito),
      AñoFiscal = factor(case_when(
        AñoFiscal == "2020-2021" ~ "2021",
        AñoFiscal == "2021-2022" ~ "2022",
        AñoFiscal == "2022-2023" ~ "2023",
        AñoFiscal == "2023-2024" ~ "2024",
        AñoFiscal == "2024-2025" ~ "2025",
        TRUE ~ as.character(AñoFiscal)
      )),
      ÓrdenesEmitidas = replace_na(ÓrdenesEmitidas, 0)
    ) %>%
    relocate(AñoFiscal, Delito, Región, ÓrdenesEmitidas)
  
  return(df)
}

#### cleanSheet_OP_LEY148Genero ####
cleanSheet_OP_LEY148Genero <- function(file, sheets = c("2020-2021", "2021-2022", 
                                                        "2022-2023", "2023-2024")) {
  
  df_list <- lapply(sheets, function(sheet_name) {
    
    read_excel(file, sheet = sheet_name) %>%
      pivot_longer(
        !Sexo, 
        names_to = "Parte", 
        values_to = "Solicitudes"
      ) %>%
      mutate(
        AñoFiscal = factor(sheet_name)
      ) %>%
      filter(
        Sexo != "Total"
      )
    
  })
  
  df <- df_list %>%
    bind_rows() %>%
    mutate(
      Sexo = factor(Sexo),
      Parte = factor(Parte),
      AñoFiscal = factor(case_when(
        AñoFiscal == "2020-2021" ~ "2021",
        AñoFiscal == "2021-2022" ~ "2022",
        AñoFiscal == "2022-2023" ~ "2023",
        AñoFiscal == "2023-2024" ~ "2024",
        TRUE ~ as.character(AñoFiscal)
      ))
    ) %>%
    replace_na(list(Solicitudes = 0)) %>%
    relocate(AñoFiscal, Parte, Sexo, Solicitudes)
  
  return(df)
}


#### cleanSheet_tribCasosCrim ####
cleanSheet_tribCasosCrim <- function(file, 
                                     sheets = c("2019-2020", "2020-2021", "2021-2022", 
                                                "2022-2023", "2023-2024", "2024-2025", "2025-2026"),
                                     new_names = c("Delito", "Pendiente Inicio", "Presentados", 
                                                   "A Resolver", "Condenas", "Absoluciones", 
                                                   "Archivos", "Traslados", "Otros", "Total", "Pendiente Final")) {
  
  df_list <- lapply(sheets, function(sheet_name) {
    read_excel(file, sheet = sheet_name) %>%
      rename_at(vars(1:11), ~ new_names) %>%      # Renombra las primeras 11 columnas
      pivot_longer(                                # Convierte las columnas a formato largo
        !Delito, 
        names_to = "Casos", 
        values_to = "Cantidad"
      ) %>%
      mutate(
        AñoFiscal = factor(sheet_name)             # Añade el año fiscal como factor
      ) %>%
      filter(
        !(Casos %in% c("Total"))                   # Filtra las filas con 'Total' en Casos
      )
    
  })
  
  df <- df_list %>%
    bind_rows() %>%
    mutate(
      Delito = factor(Delito),
      Casos = factor(Casos, levels = c("A Resolver", "Absoluciones", "Archivos", "Condenas", 
                                       "Pendiente Inicio", "Pendiente Final", "Presentados",
                                       "Traslados", "Otros"), ordered = TRUE),
      AñoFiscal = recode(AñoFiscal,
                         "2019-2020" = "2020",
                         "2020-2021" = "2021",
                         "2021-2022" = "2022",
                         "2022-2023" = "2023",
                         "2023-2024" = "2024",
                         "2024-2025" = "2025",
                         "2025-2026" = "2026"),
      AñoFiscal = factor(AñoFiscal),
    ) %>%
    replace_na(list(Cantidad = 0)) %>%
    relocate(AñoFiscal, Delito, Casos, Cantidad)
  
  return(df)
}






#### Centro de Ayuda a Victimas de Violencia ####
#### cleansheet_safekitsDF ####
cleansheet_safekitsDF <- function(file){
  
  df <- read_excel(file, sheet = "Data")
  
  df_clean <- df %>%
    mutate(
      `Total de Kits` = `Total con querella` + `Total sin querella`
    ) %>%
    pivot_longer(
      !Año,
      names_to = "Kits",
      values_to = "Total"
    ) %>%
    filter(
      Kits != "Total de Kits"
    ) %>% 
    mutate(
      Kits = factor(Kits, 
                    levels = c("Total con querella", "Total sin querella")),
      Año = factor(Año)
    )
  
  return(df_clean)
  
}

#### cleansheet_safekitsDF_edades ####
cleansheet_safekitsDF_edades <- function(file){
  
  df <- read_excel(file, sheet = "Edades")
  
  df_clean <- df %>%
    mutate(
      `Total de Kits con querella` = `Cantidad de menores` + `Cantidad de mayores` + `No identificado`
    ) %>%
    pivot_longer(
      !Año,
      names_to = "Categoria",
      values_to = "Total"
    ) %>%
    filter(
      Categoria != "Total de Kits con querella"
    ) %>%
    mutate(
      Categoria = case_when(
        Categoria == "Cantidad de menores" ~ "Menores de edad",
        Categoria == "Cantidad de mayores" ~ "Mayores de edad",
        Categoria == "No identificado" ~ "No identificado",
        TRUE ~ Categoria # Opción para manejar cualquier valor inesperado
      ),
      Categoria = factor(Categoria, 
                         levels = c("Menores de edad", "Mayores de edad", "No identificado")),
      Año = factor(Año))
  
  return(df_clean)
  
}

#### cleansheet_safekitsDF_edades ####
cleansheet_safekitsDF_edades <- function(file){
  
  df <- read_excel(file, sheet = "Edades")
  
  df_clean <- df %>%
    mutate(
      `Total de Kits con querella` = `Cantidad de menores` + `Cantidad de mayores` + `No identificado`
    ) %>%
    pivot_longer(
      !Año,
      names_to = "Categoria",
      values_to = "Total"
    ) %>%
    filter(
      Categoria != "Total de Kits con querella"
    ) %>%
    mutate(
      Categoria = case_when(
        Categoria == "Cantidad de menores" ~ "Menores de edad",
        Categoria == "Cantidad de mayores" ~ "Mayores de edad",
        Categoria == "No identificado" ~ "No identificado",
        TRUE ~ Categoria # Opción para manejar cualquier valor inesperado
      ),
      Categoria = factor(Categoria, 
                         levels = c("Menores de edad", "Mayores de edad", "No identificado")),
      Año = factor(Año))
  
  return(df_clean)
  
}

#### cleansheet_safekitsDF_analizados ####
cleansheet_safekitsDF_analizados <- function(file){
  
  df <- read_excel(file, sheet = "Kit Analizados")
  
  df_clean <- df %>%
    pivot_longer(
      !Año,
      names_to = "Laboratorio",
      values_to = "Total"
    ) %>%
    mutate(
      Laboratorio = factor(Laboratorio, 
                           levels = c("Laboratorio Forense", 
                                      "Laboratorios Externos: BODE",
                                      "Laboratorios Externos: DNA Solutions")),
      Año = factor(Año)
    )
  
  return(df_clean)
  
}

#### cleansheet_safekitsDF_analizados ####
cleansheet_safekitsDF_analizados <- function(file){
  
  df <- read_excel(file, sheet = "Kit Analizados")
  
  df_clean <- df %>%
    pivot_longer(
      !Año,
      names_to = "Laboratorio",
      values_to = "Total"
    ) %>%
    mutate(
      Laboratorio = factor(Laboratorio, 
                           levels = c("Laboratorio Forense", 
                                      "Laboratorios Externos: BODE",
                                      "Laboratorios Externos: DNA Solutions")),
      Año = factor(Año)
    )
  
  return(df_clean)
  
}

#### cleansheet_safekits_region ####
cleansheet_safekits_region <- function(file){
  
  df <- read_excel(file, sheet = "Región Policiaca")
  
  df_clean <- df %>%
    pivot_longer(
      cols = 2:7,
      names_to = "Año",
      values_to = "Cantidad"
    ) %>%
    filter(
      `Región Policiaca` != "Total"
    ) %>%
    rename(Región = `Región Policiaca`) %>%
    mutate(
      Región = factor(Región),
      Año = factor(Año)
    )
  
  return(df_clean)
  
}




###############################
#### Helper Functions: UI  ####
###############################
#### createDropdownCheckbox / createDropdownCheckbox_añoFiscal ####
# Estas funciones generan selectores de checkboxes dentro de un botón desplegable.
createDropdownCheckbox_añoFiscal <- function(label, choices, selected, id) {
  unique_years <- unique(as.character(choices))
  formatted_choices <- setNames(unique_years, paste0("jul ", as.numeric(unique_years) - 1, " - jun ", unique_years))
  if (is.null(selected)) selected <- unique_years
  buttonLabel <- if (length(selected) > 1) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
  div(class = "dropdown-checkbox-container",
      dropdownButton(circle = FALSE, label = lowercaseTitle(label), status = "default", size = "default",
                     checkboxGroupInput(paste0("checkGroup_", id), label = "", choices = formatted_choices, selected = selected),
                     actionButton(paste0("deselectAll_", id), buttonLabel, class = "btn-toggle-select")
      )
  )
}

createDropdownCheckbox <- function(label, choices, selected, id) {
  choices <- levels(choices)
  if (is.null(selected)) selected <- choices else selected <- choices[selected]
  buttonLabel <- if (length(selected) > 1) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
  div(class = "dropdown-checkbox-container",
      dropdownButton(circle = FALSE, label = lowercaseTitle(label), status = "default", size = "default",
                     checkboxGroupInput(paste0("checkGroup_", id), label = "", choices = choices, selected = selected),
                     actionButton(paste0("deselectAll_", id), buttonLabel, class = "btn-toggle-select")
      )
  )
}

createDropdownCheckbox_region <- function(label, choices, selected = NULL, id) {
  choices <- levels(choices)
  if (is.null(selected)) selected <- character(0) else selected <- choices[selected]
  buttonLabel <- if (length(selected) > 1) HTML("Deseleccionar<br>todo") else HTML("Seleccionar<br>todo")
  div(class = "dropdown-checkbox-container",
      dropdownButton(circle = FALSE, label = lowercaseTitle(label), status = "default", size = "default",
                     checkboxGroupInput(paste0("checkGroup_", id), label = "", choices = choices, selected = selected),
                     actionButton(paste0("deselectAll_", id), buttonLabel, class = "btn-toggle-select")
      )
  )
}

#### embedImage ####
# Crea un elemento de lista con una imagen que enlaza a una página web especificada.
embedImage <- function(ID, img_src, link_href, link_alt, size = "60", padding_bottom = "0px") {
  tags$li(class = "embed-image-item",
          div(id = ID,
              tags$a(href = link_href,
                     tags$figure(img(src = img_src, height = size, alt = link_alt, deleteFile = FALSE, class = "embed-image"))
              )
          )
  )
}

#### showDataCheckbox ####
showDataCheckbox <- function(inputId, label = lowercaseTitle_mostrarDatos("Mostrar Datos"), value = FALSE) {
  div(class = "show-data-wrapper",
      div(class = "custom-checkbox",
          checkboxInput(inputId, tags$b(label), value = value)
      )
  )
}

#### sectionTitle ####
sectionTitle <- function(title, font_size = "20px") {
  tags$div(class = "section-title", tags$b(title))
}

#### customSeparator ####
customSeparator <- function() {
  tags$div(class = "custom-separator", "")
}

#### lowercaseTitle ####
lowercaseTitle <- function(title, font_size = "15px") {
  title_class <- if (identical(font_size, "11px")) "lowercase-title-small" else "lowercase-title"
  tags$span(class = title_class, title)
}

#### lowercaseTitle_mostrarDatos ####
lowercaseTitle_mostrarDatos <- function(title, font_size = "11px") {
  tags$span(class = "lowercase-title-show-data", title)
}

#### authorTag ####
# Función para crear sección de autores
authorTag <- function(nombre, email, puesto, grados){
  tags$html(
    h2(nombre),
    tags$b('Email'),
    p(email),
    tags$b('Puesto'),
    p(puesto),
    tags$b('Grados Académicos'),
    lapply(grados, p),
    tags$hr()
  )
}



#### createPickerFilter ####
createPickerFilter <- function(label, choices, selected = NULL, id) {
  # Limpia las opciones: quita duplicados y las ordena de menor a mayor.
  choices <- sort(unique(choices))
  
  # Si no se pasa 'selected', se seleccionan todas las opciones por defecto.
  if (is.null(selected)) {
    selected <- choices # Por defecto: todos los elementos seleccionado.
  }
  
  # El ID no es 'id' sino "checkGroup_" + id.
  # Ejemplo: id "femiDF_total_Año"  ->  input real = "checkGroup_femiDF_total_Año".
  # Para leerlo en el server usas input$checkGroup_femiDF_total_Año, no input$femiDF_total_Año.
  pickerInput(
    inputId  = paste0("checkGroup_", id), # ID para poder apuntarle desde CSS/JS.
    label    = label,
    choices  = choices,
    selected = selected,
    multiple = TRUE,                      # Permite escoger varias opciones a la vez.
    options = pickerOptions(
      actionsBox         = TRUE,                    # Mestra botones "Seleccionar/Deseleccionar todo"
      container          = "body", 
      selectedTextFormat = "count > 2",             # Con 3+ opciones marcadas, muestra un conteo.
      countSelectedText   = "{0} seleccionados",    #
      noneSelectedText   = "Ninguno seleccionado",
      selectAllText      = "Seleccionar todo",
      deselectAllText    = "Deseleccionar todo"
    )
  )
}

#### createPickerFilter_region ####

#### createPickerFilter_region ####
createPickerFilter_region <- function(label, choices, selected = NULL, id) {
  
  # Limpia las opciones: quita duplicados y las ordena.
  choices <- if (is.factor(choices)) {
    levels(choices)
  } else {
    sort(unique(choices))
  }
  
  # Si selected es NULL, no selecciona ninguna región por defecto.
  if (is.null(selected)) {
    selected <- character(0)
  }
  
  # Crea el selector con la misma apariencia que createPickerFilter.
  pickerInput(
    inputId = paste0("checkGroup_", id),
    label = label,
    choices = choices,
    selected = selected,
    multiple = TRUE,
    
    options = pickerOptions(
      actionsBox = TRUE,
      container = "body",
      selectedTextFormat = "count > 2",
      countSelectedText = "{0} seleccionados",
      noneSelectedText = "Ninguno seleccionado",
      selectAllText = "Seleccionar todo",
      deselectAllText = "Deseleccionar todo"
    )
  )
}


#### createPickerFilter_añoFiscal ####
createPickerFilter_añoFiscal <- function(label, choices, selected = NULL, id) {
  # PREPARAR AÑOS
  
  unique_years <- sort(unique(as.character(choices)))
  
  # Crear etiquetas de año fiscal
  formatted_choices <- setNames(
    unique_years,
    paste0(
      "jul ",
      as.numeric(unique_years) - 1,
      " - jun ",
      unique_years
    )
  )
  
  # SELECCIÓN POR DEFECTO
  
  if (is.null(selected)) {
    selected <- unique_years
  }
  
  # PICKER
  
  pickerInput(
    inputId = paste0("checkGroup_", id),
    label = lowercaseTitle(label),
    choices = formatted_choices,
    selected = selected,
    multiple = TRUE,
    
    options = pickerOptions(
      actionsBox = TRUE,
      container = "body",
      selectedTextFormat = "count > 2",
      countSelectedText = "{0} seleccionados",
      noneSelectedText = "Ninguno seleccionado",
      selectAllText = "Seleccionar todo",
      deselectAllText = "Deseleccionar todo"
    )
  )
}



#### showDataToggle ####
# Crea un switch tipo (ON/OFF) para enseñar u ocultar la tabla de datos.
showDataToggle <- function(inputId, label = "Mostrar Datos", value = FALSE) {
  # value = FALSE, ya que, el switch arranca apagaso (la tabla empieza oculta).
  div(class = "custom-checkbox-wrapper",
      materialSwitch(
        inputId = inputId,                                           # ID que el server lee como input$<inputId>.
        label   = tags$b(lowercaseTitle(label, font_size = "11px")), # Etiqueta en negrita y 11px de grosor.
        value   =  value,                                            # Estado inicial (TRUE = encendido, FALSE = apagado).
        status  = "primary",                                         # Color del switch (tema Bootstrap).
        right   = TRUE                                               # Pone la etiqueta a la derecha del switch.
      )
  )
}

#### createRadioSelector ####
# createRadioSelector(): Selector segmentado tipo "botones" para escoger UNA opción (2+ valores).
createRadioSelector <- function(id, label, choices, selected = NULL, 
                                status = "combo-toggle", # Clase de estilo de los botones.
                                justified = TRUE,        # TRUE = los botones llenan todo el ancho.
                                size = "normal") {
  radioGroupButtons(
    inputId   = id,       # Aquí el ID sí se usa tal cual (input$<id> en el server).
    label     = label,
    choices   = choices,
    selected  = selected, # NULL = normalmente marca la primera opción.
    status    = status,
    size      = size,
    justified = justified
  )
}



##################################
#### Helper Functions: Server #### 
##################################

#### updateCheckboxGroup ####

# Esta función toma como entrada el `session`, el `inputId` correspondiente al grupo
# de checkbox, el objeto `input` y el conjunto de datos `data`. Dependiendo de si el
# grupo de checkbox está vacío o no, la función llena o vacía el grupo respectivamente.

updateCheckboxGroup <- function(session, inputId, input, data) {
  selected <- input[[inputId]]
  all_choices <- levels(data)
  
  is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
  
  if (is_all_selected) {
    # Deseleccionar todos
    updateCheckboxGroupInput(
      session,
      inputId,
      selected = character(0)
    )
    
  } else {
    # Seleccionar todos
    updateCheckboxGroupInput(
      session,
      inputId,
      choices = all_choices,
      selected = all_choices
    )
  }
}

updateCheckboxGroup_trib <- function(session, inputId, input, data) {
  selected <- input[[inputId]]
  all_choices <- levels(data)
  
  # Formatear las etiquetas con "jul XXXX - jun XXXX"
  unique_years <- unique(as.character(all_choices))
  formatted_choices <- setNames(
    unique_years,
    paste0("jul ", as.numeric(unique_years) - 1, " - jun ", unique_years)
  )
  
  is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
  
  if (is_all_selected) {
    # Deseleccionar todos pero conservar etiquetas formateadas
    updateCheckboxGroupInput(
      session,
      inputId = inputId,
      choices = formatted_choices,
      selected = character(0)
    )
  } else {
    # Seleccionar todos con etiquetas formateadas
    updateCheckboxGroupInput(
      session,
      inputId = inputId,
      choices = formatted_choices,
      selected = all_choices
    )
  }
}



# updateCheckboxGroup_trib <- function(session, inputId, input, data) {
#   selected <- input[[inputId]]
#   all_choices <- levels(data)
#   
#   is_all_selected <- !is.null(selected) && setequal(selected, all_choices)
#   
#   if (is_all_selected) {
#     # Deseleccionar todos
#     updateCheckboxGroupInput(
#       session,
#       inputId,
#       selected = character(0)
#     )
#     
#   } else {
#     # Seleccionar todos
#     updateCheckboxGroupInput(
#       session,
#       inputId,
#       choices = all_choices,
#       selected = all_choices
#     )
#   }
# }

#### renderLinePlot ####
renderLinePlot <- function(data, x, y, group, color, title, xlab, ylab, colorlab = color, emptyMessage) {
  data_df <- data()  # Evaluar los datos reactivos una vez
  
  if (is.null(data_df) || nrow(data_df) == 0 || is.null(data_df[[x]])) {
    # Mostrar una gráfica vacía con un mensaje si no hay datos
    p <- create_empty_plot_with_message_forLine(data, x, y, fill, title, xlab, ylab, emptyMessage)
    return(p)
  } else {
    # Filtrar los niveles del factor Año para que solo incluya cada 5 años
    x_levels <- levels(data_df[[x]])
    x_labels <- ifelse(as.numeric(x_levels) %% 5 == 0, x_levels, "")  # Mostrar etiquetas solo cada 5 años
    
    upper_y_limit <- ceiling(max(eval(parse(text = paste0("data()$", y))), na.rm = TRUE) * 1.1)
    
    p <- ggplot(data_df, aes_string(x = x, y = y, group = group, color = color)) +
      geom_line(color = "#2f2e7d", size = 1) +
      geom_point(color = "#adcc4e", size = 2,
                 aes(
                   text =
                     paste0("<b>", xlab, ":</b> ", .data[[x]],
                            "<br><b>", ylab, ":</b> ", .data[[y]]
                     )
                 )
      ) +
      expand_limits(y = 0) +  # Asegurar que el eje y comience en 0
      labs(title = title, x = xlab, y = ylab, color = colorlab) +
      scale_y_continuous(labels = function(x) scales::comma_format(big.mark = ",", decimal.mark = ".")(x) %>% paste0(" "),
                         expand = expansion(mult = c(0, 0.1))) +
      coord_cartesian(ylim = c(0, upper_y_limit)) +
      scale_x_discrete(breaks = x_labels) +  # Usar etiquetas filtradas para el eje x
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 15, colour = "black", face = "bold"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.margin = margin(t = 45, r = 10, b = 10, l = 10)
      )
    
    return(p)
  }
}

#### renderLinePlot_poli ####
renderLinePlot_poli <- function(data, x, y, group, color, title, xlab, ylab, 
                                colorlab = color, emptyMessage) {
  data_df <- data()  # Evaluar los datos reactivos una vez
  
  if (is.null(data_df) || nrow(data_df) == 0 || is.null(data_df[[x]])) {
    # Mostrar una gráfica vacía con un mensaje si no hay datos
    p <- create_empty_plot_with_message_forLine(data, x, y, color, title, xlab, ylab, emptyMessage)
    return(p)
  } else {
    # Filtrar los niveles del factor Año para que solo incluya cada 5 años
    x_levels <- levels(factor(data_df[[x]]))
    x_labels <- ifelse(as.numeric(x_levels) %% 5 == 0, x_levels, "")  
    
    upper_y_limit <- ceiling(max(data_df[[y]], na.rm = TRUE) * 1.1)
    
    p <- ggplot(data_df, aes_string(x = x, y = y, group = group, color = color)) +
      geom_line(size = 1) +
      geom_point(size = 2,
                 aes(
                   text = paste0("<b>", xlab, ":</b> ", .data[[x]],
                                 "<br><b>", ylab, ":</b> ", .data[[y]])
                 )
      ) +
      expand_limits(y = 0) +
      labs(title = title, x = xlab, y = ylab, color = colorlab) +
      scale_y_continuous(
        labels = function(x) scales::comma_format(big.mark = ",", decimal.mark = ".")(x) %>% paste0(" "),
        expand = expansion(mult = c(0, 0.1))
      ) +
      coord_cartesian(ylim = c(0, upper_y_limit)) +
      scale_x_discrete(breaks = x_labels) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 15, colour = "black", face = "bold"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.margin = margin(t = 45, r = 10, b = 10, l = 10)
      )
    
    return(p)
  }
}


#### create_empty_plot_with_message ####
create_empty_plot_with_message <- function(data, x, y, fill, xlab, ylab, emptyMessage) {
  data_df <- data()
  
  # Crear el gráfico base
  base_plot <- ggplot(data_df, aes_string(x = x, y = y)) +
    geom_blank() +
    labs(x = xlab, y = ylab) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_text(size = 12, margin = margin(t = 10)),
      axis.title.y = element_text(size = 12, margin = margin(r = 10)),
      plot.title = element_text(hjust = 0.5, size = 13, colour = "black", face = "bold"),
      panel.border = element_rect(colour = "black", fill = NA, size = 1),
      plot.margin = margin(t = 100, r = 50, b = 100, l = 50)
    )
  
  # Obtener las dimensiones de la trama
  plot_data <- ggplot_build(base_plot)
  panel_width <- diff(plot_data$layout$panel_params[[1]]$x.range)
  panel_height <- diff(plot_data$layout$panel_params[[1]]$y.range)
  
  # Calcular el tamaño del texto en función de las dimensiones del panel
  text_size <- min(panel_width, panel_height) * 1.75
  
  # Agregar el texto dinámico al gráfico
  base_plot +
    annotate(
      "text", x = 0.5, y = 0.5, label = emptyMessage,
      size = text_size, hjust = 0.5, vjust = 0.5
    )
}

#### renderBarPlot ####
#Renderiza un gráfico de barras utilizando ggplot2 en el UI de Shiny.
# renderBarPlot <- function(data, x, y, fill, title, xlab, ylab, fillLab = fill, colorFill = "Set1",
#                           emptyMessage, barWidth = 1, xGap = 0.1) {
# 
#   data_df <- data()  # Evaluate the reactive data once
# 
#   if (is.null(data_df) || nrow(data_df) == 0 || is.null(data_df[[x]]) || is.null(data_df[[y]]) || is.null(data_df[[fill]])) {
#     # If there are no data or x/y variables are null, display an empty plot with axes and a message
#     p <- create_empty_plot_with_message(data, x, y, fill, title, xlab, ylab,emptyMessage)
# 
#     return(p)
#   } else {
# 
#     upper_y_limit <- ceiling(max(eval(parse(text = paste0("data()$", y))), na.rm = TRUE) * 1.2)
# 
#     p <- ggplot(data_df, aes_string(x = x, y = y, fill = fill)) +
#       geom_bar(stat = "identity",
#                position = position_dodge2(width = barWidth, padding = xGap),
#                width = 0.7,
#                aes(
#                  text = paste(
#                    paste0("<b>", ylab, ":</b> ", after_stat(y)), "<br>",
#                    paste0("<b>", fillLab, ":</b> ", after_stat(fill)), "<br>"
#                  )
#                )) +
#       scale_fill_manual(values = colorFill) +
#       scale_y_continuous(labels = function(x) scales::comma_format(big.mark = ",", decimal.mark = ".")(x) %>% paste0(" "),
#                          expand = expansion(mult = c(0, 0.1))) +
#       coord_cartesian(ylim = c(0, upper_y_limit)) +
#       labs(title = title, x = xlab, y = ylab, fill = fillLab) +
#       theme_minimal() +
#       theme(
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.title = element_text(hjust = 0.5, size = 13, colour = "black", face = "bold"),
#         panel.border = element_rect(colour = "black", fill = NA, size = 1),
#         plot.margin = margin(t = 150, r = 50, b = 100, l = 50)
#       )
# 
#     return(p)
#   }
# }

#### renderBarPlot_facets ####
#Funcion para las graficas con FACETAS
#Renderiza un gráfico de barras utilizando ggplot2 en el UI de Shiny.
renderBarPlot_facets <- function(data, x, y, fill, xlab, ylab, fillLab = fill, colorFill = "Set1",
                                 emptyMessage, barWidth = 1, xGap = 0.1) {
  
  data_df <- data()  # Evaluate the reactive data once
  
  if (is.null(data_df) || nrow(data_df) == 0 || is.null(data_df[[x]]) || is.null(data_df[[y]]) || is.null(data_df[[fill]])) {
    # If there are no data or x/y variables are null, display an empty plot with axes and a message
    p <- create_empty_plot_with_message(data, x, y, fill, title, xlab, ylab,emptyMessage)
    
    return(p)
  } else {
    
    upper_y_limit <- ceiling(max(eval(parse(text = paste0("data()$", y))), na.rm = TRUE) * 1.5)
    
    if (upper_y_limit < 20) {
      
      y_scale <- scale_y_continuous(
        breaks = seq(0, upper_y_limit, by = 2),
        # labels = scales::label_number(accuracy = 1),
        labels = function(x)
          scales::comma_format(big.mark = ",", decimal.mark = ".")(x) %>% paste0(" "),
        expand = expansion(mult = c(0, 0.1))
      )
      
    } else {
      
      y_scale <- scale_y_continuous(
        labels = function(x)
          scales::comma_format(big.mark = ",", decimal.mark = ".")(x) %>% paste0(" "),
        expand = expansion(mult = c(0, 0.1))
      )
      
    }
    
    
    p <- ggplot(data_df, aes_string(x = x, y = y, fill = fill)) +
      geom_bar(stat = "identity",
               position = position_dodge2(width = barWidth, padding = xGap),
               width = 0.7,
               aes(
                 text = paste(
                   # paste0("<b>", ylab, ":</b> ", after_stat(y)), "<br>",
                   paste0("<b>", ylab, ":</b> ", scales::comma(after_stat(y), big.mark = ","),"<br>"),
                   paste0("<b>", fillLab, ":</b> ", after_stat(fill)), "<br>"
                 )
               )) +
      scale_fill_manual(values = colorFill) +
      y_scale +
      # scale_y_continuous(labels = function(x) scales::comma_format(big.mark = ",", decimal.mark = ".")(x) %>% paste0(" "),
      #                    expand = expansion(mult = c(0, 0.1))) +
      coord_cartesian(ylim = c(0, upper_y_limit)) +
      labs(x = xlab, y = ylab, fill = fillLab) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.margin = margin(t = 105, r = 50, b = 100, l = 50),
        panel.grid.major.x = element_blank() # Elimina el grid line por defacto en el eje de X
      ) 
    if (length(unique(data_df[[x]])) > 1) {
      p <- p + geom_vline(xintercept = seq(1.5, length(unique(data_df[[x]])) - 0.5, by = 1), #Grid line en x
                          linetype = "dotted", color = "grey", size = 0.3, alpha = 0.5) #Visualización del grid line
    }
    
    return(p)
  }
}



#### renderBarPlot_stack ####
# Renderiza un gráfico de barras apiladas 
renderBarPlot_stack <- function(data, x, y, fill, title, xlab, ylab, fillLab = fill, colorFill = "Set1",
                                emptyMessage, barWidth = 0.4, xGap = 0.1) {  
  
  data_df <- data()  # Evalúa los datos reactivos una vez
  
  if (is.null(data_df) || nrow(data_df) == 0 || is.null(data_df[[x]]) || is.null(data_df[[y]]) || is.null(data_df[[fill]])) {
    # Si no hay datos o las variables x/y son nulas, muestra un gráfico vacío con un mensaje
    p <- create_empty_plot_with_message(data, x, y, fill, title, xlab, ylab, emptyMessage)
    
    return(p)
  } else {
    # Eliminar el filtro de categorías específicas
    apiladas_df <- data_df  # No se filtran categorías
    
    upper_y_limit <- ceiling(max(eval(parse(text = paste0("data()$", y))), na.rm = TRUE) * 1.9)
    
    p <- ggplot(apiladas_df, aes_string(x = x, y = y, fill = fill)) +
      geom_bar(stat = "identity",
               position = position_stack(reverse = TRUE),  # Invierte el apilado
               width = barWidth,  
               aes(
                 text = paste(
                   paste0("<b>", ylab, ":</b> ", after_stat(y)), "<br>",
                   paste0("<b>", fillLab, ":</b> ", after_stat(fill)), "<br>"
                 )
               )) +
      scale_fill_manual(values = colorFill) +
      scale_y_continuous(labels = function(x) scales::comma_format(big.mark = ",", decimal.mark = ".")(x) %>% paste0(" "),
                         expand = expansion(mult = c(0, 0.1))) +
      coord_cartesian(ylim = c(0, upper_y_limit)) +
      labs(title = title, x = xlab, y = ylab, fill = fillLab) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 13, colour = "black", face = "bold"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.margin = margin(t = 100, r = 50, b = 100, l = 50),
        panel.grid.major.x = element_blank() # Elimina el grid line por defecto en el eje de X
      ) 
    
    if (length(unique(data_df[[x]])) > 1) {
      p <- p + geom_vline(xintercept = seq(1.5, length(unique(data_df[[x]])) - 0.5, by = 1), #Grid line en x
                          linetype = "dotted", color = "grey", size = 0.3, alpha = 0.5) #Visualización del grid line
    }
    
    return(p)
  }
}


#### convert_to_plotly ####
# Transforma un gráfico ggplot en un gráfico interactivo plotly y configura el tooltip.
convert_to_plotly <- function(p, tooltip_value, isFacets = FALSE, numPlots = 1,
                              width= "100%", height= "100%") {
  # Obtener los títulos de los ejes y el título del gráfico desde el objeto ggplot
  x_axis_title <- p$labels$x
  y_axis_title <- p$labels$y
  plot_title <- p$labels$title
  legend_title <- p$labels$colour %||% p$labels$fill  # Usar el nombre de la leyenda de 'colour' o 'fill'
  
  # Extraer los márgenes del objeto ggplot
  margins <- if (!is.null(p$theme$plot.margin)) {
    p$theme$plot.margin
  } else {
    margin(150, 50, 150, 50)  # Márgenes predeterminados si no están definidos en el ggplot
  }
  
  # Convertir márgenes de ggplot a formato compatible con plotly
  plotly_margins <- list(
    t = as.numeric(margins[1]),  # Top
    r = as.numeric(margins[2]),  # Right
    b = as.numeric(margins[3]),  # Bottom
    l = as.numeric(margins[4])   # Left
  )
  
  # Ajustar los tamaños basados en el tamaño del contenedor
  title_size <- ifelse(width < 500, 16, ifelse(width < 800, 18, 22))
  axis_text_size <- ifelse(width < 500, 13, ifelse(width < 800, 15, 18))
  tick_size <- ifelse(height < 400, 12, 14)
  legend_font_size <- ifelse(width < 500, 10, 12)  # Ajuste dinámico del tamaño de fuente de la leyenda
  
  # Definir la posición de la leyenda y tamaño basado en el tamaño del contenedor
  if (width < 800 && isFacets == FALSE) {
    legend_position <- list(
      x = 0.5,
      y = 1.01,  # Ajuste dinámico para evitar solapamiento
      orientation = "h",
      xanchor = "center",
      yanchor = "bottom"
    )
  } else if (width < 800 && isFacets == TRUE) {
    legend_position <- list(
      x = 0.5,
      y = cacl_Y_Axis(numPlots), # Ajuste dinámico para evitar solapamiento
      orientation = "h",
      xanchor = "center",
      yanchor = "bottom"
    ) 
  } else {
    legend_position <- list(
      x = 1.05,
      y = 0.5,
      xanchor = "left",
      yanchor = "middle"
    )
  }
  
  # Convertir ggplot en un objeto plotly
  p_plotly <- ggplotly(p, tooltip = tooltip_value)
  
  # Ajustar el diseño para que la leyenda se acople dinámicamente
  p_plotly <- p_plotly %>% layout(
    autosize = TRUE,
    title = list(
      text = plot_title,
      font = list(size = title_size, family = "Arial", color = "black", weight = "bold")
    ),
    legend = list(
      x = legend_position$x,
      y = legend_position$y,
      orientation = legend_position$orientation,
      xanchor = legend_position$xanchor,
      yanchor = legend_position$yanchor,
      font = list(size = legend_font_size, family = "Arial", color = "black"),
      title = list(
        text = paste0(legend_title, "\n"),  # Usar `\n` en lugar de `<br>`
        font = list(size = axis_text_size, family = "Arial", color = "black", weight = "bold")
      )
    ),
    margin = plotly_margins
  ) %>%
    onRender("
      function(el, x) {
        function adjustLegendSpacing() {
          console.log('Adjusting legend spacing');
          var legendTexts = el.querySelectorAll('.legendtext'); // legendtext contiene el texto de los elementos de la leyenda
          console.log('Number of legend texts found:', legendTexts.length);
          if (legendTexts.length > 0) {
            var fixedSpacingValue = 40;  // El spacing para todos los elementos de la leyenda
            legendTexts.forEach(function(textNode) {
              var parentG = textNode.closest('g');
              var colorBox = parentG ? parentG.querySelector('rect') : null;
              if (textNode && colorBox) {
                console.log('Applying fixed spacing value:', fixedSpacingValue);
                textNode.setAttribute('x', fixedSpacingValue);
              }
            });
          }
        }
        
        adjustLegendSpacing();
        
        // Observa si hay cambios en la leyenda(Seleccionar o Deseleccionar)
        var observer = new MutationObserver(function(mutations) {
          mutations.forEach(function(mutation) {
            adjustLegendSpacing();
          });
        });
        
        var legendNode = el.querySelector('.legend');
        if (legendNode) {
          observer.observe(legendNode, { childList: true, subtree: true });
        }
      }
    ")
  
  # Ajustar las opciones de la leyenda en dispositivos pequeños
  if (width < 800) {
    p_plotly <- p_plotly %>% layout(
      legend = list(
        orientation = "h",
        font = list(size = legend_font_size, family = "Arial", color = "black"),
        title = list(
          text = paste0("     <b>",legend_title, "</b>", "\n"),  # Cambiar a `\n` para salto de línea
          font = list(size = axis_text_size, family = "Arial", color = "black"),
          side = "top"
        ),
        traceorder = "normal",
        itemwidth = 50,  # Ajuste del ancho de cada categoría
        valign = "top"   # Alinear el texto de la leyenda en la parte superior
      )
    ) %>%
      onRender("
      function(el, x) {
        function adjustLegendSpacing() {
          console.log('Adjusting legend spacing');
          var legendTexts = el.querySelectorAll('.legendtext'); // legendtext contiene el texto de los elementos de la leyenda
          console.log('Number of legend texts found:', legendTexts.length);
          if (legendTexts.length > 0) {
            var fixedSpacingValue = 40;  // El spacing para todos los elementos de la leyenda
            legendTexts.forEach(function(textNode) {
              var parentG = textNode.closest('g');
              var colorBox = parentG ? parentG.querySelector('rect') : null;
              if (textNode && colorBox) {
                console.log('Applying fixed spacing value:', fixedSpacingValue);
                textNode.setAttribute('x', fixedSpacingValue);
              }
            });
          }
        }
        
        adjustLegendSpacing();
        
        // Observa si hay cambios en la leyenda(Seleccionar o Deseleccionar)
        var observer = new MutationObserver(function(mutations) {
          mutations.forEach(function(mutation) {
            adjustLegendSpacing();
          });
        });
        
        var legendNode = el.querySelector('.legend');
        if (legendNode) {
          observer.observe(legendNode, { childList: true, subtree: true });
        }
      }
    ")
  }
  
  return(p_plotly)
}

#### createFuenteDiv ####
createFuenteDiv <- function(hyperlinks, fuenteTexts) {
  if (length(hyperlinks) != length(fuenteTexts)) stop("The length of hyperlinks and fuenteTexts must be the same.")
  combined <- sapply(seq_along(hyperlinks), function(i) {
    paste0(tags$a(href = hyperlinks[i], fuenteTexts[i], class = "source-inline-link"))
  })
  combined_text <- paste(combined, collapse = ", ")
  tags$div(class = "source-footer",
           tags$span("Fuentes: ", class = "source-footer-title"),
           tags$span(HTML(combined_text), class = "source-footer-text")
  )
}

#### renderBoxPlot ####
# Renderiza un gráfico de caja utilizando ggplot2 en el UI de Shiny.
renderBoxPlot <- function(data, x, y, color, title, xlab, ylab, colorlab = color) {
  p <- ggplot(data, aes(x = x, y = y)) +
    geom_boxplot() +
    geom_point(aes(color = color)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = title, x = xlab, y = ylab, color = colorlab)
  print(p)
}

#### renderDataTable ####
# Renderiza una tabla de datos utilizando el paquete DT en el UI de Shiny. Utilizada para presentar los datos
renderDataTable <- function(filtered_data, title, font_size = "18px") {
  datatable(
    filtered_data,
    extensions = c('Buttons'),
    options = list(
      pageLength = 5,
      lengthMenu = c(5, nrow(filtered_data) / 2, nrow(filtered_data)),
      scrollX = TRUE,
      paging = TRUE,
      searching = TRUE,
      fixedColumns = list(leftColumns = 1),
      autoWidth = FALSE,
      destroy = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip',
      buttons = list(
        #Boton para copiar la data
        #Para poder utilizar exportOption: modifier la funcion renderDT() debe
        #recibir como argumento server = FALSE. Ya que por defacto siempre es TRUE.
        list(
          extend = 'copy',
          text = 'Copiar',
          title = title,        #El argumento que se recibe como titulo de la data.
          exportOptions = list(
            columns = ":visible",     #Solo se va a copiar las columnas visibles de la data.
            modifier = list(page = "all") # ALL descarga toda la data seleccionada por el usuario. 
          )
        ),
        list(
          extend = 'collection',
          buttons = list(
            list(
              extend = 'csv',
              title = title,
              exportOptions = list(
                columns = ":visible", 
                modifier = list(page = "all") # 'all' descarga toda la data seleccionada por el usuario.
              ),                            # 'current' descarga solo la pagina visible.
              filename = title
              #filename = "datos_ViolenciaGeneroPR" #Nombre del archivo sin extension
            ),
            list(
              extend = 'excel',
              title = title,
              exportOptions = list(
                columns = ":visible", 
                modifier = list(page = "all") # ALL descarga toda la data seleccionada por el usuario.
              ),
              filename = title
              #filename = "datos_ViolenciaGeneroPR" #Nombre del archivo sin extension
            )
          ),
          text = 'Descargar Datos'
        )
      ),
      initComplete = JS(
        sprintf(
          "function(settings, json) {
            var container = $(this.api().table().container());
            container.prepend('<h3 class=\"custom-title\">%s</h3>');
            container.find('.dt-buttons').css({'padding-top': '10px', 'padding-bottom': '10px'});
            container.find('.dataTables_paginate').css('padding-top', '10px');
            container.find('.dataTables_filter').css('padding-top', '10px');
            container.find('.custom-title').css({'text-align': 'center', 'margin-bottom': '10px', 'font-size': '%s', 'font-weight': 'bold'});
          }",
          title, font_size
        )
      )
    )
  )
}

#### renderDataTable_Definitions ####
# Renderizar Tabla de Definiciones con Botones de Exportación
renderDataTable_Definitions <- function(filtered_data, title) {
  datatable(
    filtered_data,
    extensions = c('Buttons'),
    escape = c(FALSE, TRUE),
    options = list(
      pageLength = 10,
      lengthMenu = c(10, nrow(filtered_data) / 2, nrow(filtered_data)),
      scrollX = TRUE,
      paging = FALSE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = FALSE,
      ordering = TRUE,
      dom = 'Bfrtip', 
      buttons = list(
        list(
          extend = 'copy',
          text = 'Copiar',
          title = title,        #El argumento que se recibe como titulo de la data.
          exportOptions = list(
            columns = ":visible",     #Solo se va a copiar las columnas visibles de la data.
            modifier = list(page = "all") # ALL descarga toda la data seleccionada por el usuario. 
          )
        ),
        list(
          extend = 'collection',
          text = 'Descargar Conceptos',
          buttons = list(
            list(extend = 'csv', text = 'CSV', title = title, filename = 'Definiciones_ViolenciaGeneroPR'),
            list(extend = 'excel', text = 'Excel', title = title, filename = 'Definiciones_ViolenciaGeneroPR'),
            list(extend = 'pdf', text = 'PDF', title = title, filename = 'Definiciones_ViolenciaGeneroPR')
          )
        )
      ),
      drawCallback = JS(
        "function(settings) {",
        "var api = this.api();",
        "api.rows({ search: 'applied' }).every(function (rowIdx) {",
        "var dataIndex = rowIdx + 1;",
        "api.cell(rowIdx, 0).data(dataIndex);", 
        "});",
        "$(this.api().table().container()).find('.dt-buttons').css({'margin-top': '10px', 'padding-bottom': '10px'});",
        "$(this.api().table().container()).find('.dataTables_filter').css('padding-top', '10px');",
        "$(this.api().table().container()).find('.dataTables_paginate').css({'padding-top': '10px', 'padding-bottom': '10px'});",
        "$(this.api().table().container()).find('.dataTables_info').css('padding-top', '20px');",
        "}"
      ),
      columnDefs = list(
        list(targets = 0, orderable = FALSE) # Disable ordering for the index column
      )
    ),
    class = 'display compact' 
  )
}



#### renderHeatmap_facets ####

renderHeatmap_facets <- function(data, 
                                 x,           # "Sexo"
                                 y,           # "Maltrato"
                                 value,       # "Casos" (numérico)
                                 facet,       # "Año"
                                 xlab, ylab, fillLab = value,
                                 emptyMessage,
                                 lowColor = "white", highColor = "steelblue") {
  data_df <- data()  # evaluar el reactive una vez
  
  # Validaciones y gráfico vacío
  if (is.null(data_df) || nrow(data_df) == 0 ||
      is.null(data_df[[x]]) || is.null(data_df[[y]]) || is.null(data_df[[value]]) || is.null(data_df[[facet]])) {
    return(create_empty_plot_with_message(data, x, y, value, title = "", xlab, ylab, emptyMessage))
  }
  
  # Asegurar que existan todas las combinaciones (Sexo x Maltrato x Año) y rellenar con 0
  # Conservamos los niveles/orden existentes si son factor
  x_lv <- if (is.factor(data_df[[x]])) levels(data_df[[x]]) else sort(unique(data_df[[x]]))
  y_lv <- if (is.factor(data_df[[y]])) levels(data_df[[y]]) else sort(unique(data_df[[y]]))
  f_lv <- if (is.factor(data_df[[facet]])) levels(data_df[[facet]]) else sort(unique(data_df[[facet]]))
  
  df_complete <- data_df %>%
    tidyr::complete(!!rlang::sym(x) := x_lv,
                    !!rlang::sym(y) := y_lv,
                    !!rlang::sym(facet) := f_lv) %>%
    dplyr::mutate(!!rlang::sym(value) := dplyr::coalesce(!!rlang::sym(value), 0))
  
  # Restaurar levels (si aplica) para controlar el orden en ejes y facetas
  if (is.factor(data_df[[x]]))    df_complete[[x]]     <- factor(df_complete[[x]],     levels = x_lv)
  if (is.factor(data_df[[y]]))    df_complete[[y]]     <- factor(df_complete[[y]],     levels = y_lv)
  if (is.factor(data_df[[facet]])) df_complete[[facet]] <- factor(df_complete[[facet]], levels = f_lv)
  
  # Límite superior para la escala de color
  max_val <- max(df_complete[[value]], na.rm = TRUE)
  upper_limit <- ceiling(max_val * 1.0)  # 1.0 = directo; si quieres más contraste usa 1.2-1.5
  
  # Tooltips personalizados (convert_to_plotly(tooltip="text") leerá esta columna)
  df_complete <- df_complete %>%
    dplyr::mutate(
      .tooltip = paste0(
        "<b>", xlab, ":</b> ", .data[[x]], "<br>",
        "<b>", ylab, ":</b> ", scales::comma(.data[[value]]), "<br>",
        "<b>", fillLab, ":</b> ", .data[[y]], "<br>",
        "<b>Año:</b> ", .data[[facet]]
      )
    )
  
  p <- ggplot(df_complete, aes_string(x = x, y = y, fill = value, text = ".tooltip")) +
    geom_tile(color = "white", linewidth = 0.3) +
    scale_fill_gradient(
      low = lowColor, high = highColor,
      limits = c(0, upper_limit),
      labels = function(z) paste0(scales::comma(z), " ")
    ) +
    labs(x = xlab, y = fillLab, fill = ylab) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
      plot.margin = margin(t = 80, r = 40, b = 80, l = 40),
      panel.grid = element_blank()
    )
  
  return(p)
}



#### renderMap ####
renderMap <- function(data, value_col, value_col_region, map_zoom, 
                      municipios_geo) {
  
  # CARTO BASEMAP
  carto_api_key <- Sys.getenv("CARTO_API_KEY")
  
  carto_positron_url <- paste0(
    "https://{s}.basemaps.cartocdn.com/rastertiles/",
    "light_all/{z}/{x}/{y}.png?key=",
    carto_api_key
  )
  
  carto_attribution <- paste0(
    '&copy; <a href="https://www.openstreetmap.org/copyright" ',
    'target="_blank">OpenStreetMap</a> contributors ',
    '&copy; <a href="https://carto.com/attributions" ',
    'target="_blank">CARTO</a>'
  )
  
  # VERIFICAR DATOS
  
  if (
    nrow(data) == 0 ||
    !value_col %in% colnames(data) ||
    !value_col_region %in% colnames(data)
  ) {
    return(
      leaflet() %>%
        addTiles()
    )
  }
  
  # Valores dinámicos
  values <- data[[value_col]]
  regiones <- data[[value_col_region]]
  
  # Convertir a numérico si fuera necesario
  values <- as.numeric(values)
  
  # ESCALA DE COLORES
  
  min_val <- min(values, na.rm = TRUE)
  max_val <- max(values, na.rm = TRUE)
  
  if (!is.finite(min_val) || !is.finite(max_val)) {
    return(
      leaflet() %>%
        setView(
          lng = -66.5,
          lat = 18.2,
          zoom = map_zoom
        ) %>%
        addTiles(
          urlTemplate = carto_positron_url,
          attribution = carto_attribution,
          options = tileOptions(
            subdomains = "abcd",
            maxZoom = 20
          )
        )
    )
  }
  
  rango <- max_val - min_val
  
  num_bins <- 3
  
  if (rango < num_bins) {
    step <- 1
    max_val <- min_val + (num_bins - 1) * step
  } else {
    step <- ceiling(rango / num_bins)
  }
  
  bins <- seq(
    floor(min_val / step) * step,
    ceiling(max_val / step) * step,
    by = step
  )
  
  if (length(bins) < 2) {
    bins <- c(min_val, min_val + step)
  }
  
  violet_colors <- c(
    "#f9f5ff",
    "#e2b3ff",
    "#cc80ff",
    "#a64dff",
    "#8c33ff"
  )
  
  num_bins <- length(bins) - 1
  
  colors <- violet_colors[
    seq_len(min(num_bins, length(violet_colors)))
  ]
  
  pal <- colorBin(
    colors,
    domain = values,
    bins = bins,
    na.color = "transparent"
  )
  
  # LABELS
  
  labels <- paste0(
    value_col_region, ": ", regiones,
    "<br>",
    value_col, ": ", values
  ) %>%
    lapply(htmltools::HTML)
  
  # MAPA
  
  leaflet(data) %>%
    setView(
      lng = -66.5,
      lat = 18.2,
      zoom = map_zoom
    ) %>%
    
    addTiles(
      urlTemplate = carto_positron_url,
      attribution = carto_attribution,
      options = tileOptions(
        subdomains = "abcd",
        maxZoom = 20
      )
    ) %>%
    
    addPolygons(
      fillColor = pal(values),
      weight = 1,
      opacity = 1,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      label = labels,
      
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.9,
        bringToFront = TRUE
      )
    ) %>%
    
    addPolylines(
      data = municipios_geo,
      weight = 1,
      color = "#666",
      opacity = 1
    ) %>%
    
    addLegend(
      pal = pal,
      values = values,
      opacity = 0.7,
      title = value_col,
      position = "bottomright",
      labFormat = labelFormat(digits = 0)
    )
}

# renderMap <- function(data, value_col, value_col_region, map_zoom, provider = providers$CartoDB.Positron, 
#                       municipios_geo) {
#   # Verificar que hay datos disponibles
#   if (nrow(data) == 0 || !value_col %in% colnames(data)) {
#     return(leaflet() %>% addTiles())
#   }
#   
#   # Obtener valores de la columna dinámica
#   values <- data[[value_col]]
#   regiones <- data[[value_col_region]]
#   
#   # Calcular límites para la escala de colores
#   min_val <- min(values, na.rm = TRUE)
#   max_val <- max(values, na.rm = TRUE)
#   rango <- max_val - min_val
#   
#   # Garantizar al menos 3 rangos
#   num_bins <- 3
#   if (rango < num_bins) {
#     step <- 1
#     max_val <- min_val + (num_bins - 1) * step
#   } else {
#     step <- ceiling(rango / num_bins)
#   }
#   
#   # Crear los límites de los bins
#   bins <- seq(
#     floor(min_val / step) * step, 
#     ceiling(max_val / step) * step, 
#     by = step
#   )
#   
#   violet_colors <- c(
#     "#f9f5ff", "#e2b3ff", 
#     "#cc80ff", "#a64dff", 
#     "#8c33ff"
#   )
#   
#   
#   # Seleccionar colores según el número de categorías
#   num_bins <- length(bins) - 1
#   colors <- violet_colors[seq_len(num_bins)]
#   
#   # Crear la paleta de colores personalizada
#   pal <- colorBin(colors, domain = values, bins = bins, na.color = "transparent")
# 
#   # Crear el mapa
#   leaflet(data) %>%
#     setView(lng = -66.5, lat = 18.2, zoom = map_zoom) %>%
#     addProviderTiles(provider) %>%
#     # Polígonos de las regiones coloreadas
#     addPolygons(
#       fillColor = ~pal(values), 
#       weight = 1, 
#       opacity = 1,
#       color = "#666", 
#       dashArray = "", 
#       fillOpacity = 0.7,
#       label = ~paste0(
#         value_col_region, ": ", regiones, "<br>",
#         value_col, ": ", values
#       ) %>% lapply(htmltools::HTML), 
#       highlightOptions = highlightOptions(
#         weight = 2,
#         color = "#666",
#         dashArray = "",
#         fillOpacity = 0.9,
#         bringToFront = TRUE
#       )
#     ) %>%
#     # Fronteras de los municipios
#     addPolylines(
#       data = municipios_geo,
#       weight = 1, # Grosor de las líneas
#       color = "#666", # Color de las líneas de los municipios
#       opacity = 1
#     ) %>%
#     addLegend(
#       pal = pal,
#       values = ~values,
#       opacity = 0.7,
#       title = value_col,
#       position = "bottomright",
#       labFormat = labelFormat(digits = 0)
#     )
# }

#### renderMap_npprDS ####

renderMap_npprDS <- function(data, value_col, value_col_region, map_zoom,
                             municipios_geo) {
  # CARTO BASEMAP
  
  carto_api_key <- Sys.getenv("CARTO_API_KEY")
  
  carto_positron_url <- paste0(
    "https://{s}.basemaps.cartocdn.com/rastertiles/",
    "light_all/{z}/{x}/{y}.png?key=",
    carto_api_key
  )
  
  carto_attribution <- paste0(
    '&copy; <a href="https://www.openstreetmap.org/copyright" ',
    'target="_blank">OpenStreetMap</a> contributors ',
    '&copy; <a href="https://carto.com/attributions" ',
    'target="_blank">CARTO</a>'
  )
  
  # VERIFICAR DATOS
  if (
    nrow(data) == 0 ||
    !value_col %in% colnames(data) ||
    !value_col_region %in% colnames(data)
  ) {
    return(
      leaflet() %>%
        addTiles()
    )
  }
  
  # Obtener valores dinámicos
  values <- as.numeric(data[[value_col]])
  regiones <- data[[value_col_region]]
  
  # CALCULAR LÍMITES
  min_val <- 0
  max_val <- max(values, na.rm = TRUE)
  
  if (!is.finite(max_val)) {
    return(
      leaflet() %>%
        setView(
          lng = -66.5,
          lat = 18.2,
          zoom = map_zoom
        ) %>%
        addTiles(
          urlTemplate = carto_positron_url,
          attribution = carto_attribution,
          options = tileOptions(
            subdomains = "abcd",
            maxZoom = 20
          )
        )
    )
  }
  
  # Usar pretty() para cortes naturales
  bins <- pretty(
    c(min_val, max_val),
    n = 7
  )
  
  if (length(bins) < 2) {
    bins <- c(min_val, min_val + 1)
  }
  
  # PALETA
  
  violet_colors <- c(
    "#f9f5ff", "#f4e3ff", "#edd0ff", "#e2b3ff",
    "#d89aff", "#cc80ff", "#bf66ff", "#a64dff",
    "#8c33ff", "#701aff", "#5900f2", "#4400cc",
    "#3000a6", "#1d007d", "#0d004c"
  )
  
  num_bins <- length(bins) - 1
  
  colors <- violet_colors[
    seq_len(min(num_bins, length(violet_colors)))
  ]
  
  pal <- colorBin(
    colors,
    domain = values,
    bins = bins,
    na.color = "transparent"
  )
  
  # MAPA
  leaflet(data) %>%
    setView(
      lng = -66.5,
      lat = 18.2,
      zoom = map_zoom
    ) %>%
    
    addTiles(
      urlTemplate = carto_positron_url,
      attribution = carto_attribution,
      options = tileOptions(
        subdomains = "abcd",
        maxZoom = 20
      )
    ) %>%
    
    addPolygons(
      fillColor = pal(values),
      weight = 1,
      opacity = 1,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      
      label = paste0(
        value_col_region, ": ", regiones,
        "<br>",
        value_col, ": ", values
      ) %>%
        lapply(htmltools::HTML),
      
      highlightOptions = highlightOptions(
        weight = 1,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.9,
        bringToFront = TRUE
      )
    ) %>%
    
    addLegend(
      pal = pal,
      values = values,
      opacity = 0.7,
      title = value_col,
      position = "bottomright",
      labFormat = labelFormat(digits = 0)
    )
}

# renderMap_npprDS <- function(data, value_col, value_col_region, map_zoom,
#                              municipios_geo) {
#   
#### renderMap_npprDS NO COROPLETICO ####
#   # CARTO BASEMAP
#   carto_api_key <- Sys.getenv("CARTO_API_KEY")
#   
#   carto_positron_url <- paste0(
#     "https://{s}.basemaps.cartocdn.com/rastertiles/",
#     "light_all/{z}/{x}/{y}.png?key=",
#     carto_api_key
#   )
#   
#   carto_attribution <- paste0(
#     '&copy; <a href="https://www.openstreetmap.org/copyright" ',
#     'target="_blank">OpenStreetMap</a> contributors ',
#     '&copy; <a href="https://carto.com/attributions" ',
#     'target="_blank">CARTO</a>'
#   )
#   # VERIFICAR DATOS
#   if (
#     nrow(data) == 0 ||
#     !value_col %in% colnames(data) ||
#     !value_col_region %in% colnames(data)
#   ) {
#     
#     return(
#       leaflet() %>%
#         setView(
#           lng = -66.5,
#           lat = 18.2,
#           zoom = map_zoom
#         ) %>%
#         addTiles(
#           urlTemplate = carto_positron_url,
#           attribution = carto_attribution,
#           options = tileOptions(
#             subdomains = "abcd",
#             maxZoom = 20
#           )
#         )
#     )
#   }
#   # PREPARAR DATOS
#   data <- st_as_sf(data)
#   
#   data$region_map <- trimws(
#     as.character(data[[value_col_region]])
#   )
#   
#   data$casos_map <- suppressWarnings(
#     as.numeric(data[[value_col]])
#   )
#   
#   # Los valores faltantes se consideran 0
#   data$casos_map[is.na(data$casos_map)] <- 0
#   # COLORES FIJOS POR REGIÓN
#   # IMPORTANTE:
#   # Cada región siempre tendrá el mismo color.
#   #
#   
#   colores_regiones <- c(
#     "Aguadilla" = "#6A4C93",
#     "Arecibo"   = "#1982C4",
#     "Bayamón"   = "#3A86A8",
#     "Caguas"    = "#008C95",
#     "Carolina"  = "#52A675",
#     "Guayama"   = "#80C41C",
#     "Humacao"   = "#FFBE2E",
#     "Mayagüez"  = "#8338EC",
#     "Ponce"     = "#F4A261",
#     "San Juan"  = "#E76F51"
#   )
#   # NORMALIZAR NOMBRES PARA HACER MATCH
#   region_key <- toupper(
#     trimws(
#       data$region_map
#     )
#   )
#   
#   colores_key <- toupper(
#     names(colores_regiones)
#   )
#   # ASIGNAR COLOR A CADA POLÍGONO
#   data$color_region <- unname(
#     colores_regiones[
#       match(
#         region_key,
#         colores_key
#       )
#     ]
#   )
#   # COLOR DE RESPALDO
#   # Si existe alguna región cuyo nombre no coincide
#   # exactamente con las 8 regiones definidas arriba,
#   # se pintará gris en lugar de negro.
#   #
#   
#   data$color_region[
#     is.na(data$color_region)
#   ] <- "#BDBDBD"
#   # FORMATEAR CASOS
#   data$casos_label <- format(
#     data$casos_map,
#     big.mark = ",",
#     decimal.mark = ".",
#     scientific = FALSE,
#     trim = TRUE
#   )
#   # LABEL INTERACTIVO
#   labels <- paste0(
#     
#     "<div style='
#       font-family: Arial, Helvetica, sans-serif;
#       padding: 3px 5px;
#       min-width: 130px;
#     '>",
#     
#     "<div style='
#       font-size: 14px;
#       font-weight: 700;
#       color: #333333;
#       margin-bottom: 4px;
#     '>",
#     
#     data$region_map,
#     
#     "</div>",
#     
#     "<div style='
#       font-size: 12px;
#       color: #666666;
#     '>",
#     
#     value_col,
#     
#     ": <strong style='color:#333333;'>",
#     
#     data$casos_label,
#     
#     "</strong>",
#     
#     "</div>",
#     
#     "</div>"
#   )
#   # CREAR MAPA
#   mapa <- leaflet(data) %>%
#     
#     setView(
#       lng = -66.5,
#       lat = 18.2,
#       zoom = map_zoom
#     ) %>%
#     
#     addTiles(
#       urlTemplate = carto_positron_url,
#       attribution = carto_attribution,
#       options = tileOptions(
#         subdomains = "abcd",
#         maxZoom = 20
#       )
#     ) %>%
#   # POLÍGONOS
#   addPolygons(
#     # AQUÍ ESTÁ EL CAMBIO IMPORTANTE
#     fillColor = ~color_region,
#     
#     fillOpacity = 0.78,
#     
#     color = "#FFFFFF",
#     
#     opacity = 1,
#     
#     weight = 2,
#     
#     smoothFactor = 0.4,
#     
#     label = lapply(
#       labels,
#       htmltools::HTML
#     ),
#     
#     labelOptions = labelOptions(
#       
#       direction = "auto",
#       
#       textsize = "12px",
#       
#       style = list(
#         "background" = "rgba(255,255,255,0.97)",
#         "border" = "none",
#         "border-radius" = "7px",
#         "box-shadow" = "0 2px 7px rgba(0,0,0,0.18)",
#         "padding" = "7px 9px"
#       )
#     ),
#     
#     highlightOptions = highlightOptions(
#       weight = 3,
#       color = "#333333",
#       fillOpacity = 0.90,
#       bringToFront = TRUE
#     )
#   )
#   # POSICIÓN DE LOS NÚMEROS
#   indices_validos <- which(
#     is.finite(data$casos_map)
#   )
#   
#   
#   if (length(indices_validos) > 0) {
#     
#     # Punto que siempre queda dentro del polígono
#     puntos <- st_point_on_surface(
#       data[indices_validos, ]
#     )
#     
#     coords <- st_coordinates(
#       puntos
#     )
#     # NÚMERO SOBRE CADA REGIÓN
#     value_labels <- paste0(
#       
#       "<div class='nppr-casos-label'>",
#       
#       data$casos_label[indices_validos],
#       
#       "</div>"
#     )
#     
#     
#     mapa <- mapa %>%
#       
#       addLabelOnlyMarkers(
#         
#         lng = coords[, 1],
#         lat = coords[, 2],
#         
#         label = lapply(
#           value_labels,
#           htmltools::HTML
#         ),
#         
#         labelOptions = labelOptions(
#           
#           noHide = TRUE,
#           
#           direction = "center",
#           
#           textOnly = TRUE,
#           
#           className = "nppr-casos-marker"
#         )
#       )
#   }
#   # ESTILOS CSS
#   mapa <- mapa %>%
#     
#     addControl(
#       
#       html = "
#       
#       <style>
#       
#       /* =====================================================
#          NÚMERO DE CASOS
#          ===================================================== */
#       
#       .nppr-casos-marker {
#         background: transparent !important;
#         border: none !important;
#         box-shadow: none !important;
#         padding: 0 !important;
#       }
#       
#       .nppr-casos-label {
#         
#         font-family:
#           Arial,
#           Helvetica,
#           sans-serif;
#         
#         font-size: 13px;
#         
#         font-weight: 700;
#         
#         color: #FFFFFF;
#         
#         text-align: center;
#         
#         white-space: nowrap;
#         
#         pointer-events: none;
#         
#         text-shadow:
#           -1px -1px 2px rgba(0,0,0,0.75),
#            1px -1px 2px rgba(0,0,0,0.75),
#           -1px  1px 2px rgba(0,0,0,0.75),
#            1px  1px 2px rgba(0,0,0,0.75);
#       }
#       
#       </style>
#       
#       ",
#       
#       position = "topleft"
#     )
#   # LEYENDA
#   legend_items <- paste0(
#     
#     mapply(
#       
#       function(region, color) {
#         
#         paste0(
#           
#           "<div style='
#             display:flex;
#             align-items:center;
#             margin:4px 0;
#             font-size:12px;
#             color:#555;
#           '>",
#           
#           "<span style='
#             display:inline-block;
#             width:14px;
#             height:14px;
#             background:",
#           color,
#           ";
#             border-radius:4px;
#             margin-right:8px;
#             flex-shrink:0;
#           '></span>",
#           
#           "<span>",
#           region,
#           "</span>",
#           
#           "</div>"
#         )
#       },
#       
#       names(colores_regiones),
#       colores_regiones,
#       
#       SIMPLIFY = TRUE
#     ),
#     
#     collapse = ""
#   )
#   
#   
#   legend_html <- paste0(
#     
#     "<div style='
#       background:rgba(255,255,255,0.96);
#       padding:12px 14px;
#       border-radius:9px;
#       box-shadow:0 2px 10px rgba(0,0,0,0.18);
#       font-family:Arial, Helvetica, sans-serif;
#     '>",
#     
#     "<div style='
#       font-size:14px;
#       font-weight:700;
#       color:#333;
#       margin-bottom:7px;
#     '>",
#     
#     "Regiones",
#     
#     "</div>",
#     
#     legend_items,
#     
#     "</div>"
#   )
#   
#   
#   mapa <- mapa %>%
#     
#     addControl(
#       html = legend_html,
#       position = "bottomright"
#     )
#   # RETORNAR
#   mapa
# }


#### renderMap_vivienda ####
renderMap_vivienda <- function(data, value_col, value_col_region, map_zoom,
                               municipios_geo) {
  # =========================================================
  # CARTO BASEMAP
  # =========================================================
  
  carto_api_key <- Sys.getenv("CARTO_API_KEY")
  
  carto_positron_url <- paste0(
    "https://{s}.basemaps.cartocdn.com/rastertiles/",
    "light_all/{z}/{x}/{y}.png?key=",
    carto_api_key
  )
  
  carto_attribution <- paste0(
    '&copy; <a href="https://www.openstreetmap.org/copyright" ',
    'target="_blank">OpenStreetMap</a> contributors ',
    '&copy; <a href="https://carto.com/attributions" ',
    'target="_blank">CARTO</a>'
  )
  
  # =========================================================
  # VERIFICAR QUE HAY DATOS DISPONIBLES
  # =========================================================
  
  if (
    nrow(data) == 0 ||
    !value_col %in% colnames(data) ||
    !value_col_region %in% colnames(data)
  ) {
    return(
      leaflet() %>%
        addTiles()
    )
  }
  
  # =========================================================
  # OBTENER VALORES
  # =========================================================
  
  values <- as.numeric(data[[value_col]])
  regiones <- data[[value_col_region]]
  
  # =========================================================
  # CALCULAR LÍMITES PARA LA ESCALA
  # =========================================================
  
  min_val <- 0
  max_val <- max(values, na.rm = TRUE)
  
  # Manejar datos sin valores válidos
  if (!is.finite(max_val)) {
    return(
      leaflet() %>%
        setView(
          lng = -66.5,
          lat = 18.2,
          zoom = map_zoom
        ) %>%
        addTiles(
          urlTemplate = carto_positron_url,
          attribution = carto_attribution,
          options = tileOptions(
            subdomains = "abcd",
            maxZoom = 20
          )
        )
    )
  }
  
  rango <- max_val - min_val
  
  # =========================================================
  # DEFINIR BINS
  # =========================================================
  
  step <- ifelse(rango >= 30, 10, 5)
  
  bins <- seq(
    floor(min_val / step) * step,
    ceiling(max_val / step) * step,
    by = step
  )
  
  # Evitar problemas cuando solamente hay un bin
  if (length(bins) < 2) {
    bins <- c(min_val, min_val + step)
  }
  
  # =========================================================
  # PALETA DE COLORES
  # =========================================================
  
  violet_colors <- c(
    "#f9f5ff", "#f4e3ff", "#edd0ff", "#e2b3ff",
    "#d89aff", "#cc80ff", "#bf66ff", "#a64dff",
    "#8c33ff", "#701aff", "#5900f2", "#4400cc",
    "#3000a6", "#1d007d", "#0d004c"
  )
  
  num_bins <- length(bins) - 1
  
  colors <- violet_colors[
    seq_len(min(num_bins, length(violet_colors)))
  ]
  
  # =========================================================
  # PALETA DINÁMICA
  # =========================================================
  
  pal <- colorBin(
    colors,
    domain = values,
    bins = bins,
    na.color = "transparent"
  )
  
  # =========================================================
  # MAPA
  # =========================================================
  
  leaflet(data) %>%
    
    setView(
      lng = -66.5,
      lat = 18.2,
      zoom = map_zoom
    ) %>%
    
    # ---------------------------------------------------------
  # CARTO POSITRON
  # ---------------------------------------------------------
  
  addTiles(
    urlTemplate = carto_positron_url,
    attribution = carto_attribution,
    options = tileOptions(
      subdomains = "abcd",
      maxZoom = 20
    )
  ) %>%
    
    # ---------------------------------------------------------
  # POLÍGONOS
  # ---------------------------------------------------------
  
  addPolygons(
    fillColor = pal(values),
    weight = 1,
    opacity = 1,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    
    label = paste0(
      value_col_region, ": ", regiones,
      "<br>",
      value_col, ": ", values
    ) %>%
      lapply(htmltools::HTML),
    
    highlightOptions = highlightOptions(
      weight = 1,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  ) %>%
    
    # ---------------------------------------------------------
  # FRONTERAS DE LOS MUNICIPIOS
  # ---------------------------------------------------------
  
  # addPolylines(
  #   data = municipios_geo,
  #   weight = 1,
  #   color = "#666",
  #   opacity = 1
  # ) %>%
  
  # ---------------------------------------------------------
  # LEYENDA
  # ---------------------------------------------------------
  
  addLegend(
    pal = pal,
    values = values,
    opacity = 0.7,
    title = value_col,
    position = "bottomright",
    labFormat = labelFormat(digits = 0)
  )
}

#### renderMap_vivienda NO COROPLETICO ####
# renderMap_vivienda <- function(data, value_col, value_col_region, map_zoom,
#                                municipios_geo) {
#   
#   # =========================================================
#   # CARTO BASEMAP
#   # =========================================================
#   
#   carto_api_key <- Sys.getenv("CARTO_API_KEY")
#   
#   carto_positron_url <- paste0(
#     "https://{s}.basemaps.cartocdn.com/rastertiles/",
#     "light_all/{z}/{x}/{y}.png?key=",
#     carto_api_key
#   )
#   
#   carto_attribution <- paste0(
#     '&copy; <a href="https://www.openstreetmap.org/copyright" ',
#     'target="_blank">OpenStreetMap</a> contributors ',
#     '&copy; <a href="https://carto.com/attributions" ',
#     'target="_blank">CARTO</a>'
#   )
#   
#   
#   # =========================================================
#   # VERIFICAR DATOS
#   # =========================================================
#   
#   if (
#     nrow(data) == 0 ||
#     !value_col %in% colnames(data) ||
#     !value_col_region %in% colnames(data)
#   ) {
#     
#     return(
#       leaflet() %>%
#         setView(
#           lng = -66.5,
#           lat = 18.2,
#           zoom = map_zoom
#         ) %>%
#         addTiles(
#           urlTemplate = carto_positron_url,
#           attribution = carto_attribution,
#           options = tileOptions(
#             subdomains = "abcd",
#             maxZoom = 20
#           )
#         )
#     )
#   }
#   
#   
#   # =========================================================
#   # PREPARAR DATOS
#   # =========================================================
#   
#   data <- st_as_sf(data)
#   
#   data$region_map <- trimws(
#     as.character(data[[value_col_region]])
#   )
#   
#   data$casos_map <- suppressWarnings(
#     as.numeric(data[[value_col]])
#   )
#   
#   # Los valores faltantes se consideran 0
#   data$casos_map[is.na(data$casos_map)] <- 0
#   
#   
#   # =========================================================
#   # COLORES FIJOS POR REGIÓN
#   # =========================================================
#   
#   colores_regiones <- c(
#     "Aguadilla" = "#6A4C93",
#     "Arecibo"   = "#1982C4",
#     "Bayamón"   = "#3A86A8",
#     "Caguas"    = "#008C95",
#     "Carolina"  = "#52A675",
#     "Guayama"   = "#80C41C",
#     "Humacao"   = "#FFBE2E",
#     "Mayagüez"  = "#8338EC",
#     "Ponce"     = "#F4A261",
#     "San Juan"  = "#E76F51"
#   )
#   
#   
#   # =========================================================
#   # NORMALIZAR NOMBRES PARA HACER MATCH
#   # =========================================================
#   
#   region_key <- toupper(
#     trimws(
#       data$region_map
#     )
#   )
#   
#   colores_key <- toupper(
#     names(colores_regiones)
#   )
#   
#   
#   # =========================================================
#   # ASIGNAR COLOR A CADA POLÍGONO
#   # =========================================================
#   
#   data$color_region <- unname(
#     colores_regiones[
#       match(
#         region_key,
#         colores_key
#       )
#     ]
#   )
#   
#   
#   # =========================================================
#   # COLOR DE RESPALDO
#   # =========================================================
#   
#   data$color_region[
#     is.na(data$color_region)
#   ] <- "#BDBDBD"
#   
#   
#   # =========================================================
#   # FORMATEAR CASOS
#   # =========================================================
#   
#   data$casos_label <- format(
#     data$casos_map,
#     big.mark = ",",
#     decimal.mark = ".",
#     scientific = FALSE,
#     trim = TRUE
#   )
#   
#   
#   # =========================================================
#   # LABEL INTERACTIVO
#   # =========================================================
#   
#   labels <- paste0(
#     
#     "<div style='
#       font-family: Arial, Helvetica, sans-serif;
#       padding: 3px 5px;
#       min-width: 130px;
#     '>",
#     
#     "<div style='
#       font-size: 14px;
#       font-weight: 700;
#       color: #333333;
#       margin-bottom: 4px;
#     '>",
#     
#     data$region_map,
#     
#     "</div>",
#     
#     "<div style='
#       font-size: 12px;
#       color: #666666;
#     '>",
#     
#     value_col,
#     
#     ": <strong style='color:#333333;'>",
#     
#     data$casos_label,
#     
#     "</strong>",
#     
#     "</div>",
#     
#     "</div>"
#   )
#   
#   
#   # =========================================================
#   # CREAR MAPA
#   # =========================================================
#   
#   mapa <- leaflet(data) %>%
#     
#     setView(
#       lng = -66.5,
#       lat = 18.2,
#       zoom = map_zoom
#     ) %>%
#     
#     # =======================================================
#   # CARTO POSITRON
#   # =======================================================
#   
#   addTiles(
#     urlTemplate = carto_positron_url,
#     attribution = carto_attribution,
#     options = tileOptions(
#       subdomains = "abcd",
#       maxZoom = 20
#     )
#   ) %>%
#     
#     # =======================================================
#   # POLÍGONOS
#   # =======================================================
#   
#   addPolygons(
#     
#     fillColor = ~color_region,
#     
#     fillOpacity = 0.78,
#     
#     color = "#FFFFFF",
#     
#     opacity = 1,
#     
#     weight = 2,
#     
#     smoothFactor = 0.4,
#     
#     label = lapply(
#       labels,
#       htmltools::HTML
#     ),
#     
#     labelOptions = labelOptions(
#       
#       direction = "auto",
#       
#       textsize = "12px",
#       
#       style = list(
#         "background" = "rgba(255,255,255,0.97)",
#         "border" = "none",
#         "border-radius" = "7px",
#         "box-shadow" = "0 2px 7px rgba(0,0,0,0.18)",
#         "padding" = "7px 9px"
#       )
#     ),
#     
#     highlightOptions = highlightOptions(
#       weight = 3,
#       color = "#333333",
#       fillOpacity = 0.90,
#       bringToFront = TRUE
#     )
#   )
#   
#   
#   # =========================================================
#   # POSICIÓN DE LOS NÚMEROS
#   # =========================================================
#   
#   indices_validos <- which(
#     is.finite(data$casos_map)
#   )
#   
#   
#   if (length(indices_validos) > 0) {
#     
#     # -------------------------------------------------------
#     # PUNTO QUE SIEMPRE QUEDA DENTRO DEL POLÍGONO
#     # -------------------------------------------------------
#     
#     puntos <- st_point_on_surface(
#       data[indices_validos, ]
#     )
#     
#     coords <- st_coordinates(
#       puntos
#     )
#     
#     
#     # =======================================================
#     # NÚMERO DE CASOS SOBRE CADA REGIÓN
#     # =======================================================
#     
#     value_labels <- paste0(
#       
#       "<div class='vivienda-casos-label'>",
#       
#       data$casos_label[indices_validos],
#       
#       "</div>"
#     )
#     
#     
#     mapa <- mapa %>%
#       
#       addLabelOnlyMarkers(
#         
#         lng = coords[, 1],
#         lat = coords[, 2],
#         
#         label = lapply(
#           value_labels,
#           htmltools::HTML
#         ),
#         
#         labelOptions = labelOptions(
#           
#           noHide = TRUE,
#           
#           direction = "center",
#           
#           textOnly = TRUE,
#           
#           className = "vivienda-casos-marker"
#         )
#       )
#   }
#   
#   
#   # =========================================================
#   # ESTILOS CSS
#   # =========================================================
#   
#   mapa <- mapa %>%
#     
#     addControl(
#       
#       html = "
#       
#       <style>
#       
#       /* =====================================================
#          NÚMERO DE CASOS
#          ===================================================== */
#       
#       .vivienda-casos-marker {
#         background: transparent !important;
#         border: none !important;
#         box-shadow: none !important;
#         padding: 0 !important;
#       }
#       
#       .vivienda-casos-label {
#         
#         font-family:
#           Arial,
#           Helvetica,
#           sans-serif;
#         
#         font-size: 13px;
#         
#         font-weight: 700;
#         
#         color: #FFFFFF;
#         
#         text-align: center;
#         
#         white-space: nowrap;
#         
#         pointer-events: none;
#         
#         text-shadow:
#           -1px -1px 2px rgba(0,0,0,0.75),
#            1px -1px 2px rgba(0,0,0,0.75),
#           -1px  1px 2px rgba(0,0,0,0.75),
#            1px  1px 2px rgba(0,0,0,0.75);
#       }
#       
#       </style>
#       
#       ",
#       
#       position = "topleft"
#     )
#   
#   
#   # =========================================================
#   # LEYENDA PERSONALIZADA
#   # =========================================================
#   
#   legend_items <- paste0(
#     
#     mapply(
#       
#       function(region, color) {
#         
#         paste0(
#           
#           "<div style='
#             display:flex;
#             align-items:center;
#             margin:4px 0;
#             font-size:12px;
#             color:#555;
#           '>",
#           
#           "<span style='
#             display:inline-block;
#             width:14px;
#             height:14px;
#             background:",
#           
#           color,
#           
#           ";
#             border-radius:4px;
#             margin-right:8px;
#             flex-shrink:0;
#           '></span>",
#           
#           "<span>",
#           
#           region,
#           
#           "</span>",
#           
#           "</div>"
#         )
#       },
#       
#       names(colores_regiones),
#       colores_regiones,
#       
#       SIMPLIFY = TRUE
#     ),
#     
#     collapse = ""
#   )
#   
#   
#   legend_html <- paste0(
#     
#     "<div style='
#       background:rgba(255,255,255,0.96);
#       padding:12px 14px;
#       border-radius:9px;
#       box-shadow:0 2px 10px rgba(0,0,0,0.18);
#       font-family:Arial, Helvetica, sans-serif;
#     '>",
#     
#     "<div style='
#       font-size:14px;
#       font-weight:700;
#       color:#333;
#       margin-bottom:7px;
#     '>",
#     
#     "Regiones",
#     
#     "</div>",
#     
#     legend_items,
#     
#     "</div>"
#   )
#   
#   
#   mapa <- mapa %>%
#     
#     addControl(
#       html = legend_html,
#       position = "bottomright"
#     )
#   
#   
#   # =========================================================
#   # RETORNAR MAPA
#   # =========================================================
#   
#   mapa
# }

# #### renderMap_vivienda_new ####
# renderMap_vivienda_new <- function(data, value_col, value_col_region, map_zoom) {
#   
#   # ============================================================
#   # CARTO API KEY
#   # Se obtiene desde el archivo .Renviron
#   # ============================================================
#   
#   carto_api_key <- Sys.getenv("CARTO_API_KEY")
#   
#   # Verificar que la API Key exista
#   if (carto_api_key == "") {
#     warning(
#       "CARTO_API_KEY no está configurada. ",
#       "Verifica el archivo .Renviron."
#     )
#   }
#   
#   
#   # ============================================================
#   # URL DE CARTO POSITRON
#   # ============================================================
#   
#   carto_positron_url <- paste0(
#     "https://{s}.basemaps.cartocdn.com/rastertiles/",
#     "light_all/{z}/{x}/{y}.png?key=",
#     carto_api_key
#   )
#   
#   
#   # ============================================================
#   # VERIFICAR QUE HAYA DATOS DISPONIBLES
#   # ============================================================
#   
#   if (
#     nrow(data) == 0 ||
#     !value_col %in% colnames(data) ||
#     !value_col_region %in% colnames(data)
#   ) {
#     
#     return(
#       leaflet() %>%
#         setView(
#           lng = -66.5,
#           lat = 18.2,
#           zoom = map_zoom
#         ) %>%
#         addTiles()
#     )
#   }
#   
#   
#   # ============================================================
#   # OBTENER VALORES DE LAS COLUMNAS DINÁMICAS
#   # ============================================================
#   
#   values <- data[[value_col]]
#   regiones <- data[[value_col_region]]
#   
#   
#   # ============================================================
#   # ASEGURAR QUE LOS VALORES SEAN NUMÉRICOS
#   # ============================================================
#   
#   values <- as.numeric(values)
#   
#   
#   # ============================================================
#   # CALCULAR LÍMITES PARA LA ESCALA DE COLORES
#   # ============================================================
#   
#   min_val <- 0
#   
#   max_val <- max(
#     values,
#     na.rm = TRUE
#   )
#   
#   
#   # Si no existen valores válidos
#   if (!is.finite(max_val)) {
#     
#     return(
#       leaflet(data) %>%
#         setView(
#           lng = -66.5,
#           lat = 18.2,
#           zoom = map_zoom
#         ) %>%
#         addTiles(
#           urlTemplate = carto_positron_url,
#           attribution = paste0(
#             '&copy; <a href="https://www.openstreetmap.org/copyright">',
#             'OpenStreetMap</a> contributors ',
#             '&copy; <a href="https://carto.com/attributions">',
#             'CARTO</a>'
#           ),
#           options = tileOptions(
#             subdomains = "abcd",
#             maxZoom = 20
#           )
#         )
#     )
#   }
#   
#   
#   rango <- max_val - min_val
#   
#   
#   # ============================================================
#   # DEFINIR TAMAÑO DE LOS INTERVALOS
#   # ============================================================
#   
#   step <- ifelse(
#     rango >= 30,
#     10,
#     5
#   )
#   
#   
#   # ============================================================
#   # CREAR BINS
#   # ============================================================
#   
#   if (max_val == min_val) {
#     
#     bins <- c(
#       min_val,
#       min_val + step
#     )
#     
#   } else {
#     
#     bins <- seq(
#       floor(min_val / step) * step,
#       ceiling(max_val / step) * step,
#       by = step
#     )
#     
#   }
#   
#   
#   # Asegurar que existan al menos dos límites
#   if (length(bins) < 2) {
#     
#     bins <- c(
#       min_val,
#       min_val + step
#     )
#     
#   }
#   
#   
#   # ============================================================
#   # PALETA DE COLORES VIOLETA
#   # ============================================================
#   
#   violet_colors <- c(
#     "#f9f5ff",
#     "#f4e3ff",
#     "#edd0ff",
#     "#e2b3ff",
#     "#d89aff",
#     "#cc80ff",
#     "#bf66ff",
#     "#a64dff",
#     "#8c33ff",
#     "#701aff",
#     "#5900f2",
#     "#4400cc",
#     "#3000a6",
#     "#1d007d",
#     "#0d004c"
#   )
#   
#   
#   # ============================================================
#   # NÚMERO DE INTERVALOS
#   # ============================================================
#   
#   num_bins <- length(bins) - 1
#   
#   
#   # ============================================================
#   # AJUSTAR CANTIDAD DE COLORES
#   # ============================================================
#   
#   if (num_bins <= length(violet_colors)) {
#     
#     colors <- violet_colors[
#       seq_len(num_bins)
#     ]
#     
#   } else {
#     
#     colors <- colorRampPalette(violet_colors)(
#       num_bins
#     )
#     
#   }
#   
#   
#   # ============================================================
#   # CREAR PALETA
#   # ============================================================
#   
#   pal <- colorBin(
#     palette = colors,
#     domain = values,
#     bins = bins,
#     na.color = "transparent"
#   )
#   
#   
#   # ============================================================
#   # CREAR MAPA
#   # ============================================================
#   
#   leaflet(data) %>%
#     
#     
#     # ----------------------------------------------------------
#   # VISTA INICIAL
#   # ----------------------------------------------------------
#   
#   setView(
#     lng = -66.5,
#     lat = 18.2,
#     zoom = map_zoom
#   ) %>%
#     
#     
#     # ----------------------------------------------------------
#   # CARTO POSITRON
#   # ----------------------------------------------------------
#   
#   addTiles(
#     urlTemplate = carto_positron_url,
#     
#     attribution = paste0(
#       '&copy; <a href="https://www.openstreetmap.org/copyright" ',
#       'target="_blank">OpenStreetMap</a> contributors ',
#       '&copy; <a href="https://carto.com/attributions" ',
#       'target="_blank">CARTO</a>'
#     ),
#     
#     options = tileOptions(
#       subdomains = "abcd",
#       maxZoom = 20
#     )
#   ) %>%
#     
#     
#     # ----------------------------------------------------------
#   # POLÍGONOS
#   # ----------------------------------------------------------
#   
#   addPolygons(
#     
#     fillColor = ~pal(values),
#     
#     weight = 1,
#     
#     opacity = 1,
#     
#     color = "#666",
#     
#     dashArray = "",
#     
#     fillOpacity = 0.7,
#     
#     
#     # --------------------------------------------------------
#     # LABEL
#     # --------------------------------------------------------
#     
#     label = ~paste0(
#       value_col_region,
#       ": ",
#       regiones,
#       "<br>",
#       value_col,
#       ": ",
#       values
#     ) %>%
#       lapply(htmltools::HTML),
#     
#     
#     # --------------------------------------------------------
#     # HIGHLIGHT
#     # --------------------------------------------------------
#     
#     highlightOptions = highlightOptions(
#       weight = 1,
#       color = "#666",
#       dashArray = "",
#       fillOpacity = 0.9,
#       bringToFront = TRUE
#     )
#     
#   ) %>%
#     
#     
#     # ----------------------------------------------------------
#   # LEYENDA
#   # ----------------------------------------------------------
#   
#   addLegend(
#     
#     pal = pal,
#     
#     values = ~values,
#     
#     opacity = 0.7,
#     
#     title = value_col,
#     
#     position = "bottomright",
#     
#     labFormat = labelFormat(
#       digits = 0
#     )
#   )
# }
# 
# 
# 

#### renderMap_vivienda_new NO COROPLETICO ####
renderMap_vivienda_new <- function(data, value_col, value_col_region, map_zoom) {
  
  carto_api_key <- Sys.getenv("CARTO_API_KEY")
  
  if (carto_api_key == "") {
    warning(
      "CARTO_API_KEY no está configurada. ",
      "Verifica el archivo .Renviron."
    )
  }
  
  carto_positron_url <- paste0(
    "https://{s}.basemaps.cartocdn.com/rastertiles/",
    "light_all/{z}/{x}/{y}.png?key=",
    carto_api_key
  )
  
  carto_attribution <- paste0(
    '&copy; <a href="https://www.openstreetmap.org/copyright" ',
    'target="_blank">OpenStreetMap</a> contributors ',
    '&copy; <a href="https://carto.com/attributions" ',
    'target="_blank">CARTO</a>'
  )
  # VERIFICAR DATOS
  if (
    nrow(data) == 0 ||
    !value_col %in% colnames(data) ||
    !value_col_region %in% colnames(data)
  ) {
    
    return(
      leaflet() %>%
        setView(
          lng = -66.5,
          lat = 18.2,
          zoom = map_zoom
        ) %>%
        addTiles(
          urlTemplate = carto_positron_url,
          attribution = carto_attribution,
          options = tileOptions(
            subdomains = "abcd",
            maxZoom = 20
          )
        )
    )
  }
  
  # PREPARAR DATOS
  data <- st_as_sf(data)
  
  data$region_map <- trimws(
    as.character(data[[value_col_region]])
  )
  
  data$casos_map <- suppressWarnings(
    as.numeric(data[[value_col]])
  )
  
  # Los valores faltantes se consideran 0
  data$casos_map[is.na(data$casos_map)] <- 0
  
  
  # COLORES FIJOS POR REGIÓN
  colores_regiones <- c(
    "Aguadilla" = "#6A4C93",
    "Arecibo"   = "#1982C4",
    "Bayamón"   = "#3A86A8",
    "Caguas"    = "#008C95",
    "Carolina"  = "#52A675",
    "Guayama"   = "#80C41C",
    "Humacao"   = "#FFBE2E",
    "Mayagüez"  = "#8338EC",
    "Ponce"     = "#F4A261",
    "San Juan"  = "#E76F51"
  )
  
  
  # NORMALIZAR NOMBRES PARA HACER MATCH
  region_key <- toupper(
    trimws(
      data$region_map
    )
  )
  
  colores_key <- toupper(
    names(colores_regiones)
  )
  
  
  # ASIGNAR COLOR A CADA POLÍGONO
  data$color_region <- unname(
    colores_regiones[
      match(
        region_key,
        colores_key
      )
    ]
  )
  
  
  # COLOR DE RESPALDO
  data$color_region[
    is.na(data$color_region)
  ] <- "#BDBDBD"
  
  
  # FORMATEAR CASOS
  data$casos_label <- format(
    data$casos_map,
    big.mark = ",",
    decimal.mark = ".",
    scientific = FALSE,
    trim = TRUE
  )
  
  
  # LABEL INTERACTIVO
  labels <- paste0(
    "<div class='map-region-label'>",
    "<div class='map-region-label-title'>", data$region_map, "</div>",
    "<div class='map-region-label-value'>", value_col, ": <strong>", data$casos_label, "</strong></div>",
    "</div>"
  )
  
  
  # CREAR MAPA
  mapa <- leaflet(data) %>%
    
    setView(
      lng = -66.5,
      lat = 18.2,
      zoom = map_zoom
    ) %>%
    
    # CARTO POSITRON
    addTiles(
      urlTemplate = carto_positron_url,
      attribution = carto_attribution,
      options = tileOptions(
        subdomains = "abcd",
        maxZoom = 20
      )
    ) %>%
    
    # POLÍGONOS
    addPolygons(
      
      fillColor = ~color_region,
      
      fillOpacity = 0.78,
      
      color = "#FFFFFF",
      
      opacity = 1,
      
      weight = 2,
      
      smoothFactor = 0.4,
      
      label = lapply(
        labels,
        htmltools::HTML
      ),
      
      labelOptions = labelOptions(
        direction = "auto",
        textsize = "12px",
        className = "map-region-label-container"
      ),
      
      highlightOptions = highlightOptions(
        weight = 3,
        color = "#333333",
        fillOpacity = 0.90,
        bringToFront = TRUE
      )
    )
  
  
  # POSICIÓN DE LOS NÚMEROS
  indices_validos <- which(
    is.finite(data$casos_map)
  )
  
  
  if (length(indices_validos) > 0) {
    
    # PUNTO QUE SIEMPRE QUEDA DENTRO DEL POLÍGONO
    
    puntos <- st_point_on_surface(
      data[indices_validos, ]
    )
    
    coords <- st_coordinates(
      puntos
    )
    
    # NÚMERO DE CASOS SOBRE CADA REGIÓN
    value_labels <- paste0(
      
      "<div class='vivienda-new-casos-label'>",
      
      data$casos_label[indices_validos],
      
      "</div>"
    )
    
    
    mapa <- mapa %>%
      
      addLabelOnlyMarkers(
        
        lng = coords[, 1],
        lat = coords[, 2],
        
        label = lapply(
          value_labels,
          htmltools::HTML
        ),
        
        labelOptions = labelOptions(
          
          noHide = TRUE,
          
          direction = "center",
          
          textOnly = TRUE,
          
          className = "vivienda-new-casos-marker"
        )
      )
  }
  
  
  # ESTILOS CSS se encuentran en www/styles.css
  
  # LEYENDA PERSONALIZADA
  
  legend_items <- paste0(
    mapply(
      function(region, color) {
        
        paste0(
          "<div style='display:flex; align-items:center; ",
          "margin:4px 0; font-size:12px; color:#555;'>",
          
          "<span style='display:inline-block; ",
          "width:14px; height:14px; ",
          "min-width:14px; min-height:14px; ",
          "background-color:", color, "; ",
          "border-radius:4px; ",
          "margin-right:8px; ",
          "border:1px solid rgba(0,0,0,0.20);'></span>",
          
          "<span>",
          region,
          "</span>",
          
          "</div>"
        )
      },
      
      names(colores_regiones),
      colores_regiones,
      
      SIMPLIFY = TRUE
    ),
    
    collapse = ""
  )
  
  
  legend_html <- paste0(
    
    "<div class='map-legend'>",
    "<div class='map-legend-title'>",
    
    "Regiones",
    
    "</div>",
    
    legend_items,
    
    "</div>"
  )
  
  
  mapa <- mapa %>%
    
    addControl(
      html = legend_html,
      position = "bottomright"
    )
  
  mapa <- mapa %>%
    htmlwidgets::onRender("
    function(el, x) {

      var map = this;

      // =====================================================
      // MAP HANDLERS
      // =====================================================

      if (window.MapHandlers) {

        MapHandlers.init(map, {

          bounds: [
            [17.85, -67.30],
            [18.55, -65.20]
          ],

          totals: {},

          legendTitle: 'Casos',

          infoTitle: 'Regiones'

        });

      }

    }
  ")
  
  # RETORNAR MAPA
  mapa
}


#### renderMap_vivienda_municipio ####
renderMap_vivienda_municipio <- function(municipios_geo, value_col,
                                         value_col_region, map_zoom) {
  
  # =========================================================
  # CARTO BASemap
  # =========================================================
  
  carto_api_key <- Sys.getenv("CARTO_API_KEY")
  
  carto_positron_url <- paste0(
    "https://{s}.basemaps.cartocdn.com/rastertiles/",
    "light_all/{z}/{x}/{y}.png?key=",
    carto_api_key
  )
  
  carto_attribution <- paste0(
    '&copy; <a href="https://www.openstreetmap.org/copyright" ',
    'target="_blank">OpenStreetMap</a> contributors ',
    '&copy; <a href="https://carto.com/attributions" ',
    'target="_blank">CARTO</a>'
  )
  
  # =========================================================
  # VERIFICAR DATOS
  # =========================================================
  
  if (!"Cantidad" %in% colnames(municipios_geo)) {
    return(
      leaflet() %>%
        addTiles()
    )
  }
  
  # Obtener valores desde la columna Cantidad
  values <- municipios_geo$Cantidad
  
  # =========================================================
  # ESCALA DE COLORES
  # =========================================================
  
  min_val <- 0
  max_val <- max(values, na.rm = TRUE)
  
  if (!is.finite(max_val)) {
    return(
      leaflet() %>%
        setView(lng = -66.5, lat = 18.2, zoom = map_zoom) %>%
        addTiles(
          urlTemplate = carto_positron_url,
          attribution = carto_attribution,
          options = tileOptions(
            subdomains = "abcd",
            maxZoom = 20
          )
        )
    )
  }
  
  rango <- max_val - min_val
  
  step <- ifelse(rango >= 30, 10, 5)
  
  bins <- seq(
    floor(min_val / step) * step,
    ceiling(max_val / step) * step,
    by = step
  )
  
  # Evitar problemas cuando solo existe un valor
  if (length(bins) < 2) {
    bins <- c(min_val, min_val + step)
  }
  
  violet_colors <- c(
    "#f9f5ff", "#f4e3ff", "#edd0ff", "#e2b3ff",
    "#d89aff", "#cc80ff", "#bf66ff", "#a64dff",
    "#8c33ff", "#701aff", "#5900f2", "#4400cc",
    "#3000a6", "#1d007d", "#0d004c"
  )
  
  num_bins <- length(bins) - 1
  
  colors <- violet_colors[
    seq_len(min(num_bins, length(violet_colors)))
  ]
  
  pal <- colorBin(
    colors,
    domain = values,
    bins = bins,
    na.color = "transparent"
  )
  
  # =========================================================
  # MAPA
  # =========================================================
  
  leaflet(municipios_geo) %>%
    setView(
      lng = -66.5,
      lat = 18.2,
      zoom = map_zoom
    ) %>%
    
    addTiles(
      urlTemplate = carto_positron_url,
      attribution = carto_attribution,
      options = tileOptions(
        subdomains = "abcd",
        maxZoom = 20
      )
    ) %>%
    
    addPolygons(
      fillColor = ~pal(Cantidad),
      weight = 1,
      opacity = 1,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      
      label = ~paste0(
        "Municipio: ", municipio, "<br>",
        "Cantidad: ", Cantidad
      ) %>%
        lapply(htmltools::HTML),
      
      highlightOptions = highlightOptions(
        weight = 1,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.9,
        bringToFront = TRUE
      )
    ) %>%
    
    addLegend(
      pal = pal,
      values = ~Cantidad,
      opacity = 0.7,
      title = "Cantidad",
      position = "bottomright",
      labFormat = labelFormat(digits = 0)
    )
}

#### definitionCards ####
definitionCards <- function(definitions) {
  card_list <- lapply(definitions, function(def) {
    tags$div(class = "definition-card",
             tags$h2(class = "definition-card-title", def$word),
             tags$p(class = "definition-card-text", def$definition)
    )
  })
  do.call(tagList, card_list)
}

#### publicationCardPDF ####
publicationCardPDF <- function(id, title, pdf_file, height = "300px") {
  preview_file <- sub("\\.pdf$", "_preview.png", pdf_file)
  if (!file.exists(file.path("www", preview_file))) {
    pdf_convert(pdf = file.path("www", pdf_file), format = "png", pages = 1,
                filenames = file.path("www", preview_file))
  }
  ns <- NS(id)
  div(class = "card shadow-sm p-3 mb-4 bg-white rounded publication-card",
      tags$h4(title),
      tags$img(src = preview_file, width = "100%", height = height, class = "publication-preview"),
      br(), downloadButton(ns("download_pdf"), "Descargar PDF")
  )
}

#### publicationCardPDFServer ####
publicationCardPDFServer <- function(id, pdf_file) {
  moduleServer(id, function(input, output, session) {
    output$download_pdf <- downloadHandler(
      filename = function() { basename(pdf_file) },
      content = function(file) {
        file.copy(file.path("www", pdf_file), file)
      }
    )
  })
}



#### publicationCardWeb ####
publicationCardWeb <- function(title, url, height = "500px") {
  safe_id <- gsub("[^a-zA-Z0-9]", "_", title)
  div(id = paste0("publication-web-", safe_id),
      class = "card shadow-sm p-3 mb-4 bg-white rounded publication-card publication-web-card",
      tags$h4(title, class = "publication-card-title"),
      tags$iframe(src = url, width = "100%", height = height, class = "publication-iframe",
                  sandbox = "allow-scripts allow-same-origin allow-forms"),
      br(),
      tags$a(href = url, target = "_blank", class = "btn btn-primary", "Abrir en una nueva pestaña")
  )
}

#### setColorFill ####
setColorFill <- function(df, variable) {
  # Obtener los niveles únicos de la variable
  unique_levels <- unique(df[[variable]])
  num_colors <- length(unique_levels)
  
  # Colores base de la página web
  base_colors <- c("#8bc344", "#884e9f", "#2f2e7d", "#e84924", "#d5dc30", "#5eb7df", "#3e3f3a")
  
  # Expandir la paleta manteniendo armonía con los colores base
  extended_palette <- if (num_colors > length(base_colors)) {
    colorRampPalette(base_colors)(num_colors)
  } else {
    base_colors[1:num_colors]
  }
  
  # Convertir colores a espacio LAB para calcular distancias perceptuales
  colors_rgb <- col2rgb(extended_palette) / 255 # Normalizar a rango 0-1
  colors_lab <- convertColor(t(colors_rgb), from = "sRGB", to = "Lab")
  
  # Función para calcular la distancia entre colores
  color_distance <- function(c1, c2) sqrt(sum((c1 - c2) ^ 2))
  
  # Filtrar colores que sean muy similares (distancia perceptual < 15)
  unique_colors <- list(colors_lab[1, ])
  final_colors <- extended_palette[1]
  
  for (i in 2:nrow(colors_lab)) {
    distances <- sapply(unique_colors, color_distance, c2 = colors_lab[i, ])
    if (all(distances >= 15)) { # Distancia mínima para diferenciación
      unique_colors <- append(unique_colors, list(colors_lab[i, ]))
      final_colors <- c(final_colors, extended_palette[i])
    }
  }
  
  # Asegurar que haya suficientes colores; si no, generar variaciones adicionales
  if (length(final_colors) < num_colors) {
    warning("Algunos colores eran muy similares. Generando colores adicionales.")
    final_colors <- colorRampPalette(final_colors)(num_colors)
  }
  
  # Asignar colores a cada nivel único
  my_fill <- setNames(final_colors, unique_levels)
  return(my_fill)
}

#### plotHeight ####
# Función para calcular el numero de facetas en una grafica basado
# en la cantidad de filas.
# plotHeight <- function(plot_height, selected_plots) {
# 
#   height <- plot_height * selected_plots
# 
#   print(height)
# 
#   return(height)
# }

# plotHeight <- function(plot_height, selected_plots){
#   
#   num_selected_plots = selected_plots
#   num_rows = (num_selected_plots)
#   
#   if(num_selected_plots > 1){
#     height = (350*num_rows)
#   } else {
#     height = plot_height
#   }
#   print(height)
#   
#   return(height)
# }
# 
plotHeight <- function(plot_height, selected_plots){
  
  num_selected_plots = selected_plots
  num_rows = (num_selected_plots/2)
  
  if(num_selected_plots > 4){
    height = (350*num_rows)
  } else {
    height = plot_height
  }
  print(height)
  
  return(height)
}

#### cacl_Y_Axis ####
#Funcion para calcular la posicion de la leyenda en Y
#Esto cada vez que una fila en las graficas de facetas se elimina

cacl_Y_Axis <- function(current_rows){
  
  num_rows = ceiling(current_rows/2)
  
  if (num_rows == 2 || num_rows == 1) {
    y = 1.05
  } else if (num_rows == 3) {
    y = 1.03
  } else if (num_rows == 4) {
    y = 1.03
  } else if (num_rows == 5) {
    y = 1.02
  }else if (num_rows == 6) {
    y = 1.01
  } else {
    y = 1.01
  }
  
  return(y)
}



#######################
#### CODIGO MUERTO ####
#######################

# create_empty_plot_with_message_forLine <- function(data, x, y, fill, title, xlab, ylab, emptyMessage) {
#   data_df <- data()
#   ggplot(data_df, aes_string(x = x, y = y)) +
#     geom_blank() +  
#     labs(title = title, x = xlab, y = ylab) +
#     theme_minimal() +
#     theme(
#       axis.text.x = element_blank(),
#       axis.ticks = element_blank(),  
#       axis.text.y = element_blank(),
#       axis.title.x = element_text(size = 12, margin = margin(t = 10)), 
#       axis.title.y = element_text(size = 12, margin = margin(r = 10)),  
#       plot.title = element_text(hjust = 0.5, size = 15, colour = "black", face = "bold"),
#       panel.border = element_rect(colour = "black", fill = NA, size = 1),
#       plot.margin = margin(t = 50, r = 10, b = 10, l = 10)
#     ) +
#     annotate("text", x = 0.5, y = 0.5, label = emptyMessage, size = 4, hjust = 0.5, vjust = 0.5)
# }

# Renderiza un gráfico de lineas utilizando ggplot2 en el UI de Shiny.
# renderLinePlot <- function(data, x, y, group, color, title, xlab, ylab, colorlab = color, emptyMessage) {
# 
#   data_df <- data()  # Evaluar los datos reactivos una vez
# 
#   if (is.null(data_df) || nrow(data_df) == 0 || is.null(data_df[[x]])) {
#     # Si no hay datos o las variables x/y son nulas, mostrar una gráfica vacía con ejes y un mensaje
#     p <- create_empty_plot_with_message_forLine(data, x, y, fill, title, xlab, ylab,emptyMessage)
#     
#     return(p)
#     # Si no hay datos o las variables x/y son nulas, mostrar una gráfica vacía con ejes y un mensaje
#     # p <- ggplot(data_df, aes_string(x = x, y = y, group = group, color = color)) +
#     #   geom_blank() +  
#     #   labs(title = title, x = xlab, y = ylab) +
#     #   theme_minimal() +
#     #   theme(
#     #     axis.text.x = element_blank(),
#     #     axis.ticks = element_blank(), 
#     #     axis.text.y = element_blank(), 
#     #     axis.title.x = element_text(size = 12, margin = margin(t = 10)),  
#     #     axis.title.y = element_text(size = 12, margin = margin(r = 10)),  
#     #     plot.title = element_text(hjust = 0.5, size = 13, colour = "black", face = "bold"),
#     #     panel.border = element_rect(colour = "black", fill = NA, size = 1),
#     #     plot.margin = margin(t = 50, r = 10, b = 10, l = 10)
#     #   ) +
#     #   annotate("text", x = 0.5, y = 0.5, label = emptyMessage, size = 6, hjust = 0.5, vjust = 0.5)
#     # 
#     # return(p)
# 
#   } else {
#     # Calcular rango del eje de y
#     y_max <- max(data_df[[y]], na.rm = TRUE)
#     y_interval <- pretty(c(0, y_max), n = 5)[2]  
#     upper_y_limit <- ceiling(y_max / y_interval) * y_interval  
# 
# 
#     p <- ggplot(data_df, aes_string(x = x, y = y, group = group, color = color)) +
#       geom_line(color = "#2f2e7d", size = 1) +  
#       geom_point(color = "#adcc4e", size = 2,
#                  aes(
#                    text =
#                      paste0("<b>", xlab, ":</b> ", .data[[x]],
#                             "<br><b>", ylab, ":</b> ", .data[[y]]
#                      )
#                  )
#       ) +
#       expand_limits(y = 0) +  # Asegurar que el eje y comience en 0
#       labs(title = title, x = xlab, y = ylab, color = colorlab) +
#       scale_y_continuous(labels = function(x) scales::comma_format(big.mark = ",", decimal.mark = ".")(x) %>% paste0(" "),
#                          expand = expansion(mult = c(0, 0.1))) +
#       coord_cartesian(ylim = c(0, upper_y_limit)) +
#       theme_minimal() +
#       theme(
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.title = element_text(hjust = 0.5, size = 15, colour = "black", face = "bold"),
#         panel.border = element_rect(colour = "black", fill = NA, size = 1),
#         plot.margin = margin(t = 45, r = 10, b = 10, l = 10)
#       )
# 
#     return(p)
#   }
# }

# Crea un gráfico vacio indicando al usuario que debe seleccionar las variables a visualizar
# create_empty_plot_with_message <- function(data, x, y, fill, title, xlab, ylab, emptyMessage) {
#   data_df <- data()
#   ggplot(data_df, aes_string(x = x, y = y)) +
#     geom_blank() +  
#     labs(title = title, x = xlab, y = ylab) +
#     theme_minimal() +
#     theme(
#       axis.text.x = element_blank(),
#       axis.ticks = element_blank(),  
#       axis.text.y = element_blank(),
#       axis.title.x = element_text(size = 12, margin = margin(t = 10)), 
#       axis.title.y = element_text(size = 12, margin = margin(r = 10)),  
#       plot.title = element_text(hjust = 0.5, size = 13, colour = "black", face = "bold"),
#       panel.border = element_rect(colour = "black", fill = NA, size = 1),
#       plot.margin = margin(t = 100, r = 50, b = 100, l = 50)
#     ) +
#     annotate("text", x = 0.5, y = 0.5, label = emptyMessage, size = 4, hjust = 0.5, vjust = 0.5)
# }

# renderBarPlot <- function(data, x, y, fill, title, xlab, ylab, fillLab = fill, colorFill = "Set1",
#                           emptyMessage, barWidth = 1, xGap = 0.1) {
#   
#   data_df <- data()  # Evaluate the reactive data once
#   
#   if (is.null(data_df) || nrow(data_df) == 0 || is.null(data_df[[x]]) || is.null(data_df[[y]]) || is.null(data_df[[fill]])) {
#     # If there are no data or x/y variables are null, display an empty plot with axes and a message
#     p <- create_empty_plot_with_message(data, x, y, fill, title, xlab, ylab, emptyMessage)
#     
#     return(p)
#   } else {
#     
#     upper_y_limit <- ceiling(max(eval(parse(text = paste0("data()$", y))), na.rm = TRUE) * 1.2) 
#     
#     p <- ggplot(data_df, aes_string(x = x, y = y, fill = fill)) +
#       geom_bar(stat = "identity",
#                position = position_dodge2(width = barWidth, padding = xGap),
#                width = 0.7,
#                aes(
#                  text = paste(
#                    paste0("<b>", ylab, ":</b> ", after_stat(y)), "<br>",
#                    paste0("<b>", fillLab, ":</b> ", after_stat(fill)), "<br>"
#                  )
#                )) +
#       scale_fill_manual(values = colorFill) +
#       scale_y_continuous(labels = function(x) scales::comma_format(big.mark = ",", decimal.mark = ".")(x) %>% paste0(" "),
#                          expand = expansion(mult = c(0, 0.1))) +
#       coord_cartesian(ylim = c(0, upper_y_limit)) +
#       labs(title = title, x = xlab, y = ylab, fill = fillLab) +
#       theme_minimal()  # Usar tema básico sin estilos específicos
#     
#     return(p)
#   }
# }



# 
# convert_to_plotly <- function(p, tooltip_value, width = "100%", height = "100%") {
#   # Obtener los títulos de los ejes y el título del gráfico desde el objeto ggplot
#   x_axis_title <- p$labels$x
#   y_axis_title <- p$labels$y
#   plot_title <- p$labels$title
#   legend_title <- p$labels$colour %||% p$labels$fill
#   
#   # Convertir ggplot en un objeto plotly
#   p_plotly <- ggplotly(p, tooltip = tooltip_value)
#   
#   # Ajustar el diseño
#   p_plotly <- p_plotly %>% layout(
#     autosize = TRUE,
#     title = list(
#       text = plot_title,
#       font = list(size = 18, family = "Arial", color = "black", weight = "bold")
#     ),
#     legend = list(
#       x = 1.05,
#       y = 0.5,
#       xanchor = "left",
#       yanchor = "middle",
#       font = list(size = 12, family = "Arial", color = "black"),
#       title = list(
#         text = if (!is.null(legend_title)) paste0(legend_title, "\n") else NULL,
#         font = list(size = 14, family = "Arial", color = "black", weight = "bold")
#       )
#     ),
#     margin = list(t = 100, r = 50, b = 100, l = 50),
#     xaxis = list(
#       title = list(text = x_axis_title, standoff = 10),
#       tickfont = list(size = 12, family = "Arial", color = "black"),
#       automargin = TRUE
#     ),
#     yaxis = list(
#       title = list(text = y_axis_title, standoff = 10),
#       tickfont = list(size = 12, family = "Arial", color = "black"),
#       automargin = TRUE
#     )
#   )
#   
#   return(p_plotly)
# }



# # Renderiza un mapa utilizando ggplot2 en el UI del ShinyApp
# renderMap <- function(data, fill, title, group, fill_lab = fill,
#                       light_color = "lightblue", dark_color = "darkblue") {
#   p <- ggplot(data()) +
#     geom_sf(aes(fill = {{fill}}, group = {{group}})) +  # Incluye group como aesthetic mapping
#     # labs(title = title, fill = fill_lab) +
#     labs(fill = fill_lab) +
#     scale_fill_gradient(name = fill_lab, low = light_color, high = dark_color) +
#     theme_minimal() +
#     theme(
#       legend.position = "left",
#       axis.text = element_blank(),
#       axis.ticks = element_blank(),
#       panel.grid = element_blank(),
#       panel.border = element_rect(colour = "black", fill = NA, size = 1),
#       plot.margin = margin(20, 10, 10, 10),
#       # plot.title = element_text(hjust = 0.5)
#     )
#   print(p)
# }

# renderMapGroup <- function(data, fill, title, fill_lab = fill) {
#   p <- ggplot(data) +
#     geom_sf(aes(fill = {{fill}})) +
#     labs(title = title, fill = fill_lab) +
#     #scale_fill_gradient(name = fill_lab, low = light_color, high = dark_color) +
#     theme_minimal() +
#     theme(
#       legend.position = "bottom",
#       axis.text = element_blank(),
#       axis.ticks = element_blank(),
#       panel.grid = element_blank(),
#       plot.title = element_text(hjust = 0.5)
#     )
#   print(p)
# }

# Genera una paleta de colores para los niveles de una variable categórica.
# Esta función toma un dataframe y el nombre de una variable categórica, y devuelve una paleta de colores con un 
# color único para cada nivel de la variable.
# setColorFill <- function(df, variable) {
#   # Obtener los niveles únicos de la variable
#   unique_levels <- unique(df[[variable]])
#   
#   # Elegir una paleta de colores apropiada para el número de niveles únicos
#   num_colors <- length(unique_levels)
#   if (num_colors <= 8) {
#     palette <- brewer.pal(n = num_colors, name = "Set1")
#   } else {
#     palette <- rainbow(num_colors)
#   }
#   
#   # Generar una lista de patrones para los colores
#   patterns <- c("#CC6677", "#E69F00", "#88CCEE", "#B8E186", "#332288", "#D55E00", 
#                 "#F0E442", "#CC79A7", "#661100", "#888888", "#117733", "#000000")
#   # Asignar patrones y colores a cada nivel único
#   my_fill <- rep("black", length(unique_levels))
#   names(my_fill) <- unique_levels
#   for (i in 1:length(unique_levels)) {
#     my_fill[i] <- ifelse(i <= length(patterns), patterns[i], palette[i])
#   }
#   
#   return(my_fill)
# }

# 
# setColorFill <- function(df, variable) {
#   # Obtener los niveles únicos de la variable
#   unique_levels <- unique(df[[variable]])
#   num_colors <- length(unique_levels)
#   
#   # Colores base de la página web
#   base_colors <- c("#8bc344", "#884e9f", "#2f2e7d", "#e84924", "#d5dc30", "#5eb7df", "#3e3f3a")
#   
#   # Expandir la paleta manteniendo armonía con los colores base
#   if (num_colors > length(base_colors)) {
#     extended_palette <- colorRampPalette(base_colors)(num_colors)
#   } else {
#     extended_palette <- base_colors[1:num_colors]
#   }
#   
#   # Convertir colores a LAB para calcular distancias perceptuales
#   colors_rgb <- col2rgb(extended_palette)
#   colors_lab <- convertColor(t(colors_rgb / 255), from = "sRGB", to = "Lab")
#   
#   # Función para calcular la distancia entre colores
#   color_distance <- function(c1, c2) {
#     sqrt(sum((c1 - c2) ^ 2))
#   }
#   
#   # Eliminar colores demasiado similares (distancia perceptual < 10)
#   unique_colors <- list(colors_lab[1, ])
#   final_colors <- extended_palette[1]
#   
#   for (i in 2:nrow(colors_lab)) {
#     distances <- sapply(unique_colors, color_distance, c2 = colors_lab[i, ])
#     if (all(distances >= 15)) { # Distancia mínima para diferenciación
#       unique_colors <- append(unique_colors, list(colors_lab[i, ]))
#       final_colors <- c(final_colors, extended_palette[i])
#     }
#   }
#   
#   # Asegurar que haya suficientes colores; si no, se interpolan nuevos
#   if (length(final_colors) < num_colors) {
#     warning("Algunos colores eran muy similares. Generando colores adicionales.")
#     final_colors <- colorRampPalette(final_colors)(num_colors)
#   }
#   
#   # Asignar colores a cada nivel único
#   my_fill <- setNames(final_colors, unique_levels)
#   return(my_fill)
# }


