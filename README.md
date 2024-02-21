# Estadísticas sobre Violencia de Género en Puerto Rico

Este proyecto proporciona una aplicación Shiny para explorar y visualizar datos relacionados con la violencia de género y los feminicidios en Puerto Rico.

## Tabla de Contenidos

- [Descripción](#descripción)
- [Requisitos](#requisitos)
- [Instalación y Uso](#instalación)

## Descripción

Este proyecto tiene como objetivo proporcionar una herramienta interactiva para analizar y comprender la incidencia de la violencia de género y los feminicidios en Puerto Rico. Utiliza datos públicos y ofrece visualizaciones interactivas para explorar diferentes aspectos de este tema importante.

Para experimentar la funcionalidad completa del proyecto, te invitamos a explorar nuestra [página web](https://estadisticas.pr/en/Comite-Pare).

## Requisitos

- R >= 3.6
- Paquetes: tidyverse, readxl, kableExtra, zoo, here, viridis, shinythemes, plotly, DT, shinyWidgets, sf, roxygen2

## Instalación y Uso

### Pasos de Instalación

1. **Instala R**: Si aún no tienes R instalado en tu sistema, puedes descargar e instalar la última versión desde el sitio web oficial de R: [https://www.r-project.org/](https://www.r-project.org/). Sigue las instrucciones de instalación adecuadas para tu sistema operativo.

2. **Instala el paquete desde GitHub**: Utiliza `remotes::install_github()` para instalar el paquete directamente desde GitHub 

```R
# Instala el paquete desde GitHub
install.packages("remotes")
library(remotes)
remotes::install_github("nombre_del_usuario/ViolenciaGeneroPR")
```

3. **Ejecuta la aplicación Shiny**: Después de instalar el paquete, puedes cargar la biblioteca y ejecutar la aplicación Shiny utilizando la función run_app()
```R
# Carga la biblioteca y ejecuta la aplicación Shiny
library(ViolenciaGeneroPR)
ViolenciaGeneroPR::run_app()
```