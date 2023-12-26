# Archivo: install.R

# Lista de paquetes necesarios para compilar los scripts
packages <- c(
  "tidyverse",
  "readxl",
  "kableExtra",
  "zoo",
  "here",
  "viridis"
)

# Instalar paquetes si no están instalados
installed_packages <- installed.packages()
packages_to_install <- setdiff(packages, installed_packages[, "Package"])
if (length(packages_to_install) > 0) {
  install.packages(packages_to_install)
}
