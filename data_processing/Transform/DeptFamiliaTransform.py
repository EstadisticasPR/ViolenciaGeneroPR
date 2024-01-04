import pandas as pd
from pathlib import Path

# Path para los datos del Departamento de Familia
dfam = Path("data", "Departamento_de_Familia")

# Importar los datos de maltratos y asignar el año a cada conjunto de datos
dfMalt2018 = pd.read_excel(dfam / "dfMalt2018.xlsx")
dfMalt2018['Año'] = "2018"

dfMalt2019 = pd.read_excel(dfam / "dfMalt2019.xlsx")
dfMalt2019['Año'] = "2019"

dfMalt2020 = pd.read_excel(dfam / "dfMalt2020.xlsx")
dfMalt2020['Año'] = "2020"

dfMalt2021 = pd.read_excel(dfam / "dfMalt2021.xlsx")
dfMalt2021['Año'] = "2021"

dfMalt2022 = pd.read_excel(dfam / "dfMalt2022.xlsx")
dfMalt2022['Año'] = "2022"

# Unir todos los conjuntos de datos en uno solo
dfs = [dfMalt2018, dfMalt2019, dfMalt2020, dfMalt2021, dfMalt2022]
dfMalt = pd.concat(dfs)

# Renombrar columnas y pivotear
dfMalt = dfMalt.rename(columns={"Cantidad Masculino": "Masculino", "Cantidad Femenino": "Femenino"})
dfMalt = dfMalt.melt(id_vars=["Tipo de Maltrato", "Año"], var_name="Sexo", value_name="Casos")

# Convertir a factores
dfMalt['Tipo de Maltrato'] = pd.Categorical(dfMalt['Tipo de Maltrato'])
dfMalt['Año'] = pd.Categorical(dfMalt['Año'])
dfMalt['Sexo'] = pd.Categorical(dfMalt['Sexo'])

print(dfMalt)

