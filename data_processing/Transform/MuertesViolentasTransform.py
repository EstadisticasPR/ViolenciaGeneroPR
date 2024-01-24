import pandas as pd
from pathlib import Path

# Path para Sistema_de_Notificacion_de_Muertes_Violentas
snmv = Path("data", "Sistema_de_Notificacion_de_Muertes_Violentas")

# Importar los datos de homicidios por grupo de edad; homiEdad
homiEdad = pd.read_excel(snmv / "svmvhomiEdad.xlsx")
homiEdad = homiEdad.drop(columns='Total') \
                   .loc[~homiEdad['Grupo de edad'].str.contains("Total") & (homiEdad['Grupo de edad'] != "Desconocido")] \
                   .melt(id_vars='Grupo de edad', var_name='año', value_name='casos') \
                   .rename(columns={'Grupo de edad': 'edad'}) \
                   .astype({'edad': 'category'})

print(homiEdad)