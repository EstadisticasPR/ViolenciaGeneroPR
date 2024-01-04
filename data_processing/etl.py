import pandas as pd
from pathlib import Path

# Obtener la ruta base del proyecto
base_path = Path(__file__).resolve().parent.parent  # Ruta del directorio anterior al directorio del script actual

# Construir la ruta hacia la carpeta 'data' y 'Sistema_de_Notificacion_de_Muertes_Violentas'
data_path = base_path / 'data' / 'Sistema_de_Notificacion_de_Muertes_Violentas'

# Lectura del archivo Excel
path = data_path / "svmvhomiEdad.xlsx"
homiEdad = pd.read_excel(path)
print(homiEdad)

# Eliminar la fila 'Total'
homiEdad_long_xtotals = homiEdad[~homiEdad['Grupo de edad'].str.contains('Total') & (homiEdad['Grupo de edad'] != 'Desconocido')].drop(columns=['Total'])

# Convertir de ancho a largo (pivot_longer)
homiEdad_long_xtotals = homiEdad_long_xtotals.melt(id_vars='Grupo de edad', var_name='año', value_name='casos')

# Renombrar columna 'Grupo de edad' a 'edad'
homiEdad_long_xtotals.rename(columns={'Grupo de edad': 'edad'}, inplace=True)

# Convertir a tipo 'factor' (equivalente a 'factor' en R)
homiEdad_long_xtotals['edad'] = pd.Categorical(homiEdad_long_xtotals['edad'], categories=homiEdad_long_xtotals['edad'].unique())
homiEdad_long_xtotals['año'] = pd.Categorical(homiEdad_long_xtotals['año'])

# Imprimir los primeros registros como ejemplo
print(homiEdad_long_xtotals.head())

# Guardar el DataFrame como un nuevo archivo Excel
output_path = data_path / "svmvhomiEdad_long.xlsx"
homiEdad_long_xtotals.to_excel(output_path, index=False)