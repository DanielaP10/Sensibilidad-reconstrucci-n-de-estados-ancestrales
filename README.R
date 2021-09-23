Para realizar las reconstrucciones de dieta ancestral se corrieron los sieguientes códigos, mencionados en su orden de ejecución:

# Llamado de la tabla de dietas, contrucción de las matrices codificaciones de estado, llamado de árbol, poda y organización de las matrices para las reconstrucciones.
source(Script1_ArbolesYMatrices.Tesis.R)

# Reconstrucciones de estado con Máxima verosimilitud usando tasas de cambio iguales.
source(reconstrucciones_Mk_ER.Tesis)

# Reconstrucciones de estado con Máxima verosimilitud usando tasas de cambio desiguales.
source(reconstrucciones_Mk_DR.Tesis.R)

# Reconstrucciones de estado con Parsimonia usando tasas de cambio iguales y desiguales.
soruce(econstrucciones_Parsimonia.Tesis.R)

# Transformación de los resultados de Máxima verosimilitud y Parsimonia para facilitar las comparaciones.
source(sensibilidad-0.Discretizaciones y tales.Tesis.R)

# Comparaciones entre los resultados de las reconstrucciones según la codificación de estado.
source(sensibilidad-1.Codificaciones.Tesis.R)

# Comparaciones entre los resultados de las reconstrucciones según los métodos filogenéticos.
source(sensibilidad-2.Metodos.Tesis.R)

# Comparaciones entre los resultados de las reconstrucciones según las matrices de cambio.
source(sensibilidad-3.Transformaciones.Tesis.R)

# Cálculo del número de nodos relativos, gráficas y tablas.
source(graficas.Tesis.R)

