
# Script Maestro: Ejecución del Pipeline Completo
# Objetivo: Correr todo el proyecto en orden secuencial.

# Limpiar entorno
rm(list = ls())
cat("\014") # Limpiar consola

message(">>> INICIANDO EJECUCIÓN DEL PROYECTO GRUPO 2 - ANT VELOCIDAD <<<")
start_time <- Sys.time()

# Rutas - Asumiendo que el Working Directory es la raiz del proyecto 'Proyecto/'
# Si se ejecuta desde src/, ajustar. Se recomienda setwd() al proyecto.
# setwd("...") # Opcional si no se corre desde proyecto rproj

# 1. Carga y Limpieza
message("\n[1/4] Ejecutando 01_load_clean.R ...")
source("src/01_load_clean.R", encoding = "UTF-8")

# 2. Visualizaciones
message("\n[2/4] Ejecutando 02_visualizations.R ...")
source("src/02_visualizations.R", encoding = "UTF-8")

# 3. Parámetros
message("\n[3/4] Ejecutando 03_parameters.R ...")
source("src/03_parameters.R", encoding = "UTF-8")

# 4. Validaciones
message("\n[4/4] Ejecutando 04_checks.R ...")
source("src/04_checks.R", encoding = "UTF-8")

end_time <- Sys.time()
duracion <- end_time - start_time

message("\n>>> EJECUCIÓN COMPLETADA CON ÉXITO <<<")
message("Tiempo total: ", round(duracion, 2), " ", units(duracion))
message("Resultados generados en /data/clean, /figures, y /params.")
