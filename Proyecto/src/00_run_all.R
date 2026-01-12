# Script Maestro: Ejecución del Pipeline Completo
# Objetivo: Correr todo el proyecto en orden secuencial.

# Limpiar entorno
rm(list = ls())
cat("\014") # Limpiar consola

message(">>> INICIANDO EJECUCIÓN DEL PROYECTO GRUPO 2 - ANT VELOCIDAD <<<")
start_time <- Sys.time()

# 0. Configuración Inicial
set.seed(123) # Para reproducibilidad futura en procesos estocásticos
if (!dir.exists("reports")) dir.create("reports")

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

# 5. Generación de Reporte
message("\n[5/5] Generando Reporte Académico (HTML) ...")
if (!require(rmarkdown)) install.packages("rmarkdown")
library(rmarkdown)

render(
  input = "reports/TA-Proyecto_Grupo2_AvanceCodigoR.Rmd",
  output_dir = "reports",
  quiet = TRUE
)
message("Reporte generado en: reports/TA-Proyecto_Grupo2_AvanceCodigoR.html")

end_time <- Sys.time()
duracion <- end_time - start_time

message("\n>>> EJECUCIÓN COMPLETADA CON ÉXITO <<<")
message("Tiempo total: ", round(duracion, 2), " ", units(duracion))
message(" RESULTADOS FINALES: ")
message("  1. Datos: data/clean/dataset_limpio.csv")
message("  2. Gráficos: figures/")
message("  3. Parámetros: params/params.csv")
message("  4. Reporte: reports/TA-Proyecto_Grupo2_AvanceCodigoR.html")

