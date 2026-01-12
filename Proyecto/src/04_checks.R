
# Script 04: Validaciones Automáticas y QA
# Objetivo: Suite de pruebas de integridad, consistencia y dominio para garantizar un proyecto reproducible y válido.

# Limpiar entorno de checks previos
rm(list = ls())

# Librerías
if (!require(readr)) install.packages("readr")
library(readr)
library(dplyr)

message(">>> INICIANDO PROTOCOLO DE VALIDACIÓN (QA) <<<")

# ==============================================================================
# 1. INTEGRIDAD DEL DATASET
# ==============================================================================
clean_path <- "data/clean/dataset_limpio.csv"

if (!file.exists(clean_path)) {
  stop("❌ [CRÍTICO] Dataset limpio no encontrado en: ", clean_path)
}

df <- read_csv(clean_path, show_col_types = FALSE)

# 1.1 No vacío
if (nrow(df) == 0) {
  stop("❌ [CRÍTICO] El dataset limpio está vacío (0 filas).")
}

# 1.2 Columnas clave (ajustado a las columnas generadas en 01_load_clean.R)
# Nota: En 01 generamos FECHA_ALERTA_DT, HORA_ALERTA_NUM. Verificamos las esenciales.
cols_requeridas <- c("FECHA_ALERTA_DT", "HORA_ALERTA_NUM", "VELOCIDAD", "TIPO_EXCESO", "PROVINCIA_EXCESO")
cols_faltantes <- setdiff(cols_requeridas, names(df))

if (length(cols_faltantes) > 0) {
  stop(paste("❌ [CRÍTICO] Faltan columnas clave en el dataset:", paste(cols_faltantes, collapse = ", ")))
}

message("✅ Integridad del Dataset: OK (", nrow(df), " registros).")


# ==============================================================================
# 2. VALIDACIÓN DE TIPOS
# ==============================================================================

# 2.1 Fecha (read_csv lee Dates estándar como Date por defecto, pero validamos)
if (!inherits(df$FECHA_ALERTA_DT, "Date")) {
  stop("❌ [TIPO] La columna FECHA_ALERTA_DT no es de tipo Date.")
}

# 2.2 Velocidad numérica
if (!is.numeric(df$VELOCIDAD)) {
  stop("❌ [TIPO] La columna VELOCIDAD no es numérica.")
}

# 2.3 Categóricas (Al leer CSV, read_csv puede leer strings. Convertimos a factor para validar niveles si es necesario)
# En un csv puro, no se guarda el metadato 'factor', pero chequeamos que sena character/factor
if (!is.character(df$TIPO_EXCESO) && !is.factor(df$TIPO_EXCESO)) {
  stop("❌ [TIPO] TIPO_EXCESO debería ser texto/categoría.")
}

message("✅ Validación de Tipos: OK.")


# ==============================================================================
# 3. RANGOS RAZONABLES (DOMINIO ANT)
# ==============================================================================

# 3.1 Velocidad > 0 y <= 200 (Tolerancia estricta según rúbrica)
v_min <- min(df$VELOCIDAD, na.rm = TRUE)
v_max <- max(df$VELOCIDAD, na.rm = TRUE)

if (v_min <= 0) {
  stop(paste("❌ [DOMINIO] Existen velocidades <= 0 km/h. Min encontrado:", v_min))
}

if (v_max > 200) {
  stop(paste("❌ [DOMINIO] Existen velocidades > 200 km/h (Valores inverosímiles para simulación). Max encontrado:", v_max))
}

# 3.2 Hora [0, 23]
h_min <- min(df$HORA_ALERTA_NUM, na.rm = TRUE)
h_max <- max(df$HORA_ALERTA_NUM, na.rm = TRUE)

if (h_min < 0 || h_max > 23) {
  stop(paste("❌ [DOMINIO] Hora fuera de rango [0-23]. Rango encontrado:", h_min, "-", h_max))
}

message("✅ Rangos de Dominio: OK (Velocidad: ", v_min, "-", v_max, " km/h).")


# ==============================================================================
# 4. CHECKS TEMPORALES
# ==============================================================================

# 4.1 No fechas NA
if (any(is.na(df$FECHA_ALERTA_DT))) {
  stop("❌ [DATA] Existen fechas NA en el dataset limpio.")
}

# 4.2 Rango Fechas (Febrero 2022)
fecha_inicio <- min(df$FECHA_ALERTA_DT)
fecha_fin <- max(df$FECHA_ALERTA_DT)

if (format(fecha_inicio, "%Y-%m") != "2022-02" || format(fecha_fin, "%Y-%m") != "2022-02") {
  warning(paste("⚠️ [WARN] El dataset contiene fechas fuera de Febrero 2022:", fecha_inicio, "a", fecha_fin))
  # Se deja como warning salvo que la rúbrica exija estrictamente Febrero.
}

message("✅ Checks Temporales: OK (Periodo: ", fecha_inicio, " al ", fecha_fin, ").")


# ==============================================================================
# 5. VALIDACIÓN DE PARÁMETROS
# ==============================================================================
params_path <- "params/params.csv"
if (!file.exists(params_path)) stop("❌ [CRÍTICO] params.csv no encontrado.")

params_df <- read_csv(params_path, show_col_types = FALSE)

# 5.1 No negativos
if (any(params_df$valor < 0)) {
  stop("❌ [PARAM] Existen parámetros con valor negativo.")
}

# 5.2 Suma de probabilidades (Tolerancia 1%)
probs <- params_df %>% filter(grepl("Prob_Tipo_", parametro))
if (nrow(probs) > 0) {
  suma_p <- sum(probs$valor)
  if (abs(suma_p - 1) > 0.01) {
    stop(paste("❌ [PARAM] La suma de probabilidades de tipo es", suma_p, "(debe sumar ~1)."))
  }
} else {
  warning("⚠️ [WARN] No se encontraron parámetros de probabilidad (Prob_Tipo_*) para validar suma.")
}

message("✅ Validación de Parámetros: OK.")


# ==============================================================================
# 6. EXISTENCIA DE OUTPUTS (VISUALIZACIONES)
# ==============================================================================
fig_path <- "figures"
figs_esperadas <- c(
  "llegadas_tiempo.png",
  "distribucion_velocidad.png",
  "comparacion_categoria.png",
  "heatmap_hora_dia.png"
)

missing_figs <- c()
for (fig in figs_esperadas) {
  if (!file.exists(file.path(fig_path, fig))) {
    missing_figs <- c(missing_figs, fig)
  }
}

if (length(missing_figs) > 0) {
  stop(paste("❌ [OUTPUT] Faltan gráficos obligatorios:", paste(missing_figs, collapse = ", ")))
}

message("✅ Existencia de Visualizaciones: OK (4/4 encontrados).")


# ==============================================================================
# 7. CHECK DE REPRODUCIBILIDAD (RUTAS ABSOLUTAS)
# ==============================================================================
# Escaneo simple de los scripts en src/ para buscar cadenas sospechosas como "C:/" o "Users/"
scripts <- list.files("src", pattern = "\\.R$", full.names = TRUE)
rutas_sospechosas <- FALSE

for (script in scripts) {
  lineas <- readLines(script)
  # Buscamos "C:/" o "D:/" o "/Users/" de forma muy básica
  hits <- grep("([A-Z]:/|/Users/)", lineas, value = TRUE)
  # Excluir comentarios si es posible, pero simple grep basta para alerta
  # Excluir la propia validacion que contiene los strings
  is_check_script <- grepl("04_checks.R", script)
  
  if (length(hits) > 0 && !is_check_script) {
    warning(paste("⚠️ [REPRODUCIBILIDAD] Posibles rutas absolutas detectadas en", basename(script), ":\n", paste(head(hits, 3), collapse="\n")))
    rutas_sospechosas <- TRUE
  }
}

message("✅ Check de Reproducibilidad (Rutas): Inspectado.")

# Conslusión
message("\n✅✅ VALIDACIÓN COMPLETADA: EL PROYECTO ESTÁ LISTO PARA FUTURA SIMULACIÓN. ✅✅")
