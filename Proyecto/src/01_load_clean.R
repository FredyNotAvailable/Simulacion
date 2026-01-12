
# Script 01: Carga y Limpieza de Datos
# Objetivo: Cargar datos crudos, estandarizar tipos, tratar faltantes y generar nuevas variables.

# 1. Cargar librerías necesarias
if (!require(readr)) install.packages("readr")
if (!require(dplyr)) install.packages("dplyr")
if (!require(lubridate)) install.packages("lubridate")
if (!require(stringr)) install.packages("stringr")

library(readr)
library(dplyr)
library(lubridate)
library(stringr)

# 2. Definir rutas (relativas)
raw_path <- "data/raw/ant-exceso-velocidad-febrero-2022.csv"
clean_path <- "data/clean/dataset_limpio.csv"
dict_path <- "diccionario.csv"

# 3. Cargar datos
if (!file.exists(raw_path)) {
  stop("Error: El archivo de datos crudos no existe en ", raw_path)
}

df_raw <- read_delim(
  file = raw_path,
  delim = ";", 
  col_names = TRUE,
  locale = locale(encoding = "ISO-8859-1"), # Probar Latin1 si UTF-8 falla en tildes (Manabí)
  show_col_types = FALSE
)

# Si la carga falla o tiene 1 sola columna, intentar con coma
if (ncol(df_raw) <= 1) {
  df_raw <- read_csv(raw_path, show_col_types = FALSE, locale = locale(encoding = "ISO-8859-1"))
}

message("Dimensiones originales: ", nrow(df_raw), " filas, ", ncol(df_raw), " columnas.")

# 4. Limpieza y Conversión de Tipos
df_clean <- df_raw %>%
  mutate(
    # Fechas y horas
    FECHA_ALERTA_DT = dmy(FECHA_ALERTA), # Asumiendo formato dd/mm/yyyy
    HORA_ALERTA_NUM = as.integer(substr(HORA_ALERTA, 1, 2)), # Extraer hora hh
    
    # Numéricos (limpiar comas por puntos si es necesario)
    VELOCIDAD = as.numeric(str_replace(as.character(VELOCIDAD), ",", ".")),
    LATITUD = as.numeric(str_replace(as.character(LATITUD), ",", ".")),
    LONGITUD = as.numeric(str_replace(as.character(LONGITUD), ",", ".")),
    
    # Factores
    PROVINCIA_OPERADORA = as.factor(PROVINCIA_OPERADORA),
    TIPO_OPERADORA = as.factor(TIPO_OPERADORA),
    TIPO_EXCESO = as.factor(TIPO_EXCESO),
    PROVINCIA_EXCESO = as.factor(PROVINCIA_EXCESO)
  )

# 5. Tratamiento de Faltantes y Filtrado Temporal
# Eliminar filas sin VELOCIDAD y eliminar outliers extremos (errores de GPS > 200km/h)
n_antes <- nrow(df_clean)
df_clean <- df_clean %>% 
  filter(!is.na(VELOCIDAD)) %>%
  filter(VELOCIDAD > 0 & VELOCIDAD <= 200) %>% # Filtrar velocidades inverosímiles
  filter(FECHA_ALERTA_DT >= as.Date("2022-02-01") & FECHA_ALERTA_DT <= as.Date("2022-02-28"))

n_despues <- nrow(df_clean)
message("Filas mantenidas (Velocidad válida 0-200km/h y Feb-2022): ", n_despues)
message("Filas eliminadas (NA, outliers o fechas fuera de rango): ", n_antes - n_despues)

# Marcar lat/long faltantes (no eliminar, solo advertir)
n_missing_coords <- sum(is.na(df_clean$LATITUD) | is.na(df_clean$LONGITUD))
message("Registros con coordenadas faltantes (se mantienen para análisis no espacial): ", n_missing_coords)

# 6. Variables Derivadas
df_final <- df_clean %>%
  mutate(
    hora = HORA_ALERTA_NUM,
    dia_num = wday(FECHA_ALERTA_DT, week_start = 1), # 1=Lunes, 7=Domingo
    franja_horaria = case_when(
      hora >= 0 & hora < 6 ~ "Madrugada",
      hora >= 6 & hora < 12 ~ "Mañana",
      hora >= 12 & hora < 18 ~ "Tarde",
      hora >= 18 & hora <= 23 ~ "Noche",
      TRUE ~ "Desconocido"
    )
  ) %>%
  mutate(
      franja_horaria = factor(franja_horaria, levels = c("Madrugada", "Mañana", "Tarde", "Noche")),
      dia_semana = factor(dia_num, levels = 1:7, labels = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo"))
  )

# 7. Generar Diccionario de Datos
# Aseguramos que la extracción de tipos sea un vector de caracteres simple (maneja múltiples clases como "ordered/factor")
tipos_col <- sapply(df_final, function(x) paste(class(x), collapse = "/"))

descripcion_cols <- c(
  "Provincia de la operadora", "Ciudad de la operadora", "ID operadora", "Tipo operadora",
  "Latitud evento", "Longitud evento", "Ubicación textual", "Ciudad evento", "Provincia evento",
  "Fecha original (texto)", "Hora original (texto)", "Velocidad registrada", "Tipo de exceso",
  "Fecha formato Date", "Hora formato entero", "Hora entero (duplicado)", "Número de día (1=Lun)", "Franja horaria (cat)", "Día de la semana (factor)"
)

# Ajustar longitud si hay discrepancia (seguridad)
if(length(descripcion_cols) < length(names(df_final))) {
  descripcion_cols <- c(descripcion_cols, rep("Sin descripción", length(names(df_final)) - length(descripcion_cols)))
}
descripcion_cols <- descripcion_cols[1:length(names(df_final))]

diccionario <- data.frame(
  variable = names(df_final),
  tipo = tipos_col,
  descripcion = descripcion_cols
)

# 8. Exportar
write_csv(df_final, clean_path)
write_csv(diccionario, dict_path)

message("Datos limpios guardados en: ", clean_path)
message("Diccionario guardado en: ", dict_path)
