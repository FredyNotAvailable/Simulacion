
# Script 03: Cálculo de Parámetros
# Objetivo: Calcular parámetros estadísticos para futura simulación y exportarlos.

# 1. Cargar librerías y datos
library(readr)
library(dplyr)

clean_path <- "data/clean/dataset_limpio.csv"
params_path <- "params/params.csv"

if (!file.exists(clean_path)) {
  stop("Error: No se encuentra dataset_limpio.csv.")
}

df <- read_csv(clean_path, show_col_types = FALSE)

# 2. Cálculos

# 2.1 Tasa Global de Llegadas (Eventos por día)
# Asumimos que el dataset cubre un periodo continuo.
n_dias <- as.integer(max(df$FECHA_ALERTA_DT) - min(df$FECHA_ALERTA_DT)) + 1
if(n_dias < 1) n_dias <- 1 # Evitar div por cero si solo hay 1 día

tasa_global_diaria <- nrow(df) / n_dias

# 2.2 Tasa por Franja Horaria (promedio eventos/hora en esa franja)
# Franjas: Madrugada (6h), Mañana (6h), Tarde (6h), Noche (6h) - simplificado
eventos_franja <- df %>%
  group_by(franja_horaria) %>%
  summarise(
    total_eventos = n(),
    tasa_hora_promedio = n() / (n_dias * 6) # promedio por hora dentro de la franja
  )

# 2.3 Probabilidades por Tipo de Exceso
probs_tipo <- df %>%
  count(TIPO_EXCESO) %>%
  mutate(prop = n / sum(n))

# 2.4 Medidas de Tendencia Central para Velocidad
vel_mean <- mean(df$VELOCIDAD, na.rm = TRUE)
vel_median <- median(df$VELOCIDAD, na.rm = TRUE)
vel_sd <- sd(df$VELOCIDAD, na.rm = TRUE)

# 3. Consolidar Parámetros en Tabla
# Formato: | parametro | valor | unidad | descripcion |

tabla_params <- dplyr::bind_rows(
  data.frame(
    parametro = "Tasa_Llegada_Global",
    valor = round(tasa_global_diaria, 2),
    unidad = "eventos/dia",
    descripcion = "Ritmo general: Cuántas infracciones ocurren en un día promedio."
  ),
  data.frame(
    parametro = paste0("Tasa_Llegada_", eventos_franja$franja_horaria),
    valor = round(eventos_franja$tasa_hora_promedio, 2),
    unidad = "eventos/hora",
    descripcion = paste("Ritmo específico: Cuántas ocurren por hora en la", eventos_franja$franja_horaria)
  ),
  data.frame(
    parametro = "Velocidad_Promedio",
    valor = round(vel_mean, 2),
    unidad = "km/h",
    descripcion = "El valor 'típico' de velocidad de los infractores."
  ),
  data.frame(
    parametro = "Velocidad_Mediana",
    valor = round(vel_median, 2),
    unidad = "km/h",
    descripcion = "Punto medio: La mitad de los conductores van más lento que esto."
  ),
  data.frame(
    parametro = "Velocidad_DesvStd",
    valor = round(vel_sd, 2),
    unidad = "km/h",
    descripcion = "Caos: Si es alto, hay velocidades muy variadas (lentos y rápidos)."
  )
)

# Agregar probs de tipos
for(i in 1:nrow(probs_tipo)) {
  fila <- data.frame(
    parametro = paste0("Prob_Tipo_", make.names(probs_tipo$TIPO_EXCESO[i])),
    valor = round(probs_tipo$prop[i], 4),
    unidad = "probabilidad",
    descripcion = paste("Chance de que sea tipo", probs_tipo$TIPO_EXCESO[i], "(Ruteo)")
  )
  tabla_params <- bind_rows(tabla_params, fila)
}

# 2.5 Probabilidades por Tipo de Transporte (Operadora)
probs_transporte <- df %>%
  count(TIPO_OPERADORA) %>%
  mutate(prop = n / sum(n))

for(i in 1:nrow(probs_transporte)) {
  fila <- data.frame(
    parametro = paste0("Prob_Transporte_", make.names(probs_transporte$TIPO_OPERADORA[i])),
    valor = round(probs_transporte$prop[i], 4),
    unidad = "probabilidad",
    descripcion = paste("Chance de que sea un", probs_transporte$TIPO_OPERADORA[i])
  )
  tabla_params <- bind_rows(tabla_params, fila)
}

# 2.6 Probabilidades por Ubicación (Top 3 Provincias)
probs_provincia <- df %>%
  count(PROVINCIA_EXCESO) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(desc(prop)) %>%
  head(3) # Solo Top 3 para no llenar la tabla

for(i in 1:nrow(probs_provincia)) {
  fila <- data.frame(
    parametro = paste0("Prob_Provincia_", make.names(probs_provincia$PROVINCIA_EXCESO[i])),
    valor = round(probs_provincia$prop[i], 4),
    unidad = "probabilidad",
    descripcion = paste("Chance de que ocurra en", probs_provincia$PROVINCIA_EXCESO[i])
  )
  tabla_params <- bind_rows(tabla_params, fila)
}

# 4. Exportar
write_csv(tabla_params, params_path)

message("Parámetros calculados y guardados en: ", params_path)
print(tabla_params)
