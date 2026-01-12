
# Script 02: Visualizaciones
# Objetivo: Generar gráficos exploratorios y guardarlos en /figures.

# 1. Cargar librerías
if (!require(readr)) install.packages("readr")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(lubridate)) install.packages("lubridate")

library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)

# 2. Cargar datos limpios
clean_path <- "data/clean/dataset_limpio.csv"
if (!file.exists(clean_path)) {
  stop("Error: No se encuentra dataset_limpio.csv. Ejecute 01_load_clean.R primero.")
}

df <- read_csv(clean_path, show_col_types = FALSE)

# Convertir columnas de fecha/factor que read_csv puede haber leído como char
df <- df %>%
  mutate(
    franja_horaria = factor(franja_horaria, levels = c("Madrugada", "Mañana", "Tarde", "Noche")),
    dia_semana = factor(dia_semana, levels = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")) 
  )

# 3. Gráficos

# A. Demanda en el tiempo (Eventos por fecha)
p1 <- df %>%
  count(FECHA_ALERTA_DT) %>%
  ggplot(aes(x = FECHA_ALERTA_DT, y = n)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkblue") +
  theme_minimal() +
  labs(
    title = "Evolución Diaria de Alertas de Exceso de Velocidad",
    subtitle = "Febrero 2022",
    x = "Fecha",
    y = "Cantidad de Eventos"
  )

ggsave("figures/llegadas_tiempo.png", plot = p1, width = 8, height = 5)

# B. Distribución de Velocidad
# Pre-calcular factor de escala para densidad
n_bins <- 30
rango_vel <- range(df$VELOCIDAD, na.rm = TRUE)
bin_width <- diff(rango_vel) / n_bins
n_obs <- nrow(df)

p2 <- ggplot(df, aes(x = VELOCIDAD)) +
  geom_histogram(bins = n_bins, fill = "firebrick", color = "white", alpha = 0.8) +
  geom_density(aes(y = after_stat(density) * n_obs * bin_width), color = "black", size = 1) +
  theme_minimal() +
  labs(
    title = "Distribución de Velocidades Registradas",
    x = "Velocidad (km/h)",
    y = "Frecuencia"
  )

ggsave("figures/distribucion_velocidad.png", plot = p2, width = 8, height = 5)

# C. Comparación por Categoría (Tipo de Exceso)
p3 <- df %>%
  count(TIPO_EXCESO) %>%
  ggplot(aes(x = reorder(TIPO_EXCESO, n), y = n, fill = TIPO_EXCESO)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Cantidad de Eventos por Tipo de Exceso",
    x = "Tipo de Exceso",
    y = "Cantidad"
  ) +
  scale_fill_brewer(palette = "Set2")

ggsave("figures/comparacion_categoria.png", plot = p3, width = 8, height = 5)

# D. Heatmap: Hora vs Día de la Semana
p4 <- df %>%
  count(dia_semana, hora) %>%
  ggplot(aes(x = hora, y = dia_semana, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  theme_minimal() +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Mapa de Calor: Intensidad de Alertas",
    subtitle = "Hora del día vs Día de la semana",
    x = "Hora (0-23)",
    y = "Día de la Semana",
    fill = "Eventos"
  )

ggsave("figures/heatmap_hora_dia.png", plot = p4, width = 10, height = 6)

# E. Ubicaciones (Provincia del Evento)
p5 <- df %>%
  count(PROVINCIA_EXCESO) %>%
  top_n(10, n) %>% # Top 10 para no saturar
  ggplot(aes(x = reorder(PROVINCIA_EXCESO, n), y = n, fill = PROVINCIA_EXCESO)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 10 Provincias con más Infracciones",
    x = "Provincia",
    y = "Cantidad de Eventos"
  )
ggsave("figures/ubicaciones.png", plot = p5, width = 8, height = 6)

# F. Tipo de Transporte (Operadora)
p6 <- df %>%
  count(TIPO_OPERADORA) %>%
  ggplot(aes(x = reorder(TIPO_OPERADORA, n), y = n, fill = TIPO_OPERADORA)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Infracciones por Tipo de Transporte",
    x = "Tipo de Operadora",
    y = "Cantidad"
  ) +
  scale_fill_brewer(palette = "Set3")
ggsave("figures/tipo_transporte.png", plot = p6, width = 8, height = 5)

message("Gráficos generados exitosamente en /figures")
