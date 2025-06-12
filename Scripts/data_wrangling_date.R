#===== Librerías =====
library(tidyverse)
library(here)
library(lubridate)

#===== Parámetros definidos manualmente =====
# Cambia esta fecha a la que desees consolidar (en formato "YYYY-MM-DD")
fecha_objetivo <- as_date("2025-06-12")

# Calcula la fecha de la semana anterior
fecha_anterior <- fecha_objetivo - 7

# Patrones para buscar archivos
patron_a <- paste0(fecha_objetivo, "\\.csv$")
patron_b <- paste0(fecha_anterior, "\\.csv$")

# Nombres para exportar resultados
nuevos_registros_name <- paste0("nuevos_registros ", fecha_objetivo, ".csv")
registros_eliminados_name <- paste0("registros_eliminados ", fecha_objetivo, ".csv")
data_actual_name <- paste0("data_semanal_consolidado ", fecha_objetivo, ".csv")

#===== Función yearfrac 30/360 US NASD =====
yearfrac_30_360_nasd <- function(start_date, end_date) {
  d1 <- day(start_date); m1 <- month(start_date); y1 <- year(start_date)
  d2 <- day(end_date);   m2 <- month(end_date);   y2 <- year(end_date)

  d1_adj <- if_else(d1 == 31, 30, d1)
  d2_adj <- if_else((d2 == 31) & (d1_adj %in% c(30, 31)), 30, d2)

  ((360 * (y2 - y1)) + (30 * (m2 - m1)) + (d2_adj - d1_adj)) / 360
}

#===== Leer archivos =====
archivos_actual <- list.files(
  path = here("Input"),
  pattern = patron_a,
  full.names = TRUE,
  recursive = TRUE
)

archivos_anteriores <- list.files(
  path = here("Input"),
  pattern = patron_b,
  full.names = TRUE,
  recursive = TRUE
)

lista_semanaactual <- lapply(archivos_actual, read_csv)
lista_semanapasada <- lapply(archivos_anteriores, read_csv)

data_actual <- bind_rows(lista_semanaactual)
data_pasada <- bind_rows(lista_semanapasada)

#===== Detectar cambios =====
nuevos_ISIN     <- setdiff(data_actual$ISIN, data_pasada$ISIN)
eliminados_ISIN <- setdiff(data_pasada$ISIN, data_actual$ISIN)

nuevos_registros     <- filter(data_actual, ISIN %in% nuevos_ISIN)
eliminados_registros <- filter(data_pasada, ISIN %in% eliminados_ISIN)

cat("Número de registros NUEVOS:", nrow(nuevos_registros), "\n")
cat("Número de registros ELIMINADOS:", nrow(eliminados_registros), "\n")

#===== Transformar datos =====
data_actual <- data_actual |>
  mutate(
    Today = fecha_objetivo,
    Tenor_Date = round(yearfrac_30_360_nasd(Today, Maturity), 3)
  ) |>
  filter(Status == "Active", Currency == "USD")

data_pasada <- data_pasada |>
  mutate(
    Today = fecha_objetivo,
    Tenor_Date = round(yearfrac_30_360_nasd(Today, Maturity), 3),
    Status = "Active"
  ) |>
  filter(Currency == "USD")

#===== Exportar archivos =====
write_csv(nuevos_registros, here("Output", nuevos_registros_name))
write_csv(eliminados_registros, here("Output", registros_eliminados_name))
write_csv(data_actual, here("Output", data_actual_name))
