#===== Declaración de Librerías =======
library(tidyverse)
library(here)

#====== Params ===========

ult_viernes <- floor_date(today(),unit = "week", week_start = 5)

penult_viernes = ult_viernes-7

# Generación de patrones en nombres de archivo
patron_a <- paste0(ult_viernes, "\\.csv$")

patron_b <- paste0(penult_viernes,"\\.csv$")


#==== Almacenar subdirectorios =========

# 1.2 Listar todos los archivos de manera recursiva
#    full.names = TRUE para obtener rutas completas
archivos_t <- list.files(path = here("Input"),
                       pattern = patron_a,      # filtra sólo archivos .csv (puedes cambiar patrón)
                       full.names = TRUE,
                       recursive  = TRUE)


archivos_l <- list.files(path = here("Input"),
                         pattern = patron_b,      # filtra sólo archivos .csv (puedes cambiar patrón)
                         full.names = TRUE,
                         recursive  = TRUE)

# 1.3 Leer cada archivo y almacenarlos en una lista
lista_semanaactual <- lapply(archivos_t, read.csv, stringsAsFactors = FALSE)

lista_semanapasada <- lapply(archivos_l, read.csv, stringsAsFactors = FALSE)


# 1.4 (Opcional) Combinar todos los data.frames en uno solo
data_actual <- do.call(rbind, lista_semanaactual)

data_pasada <- do.call(rbind, lista_semanapasada)

# 1. Vector de ISIN nuevos y eliminados
nuevos_ISIN     <- setdiff(data_actual$ISIN, data_pasada$ISIN)
eliminados_ISIN <- setdiff(data_pasada$ISIN, data_actual$ISIN)

# 2. Filtrar los data.frames por esos ISIN
nuevos_registros <- subset(data_actual,   ISIN %in% nuevos_ISIN)
eliminados_registros <- subset(data_pasada, ISIN %in% eliminados_ISIN)

# 3. (Opcional) Mostrar resultados
cat("Número de registros NUEVOS:", nrow(nuevos_registros), "\n")
cat("Número de registros ELIMINADOS:", nrow(eliminados_registros), "\n")

# Ver primeros ejemplos
head(nuevos_registros)
head(eliminados_registros)


