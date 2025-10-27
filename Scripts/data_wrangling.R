#===== Declaración de Librerías =======
install.packages("tidyverse")
install.packages("here")
install.packages("stringr")

library(tidyverse)
library(here)
library(stringr)

#====== Params ===========

ultimo_procesado <- as_date("2025-10-17")     # <-- tu último corte real9
ult_viernes      <- ultimo_procesado + weeks(1)  # = 2025-08-01
penult_viernes   <- ult_viernes - weeks(1)       # = 2025-07-25

# ult_viernes <- floor_date(today(),unit = "week", week_start = 5)|>
#   as.Date(format="%Y-%m-%d")

# penult_viernes = ult_viernes-7

# Generación de patrones en nombres de archivo
patron_a <- paste0(ult_viernes, "\\.csv$")


patron_b <- paste0(penult_viernes,"\\.csv$")

# Nombres para archivos

nuevos_registros_name <- paste("nuevos_registros ",ult_viernes,".csv",sep="")
registros_eliminados_name <- paste("registros_eliminados ",ult_viernes,".csv",sep="")

data_actual_name <- paste("data_semanal_consolidado ",ult_viernes,".csv",sep = "")

# ============= Functions ===========

# Función para yearfrac 30/360 US NASD
yearfrac_30_360_nasd <- function(start_date, end_date) {
  d1 <- day(start_date); m1 <- month(start_date); y1 <- year(start_date)
  d2 <- day(end_date);   m2 <- month(end_date);   y2 <- year(end_date)
  
  d1_adj <- if_else(d1 == 31, 30, d1)
  d2_adj <- if_else((d2 == 31) & (d1_adj %in% c(30, 31)), 30, d2)
  
  ((360 * (y2 - y1)) +
      (30  * (m2 - m1)) +
      (d2_adj - d1_adj)) / 360
}

#==== Almacenar subdirectorios =========

# 1.2 Listar todos los archivos de manera recursiva
#    full.names = TRUE para obtener rutas completas
archivos_t <- list.files(path = here("Input"),
                       pattern = patron_a,      # filtra sólo archivos .csv (puedes cambiar patrón)
                       full.names = TRUE,
                       recursive  = TRUE)


archivos_l <- list.files(path = here("Input"),
                         pattern = patron_b,    # filtra sólo archivos .csv (puedes cambiar patrón)
                         full.names = TRUE,
                         recursive  = TRUE)

# 1.3 Leer cada archivo y almacenarlos en una lista
lista_semanaactual <- lapply(archivos_t, read_csv)

lista_semanapasada <- lapply(archivos_l, read_csv)


# 1.4 (Opcional) Combinar todos los data.frames en uno solo
data_actual <- do.call(rbind, lista_semanaactual)

data_pasada <- do.call(rbind, lista_semanapasada)

# 1. Vector de ISIN nuevos y eliminados
nuevos_ISIN     <- setdiff(data_actual$ISIN, data_pasada$ISIN)
eliminados_ISIN <- setdiff(data_pasada$ISIN, data_actual$ISIN)

# 2. Filtrar los data.frames por esos ISIN
nuevos_registros <- subset(data_actual,   ISIN %in% nuevos_ISIN)

if ( is.null(eliminados_ISIN)) {
  print("No se eliminaron registros")
} else{
  eliminados_registros <- subset(data_pasada, ISIN %in% eliminados_ISIN)
  
}
  
  
# 3. (Opcional) Mostrar resultados
cat("Número de registros NUEVOS:", nrow(nuevos_registros), "\n")
cat("Número de registros ELIMINADOS:", nrow(eliminados_registros), "\n")

# Ver primeros ejemplos
head(nuevos_registros)
head(eliminados_registros)

#==== Transform =========

data_actual|>
  mutate(Today = today(),
         Tenor_Date = round(yearfrac_30_360_nasd(Today,Maturity),3))|>
  filter(Status == "Active",
         Currency == "USD")|>
  mutate(
    Regulation = case_when(
      str_detect(Description, "144A") ~ "144A",
      str_detect(Description, "Reg S") ~ "Reg S",
      TRUE                            ~ "Unspecified"
    )
       )-> data_actual

data_pasada|>
  mutate(Today = today(),
         Tenor_Date = round(yearfrac_30_360_nasd(Today,Maturity),3),
         Status = "Active")|>
  filter(Currency == "USD")|>
  mutate(
    Regulation = case_when(
      str_detect(Description, "144A") ~ "144A",
      str_detect(Description, "Reg S") ~ "Reg S",
      TRUE                            ~ "Unspecified"
    ))-> data_pasada

  

#==== Exportar ==========

write_csv(nuevos_registros,here("Output",nuevos_registros_name))

write_csv(eliminados_registros,here("Output",registros_eliminados_name))

write_csv(data_actual,here("Output",data_actual_name))



















