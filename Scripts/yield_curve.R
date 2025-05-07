# 1. Declaración de librerías

library(tidyverse)
library(here)

#2. Parámetros

ult_viernes <- floor_date(today(),unit = "week", week_start = 5)

ruta <- paste0("data_semanal_consolidado ",ult_viernes,".csv")

#3. Data

data <- read_csv(here("Output",ruta))
  

#4. ggplot

ggplot(data, aes(x = factor(Tenor_Date), y = `Yield (%)`, color = Issuer)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Issuer, scales = "free") +
  labs(
    title    = paste("Yield Curve al", ult_viernes),
    x        = "Plazo (Maturity)",
    y        = "Rendimiento (%)",
    color    = "Emisor"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",          # quita leyenda al usar facet_wrap
    strip.text      = element_text(face = "bold"),
    plot.title      = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 9)
  )
