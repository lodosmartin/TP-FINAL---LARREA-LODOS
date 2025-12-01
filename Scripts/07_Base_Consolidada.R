# Cargamos Librerias

library(tidyverse)

# Definir WD
# Dejamos comentado para poner el propio WD
#setwd("~/Desktop/FCE/Ciencia de Datos/TP Final")

# Rutas de Trabajo
imput <- "Datos/Limpieza"
output <- "Datos/Limpieza"

# Cargamos Datos

pib <- read_csv(file.path(imput,"01_pib_per_capita_limpio.csv"))
desempleo <- read_csv(file.path(imput,"02_tasa_desempleo_joven_limpio.csv"))
gini  <- read_csv(file.path(imput,"03_gini_limpio.csv"))
homicidio  <- read_csv(file.path(imput,"04_tasa_homicidio_limpio.csv"))

# Unimos los files separados en una sola base para el analisis

base_consolidada <- list(pib,desempleo,gini,homicidio) %>%
  reduce(left_join, by = c("year","country","isocode","region","subregion")) %>%
  select(year, country, isocode, region, subregion, pib_per_capita, indice_gini, tasa_desempleo_joven, tasa_homicidio)

# Guardamos la base

write_csv(base_consolidada,file.path(output, "05_base_consolidada.csv"))
