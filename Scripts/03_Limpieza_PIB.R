# Cargamos Librerias

library(tidyverse)

# Definir WD
# Dejamos comentado para poner el propio WD
#setwd("~/Desktop/FCE/Ciencia de Datos/TP Final")

# Rutas de Trabajo
imput <- "Datos/Raw"
output <- "Datos/Limpieza"
auxiliar <- "Auxiliar"

# Cargamos datos

pib_per_capita_raw <- read_csv(file.path(imput,"01_pib_per_capita.csv"))
diccionario <- read_csv(file.path(auxiliar,"01_diccionario_continentes.csv"))

# Editamos nuestra tabla

pib_per_capita <- pib_per_capita_raw %>%
  left_join(diccionario %>%
              select (isocode,region,subregion), by="isocode") %>% # Agregamos Region
  drop_na(region) # Sacamos entradas con Region = NA (grupos de paises que WDI arma)

# Descargamos Archivo Limpio

write_csv(pib_per_capita,file.path(output,"01_pib_per_capita_limpio.csv"))
