# Cargamos Librerias
library(tidyverse)

# Definir WD
# Dejamos comentado para poner el propio WD
# setwd("~/Desktop/FCE/Ciencia de Datos/TP Final")

# Rutas de Trabajo
output <- "Auxiliar"

# Descargamos el diccionario de continentes

diccionario_continentes <- read_csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv") %>%
  select(
    isocode = `alpha-3`,
    country = name,
    region = region,
    subregion = `sub-region`
  )

# Guardamos el diccionario

write_csv(diccionario_continentes,file.path(output,"01_diccionario_continentes.csv"))
