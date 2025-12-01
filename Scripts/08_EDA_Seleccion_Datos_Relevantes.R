# Cargamos Librerias

library(tidyverse)

# Definir WD
# Dejamos comentado para poner el propio WD
#setwd("~/Desktop/FCE/Ciencia de Datos/TP Final")

# Rutas de Trabajo
imput <- "Datos/Limpieza"
output <- "Procesamiento/EDA"

# Cargamos Datos

base <- read_csv(file.path(imput,"05_base_consolidada.csv"))

na_por_anio <- base %>%
  group_by(year) %>%
  summarise(
    across(
      .cols = c(pib_per_capita, indice_gini, tasa_desempleo_joven, tasa_homicidio),
      .fns = ~ sum(is.na(.)),
      .names = "na_{.col}"  # Esto agrega el prefijo "na_" al nombre de las columnas resultantes
    )
  )

# Tenemos 65 anios de historia, vamos a ver los que menos NA tienen en promedio para las 4 variables

mejores_20_anios <- na_por_anio %>%
  mutate(promedio_na = rowMeans(pick(starts_with("na_")))) %>%
  arrange(promedio_na) %>%
  slice_head(n = 20)

# Seleccionamos 2010 (#1), 2015 (#7) y 2020 (#16), por condicion de distancia temporal

datos_seleccionados <- base %>%
  filter(year %in% c(2010, 2015, 2020))

# Guardamos la base 

write_csv(datos_seleccionados,file.path(output,"01_Datos_Seleccionados.csv"))
