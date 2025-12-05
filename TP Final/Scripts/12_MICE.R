# Cargamos Librerias

library(tidyverse)
options(scipen=999)
library(mice)

# Definir WD
# Dejamos comentado para poner el propio WD
#setwd("~/Desktop/FCE/Ciencia de Datos/TP Final")

# Rutas de Trabajo
imput <- "Procesamiento/Faltantes & Outliers"
output <- "Procesamiento/Evaluacion de Impacto"

# Cargamos Base de Datos

base <- read_csv(file.path(imput,"07_base_datos_dummy.csv"))

# Vamos a ejecutar la metodologia MICE para completar los datos nulos de la muestra
# Decidimos mantener la variable region como un factor para que la imputacion de los faltantes se relacione con paises vecinos

datos_para_mice <- base %>%
  select(
    year,
    region,
    pib_per_capita,
    indice_gini,
    tasa_desempleo_joven,
    tasa_homicidio,
    starts_with("dummy_outlier_")
  ) %>%
  mutate(
    region = as.factor(region)
  )

# Usamos PMM (Predictive Mean Matching) para asegurar que los valores imputados sean realistas

objeto_mice <- mice(
  datos_para_mice,
  m = 5,             # Generamos 5 escenarios imputados (estándar científico)
  method = 'pmm',    # Método robusto que respeta la distribución original
  maxit = 50,        # Iteraciones para estabilizar las predicciones
  seed = 123,        # Semilla para que el resultado sea reproducible
  printFlag = FALSE
)

# Hacemos una evaluacion visual rapida para ver los resultados

grafico_mice <-stripplot(objeto_mice, 
          pib_per_capita + indice_gini + tasa_desempleo_joven + tasa_homicidio ~ .imp, 
          pch = 19, 
          cex = 1.2,
          main = "Diagnóstico de Imputación: Observados (Azul) vs Imputados (Rojo)",
          xlab = "Número de Imputación")

print(grafico_mice)

# La distribucion, en principio, esta dentro de lo esperado en cada una de las iteraciones

# Guardamos los Resultados

saveRDS(objeto_mice,file.path(output,"01_mice.rds"))

# Generamos solamente el caso 1 de las 5 imputaciones - igualmente probable que los otros casos - para la comparacion estadistica

datos_imputados_1 <- complete(objeto_mice, 1)

# Reconstruimos la base final uniendo los identificadores originales con los datos rellenos

base_final_imputada_1 <- base %>%
  select(country, isocode, subregion) %>%
  bind_cols(datos_imputados_1)

# Guardamos la base

write_csv(base_final_imputada_1,file.path(output,"02_base_imputada_1.csv"))

