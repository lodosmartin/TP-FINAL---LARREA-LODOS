# Cargamos Librerias

library(tidyverse)
options(scipen=999)

# Definir WD
# Dejamos comentado para poner el propio WD
#setwd("~/Desktop/FCE/Ciencia de Datos/TP Final")

# Rutas de Trabajo
imput <- "Procesamiento/EDA"
output <- "Procesamiento/Faltantes & Outliers"

# Cargamos Datos

base <- read_csv(file.path(imput,"01_Datos_Seleccionados.csv"))

# Hacemos el analisis de outliers tanto con el metodo IQR como con el metodo Z-score

auditoria_outliers <- base %>%
  select(country, year, pib_per_capita, indice_gini, tasa_desempleo_joven, tasa_homicidio) %>%
  pivot_longer(
    cols = c(pib_per_capita, indice_gini, tasa_desempleo_joven, tasa_homicidio),
    names_to = "variable", 
    values_to = "valor"
  ) %>%
  filter(!is.na(valor)) %>%
  group_by(variable, year) %>% 
  mutate(
    Q1 = quantile(valor, 0.25),
    Q3 = quantile(valor, 0.75),
    Rango_IQR = Q3 - Q1,
    limite_inferior = Q1 - 1.5 * Rango_IQR,
    limite_superior = Q3 + 1.5 * Rango_IQR,
    es_outlier_iqr = valor < limite_inferior | valor > limite_superior,
    media_anio = mean(valor),
    desvio_anio = sd(valor),
    z_score = (valor - media_anio) / desvio_anio,
    es_outlier_z = abs(z_score) > 3
  ) %>%
  ungroup()

# Identificamos especificamente cuales son los casos outliers

datos_outliers <- auditoria_outliers %>%
  filter(es_outlier_iqr | es_outlier_z) %>%
  mutate(
    etiqueta_caso = paste0(country, " (", year, ")"),
    tipo_deteccion = case_when(
      es_outlier_z & es_outlier_iqr ~ "Extremo (Ambos métodos)",
      es_outlier_z ~ "Extremo (Z-Score)",
      es_outlier_iqr ~ "Moderado (Solo IQR)"
    )
  ) %>%
  select(variable, etiqueta_caso, valor, z_score, tipo_deteccion) %>%
  arrange(variable, desc(abs(z_score)))

# Visualizamos

print(datos_outliers)

# Elegimos el metodo de creacion de una variable dummy para identificarlos en la base original
# Esto sirve para controlar su efecto en las posteriores regresiones y analisis ANOVA
# Optamos por este metodo para evitar perder informacion real, ya que no se trata de estimaciones sino de valores extremos reales

# Transformamos la identificacion de outliers a una tabla para unir a la original

outliers_para_cruce <- auditoria_outliers %>%
  filter(es_outlier_iqr | es_outlier_z) %>%
  select(country, year, variable) %>%
  mutate(es_outlier = 1) %>% 
  pivot_wider(
    names_from = variable,
    values_from = es_outlier,
    names_prefix = "dummy_outlier_",
    values_fill = 0
  )

# Cruzamos con la base original (Left Join)
base_con_dummies <- base %>%
  left_join(outliers_para_cruce, by = c("country", "year")) %>%
  mutate(across(starts_with("dummy_outlier_"), ~ replace_na(., 0)))

# Chequeamos que porcentaje de outliers hay sobre el total de datos para cada variable

porcentaje_outliers <- base_con_dummies %>%
  summarise(
    across(
      .cols = starts_with("dummy_outlier_"), 
      .fns = ~ round(mean(.) * 100, 2)
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "porcentaje_outliers"
  ) %>%
  mutate(variable = str_remove(variable, "dummy_outlier_")) %>%
  arrange(desc(porcentaje_outliers))

print(porcentaje_outliers)

# Guardamos los resultados

write_csv(auditoria_outliers,file.path(output,"05_analisis_outliers.csv"))
write_csv(datos_outliers,file.path(output,'06_identificacion_outliers.csv'))
write_csv(base_con_dummies,file.path(output,"07_base_datos_dummy.csv"))
write_csv(porcentaje_outliers,file.path(output,"08_porcentaje_outliers.csv"))
