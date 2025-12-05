# Cargamos Librerias

library(tidyverse)
library(mice)
options(scipen=999)

# Definir WD
# Dejamos comentado para poner el propio WD
#setwd("~/Desktop/FCE/Ciencia de Datos/TP Final")

imput <- "Procesamiento/Evaluacion de Impacto"
output <- "Analisis"

# Cargamos los datos

datos_mice <- read_rds(file.path(imput,"01_mice.rds"))

# Test de Wald sobre Region

# Modelo 1: La violencia depende de la Región
mod_con_region <- with(datos_mice, lm(log(tasa_homicidio + 1) ~ region))

# Modelo 0: La violencia es igual para todos (Modelo Nulo)
mod_sin_region <- with(datos_mice, lm(log(tasa_homicidio + 1) ~ 1))

resultado_wald <- D1(mod_con_region, mod_sin_region)

print(resultado_wald)

# Graficamos los resultados

datos_todos_imputados <- complete(datos_mice, "long") %>%
  mutate(
    log_homicidio = log(tasa_homicidio + 1),
    region = fct_reorder(region, log_homicidio, .fun = median, .desc = TRUE)
  )

grafico_medias <- datos_todos_imputados %>%
  ggplot(aes(x = region, y = log_homicidio, fill = region)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  geom_jitter(width = 0.2, alpha = 0.1, size = 0.5) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  labs(
    title = "Comparación Regional (Datos Pooled)",
    subtitle = "Distribución de la violencia considerando las 5 imputaciones MICE combinadas",
    x = "", 
    y = "Log(Tasa Homicidios + 1)",
    caption = "Nota: El ancho de las cajas refleja la variabilidad total (intra e inter imputación)."
  ) +
  theme(legend.position = "none")

print(grafico_medias)

# Guardamos los resultados

ggsave(file.path(output,"03_medias_log_violencia_por_region.jpg"), 
       plot = grafico_medias, 
       width = 12, 
       height = 8)
