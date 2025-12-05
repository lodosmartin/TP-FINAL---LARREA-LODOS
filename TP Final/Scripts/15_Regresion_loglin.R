# Cargamos Librerias

library(tidyverse)
library(mice)
library(broom)
options(scipen=999)

# Definir WD
# Dejamos comentado para poner el propio WD
#setwd("~/Desktop/FCE/Ciencia de Datos/TP Final")

imput <- "Procesamiento/Evaluacion de Impacto"
output <- "Analisis"

# Cargamos los datos

datos_mice <- read_rds(file.path(imput,"01_mice.rds"))

# Vamos a correr el modelo log-lineal
# Hipótesis: ↑ PIB + ↓ Desigualdad + ↓ Desempleo => ↓ Homicidios

modelo_loglin <- with(data = datos_mice, 
                         expr = lm(log(tasa_homicidio + 1) ~ 
                                     pib_per_capita + 
                                     indice_gini + 
                                     tasa_desempleo_joven + 
                                     dummy_outlier_tasa_homicidio + 
                                     dummy_outlier_pib_per_capita + 
                                     dummy_outlier_indice_gini + 
                                     dummy_outlier_tasa_desempleo_joven + 
                                     region
                         )
)

# Pooling
resultados_finales <- pool(modelo_loglin)

# Tabla de Resultados
tabla_final <- summary(resultados_finales, conf.int = TRUE) %>%
  as_tibble() %>%
  select(term, estimate, std.error, p.value) %>%
  mutate(
    Significancia = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ "ns"
    ),
    Impacto_Porcentual = round(estimate * 100, 4)
  )

# Graficamos unicamente las relaciones que resultaron de mayor peso, PBI & Region
# De vuelta, utilizamos la simulacion 1 de mice como caso testigo para visualizar

datos_grafico <- complete(datos_mice,"long") %>%
  mutate(
    log_homicidio = log(tasa_homicidio + 1)
  )

grafico_regresion <- datos_grafico %>%
  ggplot(aes(x = pib_per_capita, y = log_homicidio)) +
  geom_point(aes(color = region), alpha = 0.2, size = 2) +
  geom_smooth(method = "lm", color = "black", linetype = "dashed", se = TRUE) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  labs(
    title = "Regresión: Riqueza vs. Violencia (Controlado por Región)",
    subtitle = "Se observa el menor PIB y mayor violencia de América",
    x = "PIB per Cápita (USD)",
    y = "Log(Tasa Homicidios + 1)",
    color = "Región"
  ) +
  coord_cartesian(xlim = c(0, 150000), ylim = c(0, NA))

print(grafico_regresion)

# Guardamos los resultados

write_csv(tabla_final,file.path(output,"01_Resultados_Regresion.csv"))
ggsave(file.path(output,"02_Regresion_PBI_Region_Violencia.jpg"), 
       plot = grafico_regresion, 
       width = 12, 
       height = 8)

