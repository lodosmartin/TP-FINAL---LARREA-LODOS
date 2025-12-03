# Cargamos Librerias

library(tidyverse)
library(naniar)
library(broom)
options(scipen=999)

# Definir WD
# Dejamos comentado para poner el propio WD
#setwd("~/Desktop/FCE/Ciencia de Datos/TP Final")

# Rutas de Trabajo
imput <- "Procesamiento/EDA"
output <- "Procesamiento/Faltantes & Outliers"

# Cargamos Datos

base <- read_csv(file.path(imput,"01_Datos_Seleccionados.csv"))

# Evaluamos la presencia de faltantes

nas_porcentaje <- base %>%
  group_by(year) %>%
  summarise(
    across(
      .cols = c(pib_per_capita, indice_gini, tasa_desempleo_joven, tasa_homicidio),
      .fns = ~ round(mean(is.na(.)) * 100, 2), 
      .names = "pct_na_{.col}"
    )
  )

# Hay alto nivel de faltantes para Gini & Tasa de Homicidio - Evaluamos si son MCAR 
# Hipótesis Nula: Los datos son MCAR (p-value > 0.05)
# Hipótesis Alternativa: Los datos NO son MCAR (p-value < 0.05)

test_mcar_global <- base %>%
  select(pib_per_capita, indice_gini, tasa_desempleo_joven, tasa_homicidio) %>%
  mcar_test()

print(test_mcar_global) 

# Rechazamos H0, los datos definitivamente no son faltantes al azar
# Realizamos analisis de medias para ver como impactan las distintas variables entre si

diagnostico_mar_completo <- base %>%
  bind_shadow() %>%
  summarise(
    across(
      .cols = c(pib_per_capita, indice_gini, tasa_desempleo_joven, tasa_homicidio),
      .fns = list(
        si_falta_pib = ~ mean(.[pib_per_capita_NA == "NA"], na.rm = TRUE),
        si_esta_pib  = ~ mean(.[pib_per_capita_NA == "!NA"], na.rm = TRUE),
        si_falta_hom = ~ mean(.[tasa_homicidio_NA == "NA"], na.rm = TRUE),
        si_esta_hom  = ~ mean(.[tasa_homicidio_NA == "!NA"], na.rm = TRUE),
        si_falta_gini = ~ mean(.[indice_gini_NA == "NA"], na.rm = TRUE),
        si_esta_gini  = ~ mean(.[indice_gini_NA == "!NA"], na.rm = TRUE),
        si_falta_desemp = ~ mean(.[tasa_desempleo_joven_NA == "NA"], na.rm = TRUE),
        si_esta_desemp  = ~ mean(.[tasa_desempleo_joven_NA == "!NA"], na.rm = TRUE)
      ),
      .names = "{.fn}___{.col}"
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("condicion", "variable_observada"),
    names_sep = "___",
    values_to = "valor_promedio"
  ) %>%
  pivot_wider(
    names_from = condicion,
    values_from = valor_promedio
  )

print(diagnostico_mar_completo)

# Se observa evidencia de MAR para la mayoria de los casos:
# pbi cambia drasticamente segun si estan o no presentes las otras variables
# gini no esta presente si no hay pbi reportado, se mantiene relativamente estable ante la falta de tasa de homicidio, y varia bastante segun si esta disponible la tasa de desempleo o no
# la tasa de desempleo varia unicamente segun si esta reportado o no el pbi
# la tasa de homicidio varia ante el faltante de cualquier otra variable

# Procedemos a hacer analisis de MNAR - es un analisis mas cualitativo, por ende haremos un grafico de densidad para hacer un diagnostico visual

etiquetas_variables <- c(
  "pib_per_capita" = "PIB per Cápita",
  "indice_gini" = "Índice Gini",
  "tasa_desempleo_joven" = "Desempleo Joven",
  "tasa_homicidio" = "Tasa Homicidios"
)

grafico_mnar <- base %>%
  select(pib_per_capita, indice_gini, tasa_desempleo_joven, tasa_homicidio) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "valor") %>%
  ggplot(aes(x = valor)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "gray90", color = "gray80") +
  geom_density(color = "#2c3e50", linewidth = 1, fill = "#3498db", alpha = 0.3) +
  geom_rug(alpha = 0.5, color = "darkred") +
  facet_wrap(~ variable, scales = "free", labeller = as_labeller(etiquetas_variables)) +
  theme_minimal() +
  labs(
    title = "Diagnóstico Visual de MNAR: Analisis de faltante de extremos",
    subtitle = "Observacion de cortes abruptos en las distribuciones",
    x = "Valor de la variable",
    y = "Densidad"
  )

print(grafico_mnar)

# Visualmente, no hay evidencia flagrante de truncamiento (cortes rectos donde desaparecen los datos). 
# El unico caso que despierta sospechas es Tasa de Homicidios por la brusca caida y larga cola.
# Esto refuerza nuestra conclusión anterior: el mecanismo dominante es MAR (explicable por otras variables) y no un MNAR severo que invalide el estudio.

# Guardamos los resultados

write_csv(nas_porcentaje,file.path(output,"01_Resultados_Evaluacion_Faltantes.csv"))
write_csv(test_mcar_global,file.path(output,"02_Resultados_Test_MCAR_global.csv"))
write_csv(diagnostico_mar_completo,file.path(output,"03_Resultados_Comparacion_Medias_MAR.csv"))
ggsave(file.path(output,"04_Histogramas_MNAR.jpg"), 
       plot = grafico_mnar, 
       width = 12, 
       height = 8)
