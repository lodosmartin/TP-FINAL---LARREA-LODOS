# Cargamos Librerias

library(tidyverse)
library(DescTools)
library(mice)
options(scipen=999)

# Definir WD
# Dejamos comentado para poner el propio WD
#setwd("~/Desktop/FCE/Ciencia de Datos/TP Final")

imput_pre <- "Procesamiento/EDA"
imput_post <- "Procesamiento/Evaluacion de Impacto"
output <- "Procesamiento/Evaluacion de Impacto"

# Cargamos datos

base_pre <- read_csv(file.path(imput_pre,"01_Datos_Seleccionados.csv"))
mice <- read_rds(file.path(imput_post,"01_mice.rds"))
base_post <- read_csv(file.path(imput_post,"02_base_imputada_1.csv"))

# Creamos una funcion auxiliar para las metricas estadisticas

calcular_estadisticas <- function(data, etiqueta) {
  data %>%
    summarise(
      across(
        .cols = c(pib_per_capita, indice_gini, tasa_desempleo_joven, tasa_homicidio),
        .fns = list(
          media   = ~ round(mean(., na.rm = TRUE), 2),
          mediana = ~ round(median(., na.rm = TRUE), 2),
          moda    = ~ round(Mode(., na.rm = TRUE)[1], 2),
          desvio  = ~ round(sd(., na.rm = TRUE), 2),
          iqr     = ~ round(IQR(., na.rm = TRUE), 2)
        ),
        .names = "{.fn}___{.col}"
      )
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = c("medida", "variable"),
      names_sep = "___",
      values_to = "valor"
    ) %>%
    mutate(Estado = etiqueta)
}

# Generamos los graficos

datos_combinados_graficos <- bind_rows(
  base_pre %>% mutate(Estado = "Original") %>% select(pib_per_capita, indice_gini, tasa_desempleo_joven, tasa_homicidio, Estado),
  base_post %>% mutate(Estado = "Imputado") %>% select(pib_per_capita, indice_gini, tasa_desempleo_joven, tasa_homicidio, Estado)
) %>%
  pivot_longer(
    cols = c(pib_per_capita, indice_gini, tasa_desempleo_joven, tasa_homicidio),
    names_to = "variable",
    values_to = "valor"
  ) %>%
  mutate(variable = case_match(variable,
                               "pib_per_capita"       ~ "PIB per Cápita",
                               "indice_gini"          ~ "Índice Gini",
                               "tasa_desempleo_joven" ~ "Desempleo Joven",
                               "tasa_homicidio"       ~ "Tasa Homicidios"
  ))

# Funciones de Densidad

grafico_comparativo_densidad <- datos_combinados_graficos %>%
  ggplot(aes(x = valor, fill = Estado, color = Estado)) +
  geom_density(alpha = 0.4, linewidth = 0.8) + 
  facet_wrap(~ variable, scales = "free") +
  scale_fill_manual(values = c("Imputado" = "#E76F51", "Original" = "#264653")) +
  scale_color_manual(values = c("Imputado" = "#E76F51", "Original" = "#264653")) +
  theme_minimal() +
  labs(
    title = "Impacto de MICE en la Distribución",
    subtitle = "Comparación de Densidad: Original vs Imputado",
    y = "Densidad", x = "Valor"
  )

print(grafico_comparativo_densidad)

# Boxplots Comparativos

grafico_comparativo_boxplots <- datos_combinados_graficos %>%
  ggplot(aes(x = Estado, y = valor, fill = Estado)) +
  geom_boxplot(outlier.colour = "red", outlier.alpha = 0.5) +
  facet_wrap(~ variable, scales = "free") +
  scale_fill_manual(values = c("Imputado" = "#E76F51", "Original" = "#264653")) +
  theme_minimal() +
  labs(
    title = "Control de Outliers y Dispersión",
    subtitle = "¿Cambió la estructura de los datos tras imputar?",
    x = "", y = "Valor"
  )

print(grafico_comparativo_boxplots)

# Guardamos los elementos

ggsave(file.path(output,"03_boxplots_comparados.jpg"), 
       plot = grafico_comparativo_boxplots, 
       width = 12, 
       height = 8)
ggsave(file.path(output,"04_densidades_comparadas.jpg"), 
       plot = grafico_comparativo_densidad, 
       width = 12, 
       height = 8)
