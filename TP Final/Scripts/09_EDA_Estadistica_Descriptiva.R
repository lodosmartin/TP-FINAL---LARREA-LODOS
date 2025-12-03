# Cargamos Librerias

library(tidyverse)
library(DescTools)

# Definir WD
# Dejamos comentado para poner el propio WD
#setwd("~/Desktop/FCE/Ciencia de Datos/TP Final")

# Rutas de Trabajo
imput <- "Procesamiento/EDA"
output <- "Procesamiento/EDA"

# Cargamos Datos

base <- read_csv(file.path(imput,"01_Datos_Seleccionados.csv"))

# Estadisticas Descriptivas

estadistica_descriptiva <- base %>%
  summarise(
    across(
      .cols = c(pib_per_capita, indice_gini, tasa_desempleo_joven, tasa_homicidio),
      .fns = list(
        media   = ~ round(mean(., na.rm = TRUE), 2), 
        mediana = ~ round(median(., na.rm = TRUE), 2),
        moda    = ~ round(Mode(., na.rm = TRUE)[1], 2),
        desvio_estandar = ~ round(sd(., na.rm = TRUE), 2),
        rango_IQR = ~ round(IQR(., na.rm = TRUE), 2)
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
  pivot_wider(
    names_from = medida,
    values_from = valor 
  )

# Graficamos histogramas

grafico_histogramas <- base %>%
  select(pib_per_capita = "PIB per Cápita PPA (USD Constantes)" , indice_gini = "Índice de Gini", tasa_desempleo_joven = "Tasa de Desempleo Joven 15-24 (%)", tasa_homicidio = "Tasa Homicidios (cada 100.000 habitantes)") %>%
  pivot_longer(
    cols = everything(), 
    names_to = "variable", 
    values_to = "valor"
  )) %>%
  ggplot(aes(x = valor)) +
  geom_histogram(fill = "#69b3a2", color = "white", bins = 30) + 
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(
    title = "Distribución de Variables Relevantes",
    subtitle = "Histogramas",
    x = "Valor",
    y = "Frecuencia"
  )

# Mostramos el gráfico
print(grafico_histogramas)

# Graficamos Boxplots

grafico_boxplots <- base %>%
  select(pib_per_capita, indice_gini, tasa_desempleo_joven, tasa_homicidio) %>%
  pivot_longer(
    cols = everything(), 
    names_to = "variable", 
    values_to = "valor"
  ) %>%
  mutate(variable = case_match(variable,
                               "pib_per_capita"       ~ "PIB per Cápita (USD)",
                               "indice_gini"          ~ "Índice de Gini",
                               "tasa_desempleo_joven" ~ "Desempleo Joven (%)",
                               "tasa_homicidio"       ~ "Tasa Homicidios (x 100k)"
  )) %>%
  ggplot(aes(x = valor)) +
  geom_boxplot(fill = "#69b3a2", color = "darkslategrey", outlier.colour = "red", outlier.shape = 1) +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  labs(
    title = "Análisis de Outliers y Distribución",
    subtitle = "Boxplots por variable",
    y = "",
    x = "Valor de la variable"
  )

# Mostramos el gráfico
print(grafico_boxplots)

# Guardamos los calculos y graficos

write.csv(estadistica_descriptiva,file.path(output,"02_Estadistica Descriptiva.csv"))
ggsave(file.path(output,"03_histogramas.jpg"), 
       plot = grafico_histogramas, 
       width = 12, 
       height = 8)
ggsave(file.path(output,"04_boxplots.jpg"), 
       plot = grafico_boxplots, 
       width = 12, 
       height = 8)

