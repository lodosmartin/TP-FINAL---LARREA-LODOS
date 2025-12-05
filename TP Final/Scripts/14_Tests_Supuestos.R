# Cargamos Librerias

library(tidyverse)
library(mice)
library(car)
library(lmtest)
options(scipen=999)

# Definir WD
# Dejamos comentado para poner el propio WD
#setwd("~/Desktop/FCE/Ciencia de Datos/TP Final")

imput <- "Procesamiento/Evaluacion de Impacto"

# Cargamos los datos a chequear (solo el caso 1 de los 5 imputados via MICE)
# Si los test fallan en 1 de los casos, fallan en todos - es un caso testigo de diagnostico

datos_mice <- read_rds(file.path(imput,"01_mice.rds"))
datos_diagnostico <- complete(datos_mice, 1)

# Ajustamos el modelo lineal 
modelo_diagnostico <- lm(tasa_homicidio ~ 
                           pib_per_capita + indice_gini + tasa_desempleo_joven + 
                           dummy_outlier_tasa_homicidio + dummy_outlier_pib_per_capita + 
                           dummy_outlier_indice_gini + dummy_outlier_tasa_desempleo_joven +
                           region, 
                         data = datos_diagnostico)


# TEST 1: MULTICOLINEALIDAD (VIF) 

vif_1 <- vif(modelo_diagnostico)
print(vif_1)

# TEST 2: HOMOCEDASTICIDAD (Breusch-Pagan)
# Hipotesis Nula: La varianza del error es constante

bp_1 <- bptest(modelo_diagnostico)
print(bp_1)

# TEST 3: NORMALIDAD DE RESIDUOS (Shapiro-Wilk)
# Hipotesis Nula: Los errores son normales.

shapiro_1 <- shapiro.test(residuals(modelo_diagnostico))
print(shapiro_1)

# TEST 4: AUTOCORRELACION (Durbin-Watson)
#Hipotesis Nula: No existe autocorrelacion de primer orden en los errores

dw_1 <- dwtest(modelo_diagnostico)
print(dw_1)

# Dado que los tests de homocedasticidad, normalidad de residuos y autocorrelacion fallaron
# Se decide logaritmizar la variable dependiente

modelo_log <- lm(log(tasa_homicidio + 1) ~ 
                   pib_per_capita + indice_gini + tasa_desempleo_joven + 
                   dummy_outlier_tasa_homicidio + dummy_outlier_pib_per_capita + 
                   dummy_outlier_indice_gini + dummy_outlier_tasa_desempleo_joven +
                   region, 
                 data = datos_diagnostico)

# Re-chequeamos los supuestos

print(shapiro.test(residuals(modelo_log)))
print(bptest(modelo_log))
print(dwtest(modelo_log))
