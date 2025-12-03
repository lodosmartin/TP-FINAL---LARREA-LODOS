# Descargamos las Librerias

library(WDI)
library(tidyverse)
library(owidR)

# Definir WD
# Dejamos comentado para poner el propio WD
# setwd("~/Desktop/FCE/Ciencia de Datos/TP Final")

# Rutas de Trabajo

output <- "Datos/Raw"

# Obtenemos de Tablas de Informacion Cruda

pib_per_capita <- WDI(indicator = "NY.GDP.PCAP.PP.CD") %>%
  select(year, country, isocode = iso3c, pib_per_capita = all_of("NY.GDP.PCAP.PP.CD"))

tasa_desempleo_joven <- WDI(indicator = "SL.UEM.1524.ZS") %>%
  select(year, country, isocode = iso3c, tasa_desempleo_joven = all_of("SL.UEM.1524.ZS"))

indice_gini <- WDI(indicator = "SI.POV.GINI") %>%
  select(year, country, isocode = iso3c, indice_gini = all_of("SI.POV.GINI"))

tasa_homicidio <- read_csv("https://ourworldindata.org/grapher/homicide-rate-unodc.csv?v=1&csvType=full&useColumnShortNames=true") %>%
  select(year = Year, country = Entity, isocode = Code, tasa_homicidio = value__category_total__sex_total__age_total__unit_of_measurement_rate_per_100_000_population)

# Guardamos Datos Crudos

write_csv(pib_per_capita,file.path(output,'01_pib_per_capita.csv'))
write_csv(tasa_desempleo_joven,file.path(output,"02_tasa_desempleo_joven.csv"))
write_csv(indice_gini,file.path(output,"03_indice_gini.csv"))
write_csv(tasa_homicidio,file.path(output,"04_tasa_homicidio.csv"))
