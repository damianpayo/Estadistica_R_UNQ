library(tidyverse)
library(lubridate)

Cavernas <- read_csv(file = '07-05-2021_Cavernas.csv')
Mayuato <- read_csv(file = '23-05-21_Sendero_Mayuato.csv')
Astilleros <- read_csv(file = '23-05-21_Quebrada_de_Astilleros.csv')
Acay <- read_csv(file = '08-05-2021_Acay.csv')

#Es necesario agregar una columna con de cabezal de todos los datos.

names (Cavernas) <- c('Dates', 'ACI', 'BI', 'DNSI', 'AEI', 'ADI', 'HS', 'HT', 'SC', 'dBFS')
names (Mayuato) <- c('Dates', 'ACI', 'BI', 'DNSI', 'AEI', 'ADI', 'HS', 'HT', 'SC', 'dBFS')
names (Astilleros) <- c('Dates', 'ACI', 'BI', 'DNSI', 'AEI', 'ADI', 'HS', 'HT', 'SC', 'dBFS')
names (Acay) <- c('Dates', 'ACI', 'BI', 'DNSI', 'AEI', 'ADI', 'HS', 'HT', 'SC', 'dBFS')

#Ahora necesitamos Cambiar el formato de las fechas y quedarnos solamente con las horas que vamos a analizar

Cavernas <- Cavernas %>%
  select(c('Dates', 'ACI', 'ADI', 'BI'))

Mayuato <- Mayuato %>%
  select(c('Dates', 'ACI', 'ADI', 'BI'))

Astilleros <- Astilleros %>%
  select(c('Dates', 'ACI', 'ADI', 'BI'))

Acay <- Acay %>%
  select(c('Dates', 'ACI', 'ADI', 'BI'))

# Necesitamos separar fecha de hora para, posteriormente, quedarnos con una franja horaria especifica
  

