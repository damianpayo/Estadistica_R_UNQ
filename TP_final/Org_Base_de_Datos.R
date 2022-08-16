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
#Nos quedamos solo con un rango horario

Cavernas %<>% filter(Dates>ymd_hms("2021:05:07 17:00:00")) %>% 
  filter(Dates<ymd_hms("2021:05:07 21:00:00"))
Mayuato %<>% filter(Dates>ymd_hms("2021:05:21 17:00:00")) %>% 
  filter(Dates<ymd_hms("2021:05:21 21:00:00"))
Astilleros %<>% filter(Dates>ymd_hms("2021:05:23 17:00:00")) %>% 
  filter(Dates<ymd_hms("2021:05:23 21:00:00"))
Acay %<>% filter(Dates>ymd_hms("2021:05:08 17:00:00")) %>% 
  filter(Dates<ymd_hms("2021:05:08 21:00:00"))

#Cambio el d[ia]

Cavernas %<>% mutate(Dates= hm(format(Dates,"%H:%M"))) %>%
  mutate(Dates= ymd_hms(paste("2021:01:01 ",as.character(Dates@hour),":",as.character(Dates@minute), ":0")))
Mayuato %<>% mutate(Dates= hm(format(Dates,"%H:%M"))) %>%
  mutate(Dates= ymd_hms(paste("2021:01:01 ",as.character(Dates@hour),":",as.character(Dates@minute), ":0")))
Astilleros %<>% mutate(Dates= hm(format(Dates,"%H:%M"))) %>%
  mutate(Dates= ymd_hms(paste("2021:01:01 ",as.character(Dates@hour),":",as.character(Dates@minute), ":0")))
Acay %<>% mutate(Dates= hm(format(Dates,"%H:%M"))) %>%
  mutate(Dates= ymd_hms(paste("2021:01:01 ",as.character(Dates@hour),":",as.character(Dates@minute), ":0")))

#Armar un dataset por cada indicador bioacústico en los cuatro lugares.

BI <- select(Astilleros, c(Dates, BI)) %>% rename("BI_Astilleros" = "BI") %>%
  mutate(select(Acay, c(BI))) %>% rename("BI_Acay" = "BI")
BI <-  merge(select(Mayuato, c(Dates, BI)), BI, all = TRUE) %>%
  rename("BI_Mayuato" = "BI")
BI <-  merge(select(Cavernas, c(Dates, BI)), BI, all = TRUE) %>%
  rename("BI_Cavernas" = "BI")

BI

graph <- ggplot(BI, aes(x=Dates)) +
  geom_line(aes(y=BI$BI_Cavernas)) + 
  geom_line(aes(y=BI$BI_Mayuato)) + 
  geom_line(aes(y=BI$BI_Astilleros)) + 
  geom_line(aes(y=BI$BI_Acay))
graph

ggplot() +
  geom_line(data=Cavernas, aes(x=Dates, y=BI),color='green',size=1) +
  geom_line(data=Mayuato, aes(x=Dates, y=BI),color='blue',size=1) +
  geom_line(data=Astilleros, aes(x=Dates, y=BI),color='red',size=1) +
  geom_line(data=Acay, aes(x=Dates, y=BI),color='black',size=1) +
  theme_minimal()
  
