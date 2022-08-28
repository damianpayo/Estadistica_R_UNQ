library(tidyverse)
library(lubridate)
library(Routliers)

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

#Hacemos un unico dataset

Datos <- Cavernas %>% mutate(lugar = "Cavernas") %>% 
  bind_rows(Mayuato %>% mutate(lugar = "Mayuato")) %>% 
  bind_rows(Astilleros %>% mutate(lugar = "Astilleros")) %>% 
  bind_rows(Acay %>% mutate(lugar = "Acay"))

# Primer vistazo a los datos

Datos %>% summary()

# Distribuciones / Variabilidad

Datos %>% 
  ggplot(aes(x = ACI, fill=lugar)) +
  geom_histogram(binwidth = 10) +
  theme_minimal()

Datos %>% 
  ggplot(aes(x = ADI, fill=lugar)) +
  geom_histogram(binwidth = 0.1) +
  theme_minimal()

Datos %>% 
  ggplot(aes(x = BI, fill=lugar)) +
  geom_histogram(binwidth = 1) +
  theme_minimal()

# Calculamos los outliers 

outliers <- outliers_mad(Datos$ACI)
outliers

outliers <- outliers_mad(Acay$ADI)
outliers

outliers <- outliers_mad(Acay$BI)
outliers


plot_outliers_mad(outliers, Acay$ACI)


# evolucion temporal

ggplot(data=Datos %>% filter(ACI<300), aes(x=Dates, y=ACI,group=lugar,color=lugar)) +
  geom_line()+
  theme_minimal()

ggplot(data=Datos %>% filter(ADI>1), aes(x=Dates, y=ADI,group=lugar,color=lugar)) +
  geom_line()+
  theme_minimal()

ggplot(data=Datos %>% filter(BI<6), aes(x=Dates, y=BI,group=lugar,color=lugar)) +
  geom_line()+
  theme_minimal()



# Boxplot / Covarianza

graph_ACI_boxplot <- ggplot(data=Datos%>% filter(ACI<300), aes(x=lugar, y=ACI, color=lugar)) +
  geom_boxplot(width = 0.5, outlier.alpha = 0)+
  geom_jitter(position=position_jitter(0.1), size=0.8, alpha=0.9, pch=19)+
  theme_minimal()
graph_ACI_boxplot

graph_ADI_boxplot <- ggplot(data=Datos, aes(x=lugar, y=ADI, color=lugar)) +
  geom_boxplot(width = 0.5, outlier.alpha = 0)+
  geom_jitter(position=position_jitter(0.1), size=0.8, alpha=0.9, pch=19)+
  theme_minimal()
graph_ADI_boxplot

graph_BI_boxplot <- ggplot(data=Datos%>% filter(BI<6), aes(x=lugar, y=BI, color=lugar)) +
  geom_boxplot(width = 0.5, outlier.alpha = 0)+
  geom_jitter(position=position_jitter(0.1), size=0.8, alpha=0.9, pch=19)+
  theme_minimal()
graph_BI_boxplot

# Boxplotear cada dos horas

