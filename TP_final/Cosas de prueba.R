library(tidyverse)
library(lubridate)
library(Routliers)
library(ggpubr)
library(patchwork)
library(parameters)
library(nlme)


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
  select(c('Dates', 'ACI', 'ADI'))

Mayuato <- Mayuato %>%
  select(c('Dates', 'ACI', 'ADI'))

Astilleros <- Astilleros %>%
  select(c('Dates', 'ACI', 'ADI'))

Acay <- Acay %>%
  select(c('Dates', 'ACI', 'ADI'))

# Necesitamos separar fecha de hora para, posteriormente, quedarnos con una franja horaria especifica
#Nos quedamos solo con un rango horario

# #Saco la primera y ultima hora
# Cavernas %<>% filter(Dates>ymd_hms("2021:05:07 17:00:00")) %>% 
#   filter(Dates<ymd_hms("2021:05:08 09:00:00"))
# 
# Mayuato %<>% filter(Dates>ymd_hms("2021:05:21 11:00:00")) %>% 
#   filter(Dates<ymd_hms("2021:05:21 21:00:00"))
# 
# Astilleros %<>% filter(Dates>ymd_hms("2021:05:23 17:00:00")) %>% 
#   filter(Dates<ymd_hms("2021:05:24 02:00:00"))
# 
# Acay %<>% filter(Dates>ymd_hms("2021:05:08 12:00:00")) %>% 
#   filter(Dates<ymd_hms("2021:05:09 02:00:00"))

# De 17 a 21

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

Datos <- Cavernas %>% mutate(lugar = "Cavernas", region="Puna") %>% 
  bind_rows(Mayuato %>% mutate(lugar = "Mayuato",region="Yunga")) %>% 
  bind_rows(Astilleros %>% mutate(lugar = "Astilleros",region="Yunga")) %>% 
  bind_rows(Acay %>% mutate(lugar = "Acay",region="Puna"))


# Primer vistazo a los datos
Datos %>% summary()

# Distribuciones / Variabilidad

Datos %>% filter(ACI<260 & ACI>240) %>%
  ggplot(aes(x = ACI, fill=lugar)) +
  geom_histogram(binwidth = 0.1) +
  facet_grid(lugar ~ .) +
  theme_minimal()

ggdensity(Datos_ACI%>% filter(ACI<260), 
          x = "ACI",
          add = "mean", 
          rug = TRUE,
          color = "lugar", 
          fill = "lugar")

Datos %>% ggplot(aes(x = ADI, fill=lugar)) +
  geom_histogram(binwidth = 0.05) +
  facet_grid(lugar ~ .) +
  theme_minimal()

ggdensity(Datos, 
          x = "ADI",
          add = "mean", 
          rug = TRUE,
          color = "lugar", 
          fill = "lugar")

# Calculamos los outliers 

outliers_mad(Acay$ACI)


#Median:
# [1] 242.088
# 
# MAD:
#   [1] 0.4773972
# 
# Limits of acceptable range of values:
#   [1] 240.6558 243.5202
# 
# Number of detected outliers
# extremely low extremely high          total 
# 0             12             12 

outliers_mad(Cavernas$ACI)
# Median:
#   [1] 242.656
# 
# MAD:
#   [1] 0.3691674
# 
# Limits of acceptable range of values:
#   [1] 241.5485 243.7635
# 
# Number of detected outliers
# extremely low extremely high          total 
# 0             23             23 

outliers_mad(Mayuato$ACI)

# Median:
#   [1] 245.253
# 
# MAD:
#   [1] 0.4773972
# 
# Limits of acceptable range of values:
#   [1] 243.8208 246.6852
# 
# Number of detected outliers
# extremely low extremely high          total 
# 1             28             29

outliers_mad(Astilleros$ACI)

# Median:
#   [1] 244.7295
# 
# MAD:
#   [1] 1.435898
# 
# Limits of acceptable range of values:
#   [1] 240.4218 249.0372
# 
# Number of detected outliers
# extremely low extremely high          total 
# 0             47             47


# filter(ACI>240 & ACI<250)

###########################################

# Outliers ADI
outliers_mad(Acay$ADI)

# Median:
#   [1] 0.798
# 
# MAD:
#   [1] 0.103782
# 
# Limits of acceptable range of values:
#   [1] 0.486654 1.109346
# 
# Number of detected outliers
# extremely low extremely high          total 
# 11              0             11 

outliers_mad(Cavernas$ADI)
# Median:
#   [1] 2.246
# 
# MAD:
#   [1] 0.0415128
# 
# Limits of acceptable range of values:
#   [1] 2.121462 2.370538
# 
# Number of detected outliers
# extremely low extremely high          total 
# 35              0             35 

outliers_mad(Mayuato$ADI)
# Median:
#   [1] 2.264
# 
# MAD:
#   [1] 0.0474432
# 
# Limits of acceptable range of values:
#   [1] 2.12167 2.40633
# 
# Number of detected outliers
# extremely low extremely high          total 
# 79              0             79 

outliers_mad(Astilleros$ADI)

# Median:
#   [1] 2.205
# 
# MAD:
#   [1] 0.096369
# 
# Limits of acceptable range of values:
#   [1] 1.915893 2.494107
# 
# Number of detected outliers
# extremely low extremely high          total 
# 72              0             72 

# evolucion temporal

# Considero el rango minimo y maximo de lo calculado por 
# Routliers para cada lugar
ggplot(data=Datos %>% filter(ACI>240 & ACI<250), aes(x=Dates, y=ACI,group=lugar,color=lugar)) +
  geom_line()+
  geom_smooth()+
  theme_minimal()

ggplot(data=Datos %>% filter(ACI<250 & ACI>240), aes(x=Dates, y=ACI,group=lugar,color=lugar)) +
  geom_line()+
  geom_smooth(se = FALSE,
              method = lm) +
  theme_minimal()

ggplot(data=Datos %>% filter(ADI>1), aes(x=Dates, y=ADI,group=lugar,color=lugar)) +
  geom_line()+
  theme_minimal()

# Boxplot / Covarianza ACI

Acay<-Acay %>% mutate(outlier_ACI= ACI<240.6558 | ACI>243.5202)
Cavernas <- Cavernas %>% mutate(outlier_ACI= ACI<241.5485|ACI>243.7635)
Mayuato <- Mayuato %>% mutate(outlier_ACI= ACI<243.8208|ACI>246.6852)
Astilleros <- Astilleros %>% mutate(outlier_ACI= ACI<240.4218|ACI>249.0372)

Datos <- Cavernas %>% mutate(lugar = "Cavernas", region="Puna") %>% 
  bind_rows(Mayuato %>% mutate(lugar = "Mayuato",region="Yunga")) %>% 
  bind_rows(Astilleros %>% mutate(lugar = "Astilleros",region="Yunga")) %>% 
  bind_rows(Acay %>% mutate(lugar = "Acay",region="Puna"))

Datos_ACI <- Datos %>%  filter(outlier_ACI==FALSE) %>%
  select(-c("ADI","outlier_ACI"))

ACI_boxplot <- ggplot() +
  stat_boxplot(geom = "errorbar", # Bigotes
              width = 0.2) +
  geom_boxplot(data=Datos%>%filter(ACI<300), aes(x=lugar, y=ACI, color=lugar),
               width = 0.5, outlier.alpha = 0,show.legend = FALSE) +
  geom_jitter(data=Datos%>%filter(outlier_ACI==TRUE, ACI<300),
              aes(x=lugar, y=ACI), color="red",
              position=position_jitter(0.1), 
              size=0.8, alpha=0.5, pch=19)+
  theme_minimal()


ACI_boxplot_sinO <- Datos_ACI%>% 
  ggplot(aes(x=lugar, y=ACI, color=lugar))+
  geom_boxplot(width = 0.5, outlier.alpha = 0.5,show.legend = FALSE)+
  theme_minimal()
ACI_boxplot_sinO

p1 <- ACI_boxplot | ACI_boxplot_sinO

############# # Boxplot / Covarianza ADI

Acay<-Acay %>% mutate(outlier_ADI= ADI<0.486654 | ADI>1.109346)
Cavernas <- Cavernas %>% mutate(outlier_ADI= ADI<2.121462|ADI>2.370538)
Mayuato <- Mayuato %>% mutate(outlier_ADI= ADI<2.12167|ADI>2.40633)
Astilleros <- Astilleros %>% mutate(outlier_ADI= ADI<1.915893|ADI>2.494107)

Datos <- Cavernas %>% mutate(lugar = "Cavernas", region="Puna") %>% 
  bind_rows(Mayuato %>% mutate(lugar = "Mayuato",region="Yunga")) %>% 
  bind_rows(Astilleros %>% mutate(lugar = "Astilleros",region="Yunga")) %>% 
  bind_rows(Acay %>% mutate(lugar = "Acay",region="Puna"))

Datos_ADI <- Datos %>%
  filter(outlier_ADI==FALSE) %>%
  select(-c("ACI","outlier_ACI","outlier_ADI"))


# Grafico ADI con outliers
ADI_boxplot <- ggplot() +
  geom_boxplot(data=Datos, 
               aes(x=lugar, y=ADI, color=lugar),
               width = 0.5, outlier.alpha = 0) +
  geom_jitter(data=Datos%>%filter(outlier_ADI==TRUE),
              aes(x=lugar, y=ADI), color="grey",
              position=position_jitter(0.1), 
              size=0.8, alpha=0.9, pch=19)+
  theme_minimal()
ADI_boxplot

# Grafico ADI sin outliers

ADI_boxplot_sinO <- Datos_ADI%>% 
  ggplot(aes(x=lugar, y=ADI, color=lugar))+
  geom_boxplot(width = 0.5, outlier.alpha = 0.5)+
  theme(axis.line = element_line(colour = "black", # Personalización del tema
                                 size = 0.25))+
  theme_minimal()
ADI_boxplot_sinO

# Estadistica
estadistica_ACI <- Datos_ACI %>% 
  group_by(lugar) %>%
  summarise(M = mean(ACI), S = sd(ACI), n = n(), .groups="keep")

estadistica_ADI <- Datos_ADI %>% 
  group_by(lugar) %>%
  summarise(M = mean(ADI), S = sd(ADI), n = n(), .groups="keep")

########################


###################
ACI_Acay <- Datos_ACI %>% filter(lugar=="Acay")

hist_ACI_Acay <- hist(ACI_Acay$ACI,
                      breaks = 30,
                      plot = TRUE) %$%
  tibble(from    = head(breaks, -1),
         to      = tail(breaks, -1),
         mids    = mids,
         counts  = counts, 
         density = density) %>%
  mutate(fr = counts/n())

density_Acay <- tibble(x = seq(from=240, to=245, by=0.01)) %>%
  mutate(density = dnorm(x, mean=Datos_ACI$M[1], 
                         sd=Datos_ACI$S[1]/sqrt(1)))
density_Acay

ggplot() +
  geom_col(data=hist_ACI_Acay, aes(x=mids, y=density)) +
  geom_line(data=density_Acay, aes(x=x, y=density), color='red')

###########


ACI_Astilleros   <- Datos_ACI %>% filter(lugar=="Astilleros")

hist_ACI_Astilleros   <- hist(Datos_ACI$ACI,
                              breaks = 30,
                              plot = TRUE) %$%
  tibble(from    = head(breaks, -1),
         to      = tail(breaks, -1),
         mids    = mids,
         counts  = counts, 
         density = density) %>%
  mutate(fr = counts/n())

density_Astilleros <- tibble(x = seq(from=240, to=250, by=0.01)) %>%
  mutate(density = dnorm(x, mean=estadistica$M[2], 
                         sd=estadistica$S[2]/sqrt(1)))
density_Astilleros

ggplot() +
  geom_col(data=hist_ACI_Astilleros, aes(x=mids, y=density)) +
  geom_line(data=density_Astilleros, aes(x=x, y=density), color='red')


###############
t.test(x=Acay$ACI, y = Astilleros$ACI)
t.test(x=Acay$ACI, y = Mayuato$ACI)
t.test(x=Acay$ACI, y = Cavernas$ACI)
t.test(x=Astilleros$ACI, y = Mayuato$ACI)
t.test(x=Astilleros$ACI, y = Cavernas$ACI)
t.test(x=Cavernas$ACI, y = Mayuato$ACI)


Datos %<>% mutate(franja_horaria = case_when(
  Dates < ymd_hms('2021-01-01 19:00:00') ~ 'Dia',
  Dates >= ymd_hms('2021-01-01 19:00:00') ~ 'Noche'))

yunga_ACI <- Datos%>%filter(outlier_ACI==FALSE, region=="Yunga")
puna_ACI <- Datos%>%filter(outlier_ACI==FALSE, region=="Puna")
t.test(x=yunga_ACI$ACI, y=puna_ACI$ACI)

yunga_ADI <- Datos%>%filter(outlier_ADI==FALSE, region=="Yunga")
puna_ADI <- Datos%>%filter(outlier_ADI==FALSE, region=="Puna")
t.test(x=yunga_ADI$ADI, y=puna_ADI$ADI)

Datos_region_ACI <- yunga_ACI %>%
  bind_rows(puna_ACI)
Datos_region_ADI <- yunga_ADI %>%
  bind_rows(puna_ADI)

ggplot(Datos_region_ACI, aes(x = region, y = ACI, color = region, fill=region))+
  geom_boxplot(width = 0.3, outlier.alpha = 0.5, show.legend = FALSE)+
  scale_color_manual(breaks = c("Yunga", "Puna"),
                     values = c("#124411", "#442C11")) +
  scale_fill_manual(breaks = c("Yunga", "Puna"),
                     values = c("#2ECC71", "#9C640C")) +
  labs(y = "ACI", x = "Region", title="queseyo") +
  annotate("text", x = 1.5, y = 250,  label = "p-value < 2.2e-16", size = 4) +
  annotate("segment", x = 1, xend = 2, y = 249.5, 
           yend = 249.5, colour = "black", size=.3, alpha=1,)+
  theme_minimal()

ggplot(Datos_region_ADI, aes(x = region, y = ADI, color = region, fill=region))+
  geom_boxplot(width = 0.3, outlier.alpha = 0.5, show.legend = FALSE)+
  scale_color_manual(breaks = c("Yunga", "Puna"),
                     values = c("#124411", "#442C11")) +
  scale_fill_manual(breaks = c("Yunga", "Puna"),
                    values = c("#2ECC71", "#9C640C")) +
  labs(y = "ADI", x = "Region", title="queseyo") +
  annotate("text", x = 1.5, y = 2.5,  label = "p-value < 2.2e-16", size = 4) +
  annotate("segment", x = 1, xend = 2, y = 2.4, 
           yend = 2.4, colour = "black", size=.3, alpha=1,)+
  
  theme_minimal()
#############
Datos_region_ACI %>%
  mutate(franja_horaria = factor(franja_horaria)) %>%
  ggplot(aes(x = as.numeric(franja_horaria) - 1,
             y = ACI, color=region)) +
  geom_point(position = position_jitter(width = 0.15)) +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE) +
  scale_x_continuous(breaks = c(0, 1), 
                     minor_breaks = c(0, 1),
                     limits = c(-0.2, 1.2),
                     labels = c("Dia", "Noche")) +
  labs(x = "Franja Horaria",
       y = "ACI")

Fig.ay <- Datos_region_ADI %>%
  mutate(franja_horaria = factor(franja_horaria)) %>%
  filter(lugar!="Cavernas") %>%
  ggplot(aes(x = as.numeric(franja_horaria) - 1,
             y = ADI, color=region,show.legend=FALSE)) +
  geom_point(position = position_jitter(width = 0.15),show.legend=FALSE) +
  ylim(0.5,2.3)+
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE,show.legend=FALSE) +
  scale_x_continuous(breaks = c(0, 1), 
                     minor_breaks = c(0, 1),
                     limits = c(-0.2, 1.2),
                     labels = c("Dia", "Noche")) +
  labs(x = "Franja Horaria",
       y = "ADI",title="Acay")

Fig.cnas <- Datos_region_ADI %>%
  mutate(franja_horaria = factor(franja_horaria)) %>%
  filter(lugar!="Acay") %>%
  ggplot(aes(x = as.numeric(franja_horaria) - 1,
             y = ADI, color=region)) +
  geom_point(position = position_jitter(width = 0.15)) +
  ylim(0.5,2.3)+
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE) +
  scale_x_continuous(breaks = c(0, 1), 
                     minor_breaks = c(0, 1),
                     limits = c(-0.2, 1.2),
                     labels = c("Dia", "Noche")) +
  labs(x = "Franja Horaria",
       y = "ADI",title="Cavernas")

Fig.ay | Fig.cnas

#ACI
Dia_ACI <- Datos_region_ACI %>% filter(franja_horaria=="Dia")
Noche_ACI <- Datos_region_ACI %>% filter(franja_horaria=="Noche")
t.test(Dia_ACI$ACI, Noche_ACI$ACI)

Dia_ACI_Yunga <- Datos_region_ACI %>% filter(franja_horaria=="Dia",region=="Yunga")
Noche_ACI_Yunga <- Datos_region_ACI %>% filter(franja_horaria=="Noche",region=="Yunga")
t.test(Dia_ACI_Yunga$ACI, Noche_ACI_Yunga$ACI)

Dia_ACI_Puna <- Datos_region_ACI %>% filter(franja_horaria=="Dia",region=="Yunga")
Noche_AC_Puna <- Datos_region_ACI %>% filter(franja_horaria=="Noche",region=="Yunga")
t.test(Dia_ACI_Puna$ACI, Noche_AC_Puna$ACI)

#ADI
Dia_ADI <- Datos_region_ADI %>% filter(franja_horaria=="Dia")
Noche_ADI <- Datos_region_ADI %>% filter(franja_horaria=="Noche")
t.test(Dia_ADI$ADI, Noche_ADI$ADI)

Dia_ADI_Yunga <- Datos_region_ADI %>% filter(franja_horaria=="Dia",region=="Yunga")
Noche_ADI_Yunga <- Datos_region_ADI %>% filter(franja_horaria=="Noche",region=="Yunga")
t.test(Dia_ADI_Yunga$ADI, Noche_ADI_Yunga$ADI)

Dia_ADI_Puna <- Datos_region_ADI %>% filter(franja_horaria=="Dia",region=="Yunga")
Noche_ADI_Puna <- Datos_region_ADI %>% filter(franja_horaria=="Noche",region=="Yunga")
t.test(Dia_ACI_Puna$ADI, Noche_ADI_Puna$ADI)

ACI.lme <-lme(ACI ~ ADI*region*franja_horaria, 
              data=Datos_region_ACI, random = ~ 1|lugar)

summary(ACI.lme)
anova(ACI.lme)
residuo <- abs(predict.lm(ACI.lm, births14[1,]) - births14$weight[1])


