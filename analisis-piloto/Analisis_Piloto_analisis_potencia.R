# Analisis del piloto
# Dependencias ------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(quickpsy)
library(tidyverse)
library(lme4)
library(nlme)
library(lmerTest)
library(broom)
library(broom.mixed)
library(ggpubr)
library(palmerpenguins)
library(Routliers)
library(ggbeeswarm)
library(ggthemes)
library(ggstatsplot)
library(gmodels)
library(pracma)

# Load data -----------------------------------------------------------------

tabla.raw <- read.csv('./analisis-piloto/data/data-piloto.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)

tabla.raw$SesgoAbs <-  tabla.raw$respuesta - tabla.raw$distancia
tabla.raw$SesgoRel <- (tabla.raw$respuesta - tabla.raw$distancia) / tabla.raw$distancia


f_promedio <- function(x) c(mean = mean(x),
                            sd   = sd(x),
                            var  = var(x),
                            sem  = sd(x)/sqrt(length(x)),
                            n    = length(x))

tabla.ind <- tibble(aggregate(cbind(respuesta,SesgoRel) ~ nsub*condicion_sala*distancia*nbloque,
                              data = tabla.raw,
                              FUN  = f_promedio,na.action = NULL))

# - Nivel poblacional

tabla.pob <- tibble(aggregate(cbind(respuesta[,"mean"],SesgoRel[,"mean"]) ~ condicion_sala*distancia,
                              data <- tabla.ind,
                              FUN  <- f_promedio,na.action = NULL))


tabla.pob = tabla.pob %>% rename(respuestapob = V1)
tabla.pob = tabla.pob %>% rename(sesgorelpob = V2)


#
# Copia
# Hacer una nueva tabla que tiene sesgo de cada sujeto sin distancia

tabla.outlier <- tabla.ind %>% 
  #filter(Bloque == "verbal report" & BlindCat == "Blind") %>% 
  group_by(nsub, condicion_sala) %>%
  summarise(mSesgoRel  = mean(SesgoRel[,"mean"],na.rm=TRUE))  %>%
  ungroup()


res3 <- outliers_mad(x = filter(tabla.outlier,condicion_sala == 'SALA_CHICA')$mSesgoRel ,na.rm=TRUE)

plot_outliers_mad(res3,x=filter(tabla.outlier,condicion_sala == 'SALA_CHICA')$mSesgoRel,pos_display=TRUE)

res3$outliers_pos

res3

filter(tabla.outlier, condicion_sala == 'SALA_GRANDE')[res3$outliers_pos,]

# Deteccion outliers condicion sala grande
# A tibble: 2 × 3
#nsub condicion_sala mSesgoRel
#<int> <fct>              <dbl>
#  1    22 SALA_GRANDE        0.465
#  2    28 SALA_GRANDE        0.111

res3 <- outliers_mad(x = filter(tabla.outlier,condicion_sala == 'SALA_CHICA')$mSesgoRel ,na.rm=TRUE)

plot_outliers_mad(res3,x=filter(tabla.outlier,condicion_sala == 'SALA_CHICA')$mSesgoRel,pos_display=TRUE)

res3$outliers_pos

res3

filter(tabla.outlier, condicion_sala == 'SALA_CHICA')[res3$outliers_pos,]



# Estadistica -------------------------------------------------------------

# Both tasks
tabla.ind <- tabla.ind %>% 
  mutate(log_respuesta_mean = log(respuesta[,"mean"])) %>%
  mutate(log_distancia = log(distancia))

# Analisis de potencia ------------------------------------------------

install.packages("devtools")
devtools::install_github("arcaldwell49/Superpower")
install.packages("simr")

library(Superpower)
library(simr)

#En la interaccion

#Sala chica = 8
#Sala grande = 5
# Distancias = 6
# LOG LOG
m.distancia <- lmer(log_respuesta_mean ~ condicion_sala * log_distancia + (1|nsub), 
                    data = tabla.ind)

summary(m.distancia)
ggcoefstats(m.distancia, output = "tidy") %>% select(-label)
anova(m.distancia)

#
# Aca se le agranda el effect size a la interaccion
# El valor es 0.3 ? 
# Esteban achico el valor del effect size porque se pueden pensar
# que los effect size para los pilotos a veces son anormalmente grandes

xtabs(~ condicion_sala , data=getData(m.distancia))
#fixef(m.distancia)["condicion_salaSALA_GRANDE:log_distancia"] <- -0.30048
# -0.3 aparece como estimate de la interaccion si se aumenta el n

# se calcula el poder estadistico con este nuevo modelo
power_condicion = powerSim(m.distancia, fixed("condicion_sala:log_distancia", "f"), nsim = 50, seed = 2021)
power_condicion


m.BPM2 <- extend(m.distancia, along = "nsub" , n=39)
xtabs(~ condicion_sala, data=getData(m.BPM2))
summary(m.BPM2)
# ggcoefstats(m.BPM, output = "tidy") %>% select(-label)
anova(m.BPM2)
xtabs(~ condicion_sala, data=getData(m.BPM2))


power_condicion = powerSim(m.BPM2, fixed("condicion_sala:log_distancia", "f"), nsim = 50, seed = 2021)
power_condicion

# Al extender el N para tener 96 % de potencia estadistica
# Necesitamos un N de 144 para la sala chica
# 144/ 6 = 24 sujetos por condicion
# Power for predictor 'condicion_sala:log_distancia', (95% confidence interval):
#   96.00% (86.29, 99.51)
# 
# Test: Type-II F-test (package car)
# 
# Based on 50 simulations, (0 warnings, 0 errors)
# alpha = 0.05, nrow = 234
# 
# Time elapsed: 0 h 0 m 11 s
# 
# nb: result might be an observed power calculation
# > xtabs(~ condicion_sala, data=getData(m.BPM2))
# condicion_sala
# SALA_CHICA SALA_GRANDE 
# 144          90 
# > 144/6
# [1] 24

