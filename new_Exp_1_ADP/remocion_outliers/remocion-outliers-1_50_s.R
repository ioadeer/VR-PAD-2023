# Analisis de datos del experi 50 sujetos
# No hubo remocion de outliers para los sujetos de 33 a 50

# Paran outliers

install.packages("Routliers")

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
library(Routliers)
library(ggbeeswarm)
library(ggthemes)
library(ggstatsplot)
library(gmodels)
library(pracma)
library(Routliers)

# load data new -----------------------------------------------------------

tabla.raw <- read.csv('./new_Exp_1_ADP/data/S1_S50_2_bloques.csv', header = TRUE, sep = ',', stringsAsFactors = TRUE)

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

#Median:
#  [1] -0.5570613
#
#MAD:
#  [1] 0.2980083
#
#Limits of acceptable range of values:
#  [1] -1.4510863  0.3369637
#
#Number of detected outliers
#extremely low extremely high          total 
#0              0              0 

# No hay outliers para sala chica

# Sala grande

res3 <- outliers_mad(x = filter(tabla.outlier,condicion_sala == 'SALA_GRANDE')$mSesgoRel ,na.rm=TRUE)

plot_outliers_mad(res3,x=filter(tabla.outlier,condicion_sala == 'SALA_GRANDE')$mSesgoRel,pos_display=TRUE)

res3$outliers_pos
# [1] 21 32 45

res3

#Median:
#  [1] -0.5623279
#
#MAD:
#  [1] 0.2401106
#
#Limits of acceptable range of values:
#  [1] -1.2826596  0.1580037
#
#Number of detected outliers
#extremely low extremely high          total 
#0              3              3 

filter(tabla.outlier, condicion_sala == 'SALA_GRANDE')[res3$outliers_pos,]

# Outliers para sala grande 21 32 45

tabla.raw <- subset(tabla.raw, select = -c(SesgoAbs, SesgoRel))

tabla.raw <- tabla.raw %>%
  filter(nsub != 21) %>%
  filter(nsub != 32) %>%
  filter(nsub != 45) 

write.table(tabla.raw, file="new_Exp_1_ADP/data/S1_S50_2_bloques_sin_outliers.csv", row.names = FALSE)


