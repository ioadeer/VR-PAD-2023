# Remocion de outliers para control
# oscuras vs visual entorno virtual mas grande
# Partiendo de un N de 16

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



# load data ---------------------------------------------------------------
tabla.raw <- read.csv('Exp_4_ADP_vr/data/1_16_NVI_LVE.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)

# process data ------------------------------------------------------------

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


res3 <- outliers_mad(x = filter(tabla.outlier,condicion_sala == 'No Visual Info')$mSesgoRel ,na.rm=TRUE)

plot_outliers_mad(res3,x=filter(tabla.outlier,condicion_sala == 'No Visual Info')$mSesgoRel,pos_display=TRUE)



res3$outliers_pos

res3

# [1] 5
# > res3
# Call:
#   outliers_mad.default(x = filter(tabla.outlier, condicion_sala == 
#                                     "Larger VE")$mSesgoRel, na.rm = TRUE)
# 
# Median:
#   [1] -0.4453501
# 
# MAD:
#   [1] 0.1542606
# 
# Limits of acceptable range of values:
#   [1] -0.90813180  0.01743158
# 
# Number of detected outliers
# extremely low extremely high          total 
# 0              1              1 


# SACAR 5
tabla.raw <- read.csv('Exp_4_ADP_vr/data/1_16_NVI_LVE.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)

tabla.raw <- tabla.raw %>%
  filter(nsub != 5) 


write.table(tabla.raw, file="Exp_4_ADP_vr/data/1_16_NVI_LVE_sin_outliers.csv", row.names = FALSE)
