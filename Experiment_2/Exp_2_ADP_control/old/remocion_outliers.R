# Remocion de outliers para control
# oscuras vs visual
# Partiendo de un N de 20

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
tabla.raw <- read.csv('./Experiment_2/Exp_2_ADP_control/data/1_20_control.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)

dplyr# process data ------------------------------------------------------------

tabla.raw$SesgoAbs <-  tabla.raw$respuesta - tabla.raw$distancia
tabla.raw$SesgoRel <- (tabla.raw$respuesta - tabla.raw$distancia) / tabla.raw$distancia

# signed bias
#tabla.raw$signed_bias <- (tabla.raw$respuesta - tabla.raw$distancia) / tabla.raw$distancia
# unsigen bias
#tabla.raw$unsigned_bias <- abs(tabla.raw$signed_bias)

#  unsigned log bias
tabla.raw$log_bias <-  log10(tabla.raw$respuesta/tabla.raw$distancia)
tabla.raw$log_bias_unsigned <- abs(tabla.raw$log_bias)


f_promedio <- function(x) c(mean = mean(x),
                            sd   = sd(x),
                            var  = var(x),
                            sem  = sd(x)/sqrt(length(x)),
                            n    = length(x))

tabla.ind <- tibble(aggregate(cbind(respuesta,SesgoRel, log_bias, log_bias_unsigned) ~ nsub*condicion_sala*distancia*nbloque,
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
  filter(condicion_sala == 'OSCURAS') %>% 
  group_by(nsub, condicion_sala, distancia) %>%
  summarise(mBiasUnsigned  = mean(log_bias_unsigned[,"mean"],na.rm=TRUE))  %>%
  ungroup()

res3 <- outliers_mad(x = tabla.outlier$mBiasUnsigned,threshold = 3 ,na.rm=TRUE)

plot_outliers_mad(res3,x=tabla.outlier$mBiasUnsigned,pos_display=TRUE)

tabla.outlier[res3$outliers_pos,] 

res3 <- outliers_mad(x = filter(tabla.outlier,condicion_sala == 'OSCURAS')$mSesgoRel ,na.rm=TRUE)
plot_outliers_mad(res3,x=filter(tabla.outlier,condicion_sala == 'OSCURAS')$mSesgoRel,pos_display=TRUE)



res3$outliers_pos

res3

tabla.ind.Eye <- results_tbl %>% 
  filter(condition == "Ear level", type == "NORMAL", location == "standing") %>% 
  group_by(subject,condition,target_distance) %>%
  summarise(mBiasUnsigned  = mean(log_bias_un_mean ,na.rm=TRUE))  %>%
  ungroup()
res3 <- outliers_mad(x = tabla.ind.Eye$mBiasUnsigned,threshold = 3 ,na.rm=TRUE)
plot_outliers_mad(res3,x=tabla.ind.Eye$mBiasUnsigned,pos_display=TRUE)
tabla.ind.Eye[res3$outliers_pos,] 
# 
# Median:
#   [1] -0.4794885
# 
# MAD:
#   [1] 0.3299392
# 
# Limits of acceptable range of values:
#   [1] -1.4693062  0.5103292
# 
# Number of detected outliers
# extremely low extremely high          total 
# 0              2              2 

# outliers 1 y el 10 de oscuras


# VISUAL

res3 <- outliers_mad(x = filter(tabla.outlier,condicion_sala == 'VISUAL')$mSesgoRel ,na.rm=TRUE)

plot_outliers_mad(res3,x=filter(tabla.outlier,condicion_sala == 'VISUAL')$mSesgoRel,pos_display=TRUE)

res3$outliers_pos

res3

outliers_mad.default(x = filter(tabla.outlier, condicion_sala == 
                                  "VISUAL")$mSesgoRel, na.rm = TRUE)

# Median:
#   [1] -0.3067644
# 
# MAD:
#   [1] 0.2843507
# 
# Limits of acceptable range of values:
#   [1] -1.1598165  0.5462877
# 
# Number of detected outliers
# extremely low extremely high          total 
# 0              3              3 

# Outliers mod visual 1 2 y 16

# Outliers 1 2 10 y 16

tabla.raw <- tabla.raw %>%
  filter(nsub != 1) %>%
  filter(nsub != 2) %>%
  filter(nsub != 10) %>%
  filter(nsub != 16) 

write.table(tabla.raw, file="./analisis_control/data/control_sin_outliers.csv", row.names = FALSE)

