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

tabla.raw <- read.csv('./analisis-pad-main/data/data-1-32-bloque-1.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)

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

res3 <- outliers_mad(x = filter(tabla.outlier,condicion_sala == 'SALA_GRANDE')$mSesgoRel ,na.rm=TRUE)

plot_outliers_mad(res3,x=filter(tabla.outlier,condicion_sala == 'SALA_GRANDE')$mSesgoRel,pos_display=TRUE)

res3$outliers_pos

res3

filter(tabla.outlier, condicion_sala == 'SALA_GRANDE')[res3$outliers_pos,]

tabla.raw <- tabla.raw %>%
  filter(nsub != 22) %>%
  filter(nsub != 28)

write.table(tabla.raw, file="./analisis-pad-main/data/data-1-32-bloque-1-sin-outliers.csv", row.names = FALSE)




# load data 33 - 50 -------------------------------------------------------

tabla.1_32 <- read.csv('./analisis-pad-2-salas-vacias/data/data-1-32-bloque-1-sin-outliers.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)
tabla.1_32 <- subset(tabla.1_32, select = -c(SesgoAbs, SesgoRel))
tabla.33_50 <- read.csv('./analisis-pad-2-salas-vacias/data/33_50_s/data-s33-50-1-block.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)

tabla.raw <- do.call("rbind", list(tabla.1_32, tabla.33_50))

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

# No hay outliers para sala chica

# Sala grande

res3 <- outliers_mad(x = filter(tabla.outlier,condicion_sala == 'SALA_GRANDE')$mSesgoRel ,na.rm=TRUE)

plot_outliers_mad(res3,x=filter(tabla.outlier,condicion_sala == 'SALA_GRANDE')$mSesgoRel,pos_display=TRUE)

res3$outliers_pos

res3

filter(tabla.outlier, condicion_sala == 'SALA_GRANDE')[res3$outliers_pos,]

# No hubo remocion de outliers agregando del 33 al 50

tabla.raw <- subset(tabla.raw, select = -c(SesgoAbs, SesgoRel))

write.table(tabla.raw, file="analisis-pad-2-salas-vacias/data/data-1-50-bloque-1-sin-outliers.csv", row.names = FALSE)


