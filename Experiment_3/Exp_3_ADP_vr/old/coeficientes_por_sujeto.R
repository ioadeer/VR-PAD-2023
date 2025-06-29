# Aca voy a realizar extraccion y comparacion para coeficientes
# a k y R^2 por sujeto.
# Luego hacer figuras con histograma de cada uno de los valores.
# Analisis t-test entre condiciones y ver si es necesaria la correccion
# de bonferroni dado que tenemos solo 2 grupos

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
library(rstatix)




# load data ---------------------------------------------------------------

tabla.raw <- read.csv('./analisis-pad-oscuras-vr/data/oscuras_vr_sin_outliers.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)

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

# Extraccion de coeficientes con broom  -----------  
# https://cran.r-project.org/web/packages/broom/vignettes/broom.html
#  https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html

tabla.ind <- tabla.ind %>%
  mutate(
    log_distancia = log(distancia),
    log_respuesta = log(respuesta[,"mean"]),
  )

# modelo_sujeto  <- function(df) {
#   lmer(log_respuesta ~ log_distancia + (1|nsub) , data = df)
# }

modelo_sujeto  <- function(df) {
  lm(log_respuesta ~ log_distancia, data = df)
}



# Oscuras -----------------------------------------------------------------

tabla.ind.oscuras <- tabla.ind %>%
  filter(condicion_sala == 'OSCURAS')

regressions.oscuras <- tabla.ind.oscuras %>%
  nest(data =  -nsub) %>%
  mutate(
    fit = map(data,modelo_sujeto),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

tidied.oscuras <- regressions.oscuras %>%
  unnest(tidied)

regressions.oscuras %>%
  unnest(glanced)

tidied.oscuras %>%
  unnest(augmented)

#
# CORRECCION Guardarse el R^2 tambien

r_sqrd.oscuras <- regressions.oscuras %>%
  unnest(glanced) %>%
  group_by(nsub) %>%
  select(nsub, r.squared)

#
coefs.oscuras <- regressions.oscuras %>%
  unnest(tidied) %>%
  group_by(nsub) %>%
  spread(term,estimate) %>%
  select(nsub,"(Intercept)", log_distancia) %>%
  rename(intercept = "(Intercept)") %>%
  group_by(nsub) %>%
  summarise(
    intercept = max(intercept, na.rm =T),
    coef = max(log_distancia, na.rm =T),
  )

coefs.oscuras <- merge(x=coefs.oscuras, y=r_sqrd.oscuras, by='nsub')

coefs.oscuras <- coefs.oscuras %>%
  mutate(condicion_sala = "OSCURAS")






# VR ------------------------------------------------------------------

tabla.ind.visual <- tabla.ind %>%
  filter(condicion_sala == 'VR')

regressions.visual <- tabla.ind.visual %>%
  nest(data =  -nsub) %>%
  mutate(
    fit = map(data,modelo_sujeto),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

tidied.visual <- regressions.visual %>%
  unnest(tidied)

regressions.visual %>%
  unnest(glanced)

tidied.visual %>%
  unnest(augmented)

#
# CORRECCION Guardarse el R^2 tambien

r_sqrd.visual <- regressions.visual %>%
  unnest(glanced) %>%
  group_by(nsub) %>%
  select(nsub, r.squared)

#
coefs.visual <- regressions.visual %>%
  unnest(tidied) %>%
  group_by(nsub) %>%
  spread(term,estimate) %>%
  select(nsub,"(Intercept)", log_distancia) %>%
  rename(intercept = "(Intercept)") %>%
  group_by(nsub) %>%
  summarise(
    intercept = max(intercept, na.rm =T),
    coef = max(log_distancia, na.rm =T),
  )

coefs.visual <- merge(x=coefs.visual, y=r_sqrd.visual, by='nsub')

coefs.visual <- coefs.visual %>%
  mutate(condicion_sala = "VR")


coefs.all <- rbind(coefs.oscuras, coefs.visual)

write.table(coefs.all, file="analisis-pad-oscuras-vr/data/coeficientes_por_sujeto_oscuras_vr.csv", row.names = FALSE)

# Residuos

residuos.oscuras <- regressions.oscuras %>%
  unnest(augmented) %>%
  select(-c(fit, tidied, data, glanced))

residuos.oscuras['condicion_sala'] = 'OSCURAS'

regressions.visual <- regressions.visual %>%
  unnest(augmented) %>%
  select(-c(fit, tidied, data, glanced))

regressions.visual['condicion_sala'] = 'VR'

residuos.all <- rbind(residuos.oscuras, regressions.visual)

write.table(residuos.all, file="analisis-pad-oscuras-vr/data/fitted_model_residuals.csv", row.names = FALSE)
