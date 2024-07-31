# Extraccion de coeficientes para cada participante
# Hacer t test

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
library(rstatix)


# load data new -----------------------------------------------------------

tabla.raw <- read.csv('analisis-pad-2-salas-vacias/data/data-1-50-bloque-1-sin-outliers.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)

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

# SALA Condicion sala chica

# Figuras -----------------------------------------------------------------

figures_folder = "./analisis-pad-main/figuras/"

#Grafico individual con brutos

g1 <- ggplot(tabla.ind, aes(x = distancia, y = respuesta[,"mean"])) +
  geom_errorbar(data=tabla.ind, color="black",alpha = 1, width=1.3, size=1.1,
                mapping=aes(ymin = respuesta[,"mean"] - respuesta[,"sem"],
                            ymax = respuesta[,"mean"] + respuesta[,"sem"]))+
  geom_point(color="red", size=1) +
  #geom_abline(intercept = 1, slope = 1,linetype="dashed") +       
  scale_x_continuous(name="Distance source (m)")+#, breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,20)) +
  scale_y_continuous(name="Perceived distance (m)")+#,  breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,20)) +
  geom_jitter(data = tabla.raw, mapping = aes(x=distancia, y=respuesta), color="blue", alpha=.8, shape=4, size=2.2, stroke=.2,
              position = position_jitter(width=.1, height=.1)) + 
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  facet_grid(condicion_sala ~ nsub) + 
  theme_linedraw(base_size = 9)

plot(g1)

mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "distancia_individuales-50.png", sep = '')
#ggsave(mi_nombre_de_archivo, plot = g1, width=44, height=14, units="cm", limitsize=FALSE, dpi=200)
ggsave(mi_nombre_de_archivo, plot = g1, width=100, height=20, units="cm", limitsize=FALSE, dpi=200)

# Grafico poblacional con bruto ####

g2 <- ggplot(filter(tabla.pob, condicion_sala != "SALA_PARLANTES"), aes(x = distancia, y = respuestapob[,"mean"],color = condicion_sala, group = condicion_sala)) +
  geom_errorbar(data=filter(tabla.pob, condicion_sala != "SALA_PARLANTES"), alpha = 1, width=0.5, size=0.5,
                mapping=aes(ymin = respuestapob[,"mean"] - respuestapob[,"sem"], 
                            ymax = respuestapob[,"mean"] + respuestapob[,"sem"]))+ 
  geom_point(size=1, stroke = 1) + geom_line(size = 3)+
  #geom_abline(intercept = 1, slope = 1,linetype="dashed") +       
  scale_x_continuous(name="Distance source (m)", breaks=c(0,1,2,3,4,5,6,7,8,9,10), labels=c(0,1,2,3,4,5,6,7,8,9,10), minor_breaks=NULL, limits = c(0,10)) +
  scale_y_continuous(name="Mean perceived distance +/- SEM (m)",  breaks=c(0,1,2,3,4,5,6,7,8,9,10), labels=c(0,1,2,3,4,5,6,7,8,9,10), minor_breaks=NULL, limits = c(0,15)) +
  # geom_jitter(data = filter(tabla.ind, condicion_sala != "SALA_PARLANTES"), mapping = aes(x=distancia, y=respuesta[,"mean"],group=nsub, color = condicion_sala), alpha=.8, size=0.7, stroke=.2,
  #          position = position_jitter(width=.1, height=0))+
  # geom_line(data = filter(tabla.ind, condicion_sala != "SALA_PARLANTES"), mapping = aes(x=distancia, y=respuesta[,"mean"], group=nsub, color = condicion_sala), alpha=.8, size=0.1)+
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  # facet_grid(nsub~ .)+
  theme_pubr(base_size = 9, margin = TRUE, legend = "right")

plot(g2)

mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "distancia_poblacional.png", sep = '')
ggsave(mi_nombre_de_archivo, plot=g2, width=10, height=10, units="cm", limitsize=FALSE, dpi=300)


## Sesgo
tabla_sesgo <- tabla.ind %>% 
  group_by(condicion_sala,nsub) %>%
  summarise(mDist_perc = mean(respuesta[,"mean"]),
            mSesgoRel  = mean(SesgoRel[,"mean"]))  %>%
  ungroup()

tabla_sesgo.pob <- tabla_sesgo  %>% 
  group_by(condicion_sala) %>%
  summarise(MSesgoRel = mean(mSesgoRel),
            N = n()) %>%
  ungroup()


fig.sesgo <- ggplot(tabla_sesgo, aes(x = condicion_sala,
                                     y = mSesgoRel,
                                     colour = condicion_sala))+
  geom_point(size = 4,alpha = 1,
             position = position_jitterdodge(jitter.width = .1,
                                             jitter.height = 0,
                                             dodge.width = .1)) +
  # stat_summary(fun.data = "mean_se",
  #              geom = "bar",
  #              alpha = .4,
  #              size = 1,
  #              position = position_dodge(width = 1)) +
  stat_summary(fun.data = "mean_se",
               geom = "line") +
  stat_summary(fun.data = "mean_se",
               geom = "bar",
               alpha = .4,
               size=2,
               position = position_dodge(width = 1)) +
  labs(x = "Condition de sala",
       y = "Bias") +
  theme_pubr(base_size = 9, margin = TRUE)
# theme(legend.position = "none")
fig.sesgo
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "sesgo.png", sep = '')
ggsave(mi_nombre_de_archivo, plot=fig.sesgo, width=10, height=10, units="cm", limitsize=FALSE, dpi=300)


histogram <- ggplot(tabla.ind, aes(x=respuesta[,"mean"])) + geom_histogram()

histogram <- ggplot(tabla_sesgo, aes(x=mDist_perc)) + geom_histogram()

histogram <- ggplot(tabla_sesgo, aes(x=mDist_perc, color=condicion_sala)) + 
  geom_histogram(fill="white", binwidth=1)

plot(histogram)

histograma <- ggplot(tabla_sesgo, aes(x=mDist_perc, color=condicion_sala, fill=condicion_sala)) +
#ggplot(tabla_sesgo, aes(x=mDist_perc))+
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)+
 # geom_vline(data=mean(mDist_perc), aes(xintercept=grp.mean, color=condicion_sala),
#             linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title="Respuesta histogram plot",x="Distancia", y = "Density")+
  theme_classic()

plot(histograma)

mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "histograma.png", sep = '')
ggsave(mi_nombre_de_archivo, plot=histograma, width=10, height=10, units="cm", limitsize=FALSE, dpi=300)

