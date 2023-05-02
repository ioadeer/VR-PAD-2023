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


rm(list=ls())

data_folder         = "data"
figures_folder = "figuras"
cat('/* Importando datos desde la carpeta \'', data_folder, '\':\n', sep = '')

# Este ciclo lee los archivos CSV generados en Matlab y los
# va cargando sucesivamente en una tabla (dataframe).

c <- 0
for (archivo in dir(data_folder, pattern = '*.csv')) {
  
  ruta_completa <- paste(data_folder, .Platform$file.sep, archivo, sep = '')
  cat('- Cargando archivo: \'', ruta_completa, '\'\n', sep = '')
  tabla_temp <- read.csv(ruta_completa, header = TRUE, sep = ' ', stringsAsFactors = TRUE)
  
  if (c == 0)
    tabla.raw <- tabla_temp
  else
    tabla.raw <- rbind(tabla.raw, tabla_temp)
  
  c <- c + 1
}
# tabla.raw = tabla.raw[0:336,]

cat('\\* Proceso de importacion finalizado.\n')

rm("c", "tabla_temp", "archivo", "ruta_completa")

# readline(prompt="Press [enter] to continue")

# Para finalizar, pasamos todo a tibble
tabla.raw             = tibble(tabla.raw)
idx = tabla.raw$nsub != 5
tabla.raw = tabla.raw[idx,]

idx = tabla.raw$nsub !=1
tabla.raw = tabla.raw[idx,]

idx = tabla.raw$nsub !=3
tabla.raw = tabla.raw[idx,]

idx = tabla.raw$nsub !=7
tabla.raw = tabla.raw[idx,]

idx = tabla.raw$nsub !=9
tabla.raw = tabla.raw[idx,]

idx = tabla.raw$nsub !=11
tabla.raw = tabla.raw[idx,]
# idx = tabla.raw$nbloque == 1
# tabla.raw = tabla.raw[idx,]

tabla.raw$SesgoAbs <-  tabla.raw$respuesta - tabla.raw$distancia
tabla.raw$SesgoRel <- (tabla.raw$respuesta - tabla.raw$distancia) / tabla.raw$distancia


f_promedio <- function(x) c(mean = mean(x),
                            sd   = sd(x),
                            var  = var(x),
                            sem  = sd(x)/sqrt(length(x)),
                            n    = length(x))

# - Nivel individual
tabla.ind <- tibble(aggregate(cbind(respuesta,SesgoRel) ~ nsub*condicion_sala*distancia*nbloque,
                              data = tabla.raw,
                              FUN  = f_promedio,na.action = NULL))

# - Nivel poblacional

tabla.pob <- tibble(aggregate(cbind(respuesta[,"mean"],SesgoRel[,"mean"]) ~ condicion_sala*distancia,
                              data <- tabla.ind,
                              FUN  <- f_promedio,na.action = NULL))

tabla.pob = tabla.pob %>% rename(respuestapob = V1)
tabla.pob = tabla.pob %>% rename(sesgorelpob = V2)

# Grafico individual con brutos ####
g1 <- ggplot(tabla.ind, aes(x = distancia, y = respuesta[,"mean"])) +
  geom_errorbar(data=tabla.ind, color="black",alpha = 1, width=0.3, size=0.1,
                 mapping=aes(ymin = respuesta[,"mean"] - respuesta[,"sem"],
                             ymax = respuesta[,"mean"] + respuesta[,"sem"]))+
  geom_point(color="red", size=4) +
  #geom_abline(intercept = 1, slope = 1,linetype="dashed") +       
  scale_x_continuous(name="Distance source (m)")+#, breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,20)) +
  scale_y_continuous(name="Perceived distance (m)")+#,  breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,20)) +
  geom_jitter(data = tabla.raw, mapping = aes(x=distancia, y=respuesta), color="blue", alpha=.8, shape=4, size=2.2, stroke=.2,
              position = position_jitter(width=.1, height=.1)) + 
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  facet_grid(condicion_sala ~ nsub) + 
  theme_linedraw(base_size = 9)

plot(g1)
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "distancia_individuales.png", sep = '')
ggsave(mi_nombre_de_archivo, plot = g1, width=44, height=14, units="cm", limitsize=FALSE, dpi=200)

# Grafico poblacional con bruto ####

g2 <- ggplot(filter(tabla.pob, condicion_sala != "SALA_PARLANTES"), aes(x = distancia, y = respuestapob[,"mean"],color = condicion_sala, group = condicion_sala)) +
  # geom_errorbar(data=filter(tabla.pob, condicion_sala != "SALA_PARLANTES"), alpha = 1, width=0.5, size=1.2,
  #               mapping=aes(ymin = respuestapob[,"mean"] - respuestapob[,"sem"], 
  #                           ymax = respuestapob[,"mean"] + respuestapob[,"sem"]))+ 
  geom_point(size=3, stroke = 1) + geom_line(size = 3)+
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

######

library(lme4)
library(lmerTest)
library(emmeans)
library(jtools)
library(broom)
library(ggstatsplot)
library(gmodels)

# Data Loading ####
tabla.ind$nsub <- factor(tabla.ind$nsub)
tabla.ind$condicion_sala <- factor(tabla.ind$condicion_sala)
# tabla.ind$Condicion <- factor(tabla.ind$Condicion)
# tabla.ind$Silencio <- factor(tabla.ind$Silencio, levels = c("Ninguno", "Corto", "Largo", "Extra largo"))

# Statistical analysis bias ####
# Both tasks
m.distancia <- lmer(respuesta[,"mean"] ~ condicion_sala*distancia + (1|nsub), 
              data = tabla.ind)
summary(m.distancia)
ggcoefstats(m.distancia, output = "tidy") %>% select(-label)
anova(m.distancia)


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


m.Reaching <- lm(mSesgoRel ~ condicion_sala, 
                 data = tabla_sesgo)
summary(m.Reaching)
summ(m.Reaching)
anova(m.Reaching)


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

