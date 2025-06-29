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



# Figuras -----------------------------------------------------------------

figures_folder = "./analisis-piloto/figuras/"

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

mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "distancia_individuales.png", sep = '')
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



# Estadistica -------------------------------------------------------------

# Both tasks
tabla.ind <- tabla.ind %>% 
  mutate(log_respuesta_mean = log(respuesta[,"mean"])) %>%
  mutate(log_distancia = log(distancia))

# OLD  
m.distancia <- lmer(respuesta[,"mean"] ~ condicion_sala + distancia + (1|nsub), 
                    data = tabla.ind)
# LOG LOG
m.distancia <- lmer(log_respuesta_mean ~ condicion_sala * log_distancia + (1|nsub), 
                    data = tabla.ind)

summary(m.distancia)
ggcoefstats(m.distancia, output = "tidy") %>% select(-label)
anova(m.distancia)

sink("./analisis-piloto/res-estadistica/respuesta/lm_con_log-respuesta-mean_fijos-sala*log-distancia_aleatorio-nsub.txt")
print(summary(m.distancia))
sink() 

sink("./analisis-piloto/res-estadistica/respuesta/ANOVA_de_lm_con_log-respuesta-mean_fijos-sala*distancia_aleatorio-nsub.txt")
print(anova(m.distancia))
sink() 

m.distancia_fixed <- lm(log_respuesta_mean ~ condicion_sala*log_distancia, 
                 data = tabla.ind)

summary(m.distancia_fixed)


# Analisis de potencia ------------------------------------------------




# Statistical analysis bias ####

m.Reaching <- lm(mSesgoRel ~ condicion_sala, 
                 data = tabla_sesgo)

sink("./analisis-piloto/res-estadistica/sesgo/lm_con_meanSesgoRel_fijos-sala.txt")
print(summary(m.Reaching))
sink() 

sink("./analisis-piloto/res-estadistica/sesgo/ANOVA_de_lm_con_meanSesgoRel_fijos-sala.txt")
print(anova(m.Reaching))
sink()





# Graficando estadistica --------------------------------------------------

data_m1.lm <- tabla.ind %>% mutate(pred = predict(m.distancia), res = resid(m.distancia))

data_m1.lm_sala_chica <- tabla.ind %>% 
  filter(condicion_sala == "SALA_CHICA") %>%
  mutate(pred = predict(m.distancia), res = resid(m.distancia))

data_m1.lm <- tabla.ind %>% mutate(pred = predict(m.distancia), res = resid(m.distancia))

figura2 = ggplot(tabla.ind, aes(x=distancia, y=respuesta[,"mean"] ))+
  geom_point(alpha = 0.3)+
 # facet_grid(condicion_sala)+
  theme_pubr(base_size = 12, margin = TRUE)+
  #scale_x_continuous(name= "Tiempo [seg]", breaks=c(0,3500,7000), labels=c("0","3k5","7k")) +
  #labs(y = "Nivel de presión [dBA]") +
  geom_point(data=data_m1.lm, aes(x=respuesta[,"mean"])) +
  theme(legend.position = "top",
        legend.title = element_blank())+
  geom_line(data=data_m1.lm, aes(x=respuesta[,"mean"],group=condicion_sala), color='red')

figura2

#fm1 <- lmer("Reaction ~ Days + (Days | Subject)", sleepstudy)

  ## define n colors

par(mfrow=c(1, 2))

m.distancia_fixed <- lm(log_respuesta_mean ~ condicion_sala * log_distancia, 
                        data = tabla.ind)

coef <- coefficients(m.distancia_fixed)

summary(m.distancia_fixed)
anova(m.distancia_fixed)

#fe <- fixef(m.distancia_fixed)
plot(log_respuesta_mean ~  log_distancia , tabla.ind, col=clr[as.numeric(nsub)], main='Pred w/ points')
abline(coef[1], coef[2], col ="red")
abline(coef[1], coef[3], col = "green")
abline(coef[1], coef[2]+coef[3], col= "blue")
abline(coef[1], coef[4], col= "blue")



m.distancia <- lmer(log_respuesta_mean ~ condicion_sala * log_distancia + (1|nsub), 
                    data = tabla.ind)



summary(m.distancia)

fe <- fixef(m.distancia)
re <- ranef(m.distancia)$nsub

clr <- rainbow(nrow(re))

#m.distancia <- lmer(log_respuesta_mean ~ condicion_sala + log_distancia + (1|nsub), 
#                    data = tabla.ind)

par(mfrow=c(2, 2))

plot1 <- plot(log_respuesta_mean ~  log_distancia , tabla.ind, col=clr[as.numeric(nsub)], main='Pred w/ points')
lapply(seq_len(nrow(re)), \(x) abline(fe[1] + re[x,1], fe[2] + re[x,1], col=clr[x]))

plot1 <- plot(log_respuesta_mean ~  log_distancia , tabla.ind, col=clr[as.numeric(nsub)], main='Pred w/ points')
lapply(seq_len(nrow(re)), \(x) abline(fe[1] + re[x,1], fe[3] + re[x,1], col=clr[x]))

plot1 <- plot(log_respuesta_mean ~  log_distancia , tabla.ind, col=clr[as.numeric(nsub)], main='Pred w/ points')
lapply(seq_len(nrow(re)), \(x) abline(fe[1] + re[x,1], fe[2]+fe[3] + re[x,1], col=clr[x]))

plot1 <- plot(log_respuesta_mean ~  log_distancia , tabla.ind, col=clr[as.numeric(nsub)], main='Pred w/ points')
lapply(seq_len(nrow(re)), \(x) abline(fe[1] + re[x,1], fe[2]+fe[3]+fe[4] + re[x,1], col=clr[x]))

mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "modelos.png", sep = '')
ggsave(mi_nombre_de_archivo, plot=plot1, width=10, height=10, units="cm", limitsize=FALSE, dpi=300)

png(file="analisis-piloto/figuras/models.png",
    width=600, height=350)
plot1
dev.off()

lapply(seq_len(nrow(re)), \(x) print("s"))

summary(m.distancia)

#plot(Reaction ~ Days, sleepstudy, col=clr[as.numeric(Subject)], main='Pred w/o points', type='n')
#lapply(seq_len(nrow(re)), \(x) abline(fe[1] + re[x, 1], fe[2] + re[x, 2], col=clr[x]))

