# Analisis de datos del experi 32 sujetos
# El 5 es outlier

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


# Load data -----------------------------------------------------------------

## En este script vamos a analizar todo lo que teniamos
## Primeros 11
tabla.primeros_11 <- read.csv('./analisis-pad-main/data/1_11_s/data_1-11.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)
# nos quedamos con 1er bloque
# sacar 5 porque es outlier
tabla.primeros_11 <- tabla.primeros_11 %>%
  filter(nbloque == 1) %>%
  filter(nsub != 5) %>%
  arrange(nsub)

tabla.12_al_32 <- read.csv('./analisis-pad-main/data/12_32_s/data-s12-32-1-block.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)

tabla.12_al_32 <- tabla.12_al_32 %>%
  filter(nbloque == 1)

tabla.raw <- do.call("rbind", list(tabla.primeros_11, tabla.12_al_32))

write.table(tabla.raw, file="./analisis-pad-main/data/data-1-32-bloque-1.csv", row.names = FALSE)

# DESDE ACA

tabla.raw <- read.csv('./analisis-pad-main/data/data-1-32-bloque-1.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)



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

mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "histograma.png", sep = '')
ggsave(mi_nombre_de_archivo, plot=histograma, width=10, height=10, units="cm", limitsize=FALSE, dpi=300)


# Estadistica -------------------------------------------------------------



tabla.test = subset(tabla.ind, select = c(nsub,log_distancia, log_respuesta_mean ))

# Intento con broom    -----------  
# https://cran.r-project.org/web/packages/broom/vignettes/broom.html
#  https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html
lmexponent = tabla.test %>% group_by(nsub) %>% do(model = lm(log_respuesta_mean ~ log_distancia, data = .))
different_curves_per_subject <- tabla.test %>% group_by(nsub) %>% do(model = lm(log_respuesta_mean ~ log_distancia, data = .))
test <- tidy(lmexponent[1])
tidy(tabla.test)

augmented = augment(linear_reg, different_curves_per_subject)

augmented = augment(tabla.test)

augmented %>%
  ggplot(mapping = aes(x = log_distancia)) +
  geom_point(mapping = aes(y = log_respuesta_mean)) +
  geom_line(mapping = aes(y = .fitted), color = "red")

# Intento dos con broom
tabla.ind <- tabla.ind %>%
  arrange(nsub)

lmfit <- lmer(log_respuesta_mean ~ log_distancia * condicion_sala+ (1|nsub), tabla.ind)

summary(lmfit)
tidy(lmfit)
broom_augemnted_tibble <- augment(lmfit, tabla.ind)

log_fit <- ggplot(broom_augemnted_tibble, aes(x=.fitted, y=log_respuesta_mean)) +
  geom_point(aes(color = nsub), alpha =0.6) +
  geom_smooth(method="loess", alpha = 0.2, color = "#20948b") +
  labs(x = '', y = '', title = "Fitted vs valores reales log log")

plot(log_fit)
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "broom/fit_log_log.png", sep = '')
ggsave(mi_nombre_de_archivo, plot =log_fit, dpi=200)

log_resid <- ggplot(broom_augemnted_tibble, aes(x=.resid)) +
  geom_histogram(bins = 15, fill="#20948b") +
  labs(x="", y="", title ='Distribucion residuos modelo log log')

plot(log_resid)
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "broom/resi_log_log.png", sep = '')
ggsave(mi_nombre_de_archivo, plot =log_resid, dpi=200)

# Linealizando modelo

broom_augemnted_tibble <- broom_augemnted_tibble %>% 
  mutate(fitted_lineal = exp(.fitted)) %>%
  mutate(resid_lineal = exp(.resid))

linear_fit <- ggplot(broom_augemnted_tibble, aes(x=fitted_lineal, y=respuesta[,"mean"])) +
  geom_point(aes(color = nsub), alpha =0.6) +
  geom_smooth(method="loess", alpha = 0.2, color = "#20948b") +
  labs(x = '', y = '', title = "Fitted vs valores reales linear")

plot(linear_fit)
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "broom/fit_linear.png", sep = '')
ggsave(mi_nombre_de_archivo, plot =linear_fit, dpi=200)

linear_resid <- ggplot(broom_augemnted_tibble, aes(x=resid_lineal)) +
  geom_histogram(bins = 15, fill="#20948b") +
  labs(x="", y="", title ='Distribucion residuos modelo lienal')

plot(linear_resid)
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "broom/resid_linear.png", sep = '')
ggsave(mi_nombre_de_archivo, plot =linear_resid, dpi=200)


# otro intento ------------------------------------------------------------

# ref https://r4ds.had.co.nz/many-models.html
tabla.ind %>%
  ggplot(aes(log_distancia, log_respuesta_mean, group = nsub)) +
  geom_line(alpha = 1/3)

# Agrupar por sujeto 

por_sujeto <- tabla.ind %>%
  group_by(nsub) %>%
  nest()

#  Estp de modelo para aplicar como funcion tiene que ser asi no 
#  como el que use en modelo_sujeto
# country_model <- function(df) {
#   lm(lifeExp ~ year, data = df)
# }

# ASI NO
modelo_sujeto <- function(df) {
  lmer(log_respuesta_mean ~ log_distancia * condicion_sala+ (1|nsub), tabla.ind)
}

# ASI SI
modelo_sujeto  <- function(df) {
  lmer(log_respuesta_mean ~ log_distancia , data = df)
}

#modelos <- map(por_sujeto$data, modelo_sujeto)

library(modelr)

por_sujeto <- por_sujeto %>%
  mutate(model = map(data, modelo_sujeto))

por_sujeto <- por_sujeto %>%
  mutate(resids = map2(data, model, add_residuals))

resids <- unnest(por_sujeto, resids)

resids_modelr <- resids %>% 
  ggplot(aes(log_distancia, resid)) +
  geom_line(aes(group = nsub), alpha = 1 / 3) + 
  geom_smooth(se = FALSE)

model_modelr <- resids %>% 
  ggplot(aes(log_distancia,log_respuesta_mean)) +
  geom_line(aes(group = nsub), alpha = 1 / 3) + 
  geom_smooth(se = FALSE) +
  label()

graphs <- ggarrange(model_modelr,resids_modelr,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)
plot(graphs)
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "adjust-and-resids_modelr.png", sep = '')
ggsave(mi_nombre_de_archivo, plot =graphs, dpi=200)

d = coef(por_sujeto$model[[2]])[1][[1]]
d = coef(por_sujeto$model[[2]])[2][[1]]

d[[1]]

por_sujeto %>% 
  ggplot(aes(log_distancia, log_respuesta_mean)) +
  geom_line(aes(group = nsub), alpha = 1 / 3) + 
  geom_smooth(se = FALSE)

library(broom)
  
por_sujeto_test <- por_sujeto %>%
  mutate(sum = model[[1]][1]) %>%
  mutate(intercept = sum[[1]][1]) %>%
  mutate(slope = sum[[1]][2])

por_sujeto_test_unnested <- unnest(por_sujeto_test,data)

por_sujeto_test_unnested %>% 
  ggplot(aes(log_distancia, log_respuesta_mean)) +
  geom_point(aes(y =log_respuesta_mean)) +
  geom_abline(aes(intercept = intercept, slope = slope)) +
  geom_smooth(se = FALSE)

por_sujeto_test_unnested %>% 
  ggplot(aes(log_distancia, log_respuesta_mean)) +
  geom_point(aes(y =log_respuesta_mean)) +
  geom_abline(aes(intercept = intercept, slope = slope)) +
  geom_smooth(se = FALSE)

# Ajustes individuales

ajust_sub <- por_sujeto_test_unnested %>% 
  ggplot(aes(log_distancia, log_respuesta_mean)) +
  geom_abline(aes(intercept = intercept, slope = slope)) +
  facet_grid(condicion_sala ~ nsub) +
  geom_smooth(se = FALSE)

plot(ajust_sub)

mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "ajuste_sub.png", sep = '')
#ggsave(mi_nombre_de_archivo, plot = g1, width=44, height=14, units="cm", limitsize=FALSE, dpi=200)
ggsave(mi_nombre_de_archivo, plot =ajust_sub, width=100, height=20, units="cm", limitsize=FALSE, dpi=200)

# Histograma de exponentes de respuesta

histograma_exponentes <- ggplot(por_sujeto_test_unnested, aes(x=slope)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)+
  # geom_vline(data=mean(mDist_perc), aes(xintercept=grp.mean, color=condicion_sala),
  #             linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title="Respuesta histogram plot",x="Exponentes", y = "Density")+
  theme_classic()

plot(histograma_exponentes)

mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "histogramas_exponentes_sub.png", sep = '')
ggsave(mi_nombre_de_archivo, plot =histograma_exponentes, dpi=200)

por_sujeto_test_2 <- por_sujeto_test_unnested %>%
  mutate(predi = predict(model[[1]]))

por_sujeto_test_2 <- por_sujeto_test_2 %>%
  mutate(predi_linear = exp(predi))

predictions  <- por_sujeto_test_2 %>% 
  ggplot(aes(distancia, predi_linear)) +
  geom_point(aes(y =respuesta[,1])) +
  facet_grid(condicion_sala ~ nsub) +
  geom_smooth(se = FALSE) +
  labs(x="", y="", title ='Prediccion hecha con log log pasada lineal')

plot(predictions)  

mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "prediccion_lineal.png", sep = '')
ggsave(mi_nombre_de_archivo, plot =predictions, width=100, height=20, units="cm",limitsize=FALSE,  dpi=200)


#lm 
# Modelo lineal de tabla poblacional
# tabla pob_2 va tener valores log log
tabla.pob_2 <- tabla.pob %>%
  mutate(log_respuesta = log(respuestapob[,"mean"])) %>%
  mutate(log_distancia = log(distancia))

pob_model <- lm(log_respuesta ~ log_distancia, tabla.pob_2)

pob_model <- lm(log_respuesta ~ log_distancia + condicion_sala, tabla.pob_2)

pob_model <- lm(log(respuestapob[,"mean"]) ~ log(distancia), tabla.pob_2)

summary(pob_model)
anova(pob_model)

intercept = pob_model[[1]][[1]]
slope = pob_model[[1]][[2]]
slope2 = pob_model[[1]][[3]]

tabla.pob_2 <- tabla.pob_2 %>%
  mutate(predi = predict(pob_model))

tabla.pob_2 <- tabla.pob_2 %>%
  mutate(predi_linear = exp(predi))

tabla.pob_2 <- tabla.pob_2 %>%
  mutate(predi_linear2 = exp(intercept)* distancia^slope)

poblacional_model <- tabla.pob_2 %>% 
  ggplot(aes(distancia, predi),color = condicion_sala, group = condicion_sala) +
  geom_point(aes(y =respuestapob[,"mean"])) +
  facet_grid(condicion_sala) +
  geom_smooth(se = FALSE) +
  labs(x="", y="", title ='Prediccion hecha con log log pasada lineal')

poblacional_model_log_log <- ggplot(tabla.pob_2, aes(x = log_distancia, y = predi)) +
  geom_line(color="red", size=1) +
  scale_x_continuous(name="Distance source (m) log")+#, breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,20)) +
  scale_y_continuous(name="Perceived distance (m) log")+#,  breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,20)) +
  #geom_line(aes(x=distancia, y=respuestapob[,"mean"]),color="blue", size = 1) +
  geom_line(aes(x=log_distancia, y=log_respuesta),color="blue", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  facet_grid(. ~ condicion_sala) + 
  theme_linedraw(base_size = 9)

plot(poblacional_model_log_log)

poblacional_model_linear <- ggplot(tabla.pob_2, aes(x = distancia, y = predi_linear2)) +
  geom_line(color="red", size=1) +
  geom_errorbar(data=tabla.pob_2, color="black",alpha = 0.5, width=0.75, size=0.75,
                mapping=aes(ymin = respuestapob[,"mean"] - respuestapob[,"sem"],
                            ymax = respuestapob[,"mean"] + respuestapob[,"sem"]))+
  #limitado
  scale_x_continuous(name="Distance source (m)" ) + #, breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,20)) +
  scale_y_continuous(name="Perceived distance (m)")  +#,  breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,20)) +
  # entero
  #scale_x_continuous(name="Distance source (m)" , breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,8)) +
  #scale_y_continuous(name="Perceived distance (m)",  breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,8)) +
  geom_line(aes(x=distancia, y=respuestapob[,"mean"]),color="blue", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  facet_grid(. ~ condicion_sala) + 
  theme_linedraw(base_size = 9)

plot(poblacional_model_linear)

poblacional_model_log_en_uno <- ggplot(tabla.pob_2, aes(x = log_distancia, y = predi)) +
  geom_line(color="red", size=1) +
  #limitado
  scale_x_continuous(name="Distance source (m)" ) + #, breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,20)) +
  scale_y_continuous(name="Perceived distance (m)")  +#,  breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,20)) +
  # entero
  #scale_x_log10(name="Distance source (m)" , breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,10)) +
  #scale_y_log10(name="Perceived distance (m)",  breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,10)) +
  geom_line(aes(x=log_distancia, y=log_respuesta, color=condicion_sala, group= condicion_sala), size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  #facet_grid(. ~ condicion_sala) + 
  theme_linedraw(base_size = 9) +
  labs(x="", y="", title ='Prediccion hecha con log log, vista con dos salas')

plot(poblacional_model_log_en_uno)

mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "prediccion_dos_salas_en_uno-entero.png", sep = '')
ggsave(mi_nombre_de_archivo, plot =poblacional_model_linear_en_uno,limitsize=FALSE,  dpi=200)

poblacional_model_linear_en_uno <- ggplot(tabla.pob_2, aes(x = distancia, y = predi_linear2)) +
  geom_line(aes(x = distancia, y = predi_linear2),color="red", size=1) +
  geom_line(aes(x = distancia, y = predi_linear3),color="green", size =1) +
  geom_errorbar(data=tabla.pob_2, color="black",alpha = 0.5, width=0.25, size=0.75,
                mapping=aes(ymin = respuestapob[,"mean"] - respuestapob[,"sem"],
                            ymax = respuestapob[,"mean"] + respuestapob[,"sem"]))+
  #limitado
  #scale_x_continuous(name="Distance source (m)" ) + #, breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,20)) +
  #scale_y_continuous(name="Perceived distance (m)")  +#,  breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,20)) +
  # entero
  scale_x_continuous(name="Distance source (m)" , breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,10)) +
  scale_y_continuous(name="Perceived distance (m)",  breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,10)) +
  geom_line(aes(x=distancia, y=respuestapob[,"mean"], color=condicion_sala, group= condicion_sala), size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  #facet_grid(. ~ condicion_sala) + 
  theme_linedraw(base_size = 9) +
  labs(x="", y="", title ='Prediccion hecha con log log pasada lineal, vista con dos salas')

plot(poblacional_model_linear_en_uno)




## Guardando resultados de modelos estadisticos

pob_model <- lm(log_respuesta ~ log_distancia, tabla.pob_2)
pob_model <- lm(log(respuestapob[,"mean"]) ~ log(distancia), tabla.pob_2)

sink("./analisis-pad-main/resutlados_estadisticos/22-junio-2023/lm_simple/lm_log-dist-percibida_log-dis-real.png")
print(summary(pob_model))
sink()

sink("./analisis-pad-main/resutlados_estadisticos/22-junio-2023/lm_simple/lm_log-dist-percibida_log-dis-real-ANOVA.png")
anova(pob_model)
sink()

## Modelos con 2 predictores (condicion sala)

pob_model_2_pred <- lm(log_respuesta ~ log_distancia + condicion_sala, tabla.pob_2)

sink("./analisis-pad-main/resutlados_estadisticos/22-junio-2023/lm_2_predictors/lm_log-dist-percibida_log-dis-real_+_cond-sala.png")
print(summary(pob_model_2_pred))
sink()

sink("./analisis-pad-main/resutlados_estadisticos/22-junio-2023/lm_2_predictors/lm_log-dist-percibida_log-dis-real_+_cond-sala-ANOVA.png")
anova(pob_model_2_pred)
sink()


boxplot <- ggplot(por_sujeto_test_2, aes(x=nsub, y=slope))+
  geom_boxplot() 
  #geom_jitter(shape=1, position=position_jitter(0.2))

plot(boxplot)

# Correlacion 
# Distancia de la sala
# maxima distancia reportada
# esto esta en

# modelo de efectos mixtos 
m.distancia <- lmer(log_respuesta_mean ~ condicion_sala * log_distancia + (1|nsub), 
                    data = tabla.ind)

sink("./analisis-pad-main/resutlados_estadisticos/22-junio-2023/lm_2_predictors/lmer_log-dist-percibida_log-dis-real_+_cond-sala.png")
print(summary(m.distancia))
sink()

sink("./analisis-pad-main/resutlados_estadisticos/22-junio-2023/lm_2_predictors/lmer_log-dist-percibida_log-dis-real_+_cond-sala-ANOVA.png")
anova(m.distancia)
sink()
