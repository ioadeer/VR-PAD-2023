# Experimento 3
#  Ultimo con outliers scados con sesgo logaritmico
# y hacemos modelo d eefectos mixtos con sesgo logaritmico
#  En este script voy a armar la figura
#  La idea es tener por una lado
#  Un grafico con el modelo, y las medidas promedio de PAD
#  Por otro lado tener las dos figuras de sesgo

# dependencies ------------------------------------------------------------
#library(tidyverse)
library(tidyr)
library(dplyr)
#library(Routliers)
library(lme4)
library(nlme)
library(sjPlot)
library(MuMIn)
library(lmerTest)
library(jtools)
library(gdtools)
library(broom)
library(ggstatsplot)
library(modelsummary)
library(ggpubr)
#library(flextable)
library(webshot)
#library(officer)
library(effects)
library(effectsize)



# load data  ---------------------------------------------------------------


#theme_set(theme_gray(base_family = "DejaVuSerif"))
rm(list=ls())
figures_folder = "./Experiment_3/Exp_3_ADP_vr/Figura_final"
results_tbl <- read.csv("Experiment_3/Exp_3_ADP_vr/ResultsData/Dresults_nuevos.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)

#cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")

myViridis <- viridisLite::viridis(alpha=1, n= 3)


# make model figure -------------------------------------------------------


results_tbl <- results_tbl %>%
  mutate(
    perc_dist_log_10 = log10(perc_dist)
  )

m.Dist1 <-  lmer(log10(perc_dist) ~ log10(target_distance)*room_condition+(1+log10(target_distance)|subject)+(0+room_condition|subject),
                 data = results_tbl)

Final.Fixed<-effect(c("log10(target_distance)*room_condition"), m.Dist1)

#Final.Fixed<-effect(c("log10(target_distance)*room_condition"), m.Dist1, xlevels=list(target_distance=seq(2,9,1)))
#2,2.7,3.65,4.9,6.65,9
# xlevels=list(TimeStep=seq(0,4,1))
# Grafico poblacional
Final.Fixed<-as.data.frame(Final.Fixed)

mDist1stats <- extract_stats(ggcoefstats(m.Dist1))
mDist1stats$tidy_data
anov1 <- anova(m.Dist1)
anov1
#write.csv(anov1, file="./Experiment_2/Exp_2_ADP_control/stats/model_anova.csv")
effect_size = eta_squared(anov1)
#write.csv(effect_size, file="./Experiment_2/Exp_2_ADP_control/stats/model_eta_squared.csv")
partial_effect_size = eta_squared(anov1, partial= TRUE)
#anov1
# F value, NumDF, DenDF

r.squaredGLMM(m.Dist1)
# para imprimir
#tab_model(m.Dist1, file ="./Experiment_2/Exp_2_ADP_control/stats/model.html")


eq1 <- substitute("NVI:"~italic(y) == k %.% italic(X)^italic(a),
                  list(k = round(10^mDist1stats$tidy_data$estimate[[1]],digits = 2),
                       a = round(mDist1stats$tidy_data$estimate[[2]], digits = 2)))
eq2 <- substitute("VE:"~italic(y) == k %.% italic(X)^italic(a),
                  list(k = round(10^(mDist1stats$tidy_data$estimate[[1]]+mDist1stats$tidy_data$estimate[[3]]), digits = 2),
                       a = round(mDist1stats$tidy_data$estimate[[2]]+mDist1stats$tidy_data$estimate[[4]], digits = 2)))
#eq3 <- substitute("r.squared:"~~~italic(R)^italic(2) == italic(b),
#                  list(b = round(r.squaredGLMM(m.Dist1)[2], digits = 2)))
eq1

tabla.pob <- results_tbl %>% group_by(target_distance,room_condition) %>%
  summarise(Mperc_dist  = mean(perc_dist_log_10),
            SDperc_dist = sd(perc_dist_log_10)/sqrt(n()))  %>%
  ungroup()

f1 <- ggplot(tabla.pob, aes(x=target_distance, y =10^Mperc_dist, group = room_condition, color  = room_condition)) +
  geom_pointrange(aes(x = target_distance, y = 10^Mperc_dist, ymin = 10^(Mperc_dist-SDperc_dist), ymax = 10^(Mperc_dist+SDperc_dist)),alpha = 1,
                  position = position_jitterdodge(jitter.width = .1,
                                                  jitter.height = 0,
                                                  dodge.width = .1 ))+
  geom_abline(intercept = 0, slope = 1, linetype=2) +
  geom_line(data = Final.Fixed, aes(x = target_distance, y =10^fit, group=room_condition, color=room_condition))+
  geom_label(x = -0.1, y = 4.0, label = as.character(as.expression(eq1)), 
            hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = myViridis[1],
            )+
            #family="Times New Roman")+
  geom_label(x = -0.1, y = 4.85, label = as.character(as.expression(eq2)), 
            hjust = 0, nudge_x =  0,parse = TRUE, size = 4, color = myViridis[2],
            )+
            #family="Times New Roman")+
  #geom_text(x = 0.2, y = 6.0, label = as.character(as.expression(eq3)), hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#009E73")+
  scale_x_continuous(name="Distance source (m)", limits = c(0,10)) +
  scale_y_continuous(name="Perceived distance (m)",   limits = c(0,10)) +
  scale_color_manual(labels = c("NVI", "VE"), values =c(myViridis[1], myViridis[2]))+
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank(),)
        #text=element_text(family="Times New Roman", size=10)) 


f1
#mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "5. Lme Lineal-Normal", ".jpg", sep = '')
#ggsave(mi_nombre_de_archivo, plot=f1, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)
#anov = anova(m.Dist1)
#anov





# signed bias -------------------------------------------------------------


#m.logSignedBias <-  lmer(log_bias_m ~ log10(target_distance)*room_condition+(1+log10(target_distance)|subject)+(0+room_condition|subject),
#                 data = results_tbl)

#Final.FixedlogSB <-effect(c("log10(target_distance)*room_condition"), m.logSignedBias)


m.logSignedBias <-  lmer(log_bias_m ~target_distance*room_condition+(1+target_distance|subject)+(0+room_condition|subject),
                         data = results_tbl)

Final.FixedlogSB <-effect(c("target_distance*room_condition"), m.logSignedBias)

Final.FixedlogSB<-as.data.frame(Final.FixedlogSB)

logSignedBiasStats <- extract_stats(ggcoefstats(m.logSignedBias))
logSignedBiasStats$tidy_data
anovlogSB <- anova(m.logSignedBias)
anovlogSB

effect_size = eta_squared(anovlogSB)

#results_tbls <- results_tbl %>% 
#  group_by(room_condition,subject) %>%
#  summarise(mBiasSigned  = mean(rel_bias_signed,na.rm=TRUE),
#            SdBiasSigned  = sd(rel_bias_signed,na.rm=TRUE)/sqrt(length(rel_bias_signed)),
#            mBiasUnSigned  = mean(rel_bias_unsigned,na.rm=TRUE),
#            SdBiasUnSigned  = sd(rel_bias_unsigned,na.rm=TRUE)/sqrt(length(rel_bias_unsigned)))  %>%
#  ungroup()

tabla.pob_logSB <- results_tbl %>% group_by(target_distance,room_condition) %>%
  summarise(MlogSB  = mean(log_bias_m),
            SDlogSB = sd(log_bias_m)/sqrt(n()))  %>%
  ungroup()


f2 <- ggplot(tabla.pob_logSB, aes(x=target_distance, 
                                  #y =10^MlogSB,
                                  y =MlogSB, 
                                  group = room_condition, 
                                  color  = room_condition)) +
  geom_pointrange(aes(x = target_distance, 
                      #y = 10^MlogSB, 
                      #ymin = 10^(MlogSB-SDlogSB), 
                      #ymax = 10^(MlogSB+SDlogSB)),
                      y = MlogSB, 
                      ymin = (MlogSB-SDlogSB), 
                      ymax = (MlogSB+SDlogSB)),
                      alpha = 1,
                      position = position_jitterdodge(jitter.width = .1,
                                                      jitter.height = 0,
                                                      dodge.width = .1 ))+
  #geom_abline(intercept = 0, slope = 1, linetype=2) +
  #geom_line(data = Final.FixedlogSB, aes(x = target_distance, y =10^fit, group=room_condition, color=room_condition))+
  geom_line(data = Final.FixedlogSB, aes(x = target_distance, y =fit, group=room_condition, color=room_condition))+
  scale_x_continuous(name="Distance source (m)", limits = c(0,10)) +
  scale_y_continuous(name="Mean log bias (m)",   limits = c(-1,0)) +
  scale_color_manual(labels = c("NVI", "VI"), values =c(myViridis[1], myViridis[2]))+
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank(),)
#text=element_text(family="Times New Roman", size=10)) 
f2

# signed bias var -------------------------------------------------------------


m.logSignedBiasVar <-  lmer(log_bias_var ~target_distance*room_condition+(1+target_distance|subject)+(0+room_condition|subject),
                         data = results_tbl)

Final.FixedlogSBVar <-effect(c("target_distance*room_condition"), m.logSignedBiasVar)

Final.FixedlogSBVar<-as.data.frame(Final.FixedlogSBVar)

logSignedBiasVarStats <- extract_stats(ggcoefstats(m.logSignedBiasVar))
logSignedBiasVarStats$tidy_data
anovlogSBVar <- anova(m.logSignedBiasVar)
anovlogSBVar

#effect_size = eta_squared(anovlogSB)

tabla.pob_logSBVar <- results_tbl %>% group_by(target_distance,room_condition) %>%
  summarise(MlogSBVar  = mean(log_bias_var),
            SDlogSBVar= sd(log_bias_var)/sqrt(n()))  %>%
  ungroup()


f2_var <- ggplot(tabla.pob_logSBVar, aes(x=target_distance, 
                                  #y =10^MlogSB,
                                  y =MlogSBVar, 
                                  group = room_condition, 
                                  color  = room_condition)) +
  geom_pointrange(aes(x = target_distance, 
                      #y = 10^MlogSB, 
                      #ymin = 10^(MlogSB-SDlogSB), 
                      #ymax = 10^(MlogSB+SDlogSB)),
                      y = MlogSBVar, 
                      ymin = (MlogSBVar-SDlogSBVar), 
                      ymax = (MlogSBVar+SDlogSBVar)),
                  alpha = 1,
                  position = position_jitterdodge(jitter.width = .1,
                                                  jitter.height = 0,
                                                  dodge.width = .1 ))+
  #geom_abline(intercept = 0, slope = 1, linetype=2) +
  #geom_line(data = Final.FixedlogSB, aes(x = target_distance, y =10^fit, group=room_condition, color=room_condition))+
  geom_line(data = Final.FixedlogSBVar, aes(x = target_distance, y =fit, group=room_condition, color=room_condition))+
  scale_x_continuous(name="Distance source (m)", limits = c(0,10)) +
  scale_y_continuous(name="Signed log bias var (m)",   limits = c(0,.075)) +
  scale_color_manual(labels = c("NVI", "VE"), values =c(myViridis[1], myViridis[2]))+
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank(),)
#text=element_text(family="Times New Roman", size=10)) 
f2_var





# unsigned bias -------------------------------------------------------------


#m.logUnsignedBias <-  lmer(log_bias_unsigned_m ~ log10(target_distance)*room_condition+(1+log10(target_distance)|subject)+(0+room_condition|subject),
#                         data = results_tbl)

#Final.FixedlogUB <-effect(c("log10(target_distance)*room_condition"), m.logUnsignedBias)


m.logUnsignedBias <-  lmer(log_bias_unsigned_m ~target_distance*room_condition+(1+target_distance|subject)+(0+room_condition|subject),
                         data = results_tbl)

Final.FixedlogUB <-effect(c("target_distance*room_condition"), m.logUnsignedBias)

Final.FixedlogUB<-as.data.frame(Final.FixedlogUB)

logUnsignedBiasStats <- extract_stats(ggcoefstats(m.logUnsignedBias))
logUnsignedBiasStats$tidy_data
anovlogUB <- anova(m.logUnsignedBias)
anovlogUB

effect_sizeUB = eta_squared(anovlogUB)

#results_tbls <- results_tbl %>% 
#  group_by(room_condition,subject) %>%
#  summarise(mBiasSigned  = mean(rel_bias_signed,na.rm=TRUE),
#            SdBiasSigned  = sd(rel_bias_signed,na.rm=TRUE)/sqrt(length(rel_bias_signed)),
#            mBiasUnSigned  = mean(rel_bias_unsigned,na.rm=TRUE),
#            SdBiasUnSigned  = sd(rel_bias_unsigned,na.rm=TRUE)/sqrt(length(rel_bias_unsigned)))  %>%
#  ungroup()

tabla.pob_logUB <- results_tbl %>% group_by(target_distance,room_condition) %>%
  summarise(MlogUB  = mean(log_bias_unsigned_m),
            SDlogUB = sd(log_bias_unsigned_m)/sqrt(n()))  %>%
  ungroup()


f3 <- ggplot(tabla.pob_logUB, aes(x=target_distance, 
                                  #y =10^MlogUB,
                                  y =MlogUB, 
                                  group = room_condition, 
                                  color  = room_condition)) +
  geom_pointrange(aes(x = target_distance, 
                      #y = 10^MlogUB, 
                      #ymin = 10^(MlogUB-SDlogUB), 
                      #ymax = 10^(MlogUB+SDlogUB)),
                      y = MlogUB, 
                      ymin = (MlogUB-SDlogUB), 
                      ymax = (MlogUB+SDlogUB)),
                  alpha = 1,
                  position = position_jitterdodge(jitter.width = .1,
                                                  jitter.height = 0,
                                                  dodge.width = .1 ))+
  #geom_line(data = Final.FixedlogUB, aes(x = target_distance, y =10^fit, group=room_condition, color=room_condition))+
  geom_line(data = Final.FixedlogUB, aes(x = target_distance, y =fit, group=room_condition, color=room_condition))+
    scale_x_continuous(name="Distance source (m)", limits = c(0,10)) +
  scale_y_continuous(name="Unsigned log bias (m)",   limits = c(0,.75)) +
  scale_color_manual(labels = c("NVI", "VI"), values =c(myViridis[1], myViridis[2]))+
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank(),)
#text=element_text(family="Times New Roman", size=10)) 
f3



# main plot V2 ---------------------------------------------------------------

#f1 f6 y f7
main_figure <- ggarrange(f1, 
                         ggarrange(f6, f7, widths = c(2,2),
                                   ncol = 1, labels = c("B", "C")),
                         #nrow = 2, 
                         ncol = 2,
                         labels ="A",
                         heights = c(1, 1),
                         widths = c(1.75,1.25),
                         common.legend = TRUE)
#                    legend = "top")

main_figure


# save plot V2 ---------------------------------------------------------------

figures_folder = "./Exp_2_ADP_control/Figura_final_4"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "exp_V2", ".png", sep = '')
#ggsave(device = "png", mi_nombre_de_archivo, plot=main_figure, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)

## asi me lo guarda bien
png(mi_nombre_de_archivo, res=600, units="cm", width=15, height=10)
plot(main_figure)
dev.off()
#View(systemfonts::system_fonts())
#device = ragg::agg_png, # this is the relevant part

# main plot ---------------------------------------------------------------

#f1 f6 y f7
main_figure <- ggarrange(f1, 
                    ggarrange(f6, f7, widths = c(2,2),
                              ncol = 2, labels = c("B", "C")),
                    nrow = 2, 
                    labels ="A",
                    heights = c(1, 0.75),
                    common.legend = TRUE)
#                    legend = "top")

main_figure



# save plot ---------------------------------------------------------------

figures_folder = "./Exp_2_ADP_control/Figura_final_2/"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "exp", ".png", sep = '')
ggsave(device = "png", mi_nombre_de_archivo, plot=main_figure, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)

## asi me lo guarda bien
png(mi_nombre_de_archivo, res=600, units="cm", width=15, height=15)
plot(main_figure)
dev.off()
#View(systemfonts::system_fonts())
#device = ragg::agg_png, # this is the relevant part
