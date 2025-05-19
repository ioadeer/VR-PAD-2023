# Analisis de N50 de ADP
# Intra y entre

# dependencies ------------------------------------------------------------


library(tidyverse)
library(Routliers)
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
#library(webshot)
#library(officer)
library(effects)
library(effectsize)



# load data ---------------------------------------------------------------


results_tbl <- read.csv("./Experiment_1/Exp_1_ADP/ResultsData/Dresults_nuevos.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)

myViridis <- viridisLite::viridis(alpha=1, n= 3)

results_tbl <- results_tbl %>%
  subset(block == 1)

#  Entre SUJETOS solo block 1
# Primer modelo
m.Dist1 <-  lmer(log10(perc_dist) ~ log10(target_distance)*room_condition+(1+log10(target_distance)|subject)+(0+room_condition|subject),
                 data = results_tbl) 
mDist1stats <- extract_stats(ggcoefstats(m.Dist1))
r.squaredGLMM(m.Dist1)
#write.table(m.Dist1.stats , file = "new_Exp_1_ADP/stats/m_Dist1_model.csv")
tidy(m.Dist1)
#tab_model(m.Dist1, file ="./Experiment_1/Exp_1_ADP/stats/first_block_model.html")

anova(m.Dist1)
anov1 = anova(m.Dist1)

write.csv(anov1 , file = "./Experiment_1/Exp_1_ADP/stats/anova.csv")

# libreria effect size
eta_sqrd = eta_squared(anov1)

write.csv(eta_sqrd, file="./Experiment_1/Exp_1_ADP/stats/efect_size_partial_eta_sqrd.csv")

results_tbl$Modelfitted1<-predict(m.Dist1)

# Todos los sujetos
FittedlmPlot1 <-ggplot()+
  facet_grid(subject ~ room_condition, labeller=label_both)+
  geom_line(data = results_tbl, aes(x = target_distance, y = 10^Modelfitted1))+
  geom_point(data = results_tbl, aes(x = target_distance, y =perc_dist, group=subject,colour = subject), size=3)+
  #  coord_cartesian(ylim = c(.03,.074))+ 
  xlab("Targent_distance")+ylab("Perceived_distance")
FittedlmPlot1

Final.Fixed<-effect(c("log10(target_distance)*room_condition"), m.Dist1)

# Grafico poblacional
Final.Fixed<-as.data.frame(Final.Fixed)

Final.Fixed.Plot <-ggplot(data = Final.Fixed, aes(x = target_distance, y =10^fit, group=room_condition))+
  # coord_cartesian(xlim=c(0,4),ylim = c(0,.7))+ 
  geom_line(aes(color=room_condition), size=2)+
  geom_ribbon(aes(ymin=10^fit-10^se, ymax=10^fit+10^se,fill=room_condition),alpha=.2)+
  xlab("Target_distance")+
  ylab("Perceived_distance")+
  # scale_color_manual(values=c("blue", "red"))+
  # scale_fill_manual(values=c("blue", "red"))+
  theme_bw()+
  theme(text=element_text(face="bold", size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_rect(fill = NA, colour = "NA"),
        axis.line = element_line(size = 1, colour = "grey80"),
        legend.title=element_blank(),
        legend.position = c(.2, .92))
Final.Fixed.Plot


eq1 <- substitute("Congruent VE:" ~~~ italic(y) == k %.% italic(X)^italic(a),
                  list(k = round(10^mDist1stats$tidy_data$estimate[[1]],digits = 2),
                       a = round(mDist1stats$tidy_data$estimate[[2]], digits = 2)))
eq2 <- substitute("Small VE:"~~~italic(y) == k %.% italic(X)^italic(a),
                  list(k = round(10^(mDist1stats$tidy_data$estimate[[1]]+mDist1stats$tidy_data$estimate[[3]]), digits = 2),
                       a = round(mDist1stats$tidy_data$estimate[[2]]+mDist1stats$tidy_data$estimate[[4]], digits = 2)))
#eq3 <- substitute("r.squared:"~~~italic(R)^italic(2) == italic(b),
#                  list(b = round(r.squaredGLMM(m.Dist1)[2], digits = 2)))
eq1

results_tbl <- results_tbl %>%
  mutate(
    perc_dist_log_10 = log10(perc_dist)
  )

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
  scale_colour_manual(values = c(myViridis[1], myViridis[2])) +
  scale_fill_manual(values = c(myViridis[1], myViridis[2])) +
  geom_line(data = Final.Fixed, aes(x = target_distance, y =10^fit, group=room_condition, color=room_condition))+
  geom_label(x = -0.1, y = 3.8, label = as.character(as.expression(eq1)), 
             hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = myViridis[2],
             #family="Times New Roman"
  )+
  geom_label(x = -0.1, y = 4.75, label = as.character(as.expression(eq2)), 
             hjust = 0, nudge_x =  0,parse = TRUE, size = 4, 
             color = myViridis[1],
             #family="Times New Roman"
  )+
  #geom_text(x = 0.2, y = 6.0, label = as.character(as.expression(eq3)), hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#009E73")+
  scale_x_continuous(name="Distance source (m)", limits = c(0,10)) +
  scale_y_continuous(name="Perceived distance (m)",   limits = c(0,5)) +
  scale_color_manual(labels = c("Congruent VE", "Small VE"), values =c(myViridis[2], myViridis[1]))+
  #theme_pubr(base_size = 12, margin = TRUE)+
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        #text=element_text(family="Times New Roman"
        ) 


f1

# signed bias -------------------------------------------------------------


#m.logSignedBias <-  lmer(log_bias_m ~ log10(target_distance)*room_condition+(1+log10(target_distance)|subject)+(0+room_condition|subject),
#                 data = results_tbl)

#Final.FixedlogSB <-effect(c("log10(target_distance)*room_condition"), m.logSignedBias)


m.logSignedBias <-  lmer(log_bias_m ~target_distance*room_condition+(1+target_distance|subject)+(0+room_condition|subject),
                         data = results_tbl)


tab_model(m.logSignedBias, file ="./Experiment_1/Exp_1_ADP/stats/logsignedbias_model.html")

Final.FixedlogSB <-effect(c("target_distance*room_condition"), m.logSignedBias)

Final.FixedlogSB<-as.data.frame(Final.FixedlogSB)

logSignedBiasStats <- extract_stats(ggcoefstats(m.logSignedBias))
logSignedBiasStats$tidy_data
anovlogSB <- anova(m.logSignedBias)
anovlogSB
#write_csv(anovlogSB, file ="./Experiment_1/Exp_1_ADP/stats/log_signed_bias_anova.csv")
effect_size = eta_squared(anovlogSB)


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


#m.logSignedBias <-  lmer(log_bias_m ~ log10(target_distance)*room_condition+(1+log10(target_distance)|subject)+(0+room_condition|subject),
#                 data = results_tbl)

#Final.FixedlogSB <-effect(c("log10(target_distance)*room_condition"), m.logSignedBias)


m.logSignedBiasVar <-  lmer(log_bias_var ~target_distance*room_condition+(1+target_distance|subject)+(0+room_condition|subject),
                            data = results_tbl)

#tab_model(m.logSignedBiasVar, file ="./Experiment_1/Exp_1_ADP/stats/logsignedbias_var_model.html")

Final.FixedlogSBVar <-effect(c("target_distance*room_condition"), m.logSignedBiasVar)

Final.FixedlogSBVar<-as.data.frame(Final.FixedlogSBVar)

logSignedBiasVarStats <- extract_stats(ggcoefstats(m.logSignedBiasVar))
logSignedBiasVarStats$tidy_data
anovlogSBVar <- anova(m.logSignedBiasVar)
anovlogSBVar
#write_csv(anovlogSBVar, file = "./Experiment_1/Exp_1_ADP/stats/logsignedbias_var_anova.csv")

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
  scale_y_continuous(name="Signed log bias var (m)",   limits = c(0,.15)) +
  scale_color_manual(labels = c("NVI", "VI"), values =c(myViridis[1], myViridis[2]))+
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

#tab_model(m.logUnsignedBias, file ="./Experiment_1/Exp_1_ADP/stats/logUnsignedbias_model.html")
Final.FixedlogUB <-effect(c("target_distance*room_condition"), m.logUnsignedBias)

Final.FixedlogUB<-as.data.frame(Final.FixedlogUB)

logUnsignedBiasStats <- extract_stats(ggcoefstats(m.logUnsignedBias))
logUnsignedBiasStats$tidy_data
anovlogUB <- anova(m.logUnsignedBias)
anovlogUB

#write_csv(anovlogUB, file="./Experiment_1/Exp_1_ADP/stats/logunsigned_bias_anova.csv")

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
  geom_line(data = Final.FixedlogUB, aes(x = target_distance, y =fit, group=room_condition, color=room_condition))+
  scale_x_continuous(name="Distance source (m)", limits = c(0,10)) +
  scale_y_continuous(name="Unsigned log bias (m)",   limits = c(0,.7)) +
  scale_color_manual(labels = c("NVI", "VI"), values =c(myViridis[1], myViridis[2]))+
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank(),)
#text=element_text(family="Times New Roman", size=10)) 
f3



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

#plot<- ggarrange(ba,mi,fa, ncol=3, nrow=1, common.legend = TRUE,legend="bottom")

main_figure <- annotate_figure(main_figure, top = text_grob("All participants", 
                                      color = "black", face = "bold", size = 12))

# save plot ---------------------------------------------------------------

figures_folder = "./Experiment_1/Exp_1_ADP/figura_final/"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "first_block", ".png", sep = '')
ggsave(device = "png", mi_nombre_de_archivo, plot=main_figure, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)

## asi me lo guarda bien
png(mi_nombre_de_archivo, res=600, units="cm", width=15, height=15)
plot(main_figure)
dev.off()
