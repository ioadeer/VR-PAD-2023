# Analisis de N50 de ADP
# Intra y entre

# dependencies ------------------------------------------------------------

#library(tidyverse)
library(tidyr)
library(dplyr)
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
library(webshot)
#library(officer)
library(effects)
library(effectsize)


# load data ---------------------------------------------------------------

results_tbl <- read.csv("./Experiment_1/Exp_1_ADP/ResultsData/Dresults_S1_S50_2_bloques_sin_outliers.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)

#cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")

myViridis <- viridisLite::viridis(alpha=1, n= 3)

# Primero INTRA SUJETOS
# Primer modelo
m.Dist1 <-  lmer(log10(perc_dist) ~ log10(target_distance)*room_condition+(1+log10(target_distance)|subject)+(0+room_condition|subject),
                 data = results_tbl) 
m.Dist1.stats <- extract_stats(ggcoefstats(m.Dist1))
r.squaredGLMM(m.Dist1)
#write.table(m.Dist1.stats , file = "/stats/first_block/m_Dist1_model_all.csv")

anova(m.Dist1)
anov1 = anova(m.Dist1)

# libreria effect size
#eta_sqrd = eta_squared(anov1)

#write.csv(eta_sqrd, file="new_Exp_1_ADP/stats/all_effect_size_partial_eta_sqrd.csv")

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

extract_stats(ggcoefstats(m.Dist1))


anov1 <- anova(m.Dist1)
#write.csv(anov1, file="new_Exp_1_ADP/stats/all_model_anova.csv")
#r.squaredGLMM(m.Dist1)
# para imprimir
#tab_model(m.Dist1, file ="new_Exp_1_ADP/stats/all_model.html")


eq1 <- substitute("LVE:"~italic(y) == k %.% italic(X)^italic(a),
                  list(k = round(10^m.Dist1.stats$tidy_data$estimate[[1]],digits = 2),
                       a = round(m.Dist1.stats$tidy_data$estimate[[2]], digits = 2)))
eq2 <- substitute("SVE:"~italic(y) == k %.% italic(X)^italic(a),
                  list(k = round(10^(m.Dist1.stats$tidy_data$estimate[[1]]+m.Dist1.stats$tidy_data$estimate[[3]]), digits = 2),
                       a = round(m.Dist1.stats$tidy_data$estimate[[2]]+m.Dist1.stats$tidy_data$estimate[[4]], digits = 2)))
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

#tabla.pob$room_condition = factor(tabla.pob$room_condition, 
#                                    levels=c("Smaller VE", 
#                                             "Congruent VE"))

#Final.Fixed$room_condition = factor(Final.Fixed$room_condition, 
#                                   levels=c("Smaller VE", 
#                                            "Congruent VE"))


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
  scale_color_manual(labels = c("Large VE", "Small VE"), values =c(myViridis[2], myViridis[1]))+
  #theme_pubr(base_size = 12, margin = TRUE)+
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        #text=element_text(family="Times New Roman"
        , size=10) 


f1

# signed bias -------------------------------------------------------------

results_tbls <- results_tbl %>% 
  group_by(room_condition,subject) %>%
  summarise(mBiasSigned  = mean(rel_bias_signed,na.rm=TRUE),
            SdBiasSigned  = sd(rel_bias_signed,na.rm=TRUE)/sqrt(length(rel_bias_signed)),
            mBiasUnSigned  = mean(rel_bias_unsigned,na.rm=TRUE),
            SdBiasUnSigned  = sd(rel_bias_unsigned,na.rm=TRUE)/sqrt(length(rel_bias_unsigned)))  %>%
  ungroup()

results_tblp <- results_tbls %>% 
  group_by(room_condition) %>%
  summarise(MBiasSigned  = mean(mBiasSigned,na.rm=TRUE),
            SDBiasSigned  = sd(mBiasSigned,na.rm=TRUE)/sqrt(length(mBiasSigned)),
            MBiasUnSigned  = mean(mBiasUnSigned,na.rm=TRUE),
            SDBiasUnSigned  = sd(mBiasUnSigned,na.rm=TRUE)/sqrt(length(mBiasUnSigned)))  %>%
  ungroup()
# geom_flat_violin(aes(fill = Condicion),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
#   geom_point(aes(x = as.numeric(Tiempo)-.15, y = LeqAS, colour = Condicion),position = position_jitter(width = .05, height = 0), size = 1, shape = 20)+

f6 <-  ggplot(results_tblp, aes(x = room_condition,y = MBiasSigned, colour = room_condition, fill = room_condition)) +
  geom_pointrange(aes(x=room_condition, y=MBiasSigned, ymin=MBiasSigned-SDBiasSigned, ymax=MBiasSigned+SDBiasSigned), size = 0.5)+
  geom_line(aes(group = 1),size = 1.2, alpha=.5)+
  geom_point(data = results_tbls, mapping = aes(x = room_condition,y = mBiasSigned, colour = room_condition, fill = room_condition), alpha = 0.1)+
  geom_line(data = results_tbls, mapping = aes(x = room_condition,y = mBiasSigned, group = subject, colour = room_condition),alpha = 0.1)+
  geom_violin(data= results_tbls,aes(x = room_condition,y = mBiasSigned), trim=TRUE, alpha=0)+
  scale_colour_manual(values = c(myViridis[2], myViridis[1])) + 
  scale_fill_manual(values = c(myViridis[2], myViridis[1])) + 
  geom_abline(slope = 0,
              intercept = 0,
              alpha = 0.5,
              linetype = "dashed") +
  labs(x = "Condition", 
       y = "Relative signed bias") +
  # facet_grid(. ~ type) +
  #annotate("text", x = 1.5, y = 0.3,  label = "*", size = 4) +
  #annotate("segment", x = 1, xend = 2, y = 0.2, yend = 0.2, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(1.5,0,1.00,0.25), "cm"))

f6


m.RelativBias <- lm(mBiasSigned ~ room_condition, 
                    data = results_tbls)
extract_stats(ggcoefstats(m.RelativBias))
#1 (Intercept)                        -0.509
# room_conditionVisual information    0.151 

anov = anova(m.RelativBias)
anov
f6

#write.csv(anov, file="new_Exp_1_ADP/stats/all_signed_bias_anova.csv")

#tab_model(m.RelativBias, file ="new_Exp_1_ADP/stats/all_signed_bias.html")


# no es signficitavo #1 0.18301 0.183011  3.5967 0.06756 .
#mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "13. Bias signed", ".png", sep = '')
#ggsave(mi_nombre_de_archivo, plot=f6, width=15, height=10, units="cm", limitsize=FALSE, dpi=600)

# NO ES PAREADO
testSigendBias <- t.test(filter(results_tbls, 
                                room_condition=="Coincident VE" )$mBiasSigned,
                         filter(results_tbls, 
                                room_condition=="Smaller VE")$mBiasSigned, 
                         paired = FALSE)

testSigendBias

testSignedBias.tidy <- tidy(testSigendBias)
#write.csv(testSignedBias.tidy, file="new_Exp_1_ADP/stats/all_signed_bias_t-test.csv")

#95 percent confidence interval:
#  -0.31498920  0.01249011
#sample estimates:
#  mean of x  mean of y 
#-0.5090052 -0.3577557 


# unsigned bias -----------------------------------------------------------

f7 =  ggplot(results_tblp, aes(x = room_condition,y = MBiasUnSigned, colour = room_condition, fill = room_condition)) +
  geom_pointrange(aes(x=room_condition, y=MBiasUnSigned, ymin=MBiasUnSigned-SDBiasUnSigned, ymax=MBiasUnSigned+SDBiasUnSigned), size = 0.5)+
  geom_point(data = results_tbls, mapping = aes(x = room_condition,y = mBiasUnSigned, colour = room_condition, fill = room_condition), alpha = .1)+
  geom_line(aes(group = 1),size = 1.2, alpha=.5)+
  geom_line(data = results_tbls, mapping = aes(x = room_condition,y = mBiasUnSigned, group = subject, colour = room_condition),alpha = 0.1)+
  geom_violin(data= results_tbls,aes(x = room_condition,y = mBiasUnSigned), trim=TRUE, alpha=0)+
  scale_colour_manual(values = c(myViridis[2], myViridis[1])) + 
  scale_fill_manual(values = c(myViridis[2], myViridis[1])) + 
  # correr con esto
  scale_x_discrete(labels = c('LVE','SVE'))+
  geom_abline(slope = 0,
              intercept = 0,
              alpha = 0.5,
              linetype = "dashed") +
  labs(x = "Condition", 
       y = "Relative unsigned bias") +
  # facet_grid(. ~ type) +
  #annotate("text", x = 1.5, y = 1.1,  label = "**", size = 4) +
  #annotate("segment", x = 1, xend = 2, y = 1.0, yend = 1.0, colour = "black", size=.5, alpha=1,)+
  #theme_pubr(base_size = 10.95, margin = TRUE)+
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        #axis.text.x = element_blank(),
        plot.margin = unit(c(1.75,0,1.25,0.25), "cm"))

f7

m.RelativUnsignedBias <- lm(mBiasUnSigned ~ room_condition, 
                            data = results_tbls)
extract_stats(ggcoefstats(m.RelativUnsignedBias))
anov = anova(m.RelativUnsignedBias)
anov
f7

#write.csv(anov, file="new_Exp_1_ADP/stats/all_unsigned_bias_anova.csv")

#tab_model(m.RelativUnsignedBias, file ="new_Exp_1_ADP/stats/all_unsigned_bias.html")

#mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "13. Bias signed", ".png", sep = '')
#ggsave(mi_nombre_de_archivo, plot=f6, width=15, height=10, units="cm", limitsize=FALSE, dpi=600)

testUnsigendBias <- t.test(filter(results_tbls, 
              room_condition=="Coincident VE" )$mBiasUnSigned,
       filter(results_tbls, 
              room_condition=="Smaller VE")$mBiasUnSigned, 
       paired = FALSE)

testUnsigendBias.tidy <- tidy(testUnsigendBias)
#write.csv(testUnsigendBias.tidy, file="new_Exp_1_ADP/stats/all_unsigned_bias_t-test.csv")

# main plot V2 ---------------------------------------------------------------

#f1 f6 y f7
main_figure <- ggarrange(f1, 
                         ggarrange(f6, f7, 
                                   heights = c(1,1),
                                   ncol = 1, labels = c("B", "C")
                                   ),
                         #nrow = 2, 
                         ncol = 2,
                         labels ="A",
                         heights = c(1, 1),
                         widths = c(1.75,1.25),
                         common.legend = TRUE)
#                    legend = "top")

plot(main_figure)


# save plot V2 ---------------------------------------------------------------

figures_folder = "./Exp_1_ADP/figura_final"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "exp_V2_new_color", ".png", sep = '')
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

#plot<- ggarrange(ba,mi,fa, ncol=3, nrow=1, common.legend = TRUE,legend="bottom")

#main_figure <- annotate_figure(main_figure, #top = text_grob("All participants", 
#                                      color = "black", face = "bold", size = 12))

# save plot ---------------------------------------------------------------

figures_folder = "./new_Exp_1_ADP/figura_final/"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "All", ".png", sep = '')
ggsave(device = "png", mi_nombre_de_archivo, plot=main_figure, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)

## asi me lo guarda bien
png(mi_nombre_de_archivo, res=600, units="cm", width=15, height=15)
plot(main_figure)
dev.off()
s