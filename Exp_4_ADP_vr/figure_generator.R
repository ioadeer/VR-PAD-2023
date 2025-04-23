#  Oscuras vs VR sala mas grande
#  En este script voy a armar la figura
#  La idea es tener por una lado
#  Un grafico con el modelo, y las medidas promedio de PAD
#  Por otro lado tener las dos figuras de sesgo

# dependencies ------------------------------------------------------------
library(tidyverse)
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
library(flextable)
library(webshot)
library(officer)
library(effects)

# load data  ---------------------------------------------------------------


#theme_set(theme_gray(base_family = "DejaVuSerif"))
rm(list=ls())
figures_folder = "Exp_4_ADP_vr/Figura_final"
results_tbl <- read.csv("Exp_4_ADP_vr/ResultsData/DresultsExp4.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)

cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")

results_tbl$room_condition = factor(results_tbl$room_condition, levels= c("No visual information","Larger VE"))


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
anova(m.Dist1)
r.squaredGLMM(m.Dist1)

tab_model(m.Dist1, file = "./Exp_4_ADP_vr/Models/LMEM_ADP.html")

tidy(anova(m.Dist1))

#Antes
#eq1 <- substitute("No visual information:" ~~~ italic(y) == k %.% italic(X)^italic(a),
# Ahora
eq1 <- substitute("NVI:"~italic(y) == k %.% italic(X)^italic(a),                  
                  list(k = round(10^mDist1stats$tidy_data$estimate[[1]],digits = 2),
                       a = round(mDist1stats$tidy_data$estimate[[2]], digits = 2)))
eq2 <- substitute("LVE:"~italic(y) == k %.% italic(X)^italic(a),
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
  scale_colour_manual(values = cbPalette) +
  scale_fill_manual(values = cbPalette) +
  geom_line(data = Final.Fixed, aes(x = target_distance, y =10^fit, group=room_condition, color=room_condition))+
  geom_text(x = 5, y = 4.5, label = as.character(as.expression(eq1)), 
            hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#000000",)+
            #family="Times New Roman")+
  geom_text(x = 5, y = 4.25, label = as.character(as.expression(eq2)), 
            hjust = 0, nudge_x =  0,parse = TRUE, size = 4, color = "#E69F00",)+
            #family="Times New Roman")+
  #geom_text(x = 0.2, y = 6.0, label = as.character(as.expression(eq3)), hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#009E73")+
  scale_x_continuous(name="Distance source (m)", limits = c(0,10)) +
  scale_y_continuous(name="Perceived distance (m)",   limits = c(0,5)) +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank(),)
        #text=element_text(family="Arial", size=10)) 


f1
#mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "5. Lme Lineal-Normal", ".jpg", sep = '')
#ggsave(mi_nombre_de_archivo, plot=f1, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)






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
  geom_point(data = results_tbls, mapping = aes(x = room_condition,y = mBiasSigned, colour = room_condition, fill = room_condition), alpha = 0.3)+
  geom_line(data = results_tbls, mapping = aes(x = room_condition,y = mBiasSigned, group = subject, colour = room_condition),alpha = 0.3)+
  geom_violin(data= results_tbls,aes(x = room_condition,y = mBiasSigned), trim=TRUE, alpha=0)+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
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
        axis.text.x = element_blank())

f6


m.RelativBias <- lm(mBiasSigned ~ room_condition, 
                    data = results_tbls)
extract_stats(ggcoefstats(m.RelativBias))

#tab_model(m.RelativBias, file = "./Exp_4_ADP_vr/Models/signed_bias.html")

anov = anova(m.RelativBias)
anov
#Response: mBiasSigned
#Df  Sum Sq  Mean Sq F value Pr(>F)
#room_condition  1 0.00236 0.002365  0.0436 0.8362
#Residuals      28 1.51987 0.054281

f6

# no es signficitavo
#mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "13. Bias signed", ".png", sep = '')
#ggsave(mi_nombre_de_archivo, plot=f6, width=15, height=10, units="cm", limitsize=FALSE, dpi=600)

# NO ES PAREADO
testSigendBias <- t.test(filter(results_tbls, 
              room_condition=="No visual information" )$mBiasSigned,
       filter(results_tbls, 
              room_condition=="Larger VE")$mBiasSigned, 
       paired = FALSE)

testSigendBias
#t = 0.20873, df = 24.344, p-value = 0.8364
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.1576943  0.1932085
#sample estimates:
#  mean of x  mean of y 
#-0.4809290 -0.4986861 


# unsigned bias -----------------------------------------------------------

f7 =  ggplot(results_tblp, aes(x = room_condition,y = MBiasUnSigned, colour = room_condition, fill = room_condition)) +
  geom_pointrange(aes(x=room_condition, y=MBiasUnSigned, ymin=MBiasUnSigned-SDBiasUnSigned, ymax=MBiasUnSigned+SDBiasUnSigned), size = 0.5)+
  geom_point(data = results_tbls, mapping = aes(x = room_condition,y = mBiasUnSigned, colour = room_condition, fill = room_condition), alpha = .3)+
  geom_line(aes(group = 1),size = 1.2, alpha=.5)+
  geom_line(data = results_tbls, mapping = aes(x = room_condition,y = mBiasUnSigned, group = subject, colour = room_condition),alpha = 0.3)+
  geom_violin(data= results_tbls,aes(x = room_condition,y = mBiasUnSigned), trim=TRUE, alpha=0)+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  geom_abline(slope = 0,
              intercept = 0,
              alpha = 0.5,
              linetype = "dashed") +
  labs(x = "Condition", 
       y = "Relative unsigned \nbias") +
  # facet_grid(. ~ type) +
 # annotate("text", x = 1.5, y = 1.1,  label = "**", size = 4) +
#  annotate("segment", x = 1, xend = 2, y = 1.0, yend = 1.0, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

f7

m.RelativUnsignedBias <- lm(mBiasUnSigned ~ room_condition, 
                            data = results_tbls)

extract_stats(ggcoefstats(m.RelativUnsignedBias))

#tab_model(m.RelativUnsignedBias, file = "./Exp_4_ADP_vr/Models/unsigned_bias.html")
# (Intercept)                         0.593   0.95    0.506   0.680 
# room_conditionVirtual environment   0.0153  0.95   -0.108   0.139

anov = anova(m.RelativUnsignedBias)
anov
#Analysis of Variance Table
#Response: mBiasUnSigned
#Df  Sum Sq  Mean Sq F value Pr(>F)
#room_condition  1 0.00071 0.000712  0.0184 0.8932
#Residuals      28 1.08499 0.038750   
f7

#mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "13. Bias signed", ".png", sep = '')
#ggsave(mi_nombre_de_archivo, plot=f6, width=15, height=10, units="cm", limitsize=FALSE, dpi=600)

t.test(filter(results_tbls, 
              room_condition=="No visual information" )$mBiasUnSigned,
       filter(results_tbls, 
              room_condition=="Larger VE")$mBiasUnSigned, 
       paired = FALSE)

#Welch Two Sample t-test
#
#data:  filter(results_tbls, room_condition == "No visual information")$mBiasUnSigned and filter(results_tbls, room_condition == "Larger VE")$mBiasUnSigned
#t = -0.13553, df = 22.946, p-value = 0.8934
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.1584546  0.1389707
#sample estimates:
#  mean of x mean of y 
#0.5089846 0.5187265 

# main plot V2 ---------------------------------------------------------------

#f1 f6 y f7
main_figure <- ggarrange(f1, 
                         ggarrange(f6, f7, widths = c(2,2),
                                   ncol = 1, labels = c("B", "C")),
                         #nrow = 2, 
                         ncol = 2,
                         labels ="A",
                         heights = c(1, 0.75),
                         widths = c(1.75,1.25),
                         common.legend = TRUE)
#                    legend = "top")

main_figure


# save plot V2 ---------------------------------------------------------------

figures_folder = "./Exp_4_ADP_vr/Figura_final/"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "exp_V2", ".png", sep = '')
#ggsave(device = "png", mi_nombre_de_archivo, plot=main_figure, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)

## asi me lo guarda bien
png(mi_nombre_de_archivo, res=600, units="cm", width=15, height=15)
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

figures_folder = "./Exp_4_ADP_vr/Figura_final/"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "exp", ".png", sep = '')
#ggsave(device = "png", mi_nombre_de_archivo, plot=main_figure, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)

## asi me lo guarda bien
png(mi_nombre_de_archivo, res=600, units="cm", width=15, height=15)
plot(main_figure)
dev.off()
#View(systemfonts::system_fonts())
#device = ragg::agg_png, # this is the relevant part
