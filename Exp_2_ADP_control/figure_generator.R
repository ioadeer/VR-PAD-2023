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



# load data and generate image  ---------------------------------------------------------------

rm(list=ls())
figures_folder = "Figura_final"
results_tbl <- read.csv("Exp_2_ADP_control/ResultsData/Dresults.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)

cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")


m.Dist1 <-  lmer(log10(perc_dist) ~ log10(target_distance)*room_condition+(1+log10(target_distance)|subject)+(0+room_condition|subject),
                 data = results_tbl)

Final.Fixed<-effect(c("log10(target_distance)*room_condition"), m.Dist1)

Final.Fixed<-effect(c("log10(target_distance)*room_condition"), m.Dist1, xlevels=list(target_distance=seq(2,9,1)))
#2,2.7,3.65,4.9,6.65,9
# xlevels=list(TimeStep=seq(0,4,1))
# Grafico poblacional
Final.Fixed<-as.data.frame(Final.Fixed)

mDist1stats <- extract_stats(ggcoefstats(m.Dist1))
mDist1stats$tidy_data$estimate[[1]]

r.squaredGLMM(m.Dist1)


eq1 <- substitute("No visual:" ~~~italic(y) == k %.% italic(X)^italic(a),
                  list(k = round(10^mDist1stats$tidy_data$estimate[[1]],digits = 2),
                       a = round(mDist1stats$tidy_data$estimate[[2]], digits = 2)))
eq2 <- substitute("Visual:"~~~italic(y) == k %.% italic(X)^italic(a),
                  list(k = round(10^(mDist1stats$tidy_data$estimate[[1]]+mDist1stats$tidy_data$estimate[[3]]), digits = 2),
                       a = round(mDist1stats$tidy_data$estimate[[2]]+mDist1stats$tidy_data$estimate[[4]], digits = 2)))
eq3 <- substitute("r.squared:"~~~italic(R)^italic(2) == italic(b),
                  list(b = round(r.squaredGLMM(m.Dist1)[2], digits = 2)))



tabla.pob = results_tbl %>% group_by(target_distance,room_condition) %>%
  summarise(Mperc_dist  = mean(perc_dist),
            SDperc_dist = sd(perc_dist)/sqrt(n()))  %>%
  ungroup()

f1 <- ggplot(tabla.pob, aes(x=target_distance, y =Mperc_dist, group = room_condition, color  = room_condition)) +
  geom_pointrange(aes(x = target_distance, y = Mperc_dist, ymin = Mperc_dist-SDperc_dist, ymax = Mperc_dist+SDperc_dist),alpha = 1,
                  position = position_jitterdodge(jitter.width = .1,
                                                  jitter.height = 0,
                                                  dodge.width = .1 ))+
  geom_abline(intercept = 0, slope = 1, linetype=2) +
  scale_colour_manual(values = cbPalette) +
  scale_fill_manual(values = cbPalette) +
  geom_line(data = Final.Fixed, aes(x = target_distance, y =10^fit, group=room_condition, color=room_condition))+
  #geom_abline(slope =m.Dist1$coefficients$fixed[[2]],
  #            intercept =m.Dist1$coefficients$fixed[[1]],
  #            alpha = 0.5,
  #            color = "#000000") +
  #geom_abline(slope =m.Dist1$coefficients$fixed[[2]]+m.Dist1$coefficients$fixed[[4]],
  #            intercept =m.Dist1$coefficients$fixed[[1]]+m.Dist1$coefficients$fixed[[3]],
  #            alpha = 0.5,
  #            color = "#E69F00") +
  geom_text(x = 0.2, y = 7.0, label = as.character(as.expression(eq1)), hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#000000")+
  geom_text(x = 0.2, y = 6.5, label = as.character(as.expression(eq2)), hjust = 0, nudge_x =  0,parse = TRUE, size = 4, color = "#E69F00")+
  geom_text(x = 0.2, y = 6.0, label = as.character(as.expression(eq3)), hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#009E73")+
  scale_x_continuous(name="Distance source (m)", limits = c(0,10)) +
  scale_y_continuous(name="Perceived distance (m)",   limits = c(0,10)) +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank())


f1
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "5. Lme Lineal-Normal", ".jpg", sep = '')
ggsave(mi_nombre_de_archivo, plot=f1, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)



