
# dependencies ------------------------------------------------------------
library(tidyverse)
library(dplyr)
#library(Routliers)
library(lme4)
library(nlme)
# library(sjPlot)
library(MuMIn) # r.squaredGLMM
library(lmerTest)
# library(jtools)
# library(gdtools)
# library(broom)
library(ggstatsplot) # extract_stats(ggcoefstats(m.Dist3))
# library(modelsummary)
# library(ggpubr)
# library(flextable)
# library(webshot)
# library(officer)
library(effects)



# load data ---------------------------------------------------------------

results_tbl <- read.csv("Exp_4_ADP_vr/ResultsData/DresultsExp4.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)

cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")

results_tbl$room_condition = factor(results_tbl$room_condition, levels= c("No visual information","Larger VE"))


# make model figure -------------------------------------------------------

results_tbl <- results_tbl %>%
  mutate(
    perc_dist_log_10 = log10(perc_dist)
  )
# EXPERIMENTO 4 individuales -----
m.Dist3 <-  lmer(log10(perc_dist) ~ log10(target_distance)*room_condition+(1+log10(target_distance)|subject)+(0+room_condition|subject),
                 data = results_tbl) 
extract_stats(ggcoefstats(m.Dist3))
r.squaredGLMM(m.Dist3)

anova(m.Dist3)
anov1 = anova(m.Dist3)

results_tbl$Modelfitted3<-predict(m.Dist3)



# grafico 1 facet grid ----------------------------------------------------

# Todos los sujetos
FittedlmPlot1 <-ggplot()+
  facet_grid(subject ~ room_condition, labeller=label_both)+
  geom_line(data = results_tbl, aes(x = target_distance, y = 10^Modelfitted3))+
  geom_point(data = results_tbl, aes(x = target_distance, y =perc_dist, group=subject,colour = subject), size=3)+
  #  coord_cartesian(ylim = c(.03,.074))+ 
  xlab("Targent_distance")+ylab("Perceived_distance")
FittedlmPlot1

figures_folder = "./Exp_4_ADP_vr/Figura_individual/fig"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "individuales", ".png", sep = '')
#ggsave(device = "png", mi_nombre_de_archivo, plot=main_figure, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)

## asi me lo guarda bien
png(mi_nombre_de_archivo, res=600, units="cm", width=30, height=50)
plot(FittedlmPlot1)
dev.off()


# superpuestos ------------------------------------------------------------

FittedlmPlot2 <-ggplot()+
  facet_grid(subject ~ . )+
  geom_line(data = results_tbl, aes(x = target_distance, y = 10^Modelfitted3, group= room_condition, colour = block))+
  geom_point(data = results_tbl, aes(x = target_distance, y =perc_dist, group=subject,colour = block), size=3)+
  scale_colour_gradient(low = "blue", high = "red")+
  #  coord_cartesian(ylim = c(.03,.074))+ 
  xlab("Targent_distance")+ylab("Perceived_distance")

FittedlmPlot2

figures_folder = "./Exp_4_ADP_vr/Figura_individual/fig"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "individuales_superpuestos", ".png", sep = '')
#ggsave(device = "png", mi_nombre_de_archivo, plot=main_figure, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)

## asi me lo guarda bien
png(mi_nombre_de_archivo, res=600, units="cm", width=30, height=50)
plot(FittedlmPlot2)
dev.off()

