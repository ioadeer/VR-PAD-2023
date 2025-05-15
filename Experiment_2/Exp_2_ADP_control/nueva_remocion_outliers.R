
# Dependencias ------------------------------------------------------------
library(broom)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggstatsplot)
library(gridExtra)
library(htmlwidgets)
library(tidyverse)
library(lme4)
library(nlme)
library(lmerTest)
library(modelr)
library(scales) 
library(plotly)
library(Routliers)
library(processx)
library(effects)




# load data ---------------------------------------------------------------

rm(list=ls())
results_tbl <- read.csv('./Experiment_2/Exp_2_ADP_control/ResultsData/results_log_bias.csv', header = TRUE, sep = ',', stringsAsFactors = TRUE)


# remocion ----------------------------------------------------------------
idx = results_tbl$subject == "1" & results_tbl$room_condition == 'No visual information'
results_tbl = results_tbl[!idx,]

tabla.outlier <- results_tbl %>% 
  filter(room_condition == 'No visual information') %>% 
  group_by(subject, room_condition, target_distance) %>%
  summarise(mBiasUnsigned  = mean(log_bias_unsigned_m,na.rm=TRUE))  %>%
  ungroup()
res3 <- outliers_mad(x = tabla.outlier$mBiasUnsigned,threshold = 3 ,na.rm=TRUE)
plot_outliers_mad(res3,x=tabla.outlier$mBiasUnsigned,pos_display=TRUE)
tabla.outlier[res3$outliers_pos,] 

idx = results_tbl$subject == "18" & results_tbl$room_condition == 'No visual information' & 
  results_tbl$target_distance == 2  
results_tbl = results_tbl[!idx,]
idx = results_tbl$subject == "18" & results_tbl$room_condition == 'No visual information' & 
  results_tbl$target_distance == 2.7  
results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "11" & results_tbl$room_condition == 'No visual information' & 
  results_tbl$target_distance == 4.9  
results_tbl = results_tbl[!idx,]


tabla.outlier <- results_tbl %>% 
  filter(room_condition == 'Visual information') %>% 
  group_by(subject, room_condition, target_distance) %>%
  summarise(mBiasUnsigned  = mean(log_bias_unsigned_m,na.rm=TRUE))  %>%
  ungroup()
res3 <- outliers_mad(x = tabla.outlier$mBiasUnsigned,threshold = 3 ,na.rm=TRUE)
plot_outliers_mad(res3,x=tabla.outlier$mBiasUnsigned,pos_display=TRUE)
tabla.outlier[res3$outliers_pos,] 

idx = results_tbl$subject == "19" & results_tbl$room_condition == 'Visual information' & 
  results_tbl$target_distance == 2  
results_tbl = results_tbl[!idx,]

f1 <- ggplot(results_tbl , aes(x= target_distance, y= perc_dist, color = room_condition)) +
  facet_grid(subject~room_condition) +
  geom_abline(intercept = 0, slope = 1, linetype=2) +
  geom_point(color="red", size=1) +
  geom_line()+
  #geom_abline(intercept = 1, slope = 1,linetype="dashed") +       
  scale_x_continuous(name="Distance source (m)")+#, breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,20)) +
  scale_y_continuous(name="Perceived distance (m)")+#,  breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,20)) +
  theme_linedraw(base_size = 9)

plot(f1)

figures_folder = "figuras nuevas"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "distancia_poblacional.png", sep = '')
ggsave(mi_nombre_de_archivo, plot=f1, width=10, height=100, units="cm", limitsize=FALSE, dpi=300)




results_tbl

write_csv(x =results_tbl ,file ="./Experiment_2/Exp_2_ADP_control/ResultsData/Dresults_nuevos.csv")
