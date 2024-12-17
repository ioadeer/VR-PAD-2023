# Analisis de correlacion del experimento 4


# dependencies ------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr) # pivot de tabla
library(ggpubr) # test de correlacion

# format and save data ----------------------------------------------------

tabla.raw <- read.csv('Exp_4_ADP_vr/data/1_16_NVI_LVE_sin_outliers.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)

tabla.dist_max <- tabla.raw %>%
  group_by(nsub, condicion_sala) %>%
  summarise(distanca_max = max(respuesta))

tabla.raw_visual <- read.csv('Visual-de-experimento-4/data/visual_exp_4_con_outliers.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)
colnames(tabla.raw_visual)

tabla.visual <- select(tabla.raw_visual, c("Subject", "No_visual_info_depth", "Virtual_environment_depth"))

tabla.visual <- tabla.visual %>%
  rename(nsub = Subject)

tabla.visual_df <- tabla.visual %>%
  pivot_longer(
    cols = c(No_visual_info_depth, Virtual_environment_depth),
    names_to = "condicion_sala",
    values_to = "depth"
  )

tabla.visual_df <- tabla.visual_df %>%
  mutate( # vol virtual
    condicion_sala = case_when(
      condicion_sala == "No_visual_info_depth" ~ "No Visual Info",
      condicion_sala == "Virtual_environment_depth" ~ "Larger VE",
    ) 
  )

tabla.analisis_correlacion = merge(x=tabla.dist_max, y =tabla.visual_df,by=c("nsub","condicion_sala"))

write.table(tabla.analisis_correlacion, file="Exp_4_ADP_vr/data/analisis_correlacion.csv", row.names = FALSE)


# load data ---------------------------------------------------------------

tabla.analisis_correlacion <- read.csv('Exp_4_ADP_vr/data/analisis_correlacion.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)



# plot correlacion --------------------------------------------------------
## Lin Lin

## LOG LOG

tabla.analisis_correlacion <- tabla.analisis_correlacion %>%
  mutate(
    log_distancia_max = log(distanca_max),
    log_depth = log(depth)
  )

correlation_plot <- ggplot(tabla.analisis_correlacion, 
                           aes(x =log_depth, y = log_distancia_max,
                               colour = condicion_sala)) +
  geom_point() +
  geom_smooth(alpha=0.3, method= "lm")+
  stat_cor(method = "pearson")+
  ggtitle("Correlacion ambas condiciones (log log)") +
  xlab("Profundidad de sala reportada") +
  #theme(legend.title =element_blank(), legend.position = 'none')+
  ylab("Maxima distancia auditiva")

plot(correlation_plot)

figures_folder = "./Exp_4_ADP_vr/analisis_correlacion/figures/"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Analisis_correlacion.png", sep = '')

png(mi_nombre_de_archivo, res=600, units="cm", width=15, height=15)
plot(correlation_plot)
dev.off()
