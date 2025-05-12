# Analisis de correlacion del experimento 4


# dependencies ------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr) # pivot de tabla
library(ggpubr) # test de correlacion

# load data ---------------------------------------------------------------

tabla.analisis_correlacion <- read.csv('Exp_4_ADP_vr/data/analisis_correlacion.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)

tabla.analisis_correlacion <- tabla.analisis_correlacion %>%
  mutate(
    log_distancia_max = log(distanca_max),
    log_depth = log(depth)
  )

# grupo 1 -----------------------------------------------------------------
# plot correlacion grupo que dijo distancias mas largas para bloque 2
# los saque analizando grafico individuales
# subject 1, 6 ,8 14


tabla.analisis_correlacion.group_1 <- tabla.analisis_correlacion %>%
  filter(nsub == 1 | nsub == 6 | nsub == 8 | nsub == 14)


correlation_plot_g1 <- ggplot(tabla.analisis_correlacion.group_1, 
                           aes(x =log_depth, y = log_distancia_max,
                               colour = condicion_sala)) +
  geom_point() +
  geom_smooth(alpha=0.3, method= "lm")+
  stat_cor(method = "pearson")+
  ggtitle("Cor sujetos aumentan curva (log log)") +
  xlab("Profundidad de sala reportada") +
  #theme(legend.title =element_blank(), legend.position = 'none')+
  ylab("Maxima distancia auditiva")

plot(correlation_plot_g1)

figures_folder = "./Exp_4_ADP_vr/analisis_correlacion/figures/grupos/"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "g1_incrementan_curva_con_VE.png", sep = '')

png(mi_nombre_de_archivo, res=600, units="cm", width=15, height=15)
plot(correlation_plot_g1)
dev.off()


# grupo 2 -----------------------------------------------------------------
# plot correlacion grupo que dijo distancias mas cortas para bloque 2

# subject 3,4,7,12, 16


tabla.analisis_correlacion.group_2 <- tabla.analisis_correlacion %>%
  filter(nsub == 3 | nsub == 4 | nsub == 7 | nsub == 12 | nsub == 16)


correlation_plot_g2 <- ggplot(tabla.analisis_correlacion.group_2, 
                              aes(x =log_depth, y = log_distancia_max,
                                  colour = condicion_sala)) +
  geom_point() +
  geom_smooth(alpha=0.3, method= "lm")+
  stat_cor(method = "pearson")+
  ggtitle("Cor sujetos decrementan curva (log log)") +
  xlab("Profundidad de sala reportada") +
  #theme(legend.title =element_blank(), legend.position = 'none')+
  ylab("Maxima distancia auditiva")

plot(correlation_plot_g2)

figures_folder = "./Exp_4_ADP_vr/analisis_correlacion/figures/grupos/"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "g1_decrementan_curva_con_VE.png", sep = '')

png(mi_nombre_de_archivo, res=600, units="cm", width=15, height=15)
plot(correlation_plot_g2)
dev.off()


# grupo 3 -----------------------------------------------------------------
# plot correlacion grupo que dijo distancias casi iguales entre bloques


# subject 2,9,10,11,13,15

tabla.analisis_correlacion.group_3 <- tabla.analisis_correlacion %>%
  filter(nsub == 2 | nsub == 9 | nsub == 10 | nsub == 11 | nsub == 13 | nsub == 15)


correlation_plot_g3 <- ggplot(tabla.analisis_correlacion.group_3, 
                              aes(x =log_depth, y = log_distancia_max,
                                  colour = condicion_sala)) +
  geom_point() +
  geom_smooth(alpha=0.3, method= "lm")+
  stat_cor(method = "pearson")+
  ggtitle("Cor sujetos misma respuesta osc - ve(log log)") +
  xlab("Profundidad de sala reportada") +
  #theme(legend.title =element_blank(), legend.position = 'none')+
  ylab("Maxima distancia auditiva")

plot(correlation_plot_g3)

figures_folder = "./Exp_4_ADP_vr/analisis_correlacion/figures/grupos/"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "g1_empatan_oscuras_con_VE.png", sep = '')

png(mi_nombre_de_archivo, res=600, units="cm", width=15, height=15)
plot(correlation_plot_g3)
dev.off()
