# Docuemnto donde se realizara analisis de correlacion
# Entre maxima distancia reportada en PAD y visual
# Es en log log

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
library(reshape2)


# format dimension data ---------------------------------------------------

tabla_volumen_1_11 <- read.csv("./tamanio-visual-de-sala/data/data_1_11_s.csv", header = TRUE, sep = ' ', stringsAsFactors = TRUE)
tabla_volumen_12_32 <- read.csv("./tamanio-visual-de-sala/data/data_12_32_s.csv", header = TRUE, sep = ' ', stringsAsFactors = TRUE)
tabla_volumen_33_50 <- read.csv("./tamanio-visual-de-sala/data/data_33_50_s.csv", header = TRUE, sep = ' ', stringsAsFactors = TRUE)
tabla.volumen <- do.call("rbind", list(tabla_volumen_1_11, tabla_volumen_12_32, tabla_volumen_33_50))

# Filtrar tabla con dimensiones a partir de los sujetos considerados 
# no outliers a partir de analisis de PAD

tabla.raw <- read.csv('./analisis-pad-2-salas-vacias/data/data-1-50-bloque-1-sin-outliers.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)

nsub_list <- unique(tabla.raw$nsub)

tabla.volumen <- tabla.volumen %>%
  filter(nsub %in% unique(tabla.raw$nsub))

write.table(tabla.volumen, file="./analisis-pad-2-salas-vacias/data/dimensiones_de_sala_visual_1_50_sin_outliers.csv", row.names = FALSE)



# load data ---------------------------------------------------------------

tabla.dimensions <- read.csv('./analisis-pad-2-salas-vacias/data/dimensiones_de_sala_visual_1_50_sin_outliers.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)

# 
# 
# tabla.volumen <- select(tabla.volumen, c("nsub",
#                                          "SG_RV_volumen",
#                                          "SG_RV_width",
#                                          "SG_RV_depth",
#                                          "SG_RV_height",
#                                          "SR_volumen",
#                                          "SR_width",
#                                          "SR_depth",
#                                          "SR_height"
# ))


# Sacar distancia maxima reportada

tabla.raw <- read.csv('analisis-pad-2-salas-vacias/data/data-1-50-bloque-1-sin-outliers.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)

nsub_list <- unique(tabla.raw$nsub)

tabla.dist_max <- tabla.raw %>%
  group_by(nsub) %>%
  summarise(distanca_max = max(respuesta))


# crear tabla para analisis de correlacion ----------------------

# VIRTUAL
tabla.dimensions <- tabla.dimensions %>%
  mutate( # vol virtual
    SV_volumen = case_when(
      block_1 == "Sala Grande" ~ SG_RV_volumen,
      block_1 == "Sala Chica" ~ SC_RV_volumen,
    ) 
  ) %>%
  mutate( # depth virtual
    SV_depth = case_when(
      block_1 == "Sala Grande" ~ SG_RV_depth,
      block_1 == "Sala Chica" ~ SC_RV_depth,
    )
  ) %>%
  mutate(  # wide virtual
    SV_width = case_when(
      block_1 == "Sala Grande" ~ SG_RV_width,
      block_1 == "Sala Chica" ~ SC_RV_width,
    )
  ) %>%
  mutate(   # height virtual
    SV_height = case_when(
      block_1 == "Sala Grande" ~ SG_RV_height,
      block_1 == "Sala Chica" ~ SC_RV_height,
    )
  ) 

colnames(tabla.dimensions)

tabla.dimensions <- select(tabla.dimensions, c(nsub, 
                                         block_1, 
                                         SR_volumen, 
                                         SR_width, 
                                         SR_depth,
                                         SR_height,
                                         SV_volumen,
                                         SV_depth,
                                         SV_width,
                                         SV_height
))


tabla.analisis_correlacion = merge(x =tabla.dimensions, y = tabla.dist_max, by= "nsub")

write.table(tabla.analisis_correlacion, file="analisis-pad-2-salas-vacias/data/analisis_correlacion_1_50.csv", row.names = FALSE)

# analisis de correlacion usando bloque 1 ---------------------------------

tabla.analisis_cor <- read.csv("analisis-pad-2-salas-vacias/data/analisis_correlacion_1_50.csv", header = TRUE, sep = ' ', stringsAsFactors = TRUE)

tabla.analisis_cor <- select(tabla.analisis_cor, c(nsub, 
                                               block_1, 
                                               SV_depth,
                                               distanca_max
                                              ))
# LOG LOG

tabla.analisis_cor <- tabla.analisis_cor %>%
  mutate(
    log_distancia_max = log(distanca_max),
    log_SV_depth = log(SV_depth)
  )

tabla.cor_sc <- tabla.analisis_cor %>%
  filter(block_1 == "Sala Chica")

correlation_sala_chica <- cor(tabla.cor_sc$log_SV_depth, tabla.cor_sc$log_distancia_max, method= 'pearson')

print(correlation_sala_chica)
# [1] 0.3902147 r

tabla.cor_sg <- tabla.analisis_cor %>%
  filter(block_1 == "Sala Grande")

correlation_sala_grande <- cor(tabla.cor_sg$log_SV_depth, tabla.cor_sg$log_distancia_max, method= 'pearson')

print(correlation_sala_grande)
# [1] -0.08812298 r


correlation_plot_sg <- ggplot(data=filter(tabla.analisis_cor,block_1=="Sala Grande"), 
                              aes(x =log_SV_depth, y = log_distancia_max)) +
  geom_point() +
  geom_smooth(alpha=0.3, method= "lm")+ 
  stat_cor(method = "pearson")+
  ggtitle("Correlacion sala grande (log log)") +
  xlab("Profundidad de sala visual") +
  theme(legend.title =element_blank(), legend.position = 'none')+
  ylab("Maxima distancia auditiva")
  
plot(correlation_plot_sg)

figures_folder = "./analisis-pad-main/figuras/correlacion/"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "correlacion_sala_grande.png", sep = '')
ggsave(mi_nombre_de_archivo, plot = correlation_plot_sg, limitsize=FALSE, dpi=200)


correlation_plot_sc <- ggplot(data=filter(tabla.analisis_cor,block_1=="Sala Chica"), aes(x =log_SV_depth, y = log_distancia_max)) +
  geom_point() +
  geom_smooth(alpha=0.3, method= "lm")+
  stat_cor(method = "pearson")+
  ggtitle("Correlacion sala chica (log log)") +
  xlab("Profundidad de sala visual") +
  theme(legend.title =element_blank(), legend.position = 'none')+
  ylab("Maxima distancia auditiva")

plot(correlation_plot_sc)

figures_folder = "./analisis-pad-main/figuras/correlacion/"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "correlacion_sala_chica.png", sep = '')
ggsave(mi_nombre_de_archivo, plot = correlation_plot_sc, limitsize=FALSE, dpi=200)

# Ambas salas

correlation_plot <- ggplot(tabla.analisis_cor, 
                              aes(x =log_SV_depth, y = log_distancia_max,
                                  colour = block_1)) +
  geom_point() +
  geom_smooth(alpha=0.3, method= "lm")+
  stat_cor(method = "pearson")+
  ggtitle("Correlacion ambas salas (log log)") +
  xlab("Profundidad de sala visual") +
  theme(legend.title =element_blank(), legend.position = 'none')+
  ylab("Maxima distancia auditiva")

plot(correlation_plot)

figures_folder = "./analisis-pad-main/figuras/correlacion/"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "correlacion_ambas_salas.png", sep = '')
ggsave(mi_nombre_de_archivo, plot = correlation_plot, limitsize=FALSE, dpi=400)
