# Analisis de datos del experi 32 sujetos
# Analisis de volumen de sala percibida

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



# Load and join data new  -----------------------------------------------------

tabla_volumen_1_11 <- read.csv("./tamanio-visual-de-sala/data/data_1_11_s.csv", header = TRUE, sep = ' ', stringsAsFactors = TRUE)
tabla_volumen_12_32 <- read.csv("./tamanio-visual-de-sala/data/data_12_32_s.csv", header = TRUE, sep = ' ', stringsAsFactors = TRUE)

tabla.volumen <- do.call("rbind", list(tabla_volumen_1_11, tabla_volumen_12_32))

tabla.raw = read.csv("./analisis-pad-main/data/data-1-32-bloque-1-sin-outliers.csv", header = TRUE, sep = ' ', stringsAsFactors = TRUE)

colnames(tabla.volumen)

# [1] "nsub"          "block_1"       "block_2"       "SG_RV_volumen" "SG_RV_width"   "SG_RV_depth"   "SG_RV_height" 
# [8] "SC_RV_volumen" "SC_RV_width"   "SC_RV_depth"   "SC_RV_height"  "SR_volumen"    "SR_width"      "SR_depth"     
# [15] "SR_height"   

tabla.volumen <- select(tabla.volumen, c("nsub",
                        "SG_RV_volumen",
                        "SG_RV_width",
                        "SG_RV_depth",
                        "SG_RV_height",
                        "SR_volumen",
                        "SR_width",
                        "SR_depth",
                        "SR_height"
                        ))

# filtrar outliers
nsub_list <- unique(tabla.raw$nsub)

tabla.volumen <- tabla.volumen %>%
  filter(nsub %in% unique(tabla.raw$nsub))

write.table(tabla.volumen, file="./analisis-pad-main/data/volumen_sin_outliers_1_32.csv", row.names = FALSE)




# analisis volumen de aca salen figuras -------------------------------------------------------

dimensions.raw  <- read.csv('./analisis-pad-main/data/volumen_sin_outliers_1_32.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)

colnames(dimensions.raw)

dimensions.volume <- melt(dimensions.raw, id.vars='nsub',
                      measure.vars=c("SG_RV_volumen", "SR_volumen"))

dimensions.volume <- dimensions.volume %>%
  mutate(
    variable = case_when(
      variable == "SG_RV_volumen" ~ "Sala Virtual",
      variable == "SR_volumen" ~ "Sala Real",
    )
  )

# Voy a remover outliers, valores superior 1000 m3
dimensions.volume <- dimensions.volume %>%
  filter(!value >= 1000)

box_plot_volumenes <- ggplot(dimensions.volume) +
  geom_boxplot(aes(x=variable, y=value, color=variable)) +
  #scale_x_discrete(labels = c("Virtual", "Real"))+
  ggtitle("Volumen virtual y real") +
  xlab("Condicion de sala") +
  theme(legend.title =element_blank(), legend.position = 'none')+
  ylab("Volumen reportado (m3)")

plot(box_plot_volumenes)

figures_folder = "./analisis-pad-main/figuras/visual_volumen/"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "boxplot_vol_percibido.png", sep = '')
ggsave(mi_nombre_de_archivo, plot = box_plot_volumenes, limitsize=FALSE, dpi=200)

dimensions.volume_sum <- dimensions.volume %>%
  group_by(variable) %>%
  summarise(
            mean = mean(value),
            sd = sd(value)
            )

graph <- ggplot()+
  #geom_boxplot(data= dimensions.volume, aes(x=variable, y=value, color=variable),alpha = 0.3) +
  geom_point(data=dimensions.volume_sum, aes(x = variable, y = mean, col =variable),
             size = 2.5) +
  geom_errorbar(data=dimensions.volume_sum, aes(x = variable, 
                                                y = mean,
                                                ymin = mean - sd,
                                                ymax = mean + sd,
                                                col = variable),
                                                alpha = 1, width=0.5, size=1.1) +
  geom_jitter(data= dimensions.volume, aes(x = variable, y =value, col = variable),
              width = 0.25, alpha= 0.6)+
  ggtitle("Volumen virtual y real") +
  xlab("Condicion de sala") +
  theme(legend.title =element_blank(), legend.position = 'none')+
  ylab("Mean de volumen reportado (m3)")


plot(graph)

figures_folder = "./analisis-pad-main/figuras/visual_volumen/"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "mean_sd_tamanio_percibido.png", sep = '')
ggsave(mi_nombre_de_archivo, plot = graph, limitsize=FALSE, dpi=200)

# Data volumen visual max distancia reportada -----------------------------

tabla.volumen_max_dist <- read.csv("./analisis-pad-main/data/volumen_max_dist_1_32.csv", header = TRUE, sep = ' ', stringsAsFactors = TRUE)


