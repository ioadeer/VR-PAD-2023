
# dependencies ------------------------------------------------------------

library(broom)
library(broom.mixed)
library(dplyr)
library(ggbeeswarm)
library(gmodels)
library(ggplot2)
#library(ggthemes)
library(ggpubr)
library(ggstatsplot)
library(gridExtra)
library(htmlwidgets)
library(quickpsy)
library(tidyr)
library(lme4)
library(nlme)
library(lmerTest)
library(modelr)
library(scales) 
library(pracma)
library(plotly)
library(Routliers)
library(processx)
library(orca)
library(hrbrthemes)
library(viridis)
library(rstatix)
library(effects)



# load data ---------------------------------------------------------------

tabla.raw <- read.csv('./Exp_2_ADP_control/data/control_sin_outliers.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)
tabla.dimensions <- read.csv('./Exp_2_ADP_control/data/tamanio_de_sala_sin_outliers_oscuras_visual.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)



tabla.dist_max <- tabla.raw %>%
  group_by(nsub, condicion_sala) %>%
  summarise(distanca_max = max(respuesta))

tabla.depth_oscuras = merge(x =tibble(tabla.dimensions[c('nsub', 'OSCURAS_depth')]), 
                            y = filter(tabla.dist_max, condicion_sala == 'OSCURAS'), 
                            by= "nsub")

tabla.depth_oscuras <- tabla.depth_oscuras %>%
  rename("perc_depth" = "OSCURAS_depth")

tabla.depth_visual = merge(x =tibble(tabla.dimensions[c('nsub', 'VISUAL_depth')]), 
                           y = filter(tabla.dist_max, condicion_sala == 'VISUAL'), 
                           by= "nsub")

tabla.depth_visual <- tabla.depth_visual %>%
  rename("perc_depth" = "VISUAL_depth")

tabla.correlacion <- rbind(tabla.depth_oscuras, tabla.depth_visual)

tabla.correlacion <- tabla.correlacion %>%
  mutate(log_perc_depth = log(perc_depth),
         log_distancia_max = log(distanca_max))


#correlation_plot <- ggplot(tabla.correlacion, 
#                           aes(x =log_perc_depth, y = log_distancia_max,
#                               colour = condicion_sala)) +
#  geom_point() +
#  geom_smooth(alpha=0.3, method= "lm")+
#  stat_cor(method = "pearson")+
#  ggtitle("Correlacion ambas condiciones (log log)") +
#  xlab("Profundidad") +
#  #theme(legend.title =element_blank(), legend.position = 'none')+
#  ylab("Maxima distancia auditiva")

cbPalette <- c("#E69F00","#000000","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")

correlation_plot <- ggplot(tabla.correlacion, 
                           aes(x =log_perc_depth, y = log_distancia_max,
                               colour = condicion_sala)) +
  scale_colour_manual(values = cbPalette) +
  scale_fill_manual(values = cbPalette) +
  geom_point() +
  geom_smooth(alpha=0.3, method= "lm")+
  stat_cor(method = "pearson", show.legend= FALSE)+
  #ggtitle("Correlation between visual and auditory distance assesments (log log)") +
  xlab("Perceived Room Depth (m)") +
  #theme(legend.title =element_blank(), legend.position = 'none')+
  ylab("Maximum Perceived Auditory Distance (m)")+
  labs(color = "Visual condition") +  # Change legend title
  scale_color_manual(values = c("#E69F00", "#000000"), 
                     labels = c("No visual info", "Real environment"))+
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank(),
        text=element_text(family="Arial", size=10)) 


plot(correlation_plot)

# save plot ---------------------------------------------------------------

figures_folder = "./Exp_2_ADP_control/Figuras"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Correlation", ".png", sep = '')
#ggsave(device = "png", mi_nombre_de_archivo, plot=correlation_plot, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)

## asi me lo guarda bien
png(mi_nombre_de_archivo, res=600, units="cm", width=15, height=15)
plot(correlation_plot)
dev.off()


