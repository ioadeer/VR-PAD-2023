
# dependencies ------------------------------------------------------------

library(dplyr)
library(tidyr)
library(rstatix) # stat.test
library(ggplot2)
library(ggpubr)
library(ggstatsplot)
library(sjPlot)
library(effectsize)

# correlation ---------------------------------------------------------

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

nvi <- tabla.correlacion %>%
  filter(condicion_sala == 'OSCURAS')
# N 42. df = n -2 = 40

re <- tabla.correlacion %>%
  filter(condicion_sala == 'VISUAL')

corr_nvi<- cor.test(nvi$log_perc_depth,nvi$log_distancia_max , method= 'pearson')

eqn1 <- sprintf(
  "NVI: R = %.2g, p =  %.2g",
  corr_nvi[4][[1]][[1]],
  corr_nvi[3][[1]][[1]]
  )

corr_re <- cor.test(re$log_perc_depth,re$log_distancia_max , method= 'pearson')

eqn2 <- sprintf(
  "VI: R = %.2g, p =  %.2g",
  corr_re[4][[1]][[1]],
  corr_re[3][[1]][[1]])

myViridis <- viridisLite::viridis(alpha=0.75, n= 3)

correlation_plot <- ggplot(tabla.correlacion, 
                           aes(x =log_perc_depth, y = log_distancia_max,
                               colour = condicion_sala)) +
  scale_colour_manual(values = c(myViridis[1], myViridis[2]),
                      labels = c("No visual info", "Visual info")) +
  scale_fill_manual(values = c(myViridis[1], myViridis[2])) +
  geom_point() +
  geom_smooth(alpha=0.3, method= "lm")+
  xlab("Perceived Room Depth (m)") +
  ylab("Maximum Perceived Auditory Distance (m)")+
  labs(color = "Visual condition") +  # Change legend title
  annotate("label",                        # Add text for mean
           x = 1, # para fig compuesta
           y = 2.65,
           label = eqn1,
           size = 4,
           hjust = 0,
           color = myViridis[1],
  ) +
  annotate("label",                        # Add text for mean
           x = 1, # para fig compuesta
           y = 3,
           label = eqn2,
           size = 4,
           hjust = 0,
           color = myViridis[2],
  ) +
  theme_minimal()+
  theme(legend.position = "top",
        legend.title = element_blank(),
        #text=element_text(family="Arial"
        #, size=10
        )

plot(correlation_plot)


# depth -------------------------------------------------------------------

tabla.exp_2 <- read.csv("./Exp_2_3_VDP/data/visual_exp_2_sin_outliers.csv", header = TRUE, sep = ' ', stringsAsFactors = TRUE)

#DEPTH

dimensions.depth <- tabla.exp_2 %>%
  pivot_longer(cols = c("No_Visual_depth", "Visual_depth"),
               names_to = "variable",
               values_to = "value")

dimensions.depth <- dimensions.depth %>%
  mutate(
    variable = case_when(
      variable == "No_Visual_depth" ~ "NVI",
      variable == "Visual_depth" ~ "VI"
    )
  )


dimensions.depth <- dimensions.depth %>%
  rename("Depth" = "value",
         "Condition" = "variable")

dimensions.depth <- dimensions.depth %>%
  filter(!Depth >= 20)

dimensions.depth$Condition = factor(dimensions.depth$Condition, 
                                    levels=c("NVI", 
                                             "VI"
                                             ))


dimensions.depth_sum <- dimensions.depth %>%
  group_by(Condition) %>%
  summarise(
    mean = mean(Depth),
    median = median(Depth),
    sd = sd(Depth),
    se = sd / sqrt(n()),
    n = n()
  )

#eqn1 <- sprintf(
#  "M = %.3g \n ± %.2g",
#  dimensions.depth_sum$mean[1],
#  dimensions.depth_sum$se[1])
#
#eqn2 <- sprintf(
#  "M = %.3g \n ± %.2g",
#  dimensions.depth_sum$mean[2],
#  dimensions.depth_sum$se[2])
#
#eqn3 <- sprintf(
#  "M = %.3g \n ± %.2g",
#  dimensions.depth_sum$mean[3],
#  dimensions.depth_sum$se[3])
#
myViridis <- viridisLite::viridis(alpha=0.5, n= 3)
# Use single color
violin_depth <- ggplot(dimensions.depth, aes(x=Condition, y=Depth,  fill=Condition)) +
  geom_violin(trim=FALSE) +
  geom_point(data= dimensions.depth_sum, mapping = aes(y=mean))+
  geom_errorbar(data= dimensions.depth_sum, mapping = aes(y= mean , ymin=mean - se,
                                                          ymax=mean + se),
                color = "#22292F",
                width = .25) +
  annotate("text", x = 1.5, y = 18.5,  label = "**", size = 3) +
  annotate("segment", x = 1.1, xend = 1.9, y = 18, yend = 18, colour = "black", size=.5, alpha=1,)+
  labs(
    y = "Mean perceived depth ± SEM (m)",
  )+
  theme_minimal() +
  guides(fill = "none") +
  geom_hline(yintercept=12,linetype="dashed") +
  annotate("text", x=1.5, y=13, label= "12 m", size=3.5) +
  #  theme_pubr(base_size = 12, margin = TRUE)+
  scale_colour_manual(values =myViridis) +
  scale_fill_manual(values = myViridis) +
  theme(
    #axis.text.x=element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(hjust = 1),
    plot.margin = unit(c(1.75,0,1.25,0.25), "cm")# top right botom left
  ) 

plot(violin_depth)

# main fig ----------------------------------------------------------------

figure <- ggarrange(
  violin_depth,
  correlation_plot,
  ncol = 2, 
  labels =c("A", "B"),
  heights = c(0.25,1),
  widths = c(0.4, 0.6)) 


plot(figure)

# save plot ---------------------------------------------------------------

figures_folder = "./Exp_2_ADP_control/Figuras"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Depth_and_Correlation", ".png", sep = '')
#ggsave(device = "png", mi_nombre_de_archivo, plot=correlation_plot, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)

## asi me lo guarda bien
png(mi_nombre_de_archivo, res=600, units="cm", width=15, height=10)
plot(figure)
dev.off()


