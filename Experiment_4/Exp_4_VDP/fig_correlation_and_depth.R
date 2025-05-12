## New figure illustration correlation and room depth VDP


# dependencies ------------------------------------------------------------
library(dplyr)
library(tidyr)
library(rstatix) # stat.test
library(ggplot2)
library(ggpubr)
library(ggstatsplot)
library(sjPlot)
library(effectsize)

# depth -------------------------------------------------------------------

dimensions.depth <- read.csv("./Experiment_4/Exp_4_VDP/data/visual_depth_sin_outliers.csv", header = TRUE, sep = ' ', stringsAsFactors = TRUE)

#DEPTH
dimensions.depth <- dimensions.depth %>%
  mutate(
    Condition = case_when(
      Condition == "No visual information" ~ "NVI",
      Condition == "Virtual environment" ~ "LVE",
      Condition == "Real environment" ~ "RE",
    )
  )

dimensions.depth$Condition = factor(dimensions.depth$Condition, 
                                    levels=c("NVI", 
                                             "LVE",
                                             "RE"
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

eqn1 <- sprintf(
  "M = %.3g \n ± %.2g",
  dimensions.depth_sum$mean[1],
  dimensions.depth_sum$se[1])

eqn2 <- sprintf(
  "M = %.3g \n ± %.2g",
  dimensions.depth_sum$mean[2],
  dimensions.depth_sum$se[2])

eqn3 <- sprintf(
  "M = %.3g \n ± %.2g",
  dimensions.depth_sum$mean[3],
  dimensions.depth_sum$se[3])

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
  annotate("text", x = 1.5, y = 20.5,  label = "**", size = 3) +
  annotate("segment", x = 1.1, xend = 2.9, y = 20, yend = 20, colour = "black", size=.5, alpha=1,)+
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
   # axis.title.y = element_text(hjust = 1),
    plot.margin = unit(c(0.0,0,1.00,0.25), "cm")# top right botom left
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

figures_folder = "./Exp_1_VDP/figuras/"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Depth_correlation", ".png", sep = '')
#ggsave(device = "png", mi_nombre_de_archivo, plot=correlation_plot, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)

## asi me lo guarda bien
png(mi_nombre_de_archivo, res=600, units="cm", width=15, height=10)
plot(figure)
dev.off()
