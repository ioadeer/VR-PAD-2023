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


# correlation  ---------------------------------------------------------------

tabla.exp_1.corr <- read.csv("./Exp_1_VDP/data/max_audiovisual_depth_1_50_dos_bloques.csv", header = TRUE, sep = ' ', stringsAsFactors = TRUE)

tabla.analisis_cor <- tabla.exp_1.corr %>%
  mutate(
    log_distancia_max = log(distanca_max),
    log_perceived_depth = log(perceived_depth)
  )

smaller_ve <- tabla.analisis_cor %>%
  filter(room_condition == 'Sala Chica')
# N 42. df = n -2 = 40

congruent_ve <- tabla.analisis_cor %>%
  filter(room_condition == 'Sala Grande')

corr_smaller_ve <- cor.test(smaller_ve$log_perceived_depth,smaller_ve$log_distancia_max , method= 'pearson')

eqn1 <- sprintf(
  "R = %.2g, p < 0.01",
  corr_smaller_ve[4][[1]][[1]])

corr_congruent_ve <- cor.test(congruent_ve$log_perceived_depth,congruent_ve$log_distancia_max , method= 'pearson')

eqn2 <- sprintf(
  "R = %.2g, p =  %.2g",
  corr_congruent_ve[4][[1]][[1]],
  corr_congruent_ve[3][[1]][[1]])

cbPalette <- c("#E69F00","#000000","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")

correlation_plot <- ggplot(tabla.analisis_cor, 
                           aes(x =log_perceived_depth, y = log_distancia_max,
                               colour = room_condition)) +
  #scale_fill_brewer(palette="YlOrRd")+
  scale_colour_manual(values = cbPalette) +
  scale_fill_manual(values = cbPalette) +
  geom_point() +
  geom_smooth(alpha=0.3, method= "lm")+
  #stat_cor(method = "pearson", show.legend= FALSE,  position = "jitter")+
  annotate("text",                        # Add text for mean
           x = 0, # para fig compuesta
           y = 3.0,
           label = eqn1,
           size = 4,
           hjust = 0,
           color="#E69F00" ) +
  annotate("text",                        # Add text for mean
           x = 0, # para fig compuesta
           y = 2.65,
           label = eqn2,
           size = 4,
           hjust = 0,
           color="#000000" ) +
  #ggtitle("Correlation between visual and auditory distance assesments (log log)") +
  xlab("Perceived Virtual Room Depth (m)") +
  #theme(legend.title =element_blank(), legend.position = 'none')+
  ylab("Maximum Perceived Auditory Distance (m)")+
  labs(color = "Visual condition") +  # Change legend title
  scale_color_manual(values = c("#E69F00", "#000000"), 
                     labels = c("Smaller VE", "Congruent VE"))+
  theme_minimal()+
  #theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank(),
        #text=element_text(family="Arial", size=10)
        ) 


plot(correlation_plot)


# depth -------------------------------------------------------------------

tabla.exp_1 <- read.csv("./Exp_1_ADP/data/dimensiones_de_sala_visual_1_50_sin_outliers.csv", header = TRUE, sep = ' ', stringsAsFactors = TRUE)

#DEPTH

dimensions.depth <- tabla.exp_1 %>%
  pivot_longer(cols = c("SC_RV_depth", "SG_RV_depth", "SR_depth"),
               names_to = "variable",
               values_to = "value")

dimensions.depth <- dimensions.depth %>%
  mutate(
    variable = case_when(
      variable == "SC_RV_depth" ~ "SVE",
      variable == "SG_RV_depth" ~ "CVE",
      variable == "SR_depth" ~ "RE",
    )
  )


dimensions.depth <- dimensions.depth %>%
  rename("Depth" = "value",
         "Condition" = "variable")

dimensions.depth <- dimensions.depth %>%
  filter(!Depth >= 20)

dimensions.depth$Condition = factor(dimensions.depth$Condition, 
                                    levels=c("SVE", 
                                             "CVE",
                                             "RE"))


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

# Use single color
violin_depth <- ggplot(dimensions.depth, aes(x=Condition, y=Depth,  fill=Condition)) +
  geom_violin(trim=FALSE) +
  #geom_boxplot(width=0.1) +
  geom_errorbar(data= dimensions.depth_sum, mapping = aes(y= mean , ymin=mean - se,
                                                          ymax=mean + se),
                color = "#22292F",
                width = .1) +
  geom_jitter(alpha = 0.1) +
  annotate("text", x = 1.5, y = 18.5,  label = "**", size = 3) +
  annotate("segment", x = 1.1, xend = 1.9, y = 18, yend = 18, colour = "black", size=.5, alpha=1,)+
  annotate("text", x = 1.5, y = 20.5,  label = "**", size = 3) +
  annotate("segment", x = 1.1, xend = 2.9, y = 20, yend = 20, colour = "black", size=.5, alpha=1,)+
  labs(
    y = "Mean perceived depth ± SEM (m)",
  )+
  theme_minimal() +
  guides(fill = "none") +
  scale_fill_brewer(palette="YlOrRd")+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  geom_hline(yintercept=12,linetype="dashed") +
  annotate("text", x=1.5, y=13, label= "12 m", size=3.5) +
#  theme_pubr(base_size = 12, margin = TRUE)+
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

figures_folder = "./Exp_1_VDP/figuras/"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Depth_correlation", ".png", sep = '')
#ggsave(device = "png", mi_nombre_de_archivo, plot=correlation_plot, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)

## asi me lo guarda bien
png(mi_nombre_de_archivo, res=600, units="cm", width=15, height=10)
plot(figure)
dev.off()
