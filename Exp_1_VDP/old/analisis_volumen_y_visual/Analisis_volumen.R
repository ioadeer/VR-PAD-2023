# Analisis de datos del experi 50 sujetos
# Analisis de volumen de sala percibida
# Aca estan las figuras que arme para el proceeding del congresio de acustica
# aada 2023

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
library(ggpubr)
library(rstatix)
library(extrafont)
#font_import()
font_import(pattern = 'times')
#loadfonts(device = "win")

t.test
t_test

# Load and join data new  -----------------------------------------------------

tabla_volumen_1_11 <- read.csv("./tamanio-visual-de-sala/data/data_1_11_s.csv", header = TRUE, sep = ' ', stringsAsFactors = TRUE)
tabla_volumen_12_32 <- read.csv("./tamanio-visual-de-sala/data/data_12_32_s.csv", header = TRUE, sep = ' ', stringsAsFactors = TRUE)
tabla_volumen_33_50 <- read.csv("./tamanio-visual-de-sala/data/data_33_50_s.csv", header = TRUE, sep = ' ', stringsAsFactors = TRUE)
tabla.volumen <- do.call("rbind", list(tabla_volumen_1_11, tabla_volumen_12_32, tabla_volumen_33_50))

tabla.raw = read.csv("analisis-pad-2-salas-vacias/data/data-1-50-bloque-1-sin-outliers.csv", header = TRUE, sep = ' ', stringsAsFactors = TRUE)

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

write.table(tabla.volumen, file="analisis-pad-2-salas-vacias/data/volumen_sin_outliers_1_50.csv", row.names = FALSE)

write.table(tabla.volumen, file="analisis-pad-2-salas-vacias/data/volumen_1_50.csv", row.names = FALSE)




# analisis volumen de aca salen figuras -------------------------------------------------------


dimensions.raw  <- read.csv('analisis-pad-2-salas-vacias/data/volumen_sin_outliers_1_50.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)

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
  ggtitle("Volumen virtual y real (N=50)") +
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
  ggtitle("Volumen virtual y real (N=50)") +
  xlab("Condicion de sala") +
  theme(legend.title =element_blank(), legend.position = 'none')+
  ylab("Mean de volumen reportado (m3)")


plot(graph)

figures_folder = "./analisis-pad-main/figuras/visual_volumen/"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "mean_sd_tamanio_percibido.png", sep = '')
ggsave(mi_nombre_de_archivo, plot = graph, limitsize=FALSE, dpi=200)

# Data volumen visual max distancia reportada -----------------------------

tabla.volumen_max_dist <- read.csv("./analisis-pad-main/data/volumen_max_dist_1_32.csv", header = TRUE, sep = ' ', stringsAsFactors = TRUE)






# multiple plot config ----------------------------------------------------

layout(matrix(c(1, 2, 3, 4), nrow = 2, 
              ncol = 2, byrow = TRUE)) 

# t-test y barchart de volumen  --------------------------------------------------------

# https://ggplot2tutor.com/tutorials/barchart_simple

dimensions.raw  <- read.csv('analisis-pad-2-salas-vacias/data/volumen_sin_outliers_1_50.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)

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

dimensions.volume <- dimensions.volume %>%
  filter(!nsub == 18)

dimensions.volume_aggr <- dimensions.volume %>%
  group_by(variable) %>%
  summarize(
    mean_volume = mean(value),
    sd_volume = sd(value)
  )

dimensions.volume <- dimensions.volume %>%
  ungroup()

dimensions.volume <- dimensions.volume %>%
  rename("volume" = "value",
         "condicion" = "variable")

stat.test <- dimensions.volume  %>% 
  t_test(volume~condicion) %>%
  add_significance()

stat.test

# # A tibble: 1 × 9
# .y.    group1    group2          n1    n2 statistic    df     p p.signif
# <chr>  <chr>     <chr>        <int> <int>     <dbl> <dbl> <dbl> <chr>   
# volume Sala Real Sala Virtual    40    40    -0.203  76.7  0.84 ns   

dim_barchart <- dimensions.volume_aggr %>%
  ggplot(aes(variable, mean_volume)) +
  geom_col(aes(fill = variable), color ="black", width =0.85) +
  geom_errorbar(aes(ymin=mean_volume - sd_volume,
                    ymax=mean_volume + sd_volume),
                color = "#22292F",
                width = .1) +
  #scale_fill_grey(start = 0.3) +
  #ylim = c(0, 300) +
  scale_y_continuous(limits = c(0, 300), expand = c(0, 0)) +
  geom_hline(yintercept=252,linetype="dashed") +
  guides(fill = "none") +
  #theme(legend.position="none") +
  theme_minimal() +
  labs(
    #x = "Condición",
    y = expression("Volumen percibido (m"^3*")"),
    #title = "Volumen de sala real vs virtual",
    #caption = "Barra de errores indica desviación estándar"
  )+
  scale_fill_brewer(palette="PuRd")+
  theme(#axis.line = element_blank(),
      axis.title.x = element_blank(),
      #axis.ticks = element_blank(),
      axis.title.y = element_text(hjust = 0),
      text=element_text(family="Times New Roman", size=10)) 

# paired <- ggpaired(dimensions.volume, x = "condicion" , y = "volume")
# plot(paired)

stat.test <- stat.test %>% add_xy_position(x = "condicion", fun = "mean", step.increase = 10) # step.increase = 28)

dim_barchart <- dim_barchart + 
  stat_pvalue_manual(stat.test, label = "p", tip.length = 0.05, size = 3, bracket.shorten = 0.1)
  #scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))

#dim_barchart + paired

plot(dim_barchart)

# https://www.datanovia.com/en/blog/how-to-add-p-values-onto-a-grouped-ggplot-using-the-ggpubr-r-package/


figures_folder = "./analisis-pad-2-salas-vacias/analisis_volumen_y_visual"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "barchart.png", sep = '')
ggsave(mi_nombre_de_archivo, plot = dim_barchart, limitsize=FALSE, dpi=200)


# d,w, h comparasion ------------------------------------------------------
#DEPTH
dimensions.depth <- melt(dimensions.raw, id.vars='nsub',
                         measure.vars=c("SG_RV_depth", "SR_depth"))

dimensions.depth <- dimensions.depth %>%
  mutate(
    variable = case_when(
      variable == "SG_RV_depth" ~ "Virtual",
      variable == "SR_depth" ~ "Real",
    )
  )
dimensions.depth <- dimensions.depth %>%
  rename("Profundidad" = "value",
         "Condición" = "variable")

dimensions.depth <- dimensions.depth %>%
  filter(!Profundidad >= 20)

dimensions.depth_sum <- dimensions.depth %>%
  group_by(Condición) %>%
  summarise(
    mean = mean(Profundidad),
    sd = sd(Profundidad)
  )

# Use single color
violin_depth <- ggplot(dimensions.depth, aes(x=Condición, y=Profundidad,  fill=Condición)) +
  #geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1) + 
  labs(
    y = "Profundidad (m)",
  )+
  theme_minimal() +
  guides(fill = "none") +
  scale_fill_brewer(palette="PuRd")+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  geom_hline(yintercept=12,linetype="dashed") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(hjust = 0),
    text=element_text(family="Times New Roman", size=10)) 

plot(violin_depth)

figures_folder = "./analisis-pad-2-salas-vacias/analisis_volumen_y_visual"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "violin_profundidad.png", sep = '')
ggsave(mi_nombre_de_archivo, plot = violin_depth, limitsize=FALSE, dpi=200)

# ANCHO 

dimensions.width <- melt(dimensions.raw, id.vars='nsub',
                          measure.vars=c("SG_RV_width", "SR_width"))

dimensions.width <- dimensions.width %>%
  mutate(
    variable = case_when(
      variable == "SG_RV_width" ~ "Virtual",
      variable == "SR_width" ~ "Real",
    )
  )

dimensions.width <- dimensions.width %>%
  rename("Ancho" = "value",
         "Condición" = "variable")

dimensions.width <- dimensions.width %>%
  filter(!Ancho >= 10)

dimensions.width_sum <- dimensions.width %>%
  group_by(Condición) %>%
  summarise(
    mean = mean(Ancho),
    sd = sd(Ancho)
  )


# Use single color
violin_width <- ggplot(dimensions.width, aes(x=Condición, y=Ancho,  fill=Condición)) +
  #geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_violin(trim=TRUE)+
  geom_boxplot(width=0.1) + 
  theme_minimal() +
  guides(fill = "none") +
  scale_fill_brewer(palette="PuRd")+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  geom_hline(yintercept=7,linetype="dashed")+
  labs(
    y = "Ancho (m)",
  ) +
  theme(
    axis.title.x = element_blank(),
    text=element_text(family="Times New Roman", size=10)) 

plot(violin_width)

figures_folder = "./analisis-pad-2-salas-vacias/analisis_volumen_y_visual"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "violin_ancho.png", sep = '')
ggsave(mi_nombre_de_archivo, plot = violin_width, limitsize=FALSE, dpi=200)


# ALTO

dimensions.height <- melt(dimensions.raw, id.vars='nsub',
                         measure.vars=c("SG_RV_height", "SR_height"))

dimensions.height <- dimensions.height %>%
  mutate(
    variable = case_when(
      variable == "SG_RV_height" ~ "Virtual",
      variable == "SR_height" ~ "Real",
    )
  )

dimensions.height <- dimensions.height %>%
  rename("Alto" = "value",
         "Condición" = "variable")

dimensions.height <- dimensions.height %>%
  filter(!Alto >= 6)

dimensions.height_sum <- dimensions.height %>%
  group_by(Condición) %>%
  summarise(
    mean = mean(Alto),
    sd = sd(Alto)
  )

# Use single color
violin_height <- ggplot(dimensions.height, aes(x=Condición, y=Alto,  fill=Condición)) +
  #geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1) + 
  theme_minimal() +
  guides(fill = "none") +
  scale_fill_brewer(palette="PuRd")+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  geom_hline(yintercept=3,linetype="dashed") +
  labs(
    y = "Alto (m)",
  ) +
  theme(
    axis.title.x = element_blank(),
    text=element_text(family="Times New Roman", size=10)) 

plot(violin_height)

figures_folder = "./analisis-pad-2-salas-vacias/analisis_volumen_y_visual"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "violin_alto.png", sep = '')
ggsave(mi_nombre_de_archivo, plot = violin_height, limitsize=FALSE, dpi=200)

figure <- ggarrange(dim_barchart, 
                    ggarrange(violin_depth, violin_width,violin_height,
                    ncol = 3, labels = c("B", "C", "D"),
                    font.label = list(family="Times New Roman")),
                    nrow = 2, 
                    labels ="A",
                    heights = c(1, 0.75),
                    font.label = list(family="Times New Roman"))
figure

figures_folder = "./analisis-pad-2-salas-vacias/analisis_volumen_y_visual"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "multiple_final_2.png", sep = '')
ggsave(mi_nombre_de_archivo, plot = figure, limitsize=FALSE, dpi=200)



# d w h mean and sd -------------------------------------------------------


