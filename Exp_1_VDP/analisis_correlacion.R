

# format data -------------------------------------------------------------


tabla.exp_1 <- read.csv("./Exp_1_VDP/data/dimensiones_de_sala_visual_1_50_sin_outliers.csv", header = TRUE, sep = ' ', stringsAsFactors = TRUE)

tabla.exp_1 <- select(tabla.exp_1, c(nsub,block_1, block_2,SG_RV_depth, SC_RV_depth))

tabla.raw <- read.csv('./new_Exp_1_ADP/data/S1_S50_2_bloques_sin_outliers.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)

tabla.dist_max <- tabla.raw %>%
  group_by(nsub, nbloque) %>%
  summarise(distanca_max = max(respuesta))

tabla.exp_1.temp.block_1 <- select(tabla.exp_1, c(nsub, block_1,SG_RV_depth))
lookup <- c( room_condition = "block_1", perceived_depth = "SG_RV_depth")
tabla.exp_1.temp.block_1 <- rename(tabla.exp_1.temp.block_1, all_of(lookup))
tabla.exp_1.temp.block_1["nbloque"] = 1

tabla.exp_1.temp.block_2 <- select(tabla.exp_1, c(nsub, block_2,SC_RV_depth))
lookup <- c( room_condition = "block_2", perceived_depth = "SC_RV_depth")
tabla.exp_1.temp.block_2 <- rename(tabla.exp_1.temp.block_2, all_of(lookup))
tabla.exp_1.temp.block_2["nbloque"] = 2

tabla.exp_max_visual_depth <- rbind(tabla.exp_1.temp.block_1,tabla.exp_1.temp.block_2)

tabla.corr <- merge(tabla.dist_max, tabla.exp_max_visual_depth, by= c("nsub", "nbloque"))

write.table(tabla.corr, file="Exp_1_VDP/data/max_audiovisual_depth_1_50_dos_bloques.csv", row.names = FALSE)




# load data ---------------------------------------------------------------

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

corr_smaller_ve <- cor(smaller_ve$log_perceived_depth,smaller_ve$log_distancia_max , method= 'pearson')

cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")

correlation_plot <- ggplot(tabla.analisis_cor, 
                           aes(x =log_perceived_depth, y = log_distancia_max,
                               colour = room_condition)) +
  scale_colour_manual(values = cbPalette) +
  scale_fill_manual(values = cbPalette) +
  geom_point() +
  geom_smooth(alpha=0.3, method= "lm")+
  stat_cor(method = "pearson", show.legend= FALSE)+
  #ggtitle("Correlation between visual and auditory distance assesments (log log)") +
  xlab("Perceived Virtual Room Depth (m)") +
  #theme(legend.title =element_blank(), legend.position = 'none')+
  ylab("Maximum Perceived Auditory Distance (m)")+
  labs(color = "Visual condition") +  # Change legend title
  scale_color_manual(values = c("#000000","#E69F00"), 
                     labels = c("Smaller VE", "Congruent VE"))+
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank(),
        text=element_text(family="Arial", size=10)) 


plot(correlation_plot)


# save plot ---------------------------------------------------------------

figures_folder = "./Exp_1_VDP/figuras/"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "All_Correlation", ".png", sep = '')
#ggsave(device = "png", mi_nombre_de_archivo, plot=correlation_plot, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)

## asi me lo guarda bien
png(mi_nombre_de_archivo, res=600, units="cm", width=15, height=15)
plot(correlation_plot)
dev.off()
