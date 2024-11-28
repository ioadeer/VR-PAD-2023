results_tbl <- read.csv("Exp_4_ADP_vr/ResultsData/DresultsExp4.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)


g1 <- ggplot(results_tbl, aes(x = target_distance, y = perc_dist)) +
  geom_errorbar(data=results_tbl, color="black",alpha = 1, width=1.3, size=1.1,
                mapping=aes(ymin = perc_dist - perc_dist_sem,
                            ymax = perc_dist + perc_dist_sem))+
  geom_point(color="red", size=1) +
  #geom_abline(intercept = 1, slope = 1,linetype="dashed") +       
  scale_x_continuous(name="Distance source (m)")+#, breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,20)) +
  scale_y_continuous(name="Perceived distance (m)")+#,  breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,20)) +
  geom_jitter(data = results_tbl, mapping = aes(x=target_distance, y=perc_dist), color="blue", alpha=.8, shape=4, size=2.2, stroke=.2,
              position = position_jitter(width=.1, height=.1)) + 
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  facet_grid(room_condition ~ subject) + 
  theme_linedraw(base_size = 9)

plot(g1)

figures_folder = "./Exp_4_ADP_vr/Figura_individual/"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "ind", ".png", sep = '')
#ggsave(device = "png", mi_nombre_de_archivo, plot=main_figure, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)

## asi me lo guarda bien
png(mi_nombre_de_archivo, res=600, units="cm", width=100, height=30)
plot(g1)
dev.off()
