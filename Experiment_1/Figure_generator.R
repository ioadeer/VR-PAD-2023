# script para hacer una figura compuesta de
# - Curva de distancia
# - Sesgo sin signo
# - Violin plot de profundidad
# - Correlacion
main_figure <- ggarrange(
                         ggarrange(f1, f7, widths = c(0.7,0.3),
                                   ncol = 2, labels = c("A", "B")),
                         ggarrange(
                           violin_depth,
                           correlation_plot,
                           ncol = 2, 
                           labels =c("C", "D"),
                          # heights = c(0.25,1),
                           widths = c(0.25, 0.75)),
                         nrow = 2, 
                         labels ="A",
                         heights = c(1, 1),
                         common.legend = TRUE)


figures_folder = "./Experiment_1/figuras/"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Depth_correlation", ".png", sep = '')
#ggsave(device = "png", mi_nombre_de_archivo, plot=correlation_plot, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)

## asi me lo guarda bien
png(mi_nombre_de_archivo, res=600, units="cm", width=15, height=15)
plot(main_figure)
dev.off()
