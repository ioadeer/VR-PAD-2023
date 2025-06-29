# script para hacer una figura compuesta de
# aca vamos a hacer curva de distancia a la izq
# las demas a la derecha
# - Curva de distancia
# - Sesgo sin signo
# - Violin plot de profundidad
# - Correlacion


# curva de dist -----------------------------------------------------------
f1 <- ggplot(tabla.pob, aes(x=target_distance, y =10^Mperc_dist, group = room_condition, color  = room_condition)) +
  geom_pointrange(aes(x = target_distance, y = 10^Mperc_dist, ymin = 10^(Mperc_dist-SDperc_dist), ymax = 10^(Mperc_dist+SDperc_dist)),alpha = 1,
                  position = position_jitterdodge(jitter.width = .1,
                                                  jitter.height = 0,
                                                  dodge.width = .1 ))+
  geom_abline(intercept = 0, slope = 1, linetype=2) +
  scale_colour_manual(values = c(myViridis[1], myViridis[2])) +
  scale_fill_manual(values = c(myViridis[1], myViridis[2])) +
  geom_line(data = Final.Fixed, aes(x = target_distance, y =10^fit, group=room_condition, color=room_condition))+
  geom_label(x = -0.1, y = 4.75, label = as.character(as.expression(eq1)), 
             hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = myViridis[2],
             #family="Times New Roman"
  )+
  geom_label(x = -0.1, y = 4.3, label = as.character(as.expression(eq2)), 
             hjust = 0, nudge_x =  0,parse = TRUE, size = 4, 
             color = myViridis[1],
             #family="Times New Roman"
  )+
  #geom_text(x = 0.2, y = 6.0, label = as.character(as.expression(eq3)), hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#009E73")+
  scale_x_continuous(name="Distance source (m)", limits = c(0,10)) +
  scale_y_continuous(name="Perceived distance (m)",   limits = c(0,5)) +
  scale_color_manual(labels = c("Large VE", "Small VE"), values =c(myViridis[2], myViridis[1]))+
  #theme_pubr(base_size = 12, margin = TRUE)+
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        #text=element_text(family="Times New Roman"
        , size=10)

# unsigned bias -----------------------------------------------------------------

f7 =  ggplot(results_tblp, aes(x = room_condition,y = MBiasUnSigned, colour = room_condition, fill = room_condition)) +
  geom_pointrange(aes(x=room_condition, y=MBiasUnSigned, ymin=MBiasUnSigned-SDBiasUnSigned, ymax=MBiasUnSigned+SDBiasUnSigned), size = 0.5)+
  geom_point(data = results_tbls, mapping = aes(x = room_condition,y = mBiasUnSigned, colour = room_condition, fill = room_condition), alpha = .1)+
  geom_line(aes(group = 1),size = 1.2, alpha=.5)+
  geom_line(data = results_tbls, mapping = aes(x = room_condition,y = mBiasUnSigned, group = subject, colour = room_condition),alpha = 0.1)+
  geom_violin(data= results_tbls,aes(x = room_condition,y = mBiasUnSigned), trim=TRUE, alpha=0)+
  scale_colour_manual(values = c(myViridis[2], myViridis[1])) + 
  scale_fill_manual(values = c(myViridis[2], myViridis[1])) + 
  # correr con esto
  scale_x_discrete(labels = c('LVE','SVE'))+
  geom_abline(slope = 0,
              intercept = 0,
              alpha = 0.5,
              linetype = "dashed") +
  labs(x = "Condition", 
       y = "Relative\nunsigned bias") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 9, hjust = 0.0),
        #axis.text.x = element_blank(),
        plot.margin = unit(c(0.0,0.25,0.0,0.0), "cm"))




# depth -------------------------------------------------------------------
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
    y = "Mean perceived\ndepth ± SEM (m)",
  )+
  theme_minimal() +
  guides(fill = "none") +
  geom_hline(yintercept=12,linetype="dashed") +
  annotate("text", x=1.5, y=14, label= "12 m", size=3.5) +
  #  theme_pubr(base_size = 12, margin = TRUE)+
  scale_colour_manual(values =myViridis) +
  scale_fill_manual(values = myViridis) +
  theme(
    #axis.text.x=element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 9, hjust = 0.0),
    # axis.title.y = element_text(hjust = 1),
    plot.margin = unit(c(0.0,0.25,0.0,0.0), "cm")# top right botom left
  ) 

# correlacion -------------------------------------------------------------

correlation_plot <- ggplot(tabla.analisis_cor, 
                           aes(x =log_perceived_depth, y = log_distancia_max,
                               colour = room_condition)) +
  #scale_fill_brewer(palette="YlOrRd")+
  scale_colour_manual(values = c(myViridis[2], myViridis[1]),
                      labels = c("Large VE", "Small VE")) +
  scale_fill_manual(values = c(myViridis[2], myViridis[1])) +
  geom_point() +
  geom_smooth(alpha=0.3, method= "lm")+
  #stat_cor(method = "pearson", show.legend= FALSE,  position = "jitter")+
  annotate("label",                        # Add text for mean
           x = 0, # para fig compuesta
           y = 2.6,
           label = eqn1_cor,
           size = 3,
           hjust = 0,
           color = myViridis[1],
  ) +
  annotate("label",                        # Add text for mean
           x = 0, # para fig compuesta
           y = 3,
           label = eqn2_cor,
           size = 3,
           hjust = 0,
           color = myViridis[2],
  ) +
  #ggtitle("Correlation between visual and auditory distance assesments (log log)") +
  xlab("Perceived Virtual Room Depth (m)") +
  ylab("Maximum Perceived \n Auditory Distance (m)")+
  labs(color = "Visual condition") +  # Change legend title
  theme_minimal()+
  theme(legend.position = "top",
        legend.title = element_blank(),
        #text=element_text(family="Arial", size=10)
  ) 

# main --------------------------------------------------------------------


main_figure <- ggarrange(
                         f1,
                         ggarrange(
                           f7,
                           violin_depth,
                           correlation_plot,
                           nrow = 3, 
                           labels =c("B","C", "D"),
                           heights = c(.25,.25,.5)),
                           #widths = c(0.25, 0.75)),
                         ncol = 2, 
                         labels ="A",
                         heights = c(.75, 1.25),
                         common.legend = TRUE)


figures_folder = "./Experiment_1/figuras/"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "main_2", ".png", sep = '')
#ggsave(device = "png", mi_nombre_de_archivo, plot=correlation_plot, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)

## asi me lo guarda bien
png(mi_nombre_de_archivo, res=600, units="cm", width=15, height=15)
plot(main_figure)
dev.off()

