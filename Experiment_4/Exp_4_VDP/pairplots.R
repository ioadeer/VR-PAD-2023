#dimensions.depth

f6 <-  ggplot() +
  #geom_line(aes(group = 1),size = 1.2, alpha=.5)#+
  geom_point(data = dimensions.depth, mapping = aes(x = Condition,y = Depth, colour = Condition, fill = Condition), alpha = 0.3)+
  geom_line(data = dimensions.depth, mapping = aes(x = Condition,y = Depth, group = Subject),alpha = 0.5)+
  geom_violin(data= dimensions.depth,aes(x = Condition,y = Depth, colour = Condition), trim=TRUE, alpha=0)+
  geom_pointrange(data= dimensions.depth_sum, mapping= aes(x=Condition, y=mean, ymin=mean-se, ymax=mean+se), size = 0.5) +
  #scale_colour_manual(values = cbPalette) + 
  #scale_fill_manual(values = cbPalette) 
  # geom_abline(slope = 0,
  #             intercept = 0,
  #             alpha = 0.5,
  #             linetype = "dashed") +
  labs(x = "Condition", 
        y = "Depth (m)") 
  # # facet_grid(. ~ type) +
  # #annotate("text", x = 1.5, y = 0.3,  label = "*", size = 4) +
  # #annotate("segment", x = 1, xend = 2, y = 0.2, yend = 0.2, colour = "black", size=.5, alpha=1,)+
  # theme_pubr(base_size = 12, margin = TRUE)+
  # theme(legend.position = "none",
  #       axis.title.x = element_blank(),
  #       axis.text.x = element_blank())

f6

stat.test <- dimensions.depth  %>% 
  t_test(Depth~Condition, paired=FALSE) %>%
  add_significance()

stat.test

figures_folder = "./Visual-de-experimento-4/figuras/"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Pariplot_depth.png", sep = '')
ggsave(mi_nombre_de_archivo, plot =f6, width = 1400, height=1000, dpi=200, units = "px")
