dimensions.volume %>%
  ggplot( aes(x=Condition, y=Volume, fill=Volume)) +
  geom_boxplot() +
 # scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ggtitle("A boxplot with jitter") +
  xlab("")

dimensions.volume_2 <- dimensions.volume %>%
  filter(!value >= 500)

dimensions.volume_2 %>%
  ggplot( aes(x=variable, y=value, fill=value)) +
  geom_boxplot() +
  # scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  ggtitle("A boxplot with jitter") +
  xlab("")

dimensions.volume_2 <- dimensions.volume_2 %>%
  rename("Volume" = "value",
         "Condition" = "variable")

stat.test <- dimensions.volume_2  %>% 
  t_test(Volume~Condition, paired=FALSE) %>%
  add_significance()

stat.test
