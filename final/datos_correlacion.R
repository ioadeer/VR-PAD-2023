# Armar datos para correlacion
# Max distancia auditiva y depth

library(dplyr)
library(tidyr)

# Exp 
df.adp <- read.csv("final/data/ADP.csv")
df.vdp <- read.csv("final/data/VDP.csv")

df.adp_max <- df.adp %>%
  group_by(subject, room_condition, experiment) %>%
  summarise(dist_max = max(perc_dist))

df.correlation = merge(x =df.adp_max,
                        y = filter(df.vdp, dimension == "Depth"),
                        by= c("subject", "room_condition","experiment")) 

df.correlation <- df.correlation %>%
  mutate(
    log_auditory_max_dist = log(dist_max),
    log_visual_perc_depth = log(value)
    ) %>%
  rename(
    "auditory_max_dist" = "dist_max",
    "visual_perc_depth" = "value"
  ) %>%
  select(!c("dimension"))

write.csv(df.correlation, "final/data/correlation.csv", row.names = FALSE )


# correlaciones -----------------------------------------------------------

cor.test(filter(df.correlation, experiment == "1" & room_condition == "Small VE")$log_auditory_max_dist,
         filter(df.correlation, experiment == "1" & room_condition == "Small VE")$log_visual_perc_depth ,
         method= 'pearson')

cor.test(filter(df.correlation, experiment == "1" & room_condition == "Congruent VE")$log_auditory_max_dist,
         filter(df.correlation, experiment == "1" & room_condition == "Congruent VE")$log_visual_perc_depth ,
         method= 'pearson')

cor.test(filter(df.correlation, experiment == "2" & room_condition == "No visual info")$log_auditory_max_dist,
         filter(df.correlation, experiment == "2" & room_condition == "No visual info")$log_visual_perc_depth ,
         method= 'pearson')

cor.test(filter(df.correlation, experiment == "2" & room_condition == "Visual info")$log_auditory_max_dist,
         filter(df.correlation, experiment == "2" & room_condition == "Visual info")$log_visual_perc_depth ,
         method= 'pearson')

cor.test(filter(df.correlation, experiment == "1")$log_auditory_max_dist,
         filter(df.correlation, experiment == "1")$log_visual_perc_depth,
         method= 'pearson')
