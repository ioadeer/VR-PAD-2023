# Generacion de datos PAD en unico dataset


# dependencies ------------------------------------------------------------

library(dplyr)
library(tidyr)



# load data ---------------------------------------------------------------

# Experiment 1
results_tbl.exp_1 <- read.csv("./Experiment_1/Exp_1_ADP/ResultsData/Dresults_nuevos.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)
results_tbl.exp_1$experiment = "1"

# Experiment 2

results_tbl.exp_2 <- read.csv("Experiment_2/Exp_2_ADP_control/ResultsData/Dresults_nuevos.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)
results_tbl.exp_2 <- results_tbl.exp_2 %>%
  mutate(
    room_condition = case_when(
      room_condition == "Visual information" ~ "Visual info",
      room_condition == "No visual information" ~ "No visual info",
    )
  )
results_tbl.exp_2$experiment = "2"

# Experiment 3
results_tbl.exp_3 <- read.csv("Experiment_3/Exp_3_ADP_vr/ResultsData/Dresults_nuevos.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)
results_tbl.exp_3 <- results_tbl.exp_3 %>%
  mutate(
    room_condition = case_when(
      room_condition == "No visual information" ~ "No visual info",
      room_condition == "Virtual Environment" ~ "Congruent VE",
    )
  )
results_tbl.exp_3$experiment = "3"

# Experiment 4 FALTA 
results_tbl.exp_4 <- read.csv("Experiment_4/Exp_4_ADP_vr/ResultsData/Dresults_nuevos.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)
results_tbl.exp_4 <- results_tbl.exp_4 %>%
  mutate(
    room_condition = case_when(
      room_condition == "No Visual Info" ~ "No visual info",
      room_condition == "Larger VE" ~ "Large VE",
    )
  )
results_tbl.exp_4$experiment = "4"

result_tbl.all <- rbind(results_tbl.exp_1,
                        results_tbl.exp_2,
                        results_tbl.exp_3,
                        results_tbl.exp_4)

write.csv(result_tbl.all, file="./final/data/ADP.csv", row.names = FALSE)
