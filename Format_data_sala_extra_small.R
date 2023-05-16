library(dplyr)
library(tidyverse)

# En este script voy a formatear la data_sc de la sala mas chica de todas
# Son 11 participantes solo con sala chica

data_sc_list <- list()

my_path_sc = "./data-tres-salas/27_34_subject/"

for (file_name in list.files(path = my_path_sc)) {
  data_sc <- read.csv(paste0(my_path_sc,file_name))
  data_sc$condicion_sala = "SALA_MAS_CHICA"
  data_sc_list[[file_name]] = data_sc
}

all_data_sc <- do.call(rbind, data_sc_list)
rownames(all_data_sc) <- NULL

#all_data_sc <- all_data_sc %>%
#  mutate(nsub = paste0("S", nsub))

library("dplyr")
all_data_sc <- all_data_sc %>%
  rename("trial" = "itrial")

write.table(all_data_sc, file="./data-tres-salas/data_sala_mas_chica.csv", row.names = FALSE)
