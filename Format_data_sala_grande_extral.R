library(dplyr)
library(tidyverse)

# En este script agregamos mas que pasamos con sala grande
# 

data_sala_grande_extra_list <- list()

my_path_sc = "./data-tres-salas/22_26_subject/"

for (file_name in list.files(path = my_path_sc)) {
  data_sala_grande_extra <- read.csv(paste0(my_path_sc,file_name))
  data_sala_grande_extra$condicion_sala = "SALA_GRANDE"
  data_sala_grande_extra_list[[file_name]] = data_sala_grande_extra
}

all_data_sala_grande_extra <- do.call(rbind, data_sala_grande_extra_list)
rownames(all_data_sala_grande_extra) <- NULL

#all_data_sala_grande_extra <- all_data_sala_grande_extra %>%
#  mutate(nsub = paste0("S", nsub))

library("dplyr")
all_data_sala_grande_extra <- all_data_sala_grande_extra %>%
  rename("trial" = "itrial")

write.table(all_data_sala_grande_extra, file="./data-tres-salas/data_sala_grande_extra.csv", row.names = FALSE)
