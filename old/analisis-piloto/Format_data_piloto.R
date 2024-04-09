# En este script voy a formatear la data que finalmente se utiliza en piloto
# Estos son los csv de las carpetas raw
# 22_26_subject -> sala grande
# 27_34_subject -> sala extra chica
# Estan desbalanceados
# para la primera condicion hay 5 sujetos
# para ka segunda condicion sala chica hay 8 sujetos

library("dplyr")

data_sc_list <- list()

# Primero sustraemos los csv para la primer condicion. SALA GRANDE

my_path_sc = "./analisis-piloto/data/raw/22_26_subject/"

for (file_name in list.files(path = my_path_sc)) {
  data_sc <- read.csv(paste0(my_path_sc,file_name))
  data_sc$condicion_sala = "SALA_GRANDE"
  data_sc_list[[file_name]] = data_sc
}

# Segundo sustraemos los csv para la segunda cond. SALA CHICA.
# y los agregamos

my_path_sc = "./analisis-piloto/data/raw/27_34_subject/"

for (file_name in list.files(path = my_path_sc)) {
  data_sc <- read.csv(paste0(my_path_sc,file_name))
  data_sc$condicion_sala = "SALA_CHICA"
  data_sc_list[[file_name]] = data_sc
}


all_data_sc <- do.call(rbind, data_sc_list)
rownames(all_data_sc) <- NULL

#all_data_sc <- all_data_sc %>%
#  mutate(nsub = paste0("S", nsub))

all_data_sc <- all_data_sc %>%
  rename("trial" = "itrial")

# Llevo ID a rango de 1 en adelante
all_data_sc_new <- all_data_sc %>%
  mutate(nsub = nsub - 21)

write.table(all_data_sc_new, file="./analisis-piloto/data/data-piloto.csv", row.names = FALSE)
