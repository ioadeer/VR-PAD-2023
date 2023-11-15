library(dplyr)
library(tidyverse)

as <- getwd()

data_list <- list()

# Numero de Sujeto impar es 
# bloque 1 = OSCURAS
# bloque 2 = VR

my_path = "./analisis-pad-oscuras-vr/data/raw/csv/"

list.files(path=my_path)

for (file_name in list.files(path = my_path)) {
  data <- read.csv(paste0(my_path,file_name))
  print(file_name)
  num1 <- gsub(".csv", "", strsplit(file_name, "_")[[1]][1])
  num1 <- gsub("\\D","",num1)[1]
  print(num1)
  num2 <- gsub(".csv", "", strsplit(file_name, "_")[[1]][2])
  print(num2)
  if (num2 == 1) {
       data$condicion_sala = "OSCURAS"
   } else if (num2 == 2) {
    data$condicion_sala = "VR"
  }
  data_list[[file_name]] = data
}

all_data <- do.call(rbind, data_list)
rownames(all_data) <- NULL

# all_data <- all_data %>%
#   mutate(nsub = paste0("S", nsub))

all_data <- all_data %>%
  rename("trial" = "itrial")

write.table(all_data, file="./analisis-pad-oscuras-vr/data/1_20_oscuras_vr.csv", row.names = FALSE)