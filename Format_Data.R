library(dplyr)
library(tidyverse)

as <- getwd()

data_list <- list()

# Numero de Sujeto impar es 
# bloque 1 = SALA_GRANDE
# bloque 2 = SALA_CHICA
# bloque 3 = SALA_PARLANTES (para todos)
my_path = "./data-piloto/all_data_13_10_2023/csv/"

for (file_name in list.files(path = my_path)) {
  data <- read.csv(paste0(my_path,file_name))
  print(file_name)
  num1 <- gsub(".csv", "", strsplit(file_name, "_")[[1]][1])
  num1 <- gsub("\\D","",num1)[1]
  print(num1)
  num2 <- gsub(".csv", "", strsplit(file_name, "_")[[1]][2])
  print(num2)
  if (as.numeric(num1)%%2 != 0) {
    if (num2 == 1) {
      data$condicion_sala = "SALA_GRANDE"
    } else if (num2 == 2) {
      data$condicion_sala = "SALA_CHICA"
    } else if (num2 == 3) {
      data$condicion_sala = "SALA_PARLANTES"
    }
  } else {
    if (num2 == 1) {
      data$condicion_sala = "SALA_CHICA"
    } else if (num2 == 2) {
      data$condicion_sala = "SALA_GRANDE"
    } else if (num2 == 3) {
      data$condicion_sala = "SALA_PARLANTES"
    }
  }
  data_list[[file_name]] = data
  }

all_data <- do.call(rbind, data_list)
rownames(all_data) <- NULL

all_data <- all_data %>%
  mutate(nsub = paste0("S", nsub))

library("dplyr")
all_data <- all_data %>%
  rename("trial" = "itrial")

write.table(all_data, file="./data-piloto/data.csv", row.names = FALSE)
