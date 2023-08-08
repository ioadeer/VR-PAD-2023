
# Dependencies ------------------------------------------------------------

library(dplyr)
library(tidyverse)


# import data -------------------------------------------------------------

file_name_1_11 = "./tamanio-visual-de-sala/raw/1_11_s/1_11_s_visual.csv"
data_1_11 <- read.csv(file_name_1_11)

data_1_11 <- data_1_11 %>%
  slice(1:11)

colnames(data_1_11)

# Renombro SG_RV_wdh es sala grande realidad virtual width depth height
# Renombro SC_RV_wdh es sala chica realidad virtual width depth height
# Renombro SR_wdh es sala chica realidad virtual width depth height
data_1_11 <- data_1_11 %>%
  rename(
         SG_RV_wdh = Medida.Sala.Grande..Ancho..Largo..Alto.,
         SC_RV_wdh = Medida.Sala.Chica..Ancho..Largo..Alto.,
         SR_wdh = Medida.Sala.Real..Ancho..Largo..Alto.
         )

# Del chat Gpt

# # install.packages("tidyverse")
# library(tidyverse)
# 
# # Sample dataframe with a column containing values separated by "X"
# data <- data.frame(values = c("2X3X4", "5X6X7", "8X9X10"))
# 
# # Split the values, multiply them, and create new columns
# data_processed <- data %>%
#   separate(values, into = c("value1", "value2", "value3"), sep = "X") %>%
#   mutate(
#     result = as.numeric(value1) * as.numeric(value2) * as.numeric(value3),
#     second_value = as.numeric(value2)
#   )
# 
# # Print the processed dataframe
# print(data_processed)


data_1_11 <- data_1_11 %>%
  separate(SG_RV_wdh, into = c("value1", "value2", "value3"), sep = "x") %>%
  mutate(
    SG_RV_volumen = as.numeric(value1) * as.numeric(value2) * as.numeric(value3),
    SG_RV_width = as.numeric(value1),
    SG_RV_depth = as.numeric(value2),
    SG_RV_height = as.numeric(value3)
  ) %>%
  separate(SC_RV_wdh, into = c("value1", "value2", "value3"), sep = "x") %>%
  mutate(
    SC_RV_volumen = as.numeric(value1) * as.numeric(value2) * as.numeric(value3),
    SC_RV_width = as.numeric(value1),
    SC_RV_depth = as.numeric(value2),
    SC_RV_height = as.numeric(value3)
  ) %>%
  separate(SR_wdh, into = c("value1", "value2", "value3"), sep = "x") %>%
  mutate(
    SR_volumen = as.numeric(value1) * as.numeric(value2) * as.numeric(value3),
    SR_width = as.numeric(value1),
    SR_depth = as.numeric(value2),
    SR_height = as.numeric(value3)
  ) 

data_1_11 <- data_1_11 %>%
  rename(
    nsub = Número.de.Sujeto
  )

data_1_11 <- data_1_11 %>%
  rename(
    block_1 = Orden,
    block_2 = X
  )

data_1_11 <- select(data_1_11, -c(Piloto, 
                                  Nombre,
                                  Género,
                                  Edad,
                                  Altura..Oídos., 
                                  Problemas.de.Audición,
                                  Problemas.de.Visión,
                                  value1,
                                  value2,
                                  value3,
                                  Estudios.Musicales
                                  ))

print(data_1_11)

write.table(data_1_11, file="./tamanio-visual-de-sala/data/data_1_11_s.csv", row.names = FALSE)
