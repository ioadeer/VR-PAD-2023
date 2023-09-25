# Este script es para formatear datos visuales de sala para
# sala VR grande , sala VR chica, y sala normal
# Dependencies ------------------------------------------------------------

library(dplyr)
library(tidyverse)

# format data 1 11 -------------------------------------------------------------

file_name_1_11 = "./tamanio-visual-de-sala/raw/1_11_s/1_11_s_visual.csv"
data_12_32 <- read.csv(file_name_1_11)

data_12_32 <- data_12_32 %>%
  slice(1:11)

colnames(data_12_32)

# Renombro SG_RV_wdh es sala grande realidad virtual width depth height
# Renombro SC_RV_wdh es sala chica realidad virtual width depth height
# Renombro SR_wdh es sala chica realidad virtual width depth height
data_12_32 <- data_12_32 %>%
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


data_12_32 <- data_12_32 %>%
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

data_12_32 <- data_12_32 %>%
  rename(
    nsub = Número.de.Sujeto
  )

data_12_32 <- data_12_32 %>%
  rename(
    block_1 = Orden,
    block_2 = X
  )

data_12_32 <- select(data_12_32, -c(Piloto, 
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

print(data_12_32)

write.table(data_12_32, file="./tamanio-visual-de-sala/data/data_12_32_s.csv", row.names = FALSE)



# format data 12 32 -------------------------------------------------------

file_name_12_32 = "./tamanio-visual-de-sala/raw/12_32_s/12_32_s_visual.csv"
data_12_32 <- read.csv(file_name_12_32)

data_12_32 <- data_12_32 %>%
  slice(1:21)

colnames(data_12_32)

# Renombro SG_RV_wdh es sala grande realidad virtual width depth height
# Renombro SC_RV_wdh es sala chica realidad virtual width depth height
# Renombro SR_wdh es sala chica realidad virtual width depth height
data_12_32 <- data_12_32 %>%
  rename(
    SG_RV_wdh = Medida.Sala.Grande..Ancho..Largo..Alto.,
    SC_RV_wdh = Medida.Sala.Chica..Ancho..Largo..Alto.,
    SR_wdh = Medida.Sala.Real..Ancho..Largo..Alto.
  )

data_12_32 <- data_12_32 %>%
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

data_12_32 <- data_12_32 %>%
  rename(
    nsub = Número.de.Sujeto
  )

data_12_32 <- data_12_32 %>%
  rename(
    block_1 = Orden,
    block_2 = X
  )

# Nombres a sacar
# 
# [1] "Número.de.Sujeto"                        "Nombre"                                 
# [3] "Edad"                                    "Género"                                 
# [5] "Altura..Oídos."                          "Problemas.de.Audición"                  
# [7] "Problemas.de.Visión"                     "Estudios.Musicales"                     
# [9] "Orden"    

data_12_32 <- select(data_12_32, -c(Altura..Oídos., 
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

data_12_32 <- data_12_32 %>%
  mutate(nsub = nsub+11)

print(data_12_32)

write.table(data_12_32, file="./tamanio-visual-de-sala/data/data_12_32_s.csv", row.names = FALSE)



# format data 33 50 -------------------------------------------------------

file_name_33_50 = "./tamanio-visual-de-sala/raw/33_50_s/33_50_s_visual.csv"
data_33_50 <- read.csv(file_name_33_50)

data_33_50 <- data_33_50 %>%
  slice(1:18)

colnames(data_33_50)

# Renombro SG_RV_wdh es sala grande realidad virtual width depth height
# Renombro SC_RV_wdh es sala chica realidad virtual width depth height
# Renombro SR_wdh es sala chica realidad virtual width depth height
data_33_50 <- data_33_50 %>%
  rename(
    SG_RV_wdh = Medida.Sala.Grande..Ancho..Largo..Alto.,
    SC_RV_wdh = Medida.Sala.Chica..Ancho..Largo..Alto.,
    SR_wdh = Medida.Sala.Real..Ancho..Largo..Alto.
  )

data_33_50 <- data_33_50 %>%
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

data_33_50 <- data_33_50 %>%
  rename(
    nsub = Número.de.Sujeto
  )

data_33_50 <- data_33_50 %>%
  rename(
    block_1 = Orden,
    block_2 = X
  )

# Nombres a sacar
# 
# [1] "Número.de.Sujeto"                        "Nombre"                                 
# [3] "Edad"                                    "Género"                                 
# [5] "Altura..Oídos."                          "Problemas.de.Audición"                  
# [7] "Problemas.de.Visión"                     "Estudios.Musicales"                     
# [9] "Orden"    

data_33_50 <- select(data_33_50, -c(Altura..Oídos., 
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

data_33_50 <- data_33_50 %>%
  mutate(nsub = nsub+32)

print(data_33_50)

write.table(data_33_50, file="./tamanio-visual-de-sala/data/data_33_50_s.csv", row.names = FALSE)


