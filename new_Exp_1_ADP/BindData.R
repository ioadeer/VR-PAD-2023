# Este script lo voy a usar para unir los datos de los participantes en un
# solo dataset que tenga todos los participantes y los bloques.

library(dplyr)
library(tidyverse)

## En este script vamos a analizar todo lo que teniamos
## Primeros 11 con tres bloques
tabla.primeros_11 <- read.csv('./new_Exp_1_ADP/data/1_11_s/data_1-11.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)
# nos quedamos con 1er bloque
# sacar 5 porque es outlier
# sacar bloque 3 porque no lo continuamos usando
tabla.primeros_11 <- tabla.primeros_11 %>%
  filter(nbloque != 3) %>%
  filter(nsub != 5) %>%
  arrange(nsub)

tabla.12_al_32 <- read.csv(
  './new_Exp_1_ADP/data/12_32_s/raw/data-s12-32-2-blocks.csv', 
  header = TRUE, sep = ' ', stringsAsFactors = TRUE)

tabla.33_al_50 <- read.csv('./new_Exp_1_ADP/data/33_50_s/data-s33-50.csv',
                           header = TRUE, sep = ' ', stringsAsFactors = TRUE)

tabla.all <- rbind(tabla.primeros_11, tabla.12_al_32, tabla.33_al_50)

write_csv(tabla.all, file ="./new_Exp_1_ADP/data/S1_S50_2_bloques.csv")
