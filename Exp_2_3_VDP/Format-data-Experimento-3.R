# En este script vamos a formatear y ver los datos visuales del experi 3
# Estos datos son del reporte verbal que dieron para tamaño de sala
# Antes y despues de realizar tareas de PAD a oscuras y virtual

# dependencies ------------------------------------------------------------

library(dplyr)
library(tidyverse)


# load and format data ---------------------------------------------------------------

rm(list=ls())
tabla.pob_y_visual = read.csv("./Exp_3_ADP_vr/data/raw/Participantes_Exp_oscuras_virtual.csv")

tabla.pob_y_visual <- tabla.pob_y_visual %>%
  rename(
    Subject =  Número.de.Sujeto,
    No_visual_information = Sin.Ver..dxwxh.,
    VE = VR
  )

tabla.visual <- tabla.pob_y_visual %>% 
  select(c("No_visual_information", "VE", "Subject")) %>%
  slice(1:20)

tabla.visual <- tabla.visual %>%
  separate(No_visual_information, into = c("value1", "value2", "value3"), sep = "x") %>%
  mutate(
    No_visual_info_vol = as.numeric(value1) * as.numeric(value2) * as.numeric(value3),
    No_visual_info_depth = as.numeric(value1),
    No_visual_info_width = as.numeric(value2),
    No_visual_info_height = as.numeric(value3)
  ) %>%
  separate(VE, into = c("value1", "value2", "value3"), sep = "x") %>%
  mutate(
    VE_vol = as.numeric(value1) * as.numeric(value2) * as.numeric(value3),
    VE_depth = as.numeric(value1),
    VE_width = as.numeric(value2),
    VE_height = as.numeric(value3)
  ) %>% 
  select(-c("value1", "value2", "value3"))

# Estos son los outliers que sacaba de acuerdo al sesgo
# Estan calculados aca:
# './Exp_2_ADP_control/old/remocion_outliers.R'

write.table(tabla.visual, file="./Visual-de-experimento-2-3/data/visual_exp_3.csv", row.names = FALSE)

sin_outliers =  read.csv("Exp_3_ADP_vr/data/oscuras_vr_sin_outliers.csv", header = TRUE, sep = ' ', stringsAsFactors = TRUE)

nsub_selection = unique(sin_outliers$nsub)

tabla.visual <- tabla.visual %>%
  filter(Subject %in% nsub_selection)

write.table(tabla.visual, file="Exp_3_ADP_vr/data/tamanio_de_sala_sin_outliers_oscuras_virtual.csv", row.names = FALSE)
