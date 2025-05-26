# En este script vamos a formatear y ver los datos visuales del experi 2
# Estos datos son del reporte verbal que dieron para tamaño de sala
# Antes y despues de realizar tareas de PAD a oscuras y viendo

# dependencies ------------------------------------------------------------

library(dplyr)
library(tidyverse)


# load and format data ---------------------------------------------------------------

rm(list=ls())
tabla.pob_y_visual = read.csv("Experiment_2/Exp_2_ADP_control/data/raw/visual_y_poblacional/Datos_poblacionales_y_visuales_oscuras_real.csv")

tabla.pob_y_visual <- tabla.pob_y_visual %>%
  rename(
    No_Visual = Sin.Ver.w.d.h.,
    Visual = Viendo.Real,
    Subject =  Número.de.Sujeto
  )

tabla.visual <- tabla.pob_y_visual %>% 
  select(c("No_Visual", "Visual", "Subject"))

tabla.visual <- tabla.visual %>%
  separate(No_Visual, into = c("value1", "value2", "value3"), sep = "x") %>%
  mutate(
    No_Visual_volumen = as.numeric(value1) * as.numeric(value2) * as.numeric(value3),
    No_Visual_width = as.numeric(value1),
    No_Visual_depth = as.numeric(value2),
    No_Visual_height = as.numeric(value3)
  ) %>%
  separate(Visual, into = c("value1", "value2", "value3"), sep = "x") %>%
  mutate(
    Visual_volumen = as.numeric(value1) * as.numeric(value2) * as.numeric(value3),
    Visual_width = as.numeric(value1),
    Visual_depth = as.numeric(value2),
    Visual_height = as.numeric(value3)
  ) %>% 
  select(-c("value1", "value2", "value3"))

# Estos son los outliers que sacaba de acuerdo al sesgo
# Estan calculados aca:
# './Exp_2_ADP_control/old/remocion_outliers.R'

#tabla.visual <- tabla.visual %>%
#  filter(Subject != 1) %>%
#  filter(Subject != 2) %>%
#  filter(Subject != 10) %>%
#  filter(Subject != 16) 

write.table(tabla.visual, file="Experiment_2/Exp_2_VDP/data/visual_exp_2.csv", row.names = FALSE)

