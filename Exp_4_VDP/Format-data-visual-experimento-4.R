# En este script vamos a formatear y ver los datos visuales del experi 4
# Estos datos son del reporte verbal que dieron para tamaño de sala
# Antes y despues de realizar tareas de PAD a oscuras y viendo entorno virtual grande

# dependencies ------------------------------------------------------------

library(dplyr)
library(tidyverse)


# load and format data ---------------------------------------------------------------

rm(list=ls())
tabla.pob_y_visual = read.csv("./Visual-de-experimento-4/data/Visual_y_poblacional_experimento_4.csv")

tabla.pob_y_visual <- tabla.pob_y_visual %>%
  rename(
    No_visual_information = Sin.Ver..dxwxh.,
    Virtual_environment = VR,
    Real_environment  = Sala.real,
    Speaker_position_VE = Sin.Ver..speakers.,
    Speaker_position_RE = Viendo..speakers.,
    Sight = Problemas.de.Visión,
    Subject =  Número.de.Sujeto
  )

tabla.visual <- tabla.pob_y_visual %>% 
  select(c("Subject",
           "No_visual_information", 
           "Virtual_environment", 
           "Real_environment",
           "Speaker_position_VE",
           "Speaker_position_RE",
           "Sight"
           ))

tabla.visual <- tabla.visual %>%
  separate(No_visual_information, into = c("value1", "value2", "value3"), sep = "x") %>%
  mutate(
    No_visual_info_vol = as.numeric(value1) * as.numeric(value2) * as.numeric(value3),
    No_visual_info_depth = as.numeric(value1),
    No_visual_info_width = as.numeric(value2),
    No_visual_info_height = as.numeric(value3)
  ) %>%
  separate(Virtual_environment, into = c("value1", "value2", "value3"), sep = "x") %>%
  mutate(
    Virtual_environment_vol = as.numeric(value1) * as.numeric(value2) * as.numeric(value3),
    Virtual_environment_depth = as.numeric(value1),
    Virtual_environment_width = as.numeric(value2),
    Virtual_environment_height = as.numeric(value3)
  ) %>%
  separate(Real_environment, into = c("value1", "value2", "value3"), sep = "x") %>%
  mutate(
    Real_environment_vol = as.numeric(value1) * as.numeric(value2) * as.numeric(value3),
    Real_environment_depth = as.numeric(value1),
    Real_environment_width = as.numeric(value2),
    Real_environment_height = as.numeric(value3)
  ) %>%
  select(-c("value1", "value2", "value3"))

# Estos son los outliers que sacaba de acuerdo al sesgo
# Estan calculados aca:
# './Exp_2_ADP_control/old/remocion_outliers.R'

#tabla.visual <- tabla.visual %>%
#  filter(Subject != 5) %>%
#  filter(Subject != 2) %>%
#  filter(Subject != 10) %>%
#  filter(Subject != 16) 

write.table(tabla.visual, file="./Visual-de-experimento-4/data/visual_exp_4_con_outliers.csv", row.names = FALSE)

