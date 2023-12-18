# En este documento vamos a hacer el analisis de correlacion
# entre distancia maxima auditiva y tamanio de profundidad de sala
# Tambien la idea es hacer un analisis del tamanio reportado de la sala a oscuras
# vs el tamanio real de sala.

# Dependencias ------------------------------------------------------------
library(broom)
library(broom.mixed)
library(dplyr)
library(ggbeeswarm)
library(gmodels)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(ggstatsplot)
library(gridExtra)
library(htmlwidgets)
library(quickpsy)
library(tidyverse)
library(lme4)
library(nlme)
library(lmerTest)
library(modelr)
library(scales) 
library(pracma)
library(plotly)
library(Routliers)
library(processx)
library(rstatix)
library(orca)
library(reshape2)
library(ggpubr)


# importar, formatear y sacar outliers -------------------------------------

tabla.pob_y_perc_size <- read.csv("analisis_control/data/raw/visual_y_poblacional/Datos_poblacionales_y_visuales_oscuras_real.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)

tabla.pob_y_perc_size <- tabla.pob_y_perc_size %>%
  slice(1:20)

# Primero saco datos poblacionales
tabla.pob <- tabla.pob_y_perc_size[c("Número.de.Sujeto", "Nombre","Edad", "Género",
                                  "Problemas.de.Audición", "Problemas.de.Visión",
                                  "Estudios.Musicales")]

tabla.pob <- tabla.pob %>%
  rename("nsub" = "Número.de.Sujeto",
         "name" = "Nombre",
         "age"  = "Edad",
         "sex" = "Género",
         "problemas_audicion" = "Problemas.de.Audición",
         "problemas_vision" = "Problemas.de.Visión",
         "estudios_musicales"= "Estudios.Musicales")

write.table(tabla.pob, file="./analisis_control/data/datos_poblacionales.csv", row.names = FALSE)

tabla.perc_size <- tabla.pob_y_perc_size %>% 
  select(-c("Nombre","Edad", "Género",
         "Problemas.de.Audición", "Problemas.de.Visión",
          "Estudios.Musicales", "Altura..Oídos." )) %>%
  rename("OSCURAS" = "Sin.Ver.w.d.h.",
         "VISUAL" = "Viendo.Real",
         "nsub" = "Número.de.Sujeto")

# VISUAL

tabla.visual <- tabla.perc_size[c("nsub", "VISUAL")]

tabla.visual <-  tabla.visual %>% 
  separate(VISUAL, into = c("value1", "value2", "value3"), sep = "x") %>%
  mutate(
    VISUAL_volumen = as.numeric(value1) * as.numeric(value2) * as.numeric(value3),
    VISUAL_width = as.numeric(value1),
    VISUAL_depth = as.numeric(value2),
    VISUAL_height = as.numeric(value3)
  ) 

tabla.visual <- tabla.visual %>%
  select(c("nsub", 
           "VISUAL_volumen", 
           "VISUAL_width",
           "VISUAL_depth",
           "VISUAL_height" ))

# OSCURAS

tabla.oscuras <- tabla.perc_size[c("nsub", "OSCURAS")]

# saco el nsub q no tiene
tabla.oscuras <- tabla.oscuras %>%
  filter(nsub != 1)

tabla.oscuras <-  tabla.oscuras %>% 
  separate(OSCURAS, into = c("value1", "value2", "value3"), sep = "x") %>%
  mutate(
    OSCURAS_volumen = as.numeric(value1) * as.numeric(value2) * as.numeric(value3),
    OSCURAS_width = as.numeric(value1),
    OSCURAS_depth = as.numeric(value2),
    OSCURAS_height = as.numeric(value3)
  ) 

tabla.oscuras <- tabla.oscuras %>%
  select(c("nsub", 
           "OSCURAS_volumen", 
           "OSCURAS_width",
           "OSCURAS_depth",
           "OSCURAS_height" ))

tablas.all = merge(tabla.oscuras, tabla.visual)

sin_outliers =  read.csv("analisis_control/data/control_sin_outliers.csv", header = TRUE, sep = ' ', stringsAsFactors = TRUE)

nsub_selection = unique(sin_outliers$nsub)

tablas.all <- tablas.all %>%
  filter(nsub %in% nsub_selection)

write.table(tablas.all, file="./analisis_control/data/tamanio_de_sala_sin_outliers_oscuras_visual.csv", row.names = FALSE)
