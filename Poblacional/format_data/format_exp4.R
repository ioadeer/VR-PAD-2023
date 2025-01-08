# Extraccion y formateo de datos poblacionales del experimento 4


# dependencies ------------------------------------------------------------

library(dplyr)
library(tidyverse)


# load and format data ---------------------------------------------------------------

rm(list=ls())
tabla.pob_y_visual = read.csv("./Visual-de-experimento-4/data/Visual_y_poblacional_experimento_4.csv")

tabla.pob <- tabla.pob_y_visual %>%
  select(c("Número.de.Sujeto", 
           "Edad",
           "Género",
           "Problemas.de.Visión"
           )) %>%
  rename("nsub" = "Número.de.Sujeto",
         "age"  = "Edad",
         "sex" = "Género",
         "problemas_vision" = "Problemas.de.Visión")

tabla.pob <- tabla.pob %>%
  mutate("exp" = 3) %>%
  mutate(problemas_vision = case_when(
    problemas_vision == "anteojos" ~ "Si",
    problemas_vision == "No" ~ "No",
  )) %>%
  mutate(sex = case_when(
    sex == "Fem" ~ "F",
    sex == "Masc" ~ "M",
  ))



write.table(tabla.pob, file="Poblacional/data/Exp_4_demografico_16.csv", row.names = FALSE)
