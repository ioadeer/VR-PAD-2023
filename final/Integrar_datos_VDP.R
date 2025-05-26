# Script para integrar datos de VDP


# dependencies ------------------------------------------------------------

library(dplyr)
library(tidyr)

# exp 1 --------------------------------------------------------------
tabla.exp_1_a <-read.csv("Experiment_1/Exp_1_VDP/data/tamanio_visual_sala/data_1_11_s.csv", sep =" ")
tabla.exp_1_b <-read.csv("Experiment_1/Exp_1_VDP/data/tamanio_visual_sala/data_12_32_s.csv", sep =" ")
tabla.exp_1_c <-read.csv("Experiment_1/Exp_1_VDP/data/tamanio_visual_sala/data_33_50_s.csv", sep =" ")

tabla.exp_1 <- rbind(tabla.exp_1_a, tabla.exp_1_b, tabla.exp_1_c)

# solo del bloque 1
tabla.exp_1.cve_depth <- tabla.exp_1 %>%
  filter(block_1 == "Sala Grande") %>%
  pivot_longer(cols = c( "SG_RV_depth"),
               names_to = "Measure",
               values_to = "Value") %>%
  select("nsub", "block_1", "Measure", "Value") %>%
  rename("Subject" = "nsub",
         "Condition" = "block_1",
         "Dimension" = "Measure")

tabla.exp_1.cve_width <- tabla.exp_1 %>%
  filter(block_1 == "Sala Grande") %>%
  pivot_longer(cols = c("SG_RV_width"),
               names_to = "Measure",
               values_to = "Value") %>%
  select("nsub", "block_1", "Measure", "Value") %>%
  rename("Subject" = "nsub",
         "Condition" = "block_1",
         "Dimension" = "Measure")

tabla.exp_1.cve_height <- tabla.exp_1 %>%
  filter(block_1 == "Sala Grande") %>%
  pivot_longer(cols = c("SG_RV_height"),
               names_to = "Measure",
               values_to = "Value") %>%
  select("nsub", "block_1", "Measure", "Value") %>%
  rename("Subject" = "nsub",
         "Condition" = "block_1",
         "Dimension" = "Measure")

tabla.exp_1_cve.all <- rbind(tabla.exp_1.cve_depth, tabla.exp_1.cve_width, tabla.exp_1.cve_height)

tabla.exp_1_cve.all <- tabla.exp_1_cve.all %>%
  arrange(Subject)

tabla.exp_1.sve_depth <- tabla.exp_1 %>%
  filter(block_1 == "Sala Chica") %>%
  pivot_longer(cols = c("SC_RV_depth"),
               names_to = "Measure",
               values_to = "Value") %>%
  select("nsub", "block_1", "Measure", "Value") %>%
  rename("Subject" = "nsub",
         "Condition" = "block_1",
         "Dimension" = "Measure")

tabla.exp_1.sve_width <- tabla.exp_1 %>%
  filter(block_1 == "Sala Chica") %>%
  pivot_longer(cols = c("SC_RV_width"),
               names_to = "Measure",
               values_to = "Value") %>%
  select("nsub", "block_1", "Measure", "Value") %>%
  rename("Subject" = "nsub",
         "Condition" = "block_1",
         "Dimension" = "Measure")

tabla.exp_1.sve_height <- tabla.exp_1 %>%
  filter(block_1 == "Sala Chica") %>%
  pivot_longer(cols = c("SC_RV_height"),
               names_to = "Measure",
               values_to = "Value") %>%
  select("nsub", "block_1", "Measure", "Value") %>%
  rename("Subject" = "nsub",
         "Condition" = "block_1",
         "Dimension" = "Measure")

tabla.exp_1_sve.all <- rbind(tabla.exp_1.sve_depth, tabla.exp_1.sve_width, tabla.exp_1.sve_height)

tabla.exp_1_sve.all <- tabla.exp_1_sve.all %>%
  arrange(Subject)

tabla.exp_1_vdp <- rbind(tabla.exp_1_cve.all, tabla.exp_1_sve.all)

tabla.exp_1_vdp <- tabla.exp_1_vdp %>%
  mutate(
    Condition = case_when(
      Condition == "Sala Grande" ~  "Congruent VE",
      Condition == "Sala Grande" ~  "Congruent VE",
      Condition == "Sala Grande" ~ "Congruent VE",
      Condition == "Sala Chica" ~  "Small VE",
      Condition == "Sala Chica" ~  "Small VE",
      Condition == "Sala Chica" ~ "Small VE",
    )) %>%
  mutate(
    Dimension = case_when(
      Dimension == "SG_RV_depth" ~ "Depth",
      Dimension == "SG_RV_width" ~ "Width",
      Dimension == "SG_RV_height" ~ "Height",
      Dimension == "SC_RV_depth" ~ "Depth",
      Dimension == "SC_RV_width" ~ "Width",
      Dimension == "SC_RV_height" ~ "Height",
    ))  %>%
  select(c("Subject","Condition", "Dimension", "Value")) %>%
  group_by(Condition) %>%
  arrange(c("Subject")) %>%
  mutate(
    Experiment= 1
  )


# exp 2 -------------------------------------------------------------------

tabla.exp_2 <- read.csv(file ="./Experiment_2/Exp_2_VDP/data/visual_exp_2.csv", sep =' ')

tabla.exp_2.depth <- tabla.exp_2 %>%
  pivot_longer(cols = c("No_Visual_depth", "Visual_depth"),
               names_to = "Measure",
               values_to = "Value") %>%
  select(c("Subject", "Measure", "Value"))

tabla.exp_2.width <- tabla.exp_2 %>%
  pivot_longer(cols = c("No_Visual_width", "Visual_width"),
               names_to = "Measure",
               values_to = "Value")  %>%
  select(c("Subject", "Measure", "Value"))

tabla.exp_2.height <- tabla.exp_2 %>%
  pivot_longer(cols = c("No_Visual_height", "Visual_height"),
               names_to = "Measure",
               values_to = "Value")  %>%
  select(c("Subject", "Measure", "Value"))

tabla.exp_2.all <- rbind(tabla.exp_2.depth, tabla.exp_2.width, tabla.exp_2.height)

tabla.exp_2.all <- tabla.exp_2.all %>%
  mutate(
    Condition = case_when(
      Measure == "No_Visual_depth" ~ "No visual info",
      Measure == "No_Visual_width" ~ "No visual info",
      Measure == "No_Visual_height" ~ "No visual info",
      Measure == "Visual_depth" ~ "Visual info",
      Measure == "Visual_width" ~ "Visual info",
      Measure == "Visual_height" ~ "Visual info",
    )) %>%
  mutate(
    Dimension = case_when(
      Measure == "No_Visual_depth" ~ "Depth",
      Measure == "No_Visual_width" ~ "Width",
      Measure == "No_Visual_height" ~ "Height",
      Measure == "Visual_depth" ~ "Depth",
      Measure == "Visual_width" ~ "Width",
      Measure == "Visual_height" ~ "Height",
  ))  %>%
  select(c("Subject","Condition", "Dimension", "Value")) %>%
  group_by(Condition) %>%
  arrange(c("Subject")) %>%
  mutate( Experiment = 2)




# all

df_vdp <- rbind(tabla.exp_1_vdp, tabla.exp_2.all)
