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
  pivot_longer(cols = c( "SG_RV_depth"),
               names_to = "Measure",
               values_to = "Value") %>%
  mutate(Block = case_when(
    block_1 == "Sala Grande" ~ 1,
    block_1 != "Sala Grande" ~ 2,
  )) %>%
  select("nsub", "Measure", "Value", "Block") %>%
  mutate(Condition = "Sala Grande") %>%
  rename("Subject" = "nsub",
         "Dimension" = "Measure")

tabla.exp_1.cve_width <- tabla.exp_1 %>%
  pivot_longer(cols = c("SG_RV_width"),
               names_to = "Measure",
               values_to = "Value") %>%
  mutate(Block = case_when(
    block_1 == "Sala Grande" ~ 1,
    block_1 != "Sala Grande" ~ 2)) %>%
  select("nsub", "Measure", "Value", "Block") %>%
  mutate(Condition = "Sala Grande") %>%
  rename("Subject" = "nsub",
         "Dimension" = "Measure")

tabla.exp_1.cve_height <- tabla.exp_1 %>%
  pivot_longer(cols = c("SG_RV_height"),
               names_to = "Measure",
               values_to = "Value") %>%
  mutate(Block = case_when(
    block_1 == "Sala Grande" ~ 1,
    block_1 != "Sala Grande" ~ 2)) %>%
  select("nsub", "Measure", "Value", "Block") %>%
  mutate(Condition = "Sala Grande") %>%
  rename("Subject" = "nsub",
         "Dimension" = "Measure")
  
tabla.exp_1_cve.all <- rbind(tabla.exp_1.cve_depth, tabla.exp_1.cve_width, tabla.exp_1.cve_height)

tabla.exp_1_cve.all <- tabla.exp_1_cve.all %>%
  arrange(Subject)

tabla.exp_1.sve_depth <- tabla.exp_1 %>%
  pivot_longer(cols = c("SC_RV_depth"),
               names_to = "Measure",
               values_to = "Value") %>%
  mutate(Block = case_when(
    block_1 == "Sala Chica" ~ 1,
    block_1 != "Sala Chica" ~ 2)) %>%
  select("nsub", "Measure", "Value", "Block") %>%
  mutate(Condition = "Sala Chica") %>%
  rename("Subject" = "nsub",
         "Dimension" = "Measure")


tabla.exp_1.sve_width <- tabla.exp_1 %>%
  pivot_longer(cols = c("SC_RV_width"),
               names_to = "Measure",
               values_to = "Value") %>%
  mutate(Block = case_when(
    block_1 == "Sala Chica" ~ 1,
    block_1 != "Sala Chica" ~ 2)) %>%
  select("nsub", "Measure", "Value", "Block") %>%
  mutate(Condition = "Sala Chica") %>%
  rename("Subject" = "nsub",
         "Dimension" = "Measure")

tabla.exp_1.sve_height <- tabla.exp_1 %>%
  pivot_longer(cols = c("SC_RV_height"),
               names_to = "Measure",
               values_to = "Value") %>%
  mutate(Block = case_when(
    block_1 == "Sala Chica" ~ 1,
    block_1 != "Sala Chica" ~ 2)) %>%
  select("nsub", "Measure", "Value", "Block") %>%
  mutate(Condition = "Sala Chica") %>%
  rename("Subject" = "nsub",
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
  select(c("Subject","Condition", "Dimension", "Value", "Block")) %>%
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
  select(c("Subject", "Measure", "Value")) %>%
  mutate(Block = case_when(
    Measure == "No_Visual_depth" ~ 1,
    Measure == "Visual_depth" ~ 2
  ))

tabla.exp_2.width <- tabla.exp_2 %>%
  pivot_longer(cols = c("No_Visual_width", "Visual_width"),
               names_to = "Measure",
               values_to = "Value")  %>%
  select(c("Subject", "Measure", "Value")) %>%
  mutate(Block = case_when(
    Measure == "No_Visual_width" ~ 1,
    Measure == "Visual_width" ~ 2
  ))

tabla.exp_2.height <- tabla.exp_2 %>%
  pivot_longer(cols = c("No_Visual_height", "Visual_height"),
               names_to = "Measure",
               values_to = "Value")  %>%
  select(c("Subject", "Measure", "Value")) %>%
  mutate(Block = case_when(
    Measure == "No_Visual_height" ~ 1,
    Measure == "Visual_height" ~ 2
  ))

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
  select(c("Subject","Condition", "Dimension", "Value", "Block")) %>%
  group_by(Condition) %>%
  arrange(c("Subject")) %>%
  mutate( Experiment = 2)

# exp 3 -------------------------------------------------------------------

tabla.exp_3 <- read.csv(file ="./Experiment_3/Exp_2_3_VDP/data/visual_exp_3.csv", sep =' ')

tabla.exp_3.depth <- tabla.exp_3 %>%
  pivot_longer(cols = c("No_visual_info_depth", "VE_depth"),
               names_to = "Measure",
               values_to = "Value") %>%
  select(c("Subject", "Measure", "Value"))  %>%
  mutate(Block = case_when(
    Measure == "No_visual_info_depth" ~ 1,
    Measure == "VE_depth" ~ 2
  ))

tabla.exp_3.width <- tabla.exp_3 %>%
  pivot_longer(cols = c("No_visual_info_width", "VE_width"),
               names_to = "Measure",
               values_to = "Value")  %>%
  select(c("Subject", "Measure", "Value")) %>%
  mutate(Block = case_when(
    Measure == "No_visual_info_width" ~ 1,
    Measure == "VE_width" ~ 2
  ))

tabla.exp_3.height <- tabla.exp_3 %>%
  pivot_longer(cols = c("No_visual_info_height", "VE_height"),
               names_to = "Measure",
               values_to = "Value")  %>%
  select(c("Subject", "Measure", "Value")) %>%
  mutate(Block = case_when(
    Measure == "No_visual_info_height" ~ 1,
    Measure == "VE_height" ~ 2
  ))

tabla.exp_3.all <- rbind(tabla.exp_3.depth, tabla.exp_3.width, tabla.exp_3.height)

tabla.exp_3.all <- tabla.exp_3.all %>%
  mutate(
    Condition = case_when(
      Measure == "No_visual_info_depth" ~ "No visual info",
      Measure == "No_visual_info_width" ~ "No visual info",
      Measure == "No_visual_info_height" ~ "No visual info",
      Measure == "VE_depth" ~  "Congruent VE",
      Measure == "VE_width" ~  "Congruent VE",
      Measure == "VE_height" ~ "Congruent VE",
    )) %>%
  mutate(
    Dimension = case_when(
      Measure == "No_visual_info_depth" ~ "Depth",
      Measure == "No_visual_info_width" ~ "Width",
      Measure == "No_visual_info_height" ~ "Height",
      Measure == "VE_depth" ~ "Depth",
      Measure == "VE_width" ~ "Width",
      Measure == "VE_height" ~ "Height",
    ))  %>%
  select(c("Subject","Condition", "Dimension", "Value", "Block")) %>%
  group_by(Condition) %>%
  arrange(c("Subject")) %>%
  mutate( Experiment = 3)


# exp 4 -------------------------------------------------------------------

tabla.exp_4 <- read.csv(file ="./Experiment_4/Exp_4_VDP/data/visual_exp_4_con_outliers.csv", sep =' ')

tabla.exp_4.depth <- tabla.exp_4 %>%
  pivot_longer(cols = c("No_visual_info_depth", "Virtual_environment_depth"),
               names_to = "Measure",
               values_to = "Value") %>%
  select(c("Subject", "Measure", "Value")) %>%
  mutate(Block = case_when(
    Measure == "No_visual_info_depth" ~ 1,
    Measure == "Virtual_environment_depth" ~ 2
  ))

tabla.exp_4.width <- tabla.exp_4 %>%
  pivot_longer(cols = c("No_visual_info_width", "Virtual_environment_width"),
               names_to = "Measure",
               values_to = "Value")  %>%
  select(c("Subject", "Measure", "Value"))  %>%
  mutate(Block = case_when(
    Measure == "No_visual_info_width" ~ 1,
    Measure == "Virtual_environment_width" ~ 2
  ))

tabla.exp_4.height <- tabla.exp_4 %>%
  pivot_longer(cols = c("No_visual_info_height", "Virtual_environment_height"),
               names_to = "Measure",
               values_to = "Value")  %>%
  select(c("Subject", "Measure", "Value")) %>%
  mutate(Block = case_when(
    Measure == "No_visual_info_height" ~ 1,
    Measure == "Virtual_environment_height" ~ 2
  ))


tabla.exp_4.all <- rbind(tabla.exp_4.depth, tabla.exp_4.width, tabla.exp_4.height)

tabla.exp_4.all <- tabla.exp_4.all %>%
  mutate(
    Condition = case_when(
      Measure == "No_visual_info_depth" ~ "No visual info",
      Measure == "No_visual_info_width" ~ "No visual info",
      Measure == "No_visual_info_height" ~ "No visual info",
      Measure == "Virtual_environment_depth" ~  "Large VE",
      Measure == "Virtual_environment_width" ~  "Large VE",
      Measure == "Virtual_environment_height" ~ "Large VE",
    )) %>%
  mutate(
    Dimension = case_when(
      Measure == "No_visual_info_depth" ~ "Depth",
      Measure == "No_visual_info_width" ~ "Width",
      Measure == "No_visual_info_height" ~ "Height",
      Measure == "Virtual_environment_depth" ~ "Depth",
      Measure == "Virtual_environment_width" ~ "Width",
      Measure == "Virtual_environment_height" ~ "Height",
    ))  %>%
  select(c("Subject","Condition", "Dimension", "Value", "Block")) %>%
  group_by(Condition) %>%
  arrange(c("Subject")) %>%
  mutate( Experiment = 4)


# outliers all ----------------------------------------------------------------

df_vdp <- rbind(tabla.exp_1_vdp, tabla.exp_2.all, tabla.exp_3.all,tabla.exp_4.all)

df_vdp <- df_vdp %>%
  filter(!(Dimension == "Depth" & Value >= 20))

df_vdp <- df_vdp %>%
  filter(!(Dimension == "Width" & Value >= 10))

df_vdp <- df_vdp %>%
  filter(!(Dimension == "Height" & Value >= 6))


df_vdp <- df_vdp %>%
  rename(
    "subject" = "Subject",
    "room_condition" = "Condition",
    "dimension" = "Dimension",
    "value" = "Value",
    "experiment" = "Experiment",
    "block" = "Block"
  )
# save all ---------------------------------------------------------------


write.csv(df_vdp, file = "./final/data/VDP.csv", row.names = FALSE)
