library(dplyr)

# Make Format -------------------------------------------------------------


tabla.raw <- read.csv('Exp_4_ADP_vr/data/1_16_NVI_LVE_sin_outliers.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)

tabla.raw$abs_bias <-  abs(tabla.raw$respuesta - tabla.raw$distancia)

# signed bias
tabla.raw$signed_bias <- (tabla.raw$respuesta - tabla.raw$distancia) / tabla.raw$distancia
# unsigen bias
tabla.raw$unsigned_bias <- abs(tabla.raw$signed_bias)

tabla.raw = tabla.raw %>% rename(subject = nsub)   # renombro columnas
tabla.raw = tabla.raw %>% rename(percived_distance = respuesta)   # renombro columnas
tabla.raw = tabla.raw %>% rename(target_distance = distancia)   # renombro columnas
tabla.raw = tabla.raw %>% rename(room_condition = condicion_sala)   # renombro columnas
tabla.raw = tabla.raw %>% rename(block = nbloque)   # renombro columnas

f_promedio <- function(x) c(mean = mean(x),
                            sd   = sd(x),
                            var  = var(x),
                            sem  = sd(x)/sqrt(length(x)),
                            n    = length(x))

results_tbl <- tibble(aggregate(cbind(percived_distance,signed_bias,unsigned_bias,abs_bias) ~ subject*room_condition*target_distance*block,
                                data = tabla.raw,
                                FUN  = f_promedio,na.action = NULL))

results_tbl <- results_tbl %>%
  mutate(
    room_condition = case_when(
      room_condition == "No Visual Info" ~ "No visual information",
      room_condition == "Larger VE" ~ "Larger VE"
    )
  )

results_tbl %>%
  # clean_names() %>%
  mutate(subject = factor(subject),
         room_condition = factor(room_condition),
         block = factor(block),
         
         perc_dist_sd = percived_distance[,"sd"],
         perc_dist_sem = percived_distance[,"sem"],
         perc_dist_var = percived_distance[,"var"],
         perc_dist_n = percived_distance[,"n"],
         perc_dist = percived_distance[,"mean"],
         
         rel_bias_signed_sd = signed_bias[,"sd"],
         rel_bias_signed_sem = signed_bias[,"sem"],
         rel_bias_signed_var = signed_bias[,"var"],
         rel_bias_signed_n = signed_bias[,"n"],
         rel_bias_signed = signed_bias[,"mean"],
         
         rel_bias_unsigned_sd = unsigned_bias[,"sd"],
         rel_bias_unsigned_sem = unsigned_bias[,"sem"],
         rel_bias_unsigned_var = unsigned_bias[,"var"],
         rel_bias_unsigned_n = unsigned_bias[,"n"],
         rel_bias_unsigned = unsigned_bias[,"mean"],
         
         abs_bias_sd = abs_bias[,"sd"],
         abs_bias_sem = abs_bias[,"sem"],
         abs_bias_var = abs_bias[,"var"],
         abs_bias_n = abs_bias[,"n"],
         abs_bias = abs_bias[,"mean"]) %>%
  
  select(-c(percived_distance,signed_bias,unsigned_bias,abs_bias)) %>%
  write.csv("./Exp_4_ADP_vr/ResultsData/DresultsExp4.csv", row.names = FALSE)


#write.table(all_data, file="Exp_4_ADP_vr/data/1_16_NVI_LVE.csv", row.names = FALSE)  
