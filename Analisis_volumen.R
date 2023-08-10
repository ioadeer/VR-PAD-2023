# Analisis de datos del experi 32 sujetos
# Analisis de volumen de sala percibida


library(reshape2)

# Load and join data  -----------------------------------------------------

tabla_volumen_1_11 <- read.csv("./tamanio-visual-de-sala/data/data_1_11_s.csv", header = TRUE, sep = ' ', stringsAsFactors = TRUE)
tabla_volumen_12_32 <- read.csv("./tamanio-visual-de-sala/data/data_12_32_s.csv", header = TRUE, sep = ' ', stringsAsFactors = TRUE)

tabla.volumen <- do.call("rbind", list(tabla_volumen_1_11, tabla_volumen_12_32))

tabla.raw = read.csv("./analisis-pad-main/data/data-1-32-bloque-1-sin-outliers.csv", header = TRUE, sep = ' ', stringsAsFactors = TRUE)

colnames(tabla.volumen)

# VIRTUAL
tabla.volumen <- tabla.volumen %>%
  mutate( # vol virtual
    SV_volumen = case_when(
      block_1 == "Sala Grande" ~ SG_RV_volumen,
      block_1 == "Sala Chica" ~ SC_RV_volumen,
    ) 
  ) %>%
  mutate( # depth virtual
    SV_depth = case_when(
      block_1 == "Sala Grande" ~ SG_RV_depth,
      block_1 == "Sala Chica" ~ SC_RV_depth,
    )
  ) %>%
  mutate(  # wide virtual
    SV_width = case_when(
      block_1 == "Sala Grande" ~ SG_RV_width,
      block_1 == "Sala Chica" ~ SC_RV_width,
    )
  ) %>%
    mutate(   # height virtual
      SV_height = case_when(
      block_1 == "Sala Grande" ~ SG_RV_height,
      block_1 == "Sala Chica" ~ SC_RV_height,
    )
  ) 

# REAL


colnames(tabla.volumen)

tabla.volumen <- select(tabla.volumen, c(nsub, 
                                         block_1, 
                                         SR_volumen, 
                                         SR_width, 
                                         SR_depth,
                                         SR_height,
                                         SV_volumen,
                                         SV_depth,
                                         SV_width,
                                         SV_height
                                         ))

# Sacar distancia maxima reportada

tabla.raw <- read.csv('./analisis-pad-main/data/data-1-32-bloque-1-sin-outliers.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)

tabla.dist_max <- tabla.raw %>%
  group_by(nsub) %>%
  summarise(distanca_max = max(respuesta))

tabla.volumen_max_dist = merge(x =tabla.volumen, y = tabla.dist_max, by= "nsub")

write.table(tabla.volumen_max_dist, file="./analisis-pad-main/data/volumen_max_dist_1_32.csv", row.names = FALSE)


# Data volumen visual max distancia reportada -----------------------------

tabla.volumen_max_dist <- read.csv("./analisis-pad-main/data/volumen_max_dist_1_32.csv", header = TRUE, sep = ' ', stringsAsFactors = TRUE)



# boxplots ----------------------------------------------------------------


# Por condicion aca estan los boxplots de volumen sin sacar outliers

# Sala grande

tabla_new_sala_grande <- tabla.volumen_max_dist %>%
  filter(block_1 == 'Sala Grande')

tabla_new_sala_grande <- melt(tabla_new_sala_grande, id.vars='nsub',
                             measure.vars=c("SR_volumen", "SV_volumen"))

tabla_new_sala_grande <- tabla_new_sala_grande %>%
  mutate(variable, 
      variable =case_when(
      variable == "SR_volumen" ~ "Sala Real" ,
      variable == "SV_volumen" ~ "Sala Virtual",
    )
  ) 

box_plot_sala_grande <- ggplot(tabla_new_sala_grande) +
  geom_boxplot(aes(x=variable, y=value, color=variable)) +
  ggtitle("Volumen real y virtual para Sala Grande") +
  xlab("Condicion de sala")

plot(box_plot_sala_grande)

# Sala chica

tabla_new_sala_chica <- tabla.volumen_max_dist %>%
  filter(block_1 == 'Sala Chica')

tabla_new_sala_chica <- melt(tabla_new_sala_chica, id.vars='nsub',
                              measure.vars=c("SR_volumen", "SV_volumen"))

tabla_new_sala_chica <- tabla_new_sala_chica %>%
  mutate(variable, 
         variable =case_when(
           variable == "SR_volumen" ~ "Sala Real" ,
           variable == "SV_volumen" ~ "Sala Virtual",
         )
  ) 

box_plot_sala_chica <- ggplot(tabla_new_sala_chica) +
  geom_boxplot(aes(x=variable, y=value, color=variable)) +
  ggtitle("Volumen real y virtual para Sala chica") +
  xlab("Condicion de sala")

plot(box_plot_sala_chica)
