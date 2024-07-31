

# load data ---------------------------------------------------------------

tabla.raw <- read.csv('./analisis-pad-2-salas-vacias/data/data-1-50-bloque-1-sin-outliers.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)

tabla.raw$SesgoAbs <-  tabla.raw$respuesta - tabla.raw$distancia
tabla.raw$SesgoRel <- (tabla.raw$respuesta - tabla.raw$distancia) / tabla.raw$distancia

f_promedio <- function(x) c(mean = mean(x),
                            sd   = sd(x),
                            var  = var(x),
                            sem  = sd(x)/sqrt(length(x)),
                            n    = length(x))

tabla.ind <- tibble(aggregate(cbind(respuesta,SesgoRel) ~ nsub*condicion_sala*distancia*nbloque,
                              data = tabla.raw,
                              FUN  = f_promedio,na.action = NULL))

# - Nivel poblacional

tabla.pob <- tibble(aggregate(cbind(respuesta[,"mean"],SesgoRel[,"mean"]) ~ condicion_sala*distancia,
                              data <- tabla.ind,
                              FUN  <- f_promedio,na.action = NULL))


tabla.pob = tabla.pob %>% rename(respuestapob = V1)
tabla.pob = tabla.pob %>% rename(sesgorelpob = V2)



# primero ajustar modelo lineal -------------------------------------------

tabla.chica.ind <- tabla.ind %>%
  filter(condicion_sala =="SALA_CHICA")

tabla.chica.pob <- tabla.pob %>%
  filter(condicion_sala == "SALA_CHICA")

tabla.chica.pob <- tabla.chica.pob %>%
  mutate(
    log_respuesta = log(respuestapob[,"mean"]),
    log_distancia = log(distancia)
  )

tabla.chica.ind <- tabla.chica.ind %>%
  mutate(
    log_respuesta = log(respuesta[,"mean"]),
    log_distancia = log(distancia)
  )

m.distancia.sala_chica <- lm(log_respuesta ~ log_distancia, 
                    data = tabla.chica.pob)


tabla.chica.pob <- tabla.chica.pob %>%
  mutate(
    predi = predict(m.distancia.sala_chica),
  )

library(scales) 
# http://www.sthda.com/english/wiki/ggplot2-axis-scales-and-transformations
# en log log
g1 <- ggplot() +
  #geom_line(data= tabla.ind, aes(x = log(distancia), y = respuesta[,"mean"], group = nsub, col=condicion_sala), alpha = 0.2)+
  geom_line(data= tabla.chica.pob, aes(x = log_distancia, y = predi), alpha = 0.9)+
  geom_point(data = tabla.chica.pob, mapping = aes(x=log_distancia, y=log_respuesta))+
  geom_point(data = tabla.chica.ind, mapping = aes(x=log_distancia, y=respuesta[,"mean"]), 
             color='blue', alpha=.8, shape=1, size=2.2, stroke=.2) + 
  #geom_point(data = tabla.chica.ind, mapping = aes(x=log_distancia, y=log_respuesta), 
  #           color='blue', alpha=.8, shape=1, size=2.2, stroke=.2) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  xlab("Distancia real (log)")+
  ylab("Distancia percibida (log)") +
  ggtitle("Sala chica ajuste con funcion de potencia (log log)") +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  annotate("text",x = 1, y = 10, label= eqn, parse = TRUE) +
  theme_linedraw(base_size = 9)

plot(g1)

### IMPORTANTE DESDE ACA
# ref https://r-graphics.org/recipe-scatter-fitlines-text
## FUNCION PARA adjuntar r2, intercept and slope
# voya llamar al slope a. es el exponente de funcion de potencia
# voy a llamar a intercept k, o las constante que multiplica
# funcion para formatear modelo
# original
# eqn <- sprintf(
#   "italic(y) == %.3g + %.3g * italic(x) * ',' ~~ italic(r)^2 ~ '=' ~ %.2g",
#   coef(model)[1],
#   coef(model)[2],
#   summary(model)$r.squared
# )

eqn <- sprintf(
  "italic(k) == %.3g * ',' ~~ italic(a) == %.3g  * ',' ~~ italic(r)^2 ~ '=' ~ %.2g",
  coef(m.distancia.sala_chica)[1],
  coef(m.distancia.sala_chica)[2],
  summary(m.distancia.sala_chica)$r.squared
)


g1 <- ggplot() +
  geom_line(data= tabla.chica.pob, aes(x = distancia, y = exp(predi)))+
  geom_point(data = tabla.chica.pob, mapping = aes(x=distancia, y=respuestapob[,"mean"]))+
  geom_point(data = tabla.chica.ind, mapping = aes(x=distancia, y=respuesta[,"mean"]), 
             color='blue', alpha=.8, shape=1, size=2.2, stroke=.2) + 
  scale_x_continuous(name="Distance source (m)", breaks=c(0,2,2.7,3.65,4.9,6.65,9.0), labels=c(0,2,2.7,3.65,4.9,6.65,9.0), minor_breaks=NULL) +
  scale_y_continuous(name="Perceived distance (m)",  breaks=c(0,2,2.7,3.65,4.9,6.65,9.0), labels=c(0,2,2.7,3.65,4.9,6.65,9.0), minor_breaks=NULL) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  ggtitle("Sala chica ajuste con funcion de potencia") +
  annotate("text",x = 4, y = 10, label= eqn, parse = TRUE) +
  theme_linedraw(base_size = 9)

plot(g1)

figures_folder = './analisis-pad-2-salas-vacias/figuras_latest_modelos'
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "sala_chica_lin_lin.png", sep = '')
#ggsave(mi_nombre_de_archivo, plot = g1, width=44, height=14, units="cm", limitsize=FALSE, dpi=200)
ggsave(mi_nombre_de_archivo, plot =g1, limitsize=FALSE, dpi=200)

ggsave('./test.png', plot=g1)


