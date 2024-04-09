# Vamos a sacar coeficientes a y k por sujeto y hacer t test
# q hacer
# 
#Auditory/visual distance estimation: accuracy and variability. 
#Anderson y Zahorik tienen la posta de que analisis hacer.
# t test de a y k entre condiciones
# histogramas de a y  k por condiciones
# The distributions of R2 values across all participants ( histogramas)
# Log-transformed residuals from the power function fit for a single representative participant



# cargo datos -------------------------------------------------------------

tabla.raw <- read.csv('analisis-pad-2-salas-vacias/data/data-1-50-bloque-1-sin-outliers.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)

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

# Intento con broom    -----------  
# https://cran.r-project.org/web/packages/broom/vignettes/broom.html
#  https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html


tabla.ind <- tabla.ind %>%
  mutate(
    log_distancia = log(distancia),
    log_respuesta = log(respuesta[,"mean"]),
  )

modelo_sujeto  <- function(df) {
  lm(log_respuesta ~ log_distancia, data = df)
}

regressions <- tabla.ind %>%
  nest(data = -nsub) %>%
  mutate(
    fit = map(data,modelo_sujeto),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

tidied <- regressions %>%
  unnest(tidied)

regressions %>%
  unnest(glanced)

regressions %>%
  unnest(augmented)

#

coefs <- regressions %>%
  unnest(tidied) %>%
  group_by(nsub) %>%
  spread(term,estimate) %>%
  select(nsub,"(Intercept)", log_distancia) %>%
  rename(intercept = "(Intercept)") %>%
  group_by(nsub) %>%
  summarise(
    intercept = max(intercept, na.rm =T),
    coef = max(log_distancia, na.rm =T),
  )

cond <- data.frame(tabla.ind$nsub, tabla.ind$condicion_sala)

cond <- cond %>%
  rename(nsub = tabla.ind.nsub) %>%
  rename(condicion_sala = tabla.ind.condicion_sala)

cond <- cond %>%
  group_by(nsub)  %>%
  distinct()

coefs <- merge(x=coefs, y=cond, by='nsub')

write.table(coefs, file="analisis-pad-2-salas-vacias/data/coeficientes_por_sujeto.csv", row.names = FALSE)



# T-test de coeficientes --------------------------------------------------


data.clean <-  read.csv('analisis-pad-2-salas-vacias/data/coeficientes_por_sujeto.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)

p <- ggboxplot(data.clean, x = "condicion_sala", y = "coef",
               color = "condicion_sala", palette = "jco",
               add = "jitter", ylab="coef a (non-linear compresison)",
               title="T-test de coeficientes")
#  Add p-value
p + stat_compare_means()
# Change method
p + stat_compare_means(method = "t.test")


# k linear compression
p2 <- ggboxplot(data.clean, x = "condicion_sala", y = "intercept",
                color = "condicion_sala", palette = "jco",
                add = "jitter", ylab="coef k (linear compresison)",
                title="T-test de coeficiente k")
#  Add p-value
p2 + stat_compare_means()
# Change method
p2 + stat_compare_means(method = "t.test")
