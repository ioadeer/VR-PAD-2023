# Aca voy a realizar extraccion y comparacion para coeficientes
# a k y R^2 por sujeto.
# Luego hacer figuras con histograma de cada uno de los valores.
# Analisis t-test entre condiciones y ver si es necesaria la correccion
# de bonferroni dado que tenemos solo 2 grupos

# Dependencias ------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(quickpsy)
library(tidyverse)
library(lme4)
library(nlme)
library(lmerTest)
library(broom)
library(broom.mixed)
library(ggpubr)
library(Routliers)
library(ggbeeswarm)
library(ggthemes)
library(ggstatsplot)
library(gmodels)
library(pracma)
library(Routliers)
library(rstatix)




# load data ---------------------------------------------------------------

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

# Extraccion de coeficientes con broom  -----------  
# https://cran.r-project.org/web/packages/broom/vignettes/broom.html
#  https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html

tabla.ind <- tabla.ind %>%
  mutate(
    log_distancia = log(distancia),
    log_respuesta = log(respuesta[,"mean"]),
  )

modelo_sujeto  <- function(df) {
  lmer(log_respuesta ~ log_distancia + (1|nsub) , data = df)
}

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
# CORRECCION Guardarse el R^2 tambien

r_sqrd <- regressions %>%
  unnest(glanced) %>%
  group_by(nsub) %>%
  select(nsub, r.squared)

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

coefs <- merge(x=coefs, y=r_sqrd, by='nsub')

write.table(coefs, file="analisis-pad-2-salas-vacias/data/coeficientes_por_sujeto.csv", row.names = FALSE)



# T-test y boxplots  --------------------------------------------------
# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/

data.clean <-  read.csv('analisis-pad-2-salas-vacias/data/coeficientes_por_sujeto.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)

# coef a non-linear compression
p <- ggboxplot(data.clean, x = "condicion_sala", y = "coef",
               color = "condicion_sala", palette = "jco",
               add = "jitter", ylab="coef a (non-linear compression)",
               title="T-test de coeficientes")
#  Add p-value
p + stat_compare_means()
# Change method
p + stat_compare_means(method = "t.test")


# k linear compression
p2 <- ggboxplot(data.clean, x = "condicion_sala", y = "intercept",
                color = "condicion_sala", palette = "jco",
                add = "jitter", ylab="coef k (linear compression)",
                title="T-test de coeficiente k")
#  Add p-value
p2 + stat_compare_means()
# Change method
p2 + stat_compare_means(method = "t.test")

# r^2 fitness of model

p3 <- ggboxplot(data.clean, x = "condicion_sala", y = "r.squared",
                color = "condicion_sala", palette = "jco",
                add = "jitter", ylab="R^2 (fitness of model)",
                title="T-test de R^2")
#  Add p-value
#p3 + stat_compare_means(aes(label = ..p.signif..))
p3 + stat_compare_means(aes(label = "p.format"))
# Change method
p3 + stat_compare_means(method = "t.test")

t.test(r.squared~condicion_sala, data = data.clean)

# histogramas de a exponente -------------------------------------------------------------

# histogram de a (exponente)


# IQR(data.clean$coef)
# 
# quantile(data.clean$coef) # 75 % - 25 % = IQR
# 
# quantile(data.clean$coef)[2]
# quantile(data.clean$coef)[4]

#   "italic(k) == %.3g * ',' ~~ italic(a) == %.3g  * ',' ~~ italic(r)^2 ~ '=' ~ %.2g",


# sala chica

data.clean.sala_chica <- data.clean %>%
  filter(condicion_sala == "SALA_CHICA")

eqn1 <- sprintf(
  "M = %.3g ± %.2g \nMdn = %.3g (%.2g - %.2g)\nN =  %.2g",
  mean(data.clean.sala_chica$coef),
  sd(data.clean.sala_chica$coef),
  median(data.clean.sala_chica$coef),
  quantile(data.clean.sala_chica$coef)[4],
  quantile(data.clean.sala_chica$coef)[2],
  sum(data.clean.sala_chica$coef)
)

hist_a_sc <- ggplot(data.clean.sala_chica, aes(coef)) +
  geom_histogram(binwidth= 0.1, color="black",fill="white") +
  scale_x_continuous(breaks= seq(0.3,2.1, by=0.5)) +
  scale_y_continuous(breaks= seq(0,5, by = 1))+
  xlim(0.3,2.1) +
  ylim(0,5) +
  geom_vline(xintercept = round(mean(data.clean.sala_chica$coef),2),        # Add line for mean
             linetype="dashed",
             ) +
  annotate("text",                        # Add text for mean
           #x = 1.5, # para fig sola
           x = 1.4, # para fig compuesta
           y = 4.0,
           label = eqn1,
           size = 1.5,
           hjust = 0) +
  labs(x="a", y = "Count")+
  theme(
    legend.position="none",
    #axis.line = element_blank(),
    axis.title.x = element_blank(),
    #axis.ticks = element_blank(),
    axis.title.y = element_blank()) +
  theme_minimal()

hist_a_sc

# sala grande

data.clean.sala_grande <- data.clean %>%
  filter(condicion_sala == "SALA_GRANDE")

eqn1 <- sprintf(
  "M = %.3g ± %.2g \nMdn = %.3g (%.2g - %.2g)\nN =  %.2g",
  mean(data.clean.sala_grande$coef),
  sd(data.clean.sala_grande$coef),
  median(data.clean.sala_grande$coef),
  quantile(data.clean.sala_grande$coef)[4],
  quantile(data.clean.sala_grande$coef)[2],
  sum(data.clean.sala_grande$coef)
)


# scale_x_continuous(breaks= seq(0.3,2.1, by=0.5)) +
#   scale_y_continuous(breaks= seq(0,5, by = 1))+
#   xlim(0.3,2) +
#   ylim(0,5) +

hist_a_sg <- ggplot(data.clean.sala_grande, aes(coef)) +
  geom_histogram(binwidth= 0.1, color="black",fill="white") +
  geom_vline(xintercept = round(mean(data.clean.sala_grande$coef),2),        # Add line for mean
             linetype="dashed",
  ) +
  scale_x_continuous(breaks= seq(0.3,2.1, by=0.5)) +
  scale_y_continuous(breaks= seq(0,5, by = 1))+
  xlim(0.3,2.1) +
  ylim(0,5) +
  annotate("text",                        # Add text for mean
           #x = 1.5, # para fig sola
           x = 1.3, # para fig compuesta
           y = 4.0,
           label = eqn1,
           size = 1.5,
           hjust = 0) +
  labs(x="a", y = "Count")+
  theme(
    legend.position="none",
    #axis.line = element_blank(),
    axis.title.x = element_blank(),
    #axis.ticks = element_blank(),
    axis.title.y = element_blank()) +
  theme_minimal()

hist_a_sg

a_comp <- ggarrange(
  hist_a_sc,
  hist_a_sg,
  nrow = 2
)

plot(a_comp)


figures_folder = "./analisis-pad-2-salas-vacias/imgs_analisis_4_coefs"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "2_histogramas_a.png", sep = '')
ggsave(mi_nombre_de_archivo, plot = a_comp, limitsize=FALSE, dpi=200)


# histograma a apaisado
data.clean.boxplot_a <- data.clean %>%
  mutate(
    condicion_sala = case_when(
      condicion_sala == "SALA_GRANDE" ~ "Grande",
      condicion_sala == "SALA_CHICA" ~ "Chica",
    )
  )

bxp <- ggboxplot(data.clean.boxplot_a, x = "condicion_sala", y = "coef",
                 orientation = "horizontal",
               color = "condicion_sala", palette = "jco",
               add = "jitter", ylab="coef a",
               title="T-test coeficientes a") +
  theme_minimal() +
  theme(
    legend.position="none",
    #axis.line = element_blank(),
    axis.title.x = element_blank(),
    #axis.title = element_text(hjust = 0),
    #axis.ticks = element_blank(),
    axis.title.y = element_blank())

  
stat.test <- data.clean.boxplot_a  %>% 
  t_test(coef~condicion_sala) %>%
  add_significance()


stat.test <- stat.test %>% add_y_position(fun ="mean", step.increase = 35)


bxp <- bxp + stat_pvalue_manual(
  stat.test, label = "p", tip.length = 0.01,
  coord.flip = TRUE
) +
  coord_flip()

bxp

all <- ggarrange(
  bxp,
  ggarrange(
    hist_a_sc, 
    hist_a_sg,
    nrow = 2
  ),
  ncol = 2
)

plot(all)

figures_folder = "./analisis-pad-2-salas-vacias/imgs_analisis_4_coefs"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "coef_a.png", sep = '')
ggsave(mi_nombre_de_archivo, plot = all, limitsize=FALSE, dpi=200)




# histogramas de k intercept -------------------------------------------------------------

# IQR(data.clean$intercept)
# 
# quantile(data.clean$intercept) # 75 % - 25 % = IQR
# 
# quantile(data.clean$intercept)[2]
# quantile(data.clean$intercept)[4]

#   "italic(k) == %.3g * ',' ~~ italic(a) == %.3g  * ',' ~~ italic(r)^2 ~ '=' ~ %.2g",


# sala chica

data.clean.sala_chica <- data.clean %>%
  filter(condicion_sala == "SALA_CHICA")

eqn1 <- sprintf(
  "M = %.3g ± %.2g \nMdn = %.3g (%.2g - %.2g)\nN =  %.2g",
  mean(data.clean.sala_chica$intercept),
  sd(data.clean.sala_chica$intercept),
  median(data.clean.sala_chica$intercept),
  quantile(data.clean.sala_chica$intercept)[4],
  quantile(data.clean.sala_chica$intercept)[2],
  sum(data.clean.sala_chica$intercept)
)

hist_a_sc <- ggplot(data.clean.sala_chica, aes(intercept)) +
  geom_histogram(binwidth= 0.5, color="black",fill="white") +
  scale_x_continuous(breaks= seq(-3,1.0, by=1.0)) +
  scale_y_continuous(breaks= seq(0,6, by = 1))+
  xlim(-3,1) +
  ylim(0,6) +
  geom_vline(xintercept = round(mean(data.clean.sala_chica$intercept),2),        # Add line for mean
             linetype="dashed",
  ) +
  annotate("text",                        # Add text for mean
           #x = 1.5, # para fig sola
           x = 0.0, # para fig compuesta
           y = 5,
           label = eqn1,
           size = 1.5,
           hjust = 0) +
  labs(x="k", y = "Count")+
  theme(
    legend.position="none",
    #axis.line = element_blank(),
    axis.title.x = element_blank(),
    #axis.ticks = element_blank(),
    axis.title.y = element_blank()) +
  theme_minimal()

hist_a_sc

# sala grande

data.clean.sala_grande <- data.clean %>%
  filter(condicion_sala == "SALA_GRANDE")

eqn1 <- sprintf(
  "M = %.3g ± %.2g \nMdn = %.3g (%.2g - %.2g)\nN =  %.2g",
  mean(data.clean.sala_grande$intercept),
  sd(data.clean.sala_grande$intercept),
  median(data.clean.sala_grande$intercept),
  quantile(data.clean.sala_grande$intercept)[4],
  quantile(data.clean.sala_grande$intercept)[2],
  sum(data.clean.sala_grande$intercept)
)


# scale_x_continuous(breaks= seq(0.3,2.1, by=0.5)) +
#   scale_y_continuous(breaks= seq(0,5, by = 1))+
#   xlim(0.3,2) +
#   ylim(0,5) +

hist_a_sg <- ggplot(data.clean.sala_grande, aes(intercept)) +
  geom_histogram(binwidth= 0.5, color="black",fill="white") +
  geom_vline(xintercept = round(mean(data.clean.sala_grande$intercept),2),        # Add line for mean
             linetype="dashed",
  ) +
  scale_x_continuous(breaks= seq(-3,1.0, by=1.0)) +
  scale_y_continuous(breaks= seq(0,6, by = 1))+
  xlim(-3,1) +
  ylim(0,6) +
  annotate("text",                        # Add text for mean
           #x = 1.5, # para fig sola
           x = 0.0, # para fig compuesta
           y = 5.0,
           label = eqn1,
           size = 1.5,
           hjust = 0) +
  labs(x="a", y = "Count")+
  theme(
    legend.position="none",
    #axis.line = element_blank(),
    axis.title.x = element_blank(),
    #axis.ticks = element_blank(),
    axis.title.y = element_blank()) +
  theme_minimal()

hist_a_sg

a_comp <- ggarrange(
  hist_a_sc,
  hist_a_sg,
  nrow = 2
)

plot(a_comp)


figures_folder = "./analisis-pad-2-salas-vacias/imgs_analisis_4_intercepts"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "2_histogramas_a.png", sep = '')
ggsave(mi_nombre_de_archivo, plot = a_comp, limitsize=FALSE, dpi=200)


# histograma a apaisado
data.clean.boxplot_a <- data.clean %>%
  mutate(
    condicion_sala = case_when(
      condicion_sala == "SALA_GRANDE" ~ "Grande",
      condicion_sala == "SALA_CHICA" ~ "Chica",
    )
  )

bxp <- ggboxplot(data.clean.boxplot_a, x = "condicion_sala", y = "intercept",
                 orientation = "horizontal",
                 color = "condicion_sala", palette = "jco",
                 add = "jitter", ylab="intercept a",
                 title="T-test de k") +
  theme_minimal() +
  theme(
    legend.position="none",
    #axis.line = element_blank(),
    axis.title.x = element_blank(),
    #axis.title = element_text(hjust = 0),
    #axis.ticks = element_blank(),
    axis.title.y = element_blank())


stat.test <- data.clean.boxplot_a  %>% 
  t_test(intercept~condicion_sala) %>%
  add_significance()


stat.test <- stat.test %>% add_y_position(fun ="mean", step.increase = 10)


bxp <- bxp + stat_pvalue_manual(
  stat.test, label = "p", tip.length = 0.01,
  coord.flip = TRUE
) +
  coord_flip()

bxp

all <- ggarrange(
  bxp,
  ggarrange(
    hist_a_sc, 
    hist_a_sg,
    nrow = 2
  ),
  ncol = 2
)

plot(all)

figures_folder = "./analisis-pad-2-salas-vacias/imgs_analisis_4_intercepts"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "intercept_k.png", sep = '')
ggsave(mi_nombre_de_archivo, plot = all, limitsize=FALSE, dpi=200)






# histogramas de r squared -------------------------------------------------------------

# IQR(data.clean$r.squared)
# 
# quantile(data.clean$r.squared) # 75 % - 25 % = IQR
# 
# quantile(data.clean$r.squared)[2]
# quantile(data.clean$r.squared)[4]

#   "italic(k) == %.3g * ',' ~~ italic(a) == %.3g  * ',' ~~ italic(r)^2 ~ '=' ~ %.2g",


# sala chica

data.clean <- data.clean  %>%
  rename(rsqr = r.squared)

data.clean.sala_chica <- data.clean %>%
  filter(condicion_sala == "SALA_CHICA")

eqn1 <- sprintf(
  "M = %.3g ± %.2g \nMdn = %.3g (%.2g - %.2g)\nN =  %.2g",
  mean(data.clean.sala_chica$rsqr),
  sd(data.clean.sala_chica$rsqr),
  median(data.clean.sala_chica$rsqr),
  quantile(data.clean.sala_chica$rsqr)[4],
  quantile(data.clean.sala_chica$rsqr)[2],
  sum(data.clean.sala_chica$rsqr)
)

hist_a_sc <- ggplot(data.clean.sala_chica, aes(rsqr)) +
  geom_histogram(binwidth= 0.1, color="black",fill="white") +
  scale_x_continuous(breaks= seq(0.3,1.0, by=0.1)) +
  scale_y_continuous(breaks= seq(0,12, by = 1))+
  xlim(0.5,1.1) +
  ylim(0,13) +
  geom_vline(xintercept = round(mean(data.clean.sala_chica$rsqr),2),        # Add line for mean
             linetype="dashed", color = 'darkgrey'
  ) +
  annotate("text",                        # Add text for mean
           #x = 1.5, # para fig sola
           x = 0.50, # para fig compuesta
           y = 10.0,
           label = eqn1,
           size = 1.5,
           hjust = 0) +
  labs(x=expression("R"^2*""), y = "Count")+
  # (m"^3*")
  theme(
    legend.position="none",
    #axis.line = element_blank(),
    axis.title.x = element_blank(),
    #axis.ticks = element_blank(),
    axis.title.y = element_blank()) +
  theme_minimal()

hist_a_sc

# sala grande

data.clean.sala_grande <- data.clean %>%
  filter(condicion_sala == "SALA_GRANDE")

eqn1 <- sprintf(
  "M = %.3g ± %.2g \nMdn = %.3g (%.2g - %.2g)\nN =  %.2g",
  mean(data.clean.sala_grande$rsqr),
  sd(data.clean.sala_grande$rsqr),
  median(data.clean.sala_grande$rsqr),
  quantile(data.clean.sala_grande$rsqr)[4],
  quantile(data.clean.sala_grande$rsqr)[2],
  sum(data.clean.sala_grande$rsqr)
)


# scale_x_continuous(breaks= seq(0.3,2.1, by=0.5)) +
#   scale_y_continuous(breaks= seq(0,5, by = 1))+

hist_a_sg <- ggplot(data.clean.sala_grande, aes(rsqr)) +
  geom_histogram(binwidth= 0.1, color="black",fill="white") +
  geom_vline(xintercept = round(mean(data.clean.sala_grande$rsqr),2),        # Add line for mean
             linetype="dashed", col = 'darkgrey',
  ) +
  scale_x_continuous(breaks= seq(0.3,1.0, by=0.1)) +
  scale_y_continuous(breaks= seq(0,12, by = 1))+
  xlim(0.5,1.1) +
  ylim(0,13) +
  annotate("text",                        # Add text for mean
           #x = 1.5, # para fig sola
           x = 0.50, # para fig compuesta
           y = 10.0,
           label = eqn1,
           size = 1.5,
           hjust = 0) +
  labs(x=expression("R"^2*""), y = "Count")+
  theme(
    legend.position="none",
    #axis.line = element_blank(),
    axis.title.x = element_blank(),
    #axis.ticks = element_blank(),
    axis.title.y = element_blank()) +
  theme_minimal()

hist_a_sg

r_comp <- ggarrange(
  hist_a_sc,
  hist_a_sg,
  nrow = 2
)

plot(r_comp)


figures_folder = "./analisis-pad-2-salas-vacias/imgs_analisis_4_r.squareds"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "2_histogramas_r2.png", sep = '')
ggsave(mi_nombre_de_archivo, plot = r_comp, limitsize=FALSE, dpi=200)


# histograma a apaisado
data.clean.boxplot_a <- data.clean %>%
  mutate(
    condicion_sala = case_when(
      condicion_sala == "SALA_GRANDE" ~ "Grande",
      condicion_sala == "SALA_CHICA" ~ "Chica",
    )
  )

bxp <- ggboxplot(data.clean.boxplot_a, x = "condicion_sala", y = "rsqr",
                 orientation = "horizontal",
                 color = "condicion_sala", palette = "jco",
                 add = "jitter", ylab="r.squared a",
                 title="T test R squared") +
  theme_minimal() +
  theme(
    legend.position="none",
    #axis.line = element_blank(),
    axis.title.x = element_blank(),
    #axis.title = element_text(hjust = 0),
    #axis.ticks = element_blank(),
    axis.title.y = element_blank())


stat.test <- data.clean.boxplot_a  %>% 
  t_test(rsqr~condicion_sala) %>%
  add_significance()


stat.test <- stat.test %>% add_y_position(fun ="mean", step.increase = -7)


bxp <- bxp + stat_pvalue_manual(
  stat.test, label = "p", tip.length = 0.01,
  coord.flip = TRUE
) +
  coord_flip()

bxp

all <- ggarrange(
  bxp,
  ggarrange(
    hist_a_sc, 
    hist_a_sg,
    nrow = 2
  ),
  ncol = 2
)

plot(all)

figures_folder = "./analisis-pad-2-salas-vacias/imgs_analisis_4_r_squared"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "r_squared.png", sep = '')
ggsave(mi_nombre_de_archivo, plot = all, limitsize=FALSE, dpi=200)





