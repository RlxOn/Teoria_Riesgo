library(readxl)
library(readr)
library(ggplot2)
library(dplyr)
library(MASS)
library(gridExtra)
library(fitdistrplus)
library(univariateML)
library(tidyverse)
library(ruin)

set.seed(1989)

data <- read_xlsx("C:\\Users\\raulb\\OneDrive\\Documentos\\Salud_2020.xlsx", sheet = 3)
summary(data)

#Estructuramos bien el conjunto de datos y variables a utilizar
data$EDAD <- NULL
data$GENERO <- as.factor(data$GENERO)
data$`ENTIDAD DE RESIDENCIA` <- as.factor(data$`ENTIDAD DE RESIDENCIA`)
data$`ACTIVIDAD ECONÓMICA` <- NULL
data$`SUBTIPO DE SEGURO` <- as.factor(data$`SUBTIPO DE SEGURO`)
data$DIAGNÓSTICO <- NULL
data$`TIPO DE EVENTO HOSPITALARIO` <- as.factor(data$`TIPO DE EVENTO HOSPITALARIO`)
data$`PROCEDENCIA DE INGRESO` <- NULL
data$`MOTIVO DE EGRESO` <- NULL

#Filtro de montos no negativos
data <- data %>%
  filter(`MONTO DE HOSPITALIZACION` > 0)

#Criterio de chauvenet
Kn = qnorm(1/(4*16406), mean = 0, sd = 1, lower.tail = FALSE)
media_num <- mean(data$`NUMERO DE RECLAMACIONES`)
sd_num <- sd(data$`NUMERO DE RECLAMACIONES`)

media_monto <- mean(data$`MONTO DE HOSPITALIZACION`)
sd_mont <- sd(data$`MONTO DE HOSPITALIZACION`)

data <- data %>%
  filter( `MONTO DE HOSPITALIZACION` < media_monto + Kn*sd_mont) %>%
  filter(`NUMERO DE RECLAMACIONES` < media_num + Kn*sd_num)


####### ANALISIS DESCRIPTIVO DE LOS DATOS ############
#Genero
gg1 <- ggplot(data = data, aes(x = GENERO)) + geom_bar(fill = c("#7093db","#db7093","#93db70")) +
  theme_bw()
#Evento hospitalario
gg2 <- ggplot(data = data, aes(x = `TIPO DE EVENTO HOSPITALARIO`)) + 
  geom_bar(fill = c("#93db70","#70dbb8")) + theme_bw()

grid.arrange(gg1, gg2, nrow = 1, ncol = 2)

#Numero de reclamaciones
gg3 <- ggplot(data = data, aes(x =`NUMERO DE RECLAMACIONES` )) + geom_bar(fill = "#d14774") +
  theme_bw() + xlim(0,20)
mean(data$`NUMERO DE RECLAMACIONES`);sd(data$`NUMERO DE RECLAMACIONES`)

#Monto  de hospitalizacion
gg4 <- ggplot(data = data, aes(x = `MONTO DE HOSPITALIZACION`)) + geom_density(fill = "#008b8b") + 
  theme_bw() + xlim(0,20000)

grid.arrange(gg3,gg4, nrow = 1, ncol = 2)

#Boxplots
gg5 <- ggplot(data = data, aes(x = 0, y = `NUMERO DE RECLAMACIONES`)) + geom_boxplot() + theme_bw()
gg6 <- ggplot(data = data, aes(x = 0, y = `MONTO DE HOSPITALIZACION`)) + geom_boxplot() + theme_bw()

grid.arrange(gg5,gg6, nrow = 1, ncol = 2)

########## Bondad de ajuste ######
#Simulamos la variable poisson con lambda = 5000
Numero_reclamaciones <- rpois(16169, lambda = 5000)
poi_mu = 5000
poi_var = 5000

ggplot(data = NULL, aes(x = Numero_reclamaciones)) + geom_bar(fill = "#d14774") + 
  theme_bw()


# Monto de hospitalizacion
comparacion_aic <- AIC(
  mlexp(data$`MONTO DE HOSPITALIZACION`),
  mlinvgamma(data$`MONTO DE HOSPITALIZACION`),
  mlgamma(data$`MONTO DE HOSPITALIZACION`),
  mllnorm(data$`MONTO DE HOSPITALIZACION`),
  mlrayleigh(data$`MONTO DE HOSPITALIZACION`),
  mlinvgauss(data$`MONTO DE HOSPITALIZACION`),
  mlweibull(data$`MONTO DE HOSPITALIZACION`),
  mlinvweibull(data$`MONTO DE HOSPITALIZACION`),
  mlpareto(data$`MONTO DE HOSPITALIZACION`))

comparacion_aic %>% rownames_to_column(var = "distribucion") %>% arrange(AIC)

comparacion_bic <- BIC(
  mlexp(data$`MONTO DE HOSPITALIZACION`),
  mlinvgamma(data$`MONTO DE HOSPITALIZACION`),
  mlgamma(data$`MONTO DE HOSPITALIZACION`),
  mllnorm(data$`MONTO DE HOSPITALIZACION`),
  mlrayleigh(data$`MONTO DE HOSPITALIZACION`),
  mlinvgauss(data$`MONTO DE HOSPITALIZACION`),
  mlweibull(data$`MONTO DE HOSPITALIZACION`),
  mlinvweibull(data$`MONTO DE HOSPITALIZACION`),
  mlpareto(data$`MONTO DE HOSPITALIZACION`))

comparacion_bic %>% rownames_to_column(var = "distribucion") %>% arrange(BIC)

###Se elige la distr Lognormal
mllnorm(data$`MONTO DE HOSPITALIZACION`)
log_mu = exp(8.845+(2.081^2)/2)
log_var = (exp(2.081^2)-1)*exp(2*8.845+2.081^2)

## Calculo de media y varianza de S
S_media = poi_mu * log_mu
S_var = poi_var * log_mu^2 + log_var * poi_mu
S_sd = sqrt(S_var)
cat("E[S] = ", S_media, "Var(S) = ", S_var, "sd(S) = ", S_sd)

###################### Proceso de cramer lundberg #########################

#Fijamos el capital inicial igual a la prima neta y c igual a la prima
#con recargo de sd, theta = 1
u = S_media 
c = 302464264.00 + 37287086

#Construimos el modelo de cramerlundberg, tasa lambda = 50
CL_model <- CramerLundberg(initial_capital = u, premium_rate = c,
                           claim_poisson_arrival_rate = 50,
                           claim_size_generator = rlnorm,
                           claim_size_parameters = list(meanlog = 8.845, sdlog= 2.081))
#Realizamos las iteraciones 
ruin_prob <- ruin_probability(CL_model, time_horizon = 5000, simulation_number = 3,
                              ci_level = 0.95, return_paths = TRUE)

View(ruin_prob)
plot_path(ruin_prob$simulated_paths[[2]]) + theme_bw()

