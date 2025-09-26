####################################
#                                  #
#         Aula prática 04          #
#   - GLM para dados discretos -   #
#                                  #
####################################




#~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
library(dplyr)
library(GGally) #pairsplot
library(MASS) #Para rodar modelo Binomial Negativo
library(corrplot) #multicolinearidade
library(DHARMa)
library(ggeffects) #para plotar os resultados
library(performance) #avaliação dos residuos


## Definindo diretório base
setwd("~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/ECL0072-GLM_GAM/")



#~~~~~~~~~~~~~~~~~~~~~~~
# 1) Importando os dados
#~~~~~~~~~~~~~~~~~~~~~~~
dados <- readRDS("Dados/fisheries.rds")


#~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Explorando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~
str(dados)
View(dados)


# 2.1) Transformando as variáveis 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## character -> vector
dados[, c("Ano", "Mes", "Barco")] <- lapply(dados[, c("Ano", "Mes", "Barco")], factor)



# 2.2) Visualizando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Distribuição dos dados (variável y)
hist(dados$Thunnus_albacares)
boxplot(dados$Thunnus_albacares)
summary(dados$Thunnus_albacares)
dotchart(dados$Thunnus_albacares)



## Dinâmica temporal
plot(dados$Thunnus_albacares ~ dados$Mes)


## Dinâmica espacial
plot(dados$Thunnus_albacares ~ dados$Lat)
plot(dados$Thunnus_albacares ~ dados$Long)


## Dinâmica ambiental
#pairs(dados[, 28:33])


### Centralizando as covariáveis numéricas
dados[,28:33] <- scale(dados[,28:33], scale=T, center=F)


ggpairs(dados,         
        columns = c(8, 28:33),
        lower = list(continuous = "smooth")) # Columns

## Há multicolinearidade!


m_cor <- cor(dados[, 28:33])

corrplot(m_cor, method = 'ellipse', diag = FALSE, type = 'lower')
#corrplot.mixed(m_cor)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Modelando a abundância de atum
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 3.1) Distribuição poisson
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Modelo apenas com variáveis ambientais ##
mod_1 <- glm(Thunnus_albacares ~ sst + sal + depth + il,
                    data = dados,
                    family = "poisson")

summary(mod_1)

mod_1$deviance/mod_1$df.residual #há sobredispersão -> phi >> 1


# Precisamos ponderar os dados pela diferença de esforço!



## Modelo ambiental com offset ##
mod_2 <- glm(Thunnus_albacares ~ sst + sal + depth + il + offset(log(N_anzol)),
             data = dados,
             family = "poisson")


AIC(mod_1, mod_2) #Comparando 


names(mod_2)
mod_2$offset

summary(mod_2)


## Rápida inspecção residual
par(mfrow=c(2,2))
plot(mod_2)
par(mfrow=c(1,1))


check_overdispersion(mod_2)


# Será que melhoramos o ajuste adicionando o efeito de sazonalidade (mes)?

## Modelo ambiental + offset + tempo ##
mod_3 <- glm(Thunnus_albacares ~  sst + sal + depth + il + Mes + offset(log(N_anzol)),
             data = dados,
             family = "poisson")

AIC(mod_1, mod_2, mod_3) #Comparando 

summary(mod_3)


par(mfrow=c(2,2))
plot(mod_3)
par(mfrow=c(1,1))

check_overdispersion(mod_3)


# Talvez melhoramos mais adicionando o efeito da pesca?
mod_4 <- glm(Thunnus_albacares ~ sst + sal + depth + il + Mes + Barco + offset(log(N_anzol)),
             data = dados,
             family = "poisson")

AIC(mod_1, mod_2, mod_3, mod_4) #Comparando 

summary(mod_4)

par(mfrow=c(2,2))
plot(mod_4)
par(mfrow=c(1,1))

simres <- simulateResiduals(mod_4, n=1000)
plot(simres)

ggpredict(mod_4) %>% plot()


# Os dados continuam com elevada sobredispersão....
check_overdispersion(mod_4)



# 3.1) Distribuição Binomial Negativa
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mod_5 <- glm.nb(Thunnus_albacares ~ sst + sal + depth + il + Mes + Barco + offset(log(N_anzol)),
             data = dados)


AIC(mod_1, mod_2, mod_3, mod_4, mod_5) #Comparando 

summary(mod_5)

par(mfrow=c(2,2))
plot(mod_5)
par(mfrow=c(1,1))


simres2 <- simulateResiduals(mod_5, n=1000)
plot(simres2)


check_model(mod_5)

check_zeroinflation(mod_5)



# No geral, o modelo com distribuição BN está bom. Porém, podemos 
# ajustá-lo mais. Vejamos:

dotchart(dados$Thunnus_albacares) #Não há outlier aparente

dotchart(dados$N_anzol) #Há pelo menos 2 outliers bem marcantes


### Tirando o outlier
dados2 <- filter(dados, N_anzol <= 1500) 


dotchart(dados2$N_anzol) 


# Vamos rodar novamente o modelo BN
mod_5b <- glm.nb(Thunnus_albacares ~ sst + sal + depth + il + Mes + Barco + offset(log(N_anzol)),
                data = dados2)


## ATENÇÃO: Não podemos comparar mod_5 e mod_5b diretamente (e.g., anova), porque
## não são os mesmos dados! A comparação via ANOVA (LRT)/AIC só funciona qunado se está lidando com o mesmo banco de dados (e quando os modelos são aninhados!)

dim(dados2)
dim(dados)

summary(mod_5b)


ggpredict(mod_5b) %>% plot()

# modelo# modelo# modelo_full <- glm(Thunnus_albacares ~ SST + SLA + BAT + IL + Mes,
#                    family = "poisson", 
#                    data = dados)
# backward_model <- stepAIC(modelo_full, direction = "backward")
# summary(backward_model)