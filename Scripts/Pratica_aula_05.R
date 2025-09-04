####################################
#                                  #
#         Aula prática 05          #
#   - GLM para dados contínuos -   #
#                                  #
####################################




#~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
library(MASS) #seleção automática de modelos
library(dplyr)
library(DHARMa)
library(ggeffects) #para plotar os resultados
library(performance) #avaliação dos residuos



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



# 2.2) Calculando a abundância de espécies não-alvo (bycatch)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Espécies alvo (atuns)
especies_alvo <- c("Thunnus_albacares",
                   "Thunnus_atlanticus",
                   "Katsuwonus_pelamis")

## Espécies não-alvo (todo o restante)
especies_nalvo <- setdiff(colnames(dados)[8:27], especies_alvo)
length(especies_nalvo) #No. de espécies que não são alvo da pesca



## Calculando a abundância total de espécies não-alvo
### Precisamos também padronizar a abundância pelo esforço amostral (BPUE - bycatch per unit of effort)
dados <- dados %>%
         rowwise() %>%  # analisa linha por linha
         mutate(Bycatch = sum(c_across(all_of(especies_nalvo))),
                BPUE = Bycatch/N_anzol) %>%
         data.frame()





# 2.3) Visualizando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Distribuição dos dados (variável y)
hist(dados$BPUE)
summary(dados$BPUE)

#dotchart(dados$BPUE)
#dotchart(boxplot$BPUE)



## Dinâmica temporal
plot(dados$BPUE ~ dados$Mes)
plot(log1p(dados$BPUE) ~ dados$Mes)

ggplot(dados, aes(x = Mes, y = BPUE)) +
#ggplot(dados, aes(x = Mes, y = log1p(BPUE))) +
  geom_boxplot(size = 0.7, fill = 'cyan4',col = 'cyan4', alpha = 0.15) +
  stat_summary(fun = mean, geom="point", shape=19, size=1.5, color="black") +
  theme_minimal()



## Dinâmica espacial

### Latitude
ggplot(dados, aes(x = Lat, y = BPUE)) +
#ggplot(dados, aes(x = Lat, y = log1p(BPUE))) +
  geom_point(size = 3, col = 'darkorange', alpha = 0.4) +
  geom_smooth(method = "lm", col = 'cyan4') +
  theme_minimal()



### Longitude
ggplot(dados, aes(x = Long, y = BPUE)) +
  #ggplot(dados, aes(x = Long, y = log1p(BPUE))) +
  geom_point(size = 3, col = 'darkorange', alpha = 0.4) +
  geom_smooth(method = "lm", col = 'cyan4') +
  theme_minimal()



# ggplot(dados, aes(x = Long, y = log1p(BPUE))) +
#   geom_point(size = 3, col = 'darkorange', alpha = 0.4) +
#   geom_smooth(method = "lm", col = 'cyan4', formula = y ~ poly(x,2)) +
#   theme_minimal()



#~~~~~~~~~~~~~~~~~~~~
# 3) Modelando BPUE
#~~~~~~~~~~~~~~~~~~~~


# 3.1) Preparando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Lembrando que a distribuição Gama e log-Normal é para variáveis estritamente positivas.
# Isso implica que a variável resposta não pode conter o valor 0.
# Truqe matemátcio: adicionar uma pequena constante 


# Usando 10% da mediana do BPUQ
dados$BPUE2 <- dados$BPUE + (0.1 * median(dados$BPUE))

# Transformando para log
dados$BPUE2_log <- log(dados$BPUE2)



# 3.2) Rodando os modelos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Modelo Gama ##
modelo_G <- glm(BPUE2 ~ Mes + Lat*Long,
                family = Gamma(link = "log"),
                data = dados)


## Modelo log-Normal ##
modelo_LN <- glm(BPUE2_log ~ Mes + Lat*Long,
                 family = gaussian,
                 data = dados)



# 3.3) Avaliando os modelos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

## AIC
AIC(modelo_G, modelo_LN)


## Teste de razão de verossimilhança
lmtest::lrtest(modelo_G, modelo_LN) ## Quanto maior o valor de logLik, melhor o ajuste!



## Deviancia
modelo_G$deviance
modelo_LN$deviance


## Residuos
resG <- simulateResiduals(modelo_G, n=1000)
resLN <- simulateResiduals(modelo_LN, n=1000)


check_model(resG)
check_model(resLN)



# Embora ambos os modelos apresentam um bom ajuste visual, escolhe-se o modelo com
# distribuição Gama como o melhor modelo, uma vez que foi significativamente melhor
# que o modelo log-Normal (incluindo o AIC)



## Avaliando os resultados
summary(modelo_G)



### Plotando valores ajustados
names(modelo_G)

head(cbind(modelo_G$fitted.values, modelo_G$linear.predictors))


ggpredict(modelo_G) %>% plot()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                                                                 #
#### ADICIONAL: Comparando as predições entre ambos os modelos ####
#                                                                 #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Para comparar as predições, é necessário fazê-las manualmente.
# Para cada covariável que se deseja prever a sua relação com a variável resposta,
# fixa-se as demais co-variáveis no seu valor médio.

# Precisamos montar um data frame, o qual será usado para calcular as predições.
# Esse data frame irá essencialmente conter tantas colunas quantas covariáveis tiverem sido
# inseridas no modelo. No nosso caso, teremos um data frame com três colunas.

# Como queremos prever em relação de 3 co-variáveis, precisamos montar três data frames de previsão:
# @ dfpred_mes (fixa-se long e lat em seus valores medios)
# @ dfpred_lat (fixa-se long em seu valor médio e mês no seu nível de referência (mês 1))
# @ dfpred_long (fixa-se lat em seu valor médio e mês no seu nível de referência (mês 1))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Predicões ao longo dos meses  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Calculando os valores médios de Latitude e Longitude 
Lat_med <- mean(dados$Lat, na.rm = TRUE)
Long_med <- mean(dados$Long, na.rm = TRUE)



# Criando um data frame para fazer as predições
## Cuidado: os nomes tem que coincidir com os nomes das variáveis colocadas no modelo
dfpred_mes <- data.frame(Mes = factor(1:12),
                         Lat = rep(Lat_med, 12),
                         Long = rep(Long_med, 12)) #Para distribuição Gama

dfpred_mesb <- dfpred_mes #Para distribuição log-Normal



### Predições ###

### Gama
pred_G <- predict(modelo_G, 
                  newdata = dfpred_mes, 
                  type = "response", #a predição deve ser feita no espaço da resposta
                  se.fit = TRUE)

names(pred_G)


dfpred_mes$fit <- pred_G$fit
dfpred_mes$se <- pred_G$se.fit
dfpred_mes$lower <- dfpred_mes$fit - 1.96 * dfpred_mes$se
dfpred_mes$upper <- dfpred_mes$fit + 1.96 * dfpred_mes$se
dfpred_mes$Modelo <- "Gama"



### log-Normal
pred_LN <- predict(modelo_LN, 
                   newdata = dfpred_mesb, 
                   type = "response", #a previsão é feita no espaço transformado 
                   se.fit = TRUE)


dfpred_mesb$fit <- exp(pred_LN$fit)
dfpred_mesb$se <- exp(pred_LN$se.fit)
dfpred_mesb$lower <- exp(pred_LN$fit - 1.96 * pred_LN$se.fit)
dfpred_mesb$upper <- exp(pred_LN$fit + 1.96 * pred_LN$se.fit)
dfpred_mesb$Modelo <- "log-Normal"



### Reunindo tudo em um único data frame
dfpred_full <- rbind(dfpred, dfpred2)



### Figuras
ggplot(dfpred_full, aes(x = Mes, y = fit, col= Modelo, group = Modelo)) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper, col= Modelo),
                size = 1,
                width = 0.2, position = position_dodge(width = 0.5)) +
  labs(y = "BPUE (predito)",
       x = "Mês") +
  theme_minimal() +
  scale_color_manual(values = c("Gama" = "darkorange", "log-Normal" = "cyan4"))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Predicões ao longo da latitude  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Lat_seq <- seq(from = min(dados$Lat, na.rm = TRUE),
               to = max(dados$Lat, na.rm = TRUE),
               length.out = 500)


# Data frame para predições da distribuição Gama
dfpred_lat <- data.frame(Mes = factor(1), #Mês 1 como referência
                         Lat = Lat_seq,
                         Long = Long_med) 


# Data frame para predições da distribuição Gama
dfpred_latb <- dfpred_lat



### Predições ###

### Gama
pred_G <- predict(modelo_G, 
                  newdata = dfpred_lat, 
                  type = "response", #a predição deve ser feita no espaço da resposta
                  se.fit = TRUE)


dfpred_lat$fit <- pred_G$fit
dfpred_lat$se <- pred_G$se.fit
dfpred_lat$lower <- dfpred_lat$fit - 1.96 * dfpred_lat$se
dfpred_lat$upper <- dfpred_lat$fit + 1.96 * dfpred_lat$se
dfpred_lat$Modelo <- "Gama"



### log-Normal
pred_LN <- predict(modelo_LN, 
                   newdata = dfpred_latb, 
                   type = "response", 
                   se.fit = TRUE)


dfpred_latb$fit <- exp(pred_LN$fit)
dfpred_latb$se <- exp(pred_LN$se.fit)
dfpred_latb$lower <- exp(pred_LN$fit - 1.96 * pred_LN$se.fit)
dfpred_latb$upper <- exp(pred_LN$fit + 1.96 * pred_LN$se.fit)
dfpred_latb$Modelo <- "log-Normal"



### Reunindo tudo em um único data frame
dfpred_full <- rbind(dfpred_lat, dfpred_latb)



### Plot
ggplot(dfpred_full, aes(x = Lat, y = fit, col= Modelo, group = Modelo)) +
  #geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill= Modelo), alpha = 0.2) +
  geom_line(size = 1) +
  labs(y = "BPUE (predito)",
       x = "Latitude") +
  theme_minimal() +
  scale_color_manual(values = c("Gama" = "darkorange", "log-Normal" = "cyan4")) +
  facet_wrap(Modelo ~ .)



## Repetir a seção anterior para predizer a BPUE ao longo da longitude

