####################################
#                                  #
#         Aula prática 03          #
#    - GLM para dados binários -   #
#                                  #
####################################


#~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2) # para plotar
library(dplyr) #manipular os dados
library(DHARMa) #avaliação dos residuos
library(performance) #avaliação dos residuos


## Definindo diretório base
setwd("~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/ECL0072-GLM_GAM/")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Importando o banco de dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dados <- readRDS("Dados/palmerpenguins_extended.rds")



#~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Análise exploratória 
#~~~~~~~~~~~~~~~~~~~~~~~~~~
str(dados)


# 2.1) Convertendo as variáveis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cols <- c("species", "island", "sex", "diet", "life_stage", "health_metrics", "year")

dados[, cols] <- lapply(dados[, cols], factor)


# 2.2) Explorando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
table(dados$health_metrics)



## Filtrando os dados ##
dados2 <- filter(dados, !(health_metrics == "underweight"))

dados2 %>%
  group_by(health_metrics) %>% 
  summarise(Npinguins = n()) %>%
  mutate(Proporcao = Npinguins / sum(Npinguins)) #43% dos pinguins são obesos!



## Relação com o peso individual ##
# Precisamos converter as categorias de peso em formato binário

dados2$categoria <- ifelse(dados2$health_metrics == "overweight", 1, 0)

ggplot(dados2, aes(x = body_mass_g, y = categoria)) +
  geom_point(size = 4, col = 'gray50', alpha = 0.25) +
  theme_minimal()



## Relação peso individual vs. espécie ##
ggplot(dados2, aes(x = as.factor(categoria), y = body_mass_g, fill = species)) +
  geom_boxplot() +
  scale_fill_brewer(palette="BrBG") +
  theme_minimal()



## Relação peso individual vs. sexo ##
ggplot(dados2, aes(x = sex, y = body_mass_g, fill = sex)) +
    geom_boxplot() +
    scale_fill_brewer(palette="BrBG") +
    theme_minimal()


## Relação peso individual vs. sexo & espécie ##
ggplot(dados2, aes(x = sex, y = body_mass_g, fill = species)) +
    geom_boxplot() +
    scale_fill_brewer(palette="BrBG") +
    theme_minimal()



## Relação peso individual vs. dieta ##
ggplot(dados2, aes(x = diet, y = body_mass_g, fill = species)) +
    geom_boxplot() +
    scale_fill_brewer(palette="BrBG") +
    theme_minimal()



#~~~~~~~~~~~~~~~~~~~~~~
# 3) Testando modelos
#~~~~~~~~~~~~~~~~~~~~~~

## Modelo Nulo
mod0 <- glm(categoria ~ 1,
            family = binomial(link = "logit"),
            data = dados2)

## Modelo 1
mod1 <- glm(categoria ~ body_mass_g,
            family = binomial(link = "logit"),
            data = dados2)

## Modelo 2
mod2 <- glm(categoria ~ body_mass_g + species,
            family = binomial(link = "logit"),
            data = dados2)

## Modelo 3
mod3 <- glm(categoria ~ body_mass_g + species + sex,
            family = binomial(link = "logit"),
            data = dados2)

## Modelo 4
mod4 <- glm(categoria ~ body_mass_g + species + sex + diet,
            family = binomial(link = "logit"),
            data = dados2)

## Modelo 5
mod5 <- glm(categoria ~ poly(body_mass_g, 2) + species + sex + diet, 
            family = binomial(link = "logit"),
            data = dados2)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4) Comparando os modelos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Calcular os valores de AIC ##
aic <- AIC(mod0, mod1, mod2, mod3, mod4, mod5)


## Ordenar dos menores aos maiores valores ##
aic %>% arrange(AIC)


## Teste de razão de verossimilhança ##
anova(mod4, mod5, test = "Chisq")
#lmtest::lrtest(mod4, mod5) #mesma coisa



#~~~~~~~~~~~~~~~~~~~~~~~~
# 5) Validação do modelo
#~~~~~~~~~~~~~~~~~~~~~~~~
library(DHARMa)

## Simulando os resíduos a partir do modelo escolhido ##
resid_sim <- simulateResiduals(mod5, n=1000)

plot(resid_sim)


## Teste de uniformidade dos resíduos
testUniformity(resid_sim) 


## Teste de sobredispersão
testDispersion(resid_sim) # se p>0.05, não há sobredispersão


## Teste de autocorrelação
check_autocorrelation(mod5)
acf(residuals(mod5))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 6) Analsiando os resultados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(mod5)



## Visualizando os resultados ##


## Criando uma sequência de massas corporais (para prever probabilidade de sobrepeso)
massas <- seq(min(dados2$body_mass_g, na.rm = TRUE),
              max(dados2$body_mass_g, na.rm = TRUE),
              length.out = 100)



## Gerando um data frame de predição
dfpred <- expand.grid(body_mass_g =  massas,
                      species = unique(dados2$species),
                      sex = unique(dados2$sex),
                      diet = unique(dados2$diet))



## Predizendo na escala logit (necessario para calcular os ICs)
predicoes <- predict(mod5, newdata = dfpred , type = "link", se.fit = TRUE)

dfpred <- dfpred %>%
          mutate(link = predicoes$fit,
                 se = predicoes$se.fit,
                 prob = plogis(link),
                 lwr = plogis(link - 1.96 * se),
                 upr = plogis(link + 1.96 * se))



## Plotando....
ggplot(dfpred, aes(x = body_mass_g, y = prob, color = sex, fill = sex)) +
  geom_point(data = dados2, aes(x = body_mass_g, y = categoria),
             inherit.aes = FALSE, alpha = 0.1, col = "gray40") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, color = NA) +
  geom_line(size = 1) +
  facet_grid(diet ~ species) +
  labs(y = "Probabilidade de sobrepeso",
       x = "Massa corporal (g)") +
  theme_minimal() +
  theme(legend.position = "bottom")+
  scale_color_manual(values = c("#D8B365", "#5AB4AC"))
    


