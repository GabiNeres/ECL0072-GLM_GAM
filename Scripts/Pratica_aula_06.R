######################################
#                                    #
#         Aula prática 06            #
#   - GLM para dados categóricos -   #
#                                    #
######################################

# O script abordará o modelo multinomial.
# Será utilizado os dados de pinguim de palmer como exemplo prático.



#~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~
library(VGAM) #Modelo multinomial
library(nnet) #Modelo multinomial (pacote alternativo)
library(ggplot2)

library(dplyr)
library(DHARMa)
library(ggeffects) #para plotar os resultados
library(performance) #avaliação dos residuos


#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Definindo diretório base
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/ECL0072-GLM_GAM/")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Carregando funções auxiliares
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("Scripts/funcoes_auxiliares.R")


#><><><><><><><><><><><><><><><><><><><><><><><

 
#~~~~~~~~~~~~~~~~~~~~~~~
# 1) Importando os dados
#~~~~~~~~~~~~~~~~~~~~~~~
dados <- readRDS("Dados/palmerpenguins_extended.rds")


# 1.1) Transformando as variáveis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# chr -> factor

cols <- c("species", "island", "sex", "diet", "life_stage", "health_metrics")
dados[, cols] <- lapply(dados[, cols], factor)



#~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Explorando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~

## Há preferência alimentar de acordo com o tamanho dos pinguins? (usando peso como proxy de tamanho)
ggplot(dados, aes(x = body_mass_g, y = as.factor(diet))) +
  geom_boxplot(size = 0.7, fill = 'darkorange',col = 'darkorange', alpha = 0.15) +
  stat_summary(fun = mean, geom="point", shape=19, size=1.5, color="black") +
  theme_minimal(base_size = 15)



## E por espécies?
ggplot(dados, aes(x = species, fill = diet)) +
  geom_bar(position = "fill",  alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#d6ccc2", "cyan4", "darkorange", "gray50")) +
  labs(y = "Proporção", x = "") +
  theme_minimal(base_size = 15) 

# ggplot(dados, aes(x = body_mass_g, y = diet, col = species, fill = species)) +
#   geom_boxplot(size = 0.7,alpha = 0.15) +
#   scale_colour_manual(values = c("darkorange", "gray50", "cyan4")) +
#   scale_fill_manual(values = c("darkorange", "gray50", "cyan4")) +
#   theme_minimal(base_size = 15)



#~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Modelando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~

# CUIDADO: definir o nível de referência
levels(dados$diet)

# 3.1) Ajustando o modelo
#~~~~~~~~~~~~~~~~~~~~~~~~~~
modelo1 <- vglm(diet ~ body_mass_g, 
                family = multinomial(refLevel = 1),  # define a categoria de referência
                data = dados)

summary(modelo1)


library(nnet)
modelo1b <- multinom(diet ~ body_mass_g, data = dados)
summary(modelo1b)


# 3.2) Visualizando os resultados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Criando dados para predizer
dados_pred <- data.frame(body_mass_g = seq(min(dados$body_mass_g),
                                           max(dados$body_mass_g),
                                           length.out = 100))

## Predizendo...
preds <- predict(modelo1, 
                 newdata = dados_pred, 
                 type = "link", 
                 se.fit = TRUE)


## Calculando probabilidades por categoria & ICs
n_cat <- ncol(preds$fit) + 1 #no. de categorias
n_obs <- nrow(preds$fit) #no. de observações
categorias <- levels(dados$diet) #nome das categorias
cat_ref <- categorias[1] #categoria de referência
cat_outras <- categorias[-1]


preds_full <- lapply(1:n_obs, function(i) {
  
  eta <- preds$fit[i, ]
  se_eta <- preds$se.fit[i, ]
  
  probs <- softmax(eta)
  
  # Matriz de covariância dos preditores lineares (diagonal com variâncias)
  V_eta <- diag(se_eta^2)
  
  # Jacobiano da softmax
  J_soft <- softmax_jacobian(probs)
  
  # Método Delta: variância aproximada das probabilidades
  var_probs <- diag(J_soft %*% V_eta %*% t(J_soft))
  se_probs <- sqrt(var_probs)
  
  tibble(
    body_mass_g = dados_pred$body_mass_g[i],
    dieta = categorias,
    Prob = probs,
    SE = se_probs,
    IC_lower = pmax(0, probs - 1.96 * se_probs),
    IC_upper = pmin(1, probs + 1.96 * se_probs)
  )
})

preds_full <- do.call("rbind", preds_full)


## Plotando...
ggplot(preds_full, aes(x = body_mass_g, y = Prob, color = dieta, fill = dieta)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = IC_lower, ymax = IC_upper), alpha = 0.2, color = NA) +
  labs(title = "",
       x = "Massa Corporal (g)",
       y = "Probabilidade p",
       color = "Dieta",
       fill = "Dieta") +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("fish" = "darkorange", "squid" = "#d6ccc2", "parental" = "cyan4", "krill"="gray50")) +
  scale_colour_manual(values = c("fish" = "darkorange", "squid" = "#d6ccc2", "parental" = "cyan4", "krill"="gray50")) +
  theme(plot.title = element_text(hjust = 0.5))

