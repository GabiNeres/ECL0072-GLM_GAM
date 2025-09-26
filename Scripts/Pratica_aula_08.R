######################################
#                                    #
#         Aula prática 08            #
#             - GAM -                #
#                                    #
######################################



#~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~
library(mgcv) #rodar gam
library(mgcViz) #visualizar resultados do gam
library(gratia) #visualizar resultados do gam
library(ggplot2) #plotar
library(dplyr) #manipular dados
#library(DHARMa) #avaliação dos residuos
#library(performance) #avaliação dos residuos
library(ggeffects) #para plotar os resultados



#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Definindo diretório base
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/ECL0072-GLM_GAM/")



#><><><><><><><><><><><><><><><><><><><><><><><


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Importando & manipulando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# 1.1) Importando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~
dados <- readRDS("Dados/bioluminescence.rds")
str(dados)


# 1.2) Transformando as variáveis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cols <- c("Station", "Time", "Month", "Year", "Season")
dados[, cols] <- lapply(dados[, cols], factor) #Transformando para fator


# 1.3) Filtrando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~
# Análises prévias mostraram que nem todas as estações de amostragem
# apresentaram o mesmo padrão de relação entre bioluminescencia ~ profundidade.
# Para explorar melhor o poder dos modelos GAM, os dados serão filtrados
# para conter apenas o conjunto de estações que apresentaram uma relação altaramente não linear

## Vejamos...
dados %>%
  ggplot(aes(x=SampleDepth, y = Sources)) +
  geom_point(col = 'gray40', pch = 19, alpha =0.5, size = 3) +
  facet_wrap(Station ~ . , scales = "free_y") +
  #geom_smooth(method = "gam", fill = 'gray60', col = 'darkorange') +
  labs(x = "Profundidade (m)",
       y = "Bioluminescência") +
  theme_minimal(base_size = 14) 


dados2 <- filter(dados, Station %in% c(6,7,8,9,11,16,18,19))



#~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Explorando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~

## Explorando a variável resposta (Sources - bioluminescência)
ggplot(dados2, aes(x = Sources)) +
  geom_histogram(bins = 35, fill = 'cyan4', col = 'white')

# O dado apresenta uma forte assimetria à direita


## Explorando a relação bioluminescência vs. profundidade
ggplot(dados2, aes(x = SampleDepth, y = Sources)) +
  geom_point(pch = 19, alpha =0.5, size = 3,
             col = 'cyan4') +
  #geom_smooth(method = "gam",col = 'darkorange', fill = '#b6ad90') +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        plot.title = element_text(hjust = 0.5, face = 'bold'))


#~~~~~~~~~~~~~~~~~~~~~~~
# 3) Modelando os dados
#~~~~~~~~~~~~~~~~~~~~~~~

## Vamos começar com um GLM e aumentar sequencialmente o 
## nível de complexidade do modelo




# 3.1) GLM com distribuição Guassiana (Normal)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Modelo com relção puramente linear
m_glm <- glm(Sources ~ SampleDepth,
             data = dados2,
             family = gaussian)


## Modelo com termo quadrático
m_glm2 <- glm(Sources ~ poly(SampleDepth, 2),
             data = dados2,
             family = gaussian)


## Modelo com termo cúbico
m_glm3 <- glm(Sources ~ poly(SampleDepth, 3),
              data = dados2,
              family = gaussian)


## Modelo com termo à quarta
m_glm4 <- glm(Sources ~ poly(SampleDepth, 4),
              data = dados2,
              family = gaussian)


AIC(m_glm, m_glm2, m_glm3, m_glm4) # A adição de termos polinomiais melhora consideravelmente o ajuste


## Análise dos pressupostos
simres1 <- simulateResiduals(m_glm, n=1000) #modelo mais simples
simres4 <- simulateResiduals(m_glm4, n=1000) #modelo mais complexo


check_model(simres1) #Modelo mais simples
check_model(simres4) #Modelo mais complexp


# Será que usar uma distribuição gama melhora o ajuste?

## Modelo com termo à quarta & distribuição gama
### Lembrando: distribuição gama é estritamente positiva (y > 0)
summary(dados2$Sources)
dados2$Sources2 <- dados2$Sources + median(dados2$Sources)*0.1 #Adicionando 10% da mediana para evitar o valor zero


m_glm5 <- glm(Sources2 ~ poly(SampleDepth, 4),
              data = dados2,
              family = Gamma(link = 'log'))

AIC(m_glm4, m_glm5)

simres5 <- simulateResiduals(m_glm5, n=1000) #modelo mais complexo
check_model(simres5) #Modelo mais simples



## Os resíduos ainda mostram um padrão - vamos tentar um GAM!


### GAM com distribuição Normal
Ngam <- gam(Sources2 ~ s(SampleDepth, bs = "cr"),
            data = dados2,
            family = gaussian())

AIC(m_glm5, Ngam) #glm5 usa distribuição GAMA, Ngam usa distribuição normal



### GAM com distribuição Gama
Ggam <- gam(Sources2 ~ s(SampleDepth, bs = "cr"),
            data = dados2,
            family = Gamma(link = "log"))

AIC(m_glm5, Ggam) #O GAM fornece um melhor ajuste comparado ao modelo GLM


## Avaliando os pressupostos
par(mfrow = c(2,2))
gam.check(Ggam) #versão com pacote mgcv
par(mfrow = c(1,1))

gratia::appraise(Ggam,
                 method = 'simulate',
                 #use_worm = T,
                 seed = 123) #versão com pacote gratia




### Interpretando um GAM
summary(Ggam2) # edf >>> 1 (efeito não-linear)


### Visualizando os resultados
plot(Ggam)


gratia::draw(Ggam) #versão com pacote gratia




