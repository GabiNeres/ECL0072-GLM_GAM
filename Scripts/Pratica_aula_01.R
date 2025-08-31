####################################
#                                  #
#         Aula prática: LM         #
#                                  #
####################################


#~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)


#~~~~~~~~~~~~~~~~~~~~~
# Carregando os dados
#~~~~~~~~~~~~~~~~~~~~~

## Definindo diretório base
setwd("~/Library/CloudStorage/OneDrive-Personal/Arbeit/Lectures_and_talks/UFRN/Lectures/ECL0072-GLM_GAM/")

## Carregando os dados
dados <- read.csv("Dados/palmerpenguins_extended.csv")



#~~~~~~~~~~~~~~~~~~~~
# Analisando os dados
#~~~~~~~~~~~~~~~~~~~~

# Resumo geral dos dados
#~~~~~~~~~~~~~~~~~~~~~~~~~
str(dados)
summary(dados) 
glimpse(dados)



# Transformando os dados 
#~~~~~~~~~~~~~~~~~~~~~~~~
# Há variáveis classificadas como 'character'.
# Precisamos transformar elas para 'factor', de modo que sejam
# reconhecidas como categorias e assim permitir a realização de comparações

## Selecionando as colunas a serem transformada
cols <- c("species", "island","sex", "diet", "life_stage", "health_metrics")

## Transformando para fator
dados[, cols] <- lapply(dados[, cols], factor)

#glimpse(dados)


### Variável resposta
par(mfrow = c(1,3))
hist(dados$body_mass_g, main = "Histograma")
boxplot(dados$body_mass_g, main = "Boxplot")
dotchart(dados$body_mass_g, main = "Dotchart")
par(mfrow = c(1,1))


### Variável preditora
par(mfrow = c(1,3))
hist(dados$bill_length_mm, main = "Histograma")
boxplot(dados$bill_length_mm, main = "Boxplot")
dotchart(dados$bill_length_mm, main = "Dotchart")
par(mfrow = c(1,1))


### Correlação entre y-x
plot(dados$body_mass_g ~ dados$bill_length_mm) #Gráfico de dispserão
cor(dados$body_mass_g, dados$bill_length_mm) #Correlação de pearson
?cor



# Avaliando a relação entre peso vs. comprimento do bico
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Rodando o modelo linear
mod1 <- lm(body_mass_g ~ bill_length_mm, dados)


## O que tem dentro do obje
names(mod1)


## E a matriz de design (i.e., preditores + intercepto)
head(model.matrix(dados$body_mass_g ~ dados$bill_length_mm))


## Resultado numérico do modelo
summary(mod1)


## Ajuste do modelo
par(mfrow=c(2,2))
plot(mod1)
par(mfrow=c(1,1))

