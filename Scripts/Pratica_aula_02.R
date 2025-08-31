####################################
#                                  #
#         Aula prática 02          #
#     - Métodos de Estimação -     #
#                                  #
####################################

#~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)




#~~~~~~~~~~~~~~~~~~~~~~~
# 1) Importando os dados
#~~~~~~~~~~~~~~~~~~~~~~~
dados <- read.csv("Dados/palmerpenguins_extended.csv")



#~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Métodos de estimação
#~~~~~~~~~~~~~~~~~~~~~~~~~

# 2.1) Método dos mínimos quadrados 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




# 2.2) Método de Máxima Verossimilhança
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 2.2.1) Ideias gerais
#~~~~~~~~~~~~~~~~~~~~~~~

## Histograma com curva de densidade ##
ggplot(data=dados, aes(body_mass_g)) + 
  geom_histogram(aes(y = ..density..),fill="darkorange", color="white", alpha=0.6) +
  geom_density(lwd=0.8, col = 'gray20') +
  xlab("Peso (g)") + ylab("Densidade") +
  theme_bw() 



## Qual a média e desvio padrão dos dados? ##
mean(dados$body_mass_g)
sd(dados$body_mass_g)


## Calculando a verossimilhança manualmente ##
# Aqui vamos calcular a verossimilhança assumindo a média e o desivo padrão dos dados.
# A interpretação, portanto, se dará sob a forma de:
# Dado uma distribuição Normal com parâmetros mu = mean(dados$body_mass_g) e sigma = sd(dados$body_mass_g),
# qual a verossimilhança de observar um pinguim com peso x?

## Calculando a média e desvio padrão dos dados ##
mu_obs <- mean(dados$body_mass_g)
sd_obs <- sd(dados$body_mass_g)


# Vamos calcular os valores de verossimilhança para os seguintes pinguins:
# pinguim 1: 2500 g (linha azul)
# pinguim 2: 4000 g (linha vermelha)
# pinguim 3: 9000 g (linha verde)

ggplot(data=dados, aes(body_mass_g)) + 
  geom_histogram(aes(y = ..density..),fill="darkorange", color="white", alpha=0.6) +
  geom_density(lwd=0.8, col = 'gray20') +
  geom_vline(xintercept=2500,colour='cyan3',lwd=1) +
  geom_vline(xintercept=4000,colour='red',lwd=1) +
  geom_vline(xintercept=9000,colour='forestgreen',lwd=1) +
  xlab("Peso (g)") + ylab("Densidade") +
  theme_bw() 


# para calulcar, pode-se usar a função 'dnorm' conforme:

pinguim_1 <- dnorm(2500, mean = mu_obs, sd = sd_obs)
pinguim_2 <- dnorm(4000, mean = mu_obs, sd = sd_obs)
pinguim_3 <- dnorm(9000, mean = mu_obs, sd = sd_obs)

rbind(pinguim_1, pinguim_2, pinguim_3)


# Precisamos repetir o passo anterior para todos os n pinguins amostrados para, em sequência,
# calcular a verossimilhança dos dados observados dado os parâmetros especificados.
# Lembrando que para minimizar (maximizar) a função L(), é necessário logaritimizar
# a função Normal e me sequência somar os valores de verossimilhança que foram calculados
# para cada pinguim.


sum(dnorm(dados$body_mass_g, mean = mu_obs, sd = sd_obs, log = T))


# Lembrando que o valor que obtemos é referente à uma distribuição Normal cujos
# parâmetros do modelo estão centrados em cima dos parâmetros populacionais.
# De acordo com o método MLE, precisamos achar o conjunto de parâmetros que
# resulta no menor valor de log-verossimilhança. Precisamos, por tanto, testar
# esse conjunto de valores usando uma função de otimização, conforme destacado na sequência



# 2.2.2) Estimando manualmente
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A estimação "manual" dos parâmetros é dividida em duas etapas:
# 1) Definição da função de verossimilhança (no caso, a função logaritimizada e negativa)
# 2) Otimização da função (i.e., achar o conjunto de parâmetros que resulta no menor valor de log-verossimilhança)

## 1) Função de log-verossimilhança negativa (joint neg. log-likelihood) ##
fun_lvn <- function(parametros, dados) {
  
  ## Definindo os parametros
  mu <- parametros[1]
  sd <- exp(parametros[2])  # garante que sd > 0
  
  ## Calculando a verossimilhanca (para cada amostragem/observacao)
  vetor_logv <- dnorm(dados, mean = mu, sd = sd, log = TRUE)
  lvn <- -sum(vetor_logv) #somando as verossimilhanças 
  
  return(lvn)
}



## 2) Otimização da função ##

### Valores iniciais para mu e sd
mu_init <- mean(dados$body_mass_g, na.rm = TRUE)
sd_init <- log(sd(dados$body_mass_g, na.rm = TRUE))


estim_manual <- optim(par = c(mu = mu_init, sd = sd_init),
                      fn = fun_lvn,
                      dados = dados$body_mass_g,
                      method = "BFGS",
                      hessian = TRUE)

names(estim_manual)

estim_manual$par #
estim_manual$value #valor de log-verossimilhança negativa


### Calculando erros padrões
sqrt(diag(solve(estim_manual$hessian))) #solve: retorna matriz de covarianca; diag: retorna os valores diagonais, ie., variancas; sqrt - converte as variancias em desvios padroes



# 2.2.2) Estimando via lm/glm
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### LM ###
estim_lm <- lm(dados$body_mass_g ~ 1)
summary(estim_lm)

