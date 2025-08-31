
###################################
#                                 #
#          - Figuras -            #
#            Aula 01              #
#                                 #
###################################

library(ggplot2)
library(dplyr)
library(patchwork)


# Definindo o diretório base
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("~/Library/CloudStorage/OneDrive-Personal/Arbeit/Lectures_and_talks/UFRN/Lectures/ECL0072-GLM_GAM/")


# Importando o banco de dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dados <- read.csv("Dados/palmerpenguins_extended.csv")





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Scatter plot & histograma pinguins  #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## Scatter plot
p1 <- dados %>%
  slice_head(n=100) %>%
  ggplot(aes(y = body_mass_g, x = bill_length_mm)) +
  geom_point(size = 3.5, col ='darkorange', alpha = 0.5) +
  geom_smooth(method = 'lm', col = 'gray50', fill = 'gray80') +
  theme_classic(base_size = 16) +
  labs(x = "Tamanho do bico (mm)", y = "Peso (g)") +
  theme(axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -3),
        plot.margin = margin(20,20,20,20))


## Histograma
p2 <- dados %>%
  slice_head(n=100) %>%
  ggplot(aes(x = body_mass_g)) +
  geom_histogram(bins = 13, size=1.2,col = 'white', fill = 'darkorange',alpha = 0.5) +
  #geom_histogram(bins = 13, size=1.2,col = 'white', fill = 'darkorange',alpha = 0.5) +
  #geom_density(size=1, col = 'gray40') +
  theme_classic(base_size = 16) +
  labs(y = "Frequência", x = "Peso (g)") +
  theme(axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -3),
        plot.margin = margin(20,20,20,20))

pf <- (p1 + p2)

ggsave(plot = pf,
       filename ="Figuras/relacao_y_x_variabilidade.png", 
       width = 9,
       height = 4,
       dpi = 350)


#################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Exemplos de correlação   #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## Para garantir reprodutibilidade dos resultados
set.seed(987)


## Simulando os dados
x <- rnorm(100)
y_pos <- x + rnorm(100, sd = 0.5)
y_neg <- -x + rnorm(100, sd = 0.5)
y_sem <- rnorm(100)

## Salvando tudo em um data frame
df <- data.frame(x = x,
                 y_pos = y_pos,
                 y_neg = y_neg,
                 y_sem = y_sem)


## Transformando do formato largo para longo (para usar no ggplot)
dfl <- tidyr::pivot_longer(data = df, 
                           cols=y_pos:y_sem,
                           names_to = "Correlacao",
                           values_to = "y")

## Reclassificando a coluna 'Correlação' para melhorar visualização
dfl$Relacoes <- ifelse(dfl$Correlacao == "y_pos", "Positiva",
                       ifelse(dfl$Correlacao == "y_neg", "Negativa","Ausente"))

dfl$Relacoes <- factor(dfl$Relacoes, levels = c("Positiva", "Negativa", "Ausente"))

cp <- ggplot(dfl, aes(x = x, y = y)) +
      geom_point(alpha = 0.5, size = 3, colour = "darkorange") +
      geom_smooth(method = "lm", 
                  se = TRUE, 
                  col = 'gray50',
                  fill = 'gray80') +
      facet_wrap(Relacoes ~.) +
      theme_bw(base_size = 16) +
      theme(axis.title.y = element_text(vjust = 0.5, angle = 360),
            axis.title.x = element_text(vjust = -3),
            plot.margin = margin(20,20,20,20),
            panel.grid = element_blank(),
            strip.background = element_rect(fill = "white", linewidth = 1))
      

ggsave(plot = cp,
       filename ="Figuras/tipos_de_correlacoes.png", 
       width = 8,
       height = 4,
       dpi = 350)

    

#################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Exemplos de Francis Galton     #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## Carregando os dados
df <- read.csv("~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/ECL0072-GLM_GAM/Dados/Galton_Family_Heights.csv")

## Calculando altura média dos pais
df$Parent_height <- (df$Father_height + (1.08*df$Mother_height))/2 # O valor de 1.08 representa a razão de mulheres/homens, conforme calculado por Galton baseado em sua amostra

## Convertendo df para o formato longo
df2 <- pivot_longer(data=df, 
                    cols = c(Child_height,Parent_height),
                    names_to = "Familia",
                    values_to = "Alturas")


## Categorizando a família (para vias de ilustração)
df2$Familia <- as.factor(df2$Familia)
#df2$Familia2 <- ifelse(df2$Familia == "Child_height", "Criança", "Pais")
df2$Familia2 <- ifelse(df2$Familia == "Child_height", "Crianças", "Pais")
df2$Familia2 <- as.factor(df2$Familia2)


## Calculando alturas médias
# alturas_medias <- df2 %>%
#   group_by(Familia2) %>%
#   summarise(media_altura = mean(Alturas, na.rm = TRUE))
# 

## Plotando o gráfico
pg <- df2 %>%
      mutate(Familia2 = factor(Familia2, levels = c("Pais", "Crianças"))) %>%
      ggplot(aes(x = 2.54*Alturas, fill = Familia2)) +
      geom_density(position = "identity", alpha = 0.5,
                   col = 'white', linewidth = 0.7) +
      # geom_histogram(position = "identity", alpha = 0.5, bins = 20,
      #                col = 'white') +
      #facet_wrap(Familia2 ~ .) +
      scale_fill_manual(values =c("darkorange", "cyan4")) +
      ggpubr::theme_pubr(base_size = 16) +
      labs(x = "Altura (cm)") +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y.left = element_blank(),
            axis.line.y.left = element_blank(),
            axis.title.x = element_text(vjust = -3),
            legend.title = element_blank(),
            plot.margin = margin(20,20,20,20))


ggsave(plot = pg,
       filename ="Figuras/Exemplo_Gauss_alturas.png", 
       width = 5,
       height = 4,
       dpi = 350)


#################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  Exemplos de regressão linear   #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# Simulando dados não lineares
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Baseado em uma regressão polinomial de 3o grau (função cúbica)

set.seed(123) # Para garantir a reprodutibilidade

n <- 70
x <- seq(-3, 3, length.out = n)
y <- 1 + 0.5*x - 1.2*x^2 + 0.3*x^3 + rnorm(n, sd = 2)

data <- data.frame(x, y)

## regressão linear
p1 <- data %>%
      slice_head(n=100) %>%
      ggplot(aes(y = y, x = x)) +
      geom_point(size = 3.5, col ='darkorange', alpha = 0.5) +
      geom_smooth(method = 'lm', 
                  formula = y~x,
                  col = 'gray50',
                  fill = 'gray80') +
      theme_classic(base_size = 16) +
      labs(title = "Regressão linear", subtitle = expression(y[i] == beta[0] + beta[1]*x[i] + epsilon[i])) +
      theme(axis.title.y = element_text(vjust = 3),
            axis.title.x = element_text(vjust = -3),
            plot.title = element_text(hjust = 0.5, face='bold'),
            plot.subtitle = element_text(hjust = 0.5),
            plot.margin = margin(20,20,20,20))



## regressão polinomial
p2 <- data %>%
  slice_head(n=100) %>%
  ggplot(aes(y = y, x = x)) +
  geom_point(size = 3.5, col ='darkorange', alpha = 0.5) +
  geom_smooth(method = 'lm', 
              formula = y ~ poly(x, 3, raw=TRUE),
              col = 'gray50', 
              fill = 'gray80') +
  theme_classic(base_size = 16) +
  labs(title = "Regressão polinomial", subtitle = expression(y[i] == beta[0] + beta[1]*x[i] + beta[2]*x[i]^2 + beta[3]*x[i]^3 + epsilon[i])) +
  theme(axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -3),
        plot.title = element_text(hjust = 0.5, face='bold'),
        plot.subtitle = element_text(hjust = 0.5),
        plot.margin = margin(20,20,20,20))


# Combine them
full_p <- (p1 / p2 ) 

ggsave(plot = full_p,
       filename ="Figuras/exemplo_regressoes.png", 
       width = 5.5,
       height = 10,
       dpi = 350)


#################################################

#~~~~~~~~~~~~~~~~~#
# Desvio residual #
#~~~~~~~~~~~~~~~~~#


set.seed(123)

## Selecionar 100 amostragens para ajustar o modelo linear
dados_100 <- dados %>%
  slice_head(n = 100) %>%
  mutate(
    pred = predict(lm(body_mass_g ~ bill_length_mm, data = .)),
    id = row_number(),
    grupo = if_else(id %in% sample(id, 10), "highlight", "fade")
  )


## Figura
ggplot(dados_100, aes(x = bill_length_mm, y = body_mass_g)) +
  # Plot all points with appropriate alpha
  geom_point(aes(alpha = grupo), size = 3.5, color = 'darkorange') +
  
  # Plot residual segments ONLY for highlighted points
  geom_segment(
    data = filter(dados_100, grupo == "highlight"),
    aes(xend = bill_length_mm, yend = pred),
    color = "black", size = 0.6
  ) +
  
  
# Regression line with deviations
geom_smooth(method = 'lm', color = 'gray50', fill = 'gray80') +
  
  # Transparency levels
  scale_alpha_manual(values = c("highlight" = 1, "fade" = 0.20)) +
  
  # Theme and labels
  theme_classic(base_size = 16) +
  labs(x = "Tamanho do bico (mm)", y = "Peso (g)") +
  theme(
    axis.title.y = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -3),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "none"
  )


#################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Exemplos dos pressupostos #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# Independência residual
#~~~~~~~~~~~~~~~~~~~~~~~~~~


# Tamanho amostral
set.seed(321)
n <- 200
time <- 1:n


## Violação do pressuposto ##

# Simulando um processo AR(1) 
phi <- 0.8 #parâmetro de correlação. Varia de 0 a 1; quanto maior valor, maior dependência residual
ar_errors <- rep(0, n)
white_noise <- rnorm(n, mean = 0, sd = 5)

for (t in 2:n) {
  ar_errors[t] <- phi * ar_errors[t-1] + white_noise[t]
}

# Criando a serie temporal
y <- 5 + 0.5 * time + ar_errors

# Ajustando o modelo linear
mod <- lm(y ~ time)

# Calculando valores de ACF
acf_data <- acf(residuals(mod), lag.max = 20, plot = FALSE)
acf_df <- with(acf_data, data.frame(lag, acf)) # convertendo para df

# Calculando intervalo de confiança
ci <- qnorm(0.975) / sqrt(n)

# Figura
acf_p1 <- ggplot(data = acf_df, aes(x = lag, y = acf)) +
        # Add the ACF bars
        geom_segment(aes(xend = lag, yend = 0), color = "cyan4", size = 1.2) +
        # Add the horizontal zero line
        geom_hline(yintercept = 0, color = "cyan4", size = 0.9) +
        # Add the confidence interval lines (dashed blue)
        geom_hline(yintercept = ci, linetype = "dashed", color = "darkorange", size = 0.9) +
        geom_hline(yintercept = -ci, linetype = "dashed", color = "darkorange", size = 0.9) +
        # Customize labels and title
        labs(
          title = "Com dependência residual",
          x = "Lag",
          y = "ACF"
        ) +
        # Set the theme for a clean look
        theme_classic(base_size = 16) +
        theme(
          axis.title.y = element_text(vjust = 3),
          axis.title.x = element_text(vjust = -3),
          plot.margin = margin(20, 20, 20, 20),
          plot.title = element_text(hjust = 0.5, face='bold'),
          legend.position = "none"
        )


## Pressuposto adequado ##

# Criando o modelo simples
set.seed(123)
y2 <- 5 + 0.5 * time + rnorm(n, mean = 0, sd = 10)

# Ajustando o modelo
mod2 <- lm(y2 ~ time)


# Calculando valores de ACF
acf_data2 <- acf(residuals(mod2), lag.max = 20, plot = FALSE)
acf_df2 <- with(acf_data2, data.frame(lag, acf)) # convertendo para df

# Calculando intervalo de confiança
ci2 <- qnorm(0.975) / sqrt(n)

# Figura
acf_p2 <- ggplot(data = acf_df2, aes(x = lag, y = acf)) +
  # Add the ACF bars
  geom_segment(aes(xend = lag, yend = 0), color = "cyan4", size = 1.2) +
  # Add the horizontal zero line
  geom_hline(yintercept = 0, color = "cyan4", size = 0.9) +
  # Add the confidence interval lines (dashed blue)
  geom_hline(yintercept = ci, linetype = "dashed", color = "darkorange", size = 0.9) +
  geom_hline(yintercept = -ci, linetype = "dashed", color = "darkorange", size = 0.9) +
  # Customize labels and title
  labs(
    title = "Sem dependência residual",
    x = "Lag",
    y = "ACF"
  ) +
  # Set the theme for a clean look
  theme_classic(base_size = 16) +
  theme(
    axis.title.y = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -3),
    plot.title = element_text(hjust = 0.5, face='bold'),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "none"
  )

full_p2 <- (acf_p2 + acf_p1)

ggsave(plot = full_p2,
       filename ="Figuras/acf_plots.png", 
       width = 10,
       height = 5,
       dpi = 350)



# Homocedasticidade residual
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### Simulando os dados
set.seed(42)
n <- 100

x <- runif(n, 0, 10) #variável preditora


## Pressuposto é adequado

epsilon_bom <- rnorm(n, mean = 0, sd = 1) #Simulando a variável resposta
y_bom <- 5 + 2 * x + epsilon_bom #Equação da reta
mod_bom <- lm(y_bom ~ x) #Ajustando o modelo

df_bom <- data.frame(Predito = fitted(mod_bom), 
                    Residuo = residuals(mod_bom)) #Criando o df

# Gráfico valores preditos vs. residuos
homo_p1 <- ggplot(df_bom, aes(x = Predito, y = Residuo)) +
            geom_point(size = 3.5, col ='darkorange', alpha = 0.5) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "cyan4", size = 0.9) +
            labs(title = "Homocedasticidade") +
            theme_classic(base_size = 16) +
            theme(
              axis.title.y = element_text(vjust = 3),
              axis.title.x = element_text(vjust = -3),
              plot.title = element_text(hjust = 0.5, face='bold'),
              plot.margin = margin(20, 20, 20, 20),
              legend.position = "none"
            )



## Pressuposto é violado
epsilon_ruim <- rnorm(n, mean = 0, sd = 0.5 * x)
y_ruim <- 5 + 2 * x + epsilon_ruim


mod_ruim <-lm(y_ruim ~ x) #Ajustando o modelo

df_ruim <- data.frame(Predito = fitted(mod_ruim), 
                     Residuo = residuals(mod_ruim)) #Criando o df


# Gráfico valores preditos vs. residuos
homo_p2 <- ggplot(df_ruim, aes(x = Predito, y = Residuo)) +
  geom_point(size = 3.5, col ='darkorange', alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "cyan4", size = 0.9) +
  labs(title = "Heterocedasticidade") +
  theme_classic(base_size = 16) +
  theme(
    axis.title.y = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -3),
    plot.title = element_text(hjust = 0.5, face='bold'),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "none"
  )


full_p3 <- (homo_p1 + homo_p2)
  
ggsave(plot = full_p3,
       filename ="Figuras/heterocedasticidade_plots.png", 
       width = 10,
       height = 5,
       dpi = 350)


# Normalidade residual
#~~~~~~~~~~~~~~~~~~~~~~

## Simulando um dado não-normal
set.seed(333)
epsilon_ruim_norm <- rchisq(n, df = 1) - 20 # mean of chi-squared with df=3 is 3
y_ruim_norm <- 5 + 2 * x + epsilon_ruim_norm

# Ajustando o modelo
mod_ruim_norm <- lm(y_ruim_norm ~ x)

# Criando o df
df_ruim_norm <- data.frame(Predito = fitted(mod_ruim_norm), 
                           Residuo = residuals(mod_ruim_norm)) #Criando o df


## Histograma (violação de normalidade)
hist_p1 <- ggplot(df_ruim_norm, aes(x = Residuo)) +
geom_histogram(aes(y = after_stat(density)), bins = 15, size=1.2,col = 'white', fill = 'darkorange',alpha = 0.5) +
  theme_classic(base_size = 16) +
  labs(y = "Densidade", x = "Resíduo", title = "Violação de normalidade") +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))+
theme(  axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -3),
        plot.title = element_text(hjust = 0.5, face='bold'),
        plot.margin = margin(20,20,20,20))


## QQplot (violação de normalidade)
qq_p1 <- ggplot(df_ruim_norm, aes(sample = Residuo)) +
  stat_qq(size = 3.5, col ='darkorange', alpha = 0.5) +
  stat_qq_line(linetype = "dashed", color = "cyan4", size = 0.9) +
  labs(y ="Quantis observados", x = "Quantis teóricos") +
  theme_classic(base_size = 16) +
  theme(axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -3),
        plot.title = element_text(hjust = 0.5, face='bold'),
        plot.margin = margin(20,20,20,20))


## Histograma (Normalidade)
hist_p2 <- ggplot(df_bom, aes(x = Residuo)) +
  geom_histogram(aes(y = after_stat(density)), bins = 15, size=1.2,col = 'white', fill = 'darkorange',alpha = 0.5) +
  theme_classic(base_size = 16) +
  labs(y = "Densidade", x = "Resíduo", title = "Normalidade") +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))+
  theme(  axis.title.y = element_text(vjust = 3),
          axis.title.x = element_text(vjust = -3),
          plot.title = element_text(hjust = 0.5, face='bold'),
          plot.margin = margin(20,20,20,20))


## QQplot (normalidade)
qq_p2 <- ggplot(df_bom, aes(sample = Residuo)) +
  stat_qq(size = 3.5, col ='darkorange', alpha = 0.5) +
  stat_qq_line(linetype = "dashed", color = "cyan4", size = 0.9) +
  labs(y ="Quantis observados", x = "Quantis teóricos") +
  theme_classic(base_size = 16) +
  theme(axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -3),
        plot.title = element_text(hjust = 0.5, face='bold'),
        plot.margin = margin(20,20,20,20))



full_p4 <- (hist_p2 + hist_p1) / (qq_p2 + qq_p1)
full_p4
ggsave(plot = full_p4,
       filename ="Figuras/normalidade_plots.png", 
       width = 8,
       height = 6,
       dpi = 350)

