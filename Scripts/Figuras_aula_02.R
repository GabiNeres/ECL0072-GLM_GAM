#                                 #
#          - Figuras -            #
#            Aula 02              #
#                                 #
###################################



# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
library(dplyr)


# Definindo o diretório base
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/ECL0072-GLM_GAM/")




#~~~~~~~~~~~~~~~~~~~~~~~~~#
# Modelo linear ajustado  #
#~~~~~~~~~~~~~~~~~~~~~~~~~#
dados <- read.csv("~/Library/CloudStorage/OneDrive-Personal/Arbeit/Lectures_and_talks/UFRN/Lectures/ECL0072-GLM_GAM/Dados/palmerpenguins_extended.csv")

p1 <- dados %>%
      ggplot(aes(y = body_mass_g, x = bill_length_mm)) +
      geom_point(size = 3.5, col ='darkorange', alpha = 0.2) +
      geom_smooth(method = 'lm', col = 'gray20', fill = 'gray40', se = TRUE) +
      theme_classic(base_size = 16) +
      labs(
        x = "Tamanho do bico (mm)",
        y = "Peso (g)",
        title = expression(Peso[i] == 2299.7 + 65.8 * Bico[i])) +
      theme(axis.title.y = element_text(vjust = 3),
            axis.title.x = element_text(vjust = -3),
            plot.title = element_text(hjust = 0.5),
            plot.margin = margin(20, 20, 20, 20))


ggsave(plot = p1,
       filename ="Figuras/LM_ajustado.png", 
       width = 8,
       height = 5,
       dpi = 350)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Curvas de distiribuição Normal  #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## Criando a variável x (comum a todos os y's simulados)
x <- seq(-5, 5, length.out = 200)


## Simulando a variável y (com diferentes parâmetros)
set.seed(452)
df <- rbind(
  data.frame(x = x, y = dnorm(x, mean = 0, sd = 0.5), 
             dist = "mu==0 ~ ',' ~ sigma^2==0.5"),
  data.frame(x = x, y = dnorm(x, mean = -2, sd = 0.5), 
             dist = "mu==-2 ~ ',' ~ sigma^2==0.5"),
  data.frame(x = x, y = dnorm(x, mean = 0, sd = 2), 
             dist = "mu==0 ~ ',' ~ sigma^2==2")
)

## Convertendo para fator 
df$dist <- factor(df$dist, 
                  levels = c("mu==-2 ~ ',' ~ sigma^2==0.5",
                             "mu==0 ~ ',' ~ sigma^2==0.5",
                             "mu==0 ~ ',' ~ sigma^2==2"))

## Figura
p2 <- ggplot(df, aes(x = x, y = y, colour = dist)) +
  geom_line(size = 1.2) +
  scale_color_manual(
    values = c("cyan4", "gray50", "darkorange"),
    labels = c(expression(mu==-2 ~ "," ~ sigma^2==0.5),
               expression(mu==0 ~ "," ~ sigma^2==0.5),
               expression(mu==0 ~ "," ~ sigma^2==2))
  ) +
  labs(x = "x", y = "Densidade", color = "Parâmetros") +
  theme_classic(base_size = 14) +
  theme(
    axis.title.y = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -3),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = c(0.8, 0.8),         # Inside plot (x, y) coords
    legend.background = element_rect(fill = alpha("white", 0.7), colour = NA))

ggsave(plot = p2,
       filename ="Figuras/Dist_normal.png", 
       width = 8,
       height = 4,
       dpi = 350)



###################################
#    Histograma peso pinguins     #
###################################
#dados <- read.csv("~/Library/CloudStorage/OneDrive-Personal/Arbeit/Lectures_and_talks/UFRN/Lectures/ECL0072-GLM_GAM/Dados/palmerpenguins_extended.csv")

## Histograma
ph <- dados %>%
  ggplot(aes(x = body_mass_g)) +
  geom_histogram(bins = 13, size=1.2, col = 'white', fill = 'darkorange',alpha = 0.5) +
  #geom_histogram(bins = 13, size=1.2,col = 'white', fill = 'darkorange',alpha = 0.5) +
  #geom_density(size=1, col = 'gray40') +
  theme_classic(base_size = 16) +
  labs(y = "Frequência", x = "Peso (g)") +
  theme(axis.title.y = element_text(vjust = 3),
        axis.title.x = element_text(vjust = -3),
        plot.margin = margin(20,20,20,20))


ggsave(plot = ph,
       filename ="Figuras/histograma_pinguins.png", 
       width = 5,
       height = 4,
       dpi = 350)


