#                                 #
#          - Figuras -            #
#            Aula 07              #
#                                 #
###################################


# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)


# Definindo o diretório base
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/ECL0072-GLM_GAM/")



######################
# Distribuição beta #
#####################


## Sequência de valores entre 0 e 1 (x 100)
x <- seq(0, 1, length.out = 100)


## Lista com diferentes combinações de parâmetros
parametros <- list(c(1, 1), 
                   c(2, 2),
                   c(2, 5),
                   c(5, 2))


## Dataframe com densidades
dados <- data.frame()

for (p in parametros) {
  #print(p)
  alfa <- p[1]; beta <- p[2]
  dens <- dbeta(x, alfa, beta)
  dados <- rbind(dados,
                 data.frame(x = x,
                            densidade = dens,
                            parametro = paste0("α=", alfa, "; β=", beta)))
}



# Plot
p1 <- ggplot(dados, aes(x = x, y = densidade, color = parametro)) +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = c('gray50', "#bf4342","#ee9b00","#0a9396" )) +
      labs(title = "",
           x = "x",
           y = "Densidade",
           color = "Parâmetro") +
      theme_minimal(base_size = 16) 


ggsave(plot = p1,
       filename ="Figuras/Distribuicao_Beta.png", 
       width = 10,
       height = 4,
       dpi = 350)




##########################
# Dado zero-inflacionado #
##########################

dados <- readRDS("Dados/plant_herbivore.rds")


tmp <- as.data.frame(table(dados$Damaged))

p2 <- ggplot(tmp, aes(x=Var1, y = Freq)) + 
      geom_bar(stat="identity", fill='darkorange',width = 0.5) +
      #geom_rug(alpha = 0.5) +
      labs(y = "Frequência", 
           x = "Número de cabeças de flores predadas") +
      scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(breaks = seq(from = 0,to = max(tmp$Freq),by = 2)) +
      ggpubr::theme_pubr(base_size = 16) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

p2

# p2 <- ggplot(dados, aes(x =Damaged)) +
#   geom_histogram(bins = 20, 
#                  fill = 'darkorange', 
#                  col = 'white', 
#                  alpha = 0.7) +
#   labs(y = "Frequência", 
#        x = "Número de cabeças de flores predadas") +
#   theme_minimal(base_size = 16)

ggsave(plot = p2,
       filename ="Figuras/Dado_zeroinflacionado.png", 
       width = 12,
       height = 4,
       dpi = 350)
