# ECL0072 (2025-2)

<p align="center">
<img src="Figuras/detetive_ecologico.png" alt="Figuras/detetive_ecologico.png" width="400"/>
</p>


## Tópicos Especiais em Ecologia - Modelos Lineares Generalizados e Aditivos para Dados Ecológicos

Curso ministrado para o Programa de Pós-Graduação em Ecologia (PPGECO) na UFRN, durante o segundo semestre de 2025.

Este repositório contém todo o material de aula e os arquivos necessários para gerar a página da disciplina.



### Para acessar o conteúdo do curso

A página do curso é toda construída usando apenas o [R Markdown][] e, por isso, o código fonte pode ser acessado nos arquivos `Rmd`. Para gerar o site você precisará
das versões mais recentes dos pacotes `rmarkdown` e `knitr`.

1. Copie (ou fork) esse repositório
2. Apague o diretório `site_libs/`
3. Abra o R nesse diretório, carregue os pacotes e renderize o site com
   `render_site()`
   
```r

# Atualizando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~~
update.packages(knitr)
update.packages(rmarkdown)


# Compila o site da disciplina
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(knitr)
library(rmarkdown)
render_site()
```

### Licença

O conteúdo deste repositório, das páginas, e do material da disciplina
está está disponível por meio da [Licença Creative Commons 4.0][]
(Atribuição/NãoComercial/PartilhaIgual).

![Licença Creative Commons 4.0](img/CC_by-nc-sa_88x31.png)


[Licença Creative Commons 4.0]: https://creativecommons.org/licenses/by-nc-sa/4.0/deed.pt_BR
[R Markdown]: http://rmarkdown.rstudio.com
