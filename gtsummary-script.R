# ------------------------------------------------------------ #
#                                                              #
#               Visualizacao de dados - Tabelas                #
#                                                              #
# ------------------------------------------------------------ #

# ------------------------------------------------------------------------------
# ---------- 1. Instalando e carregando pacotes --------------------------------
# ------------------------------------------------------------------------------

# install.packages("gtsummary") # tabelas
# install.packages("tidyverse") # funcoes extras
library(gtsummary)
library(tidyverse)


# ------------------------------------------------------------------------------
# ---------- 2. Banco de dados -------------------------------------------------
# ------------------------------------------------------------------------------

?mtcars # conhecendo as variaveis pela documentacao do banco mtcars
banco <- mtcars # vou usar mtcars como teste
str(banco) # estrutura do banco
summary(mtcars) # estatisticas descritivas

# quais variaveis sao numericas/categoricas? (isso eh uma frescura que pode ser ignorada)

num_cat <- sapply(banco, function(x) if_else(length(unique(x)) >= 5,
                                             "numerica", "categorica")) # para cada coluna do banco, ver se a quantidade de valores unicos (unique) eh maior/menor que 5. Se maior, a variavel eh numerica. Se menor, categorica
num_cat

# colocando categoricas como factor ---------------------------------------

unique_cyl <- sort(unique(banco$cyl)); unique_cyl
banco$cyl <- factor(banco$cyl, levels = c(4, 6, 8), labels = paste(unique_cyl, "Cilíndros"))

unique_vs <- sort(unique(banco$vs)); unique_vs
banco$vs <- factor(banco$vs, levels = c(0, 1), labels = c("Motor V", "Motor straight"))

unique_am <- sort(unique(banco$am)); unique_am
banco$am <- factor(banco$am, levels = c(0, 1), labels = c("Autom.", "Manual"))

unique_gear <- sort(unique(banco$gear)); unique_gear
banco$gear <- factor(banco$gear, levels = c(3, 4, 5), labels = paste(unique_gear, "Marchas"))



# selecionando apenas algumas variaveis -----------------------------------

banco <- banco %>% select(mpg, cyl, hp, am, qsec)


# ------------------------------------------------------------------------------
# ---------- 3. gtsummary -------------------------------------------------
# ------------------------------------------------------------------------------


# tbl_summary -------------------------------------------------------------
# eh a funcao principal do pacote. Faz estatisticas descritivas das variaveis

banco %>% tbl_summary()

## label
# modifica os nomes das variaveis

### Ex 1:
banco %>%
  tbl_summary(label = mpg ~ "Milhas por galão")

### Ex 2:
banco %>%
  tbl_summary(label = list(mpg ~ "Milhas por galão",
                           cyl ~ "Cilíndros"))

### Ex 3:
nomes <- list(mpg ~ "Milhas por galão",
              cyl ~ "Cilíndros",
              hp ~ "Cavalos")
banco %>%
  tbl_summary(label = nomes)


## statistic
# modifica a estatistica usada (media, mediana, n, ...)

### Ex 1:
banco %>%
  tbl_summary()

### Ex 2:
banco %>%
  tbl_summary(statistic = mpg ~ "{mean} ({sd})")

### Ex 3: 
banco %>%
  tbl_summary(statistic = list(mpg ~ "{mean} ({sd})",
                               cyl ~ "{n}/{N} ({p}%)"))

### Ex 4:
banco %>%
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n}/{N} ({p}%)"))


## digits
# modifica o numero de digitos

### Ex:
banco %>%
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n}/{N} ({p}%)"),
              digits = list(all_continuous() ~ 1,
                            all_categorical() ~ c(0, 0, 1))) # fiz isso para colocar so as freq. relativas com 1 casa decimal


## by
# estratifica a tabela por uma variavel categorica

### Ex:
banco %>%
  tbl_summary(by = cyl)
