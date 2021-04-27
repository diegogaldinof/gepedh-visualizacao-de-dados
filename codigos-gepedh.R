#Carregar os pacotes necessarios
library(readxl)
library(dplyr)
library(gtsummary)

#Carregar o banco de dados
dados <- read_excel("banco-gepedh.xlsx")


#Categorizar variaveis
dados$parte_selecao = factor(dados$parte_selecao, levels = 1:2, labels = c("Sim","Não"))


dados <- dados %>% mutate(ranking = case_when(ranking <= 3 ~ "Medalhista",
                                              ranking >= 4 ~ "Não Medalhista"))


###Selecionando as variáveis que vão aparecer na tabela
dados <- dados %>% dplyr::select(idade:ranking, p_harmoniosa, p_obsessiva, criterio_p)









#####################################################################################
###############################   EXERCICIO 1   #####################################
#####################################################################################

#EXERCICIO 1.1 CRIAR UMA TABELA DESCRITIVA DO BANCO-GEPEDH

#Utilizar a funcao tbl_summary
banco_de_dados %>% tbl_summary()


#EXERCICIO 1.2 CRIE UMA LISTA E ALTERE O NOME DAS VARIÁVEIS

###Crie uma lista "nomes_variaveis" e altere o nome das variáveis
# idade = Idade
# tempo_pratica = Tempo de prática
# parte_selecao = Fez parte da seleção
# ranking = Ranking
# p_harmoniosa = Paixão Harmoniosa
# p_obsessiva = Paixão Obsessiva
# criterio_p = Critério da Paixão

variaveis <- list(idade ~ "Idade",
                  tempo_pratica ~ "Tempo de prática",
                  ...)



#EXERCICIO 1.3 CRIE UMA TABELA DESCRITIVA UTILIZANDO AS VARIAVEIS RENOEMADAS

#Utilize a list "nomes_variaveis"
#Utilize o argumento "label"

banco_dados %>%
  tbl_summary(label = ) 








#####################################################################################
###############################   EXERCICIO 2   #####################################
#####################################################################################

#EXERCICIO 2.1 Crie uma tabela descritiva com média e desvio padrão 
#para variáveis contínuas e n amostral para variáveis categóricas

# Utilize o argumento statistic;
# Utilize a função list();
# Lembre-se da função all_continuous() e all_categorical();
# Lembre-se: média (mean), desvio padrão (sd) e n amostral (n)

banco_dados %>%
  tbl_summary(label = ,
              statistic = list( ))
                        











#####################################################################################
###############################   EXERCICIO 3   #####################################
#####################################################################################

# EXERCICIO 3.1 Faça uma tabela em função da variável categórica Ranking:

# Utilize o argumento by

banco_dados %>%
  tbl_summary(by = ) #código simples


banco %>%
  tbl_summary(label = nomes_variaveis,
              statistic = list(all_categorical() ~ "{n}",
                               all_continuous() ~ "{mean} ({sd})"),
              by = ) #código do exercicio anterior, só inserindo o by











#####################################################################################
###############################   EXERCICIO 4   #####################################
#####################################################################################

#EXERCICIO 4.1 Crie uma tabela de comparação em função da variável PARTE_SELEÇÃO:

# Utilize o argumento by na função tbl_summary();
# Utilize a função add_p().

banco_dados %>%
  tbl_summary(by = ) %>% #código simples


banco_dados %>%
  tbl_summary(label = nomes_variaveis,
              statistic = list(all_categorical() ~ "{n}",
                               all_continuous() ~ "{mean} ({sd})"),
              by = ) %>% 







#EXERCICIO 4.2 Modifique o teste de comparação das variáveis contínuas para teste t:

# Lembre-se da função all_continuous()

banco_dados %>%
  tbl_summary(by = parte_selecao) %>%
  add_p(test = )#codigo simples



dados %>%
  tbl_summary(label = nomes_variaveis,
              statistic = list(all_categorical() ~ "{n}",
                               all_continuous() ~ "{mean} ({sd})"),
              by = parte_selecao) %>% 
  add_p(test = ) #usando o código anterior com variáveis renomeadas














## exemplo de tabela de comparação
dados2 <- dplyr::select(dados, parte_selecao, p_harmoniosa, p_obsessiva, criterio_p)

nomes_variaveis2 <- list(p_harmoniosa ~ "Paixão Harmoniosa",
                         p_obsessiva ~ "Paixão Obsessiva",
                         criterio_p ~ "Critério da Paixão")

dados2 %>%
  tbl_summary(label = nomes_variaveis2, by = parte_selecao) %>%
  add_p(test = all_continuous() ~ "t.test")




