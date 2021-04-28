install.packages("gtsummary")
library(gtsummary)
library(tidyverse)
library(huxtable)
library(flextable)
library(openxlsx)
library(gt)
library(webshot)

dados <- openxlsx::read.xlsx("banco-gepedh.xlsx")
dados$parte_selecao = factor(dados$parte_selecao, levels = 1:2, labels = c("Sim","Não"))


dados <- dados %>% mutate(ranking = case_when(ranking <= 3 ~ "Medalhista",
                                              ranking >= 4 ~ "Não Medalhista"))


dados <- dados %>% dplyr::select(idade:ranking, p_harmoniosa, p_obsessiva, criterio_p)


# fig1
dados %>% select(idade, ranking) %>% tbl_summary() %>% as_gt() %>%
                gtsave(filename = "img/fig1.png")


# fig2
dados %>% select(idade) %>%
                tbl_summary(label = idade ~ "Idade") %>% as_gt() %>%
                gtsave(filename = "img/fig2.png")


# fig3 (usar no statistic tambem)
names <- list(idade ~ "Idade",
              tempo_pratica ~ "Tempo de Prática",
              ranking ~ "Ranking")
dados %>% select(idade, tempo_pratica, ranking) %>%
                tbl_summary(label = names) %>% as_gt() %>%
                gtsave(filename = "img/fig3.png")

#fig4
dados %>% select(idade, tempo_pratica, ranking) %>%
                tbl_summary(label = names,
                            statistic = list(idade ~ "{mean} ({sd})")) %>% as_gt() %>%
                gtsave(filename = "img/fig4.png")

#fig5
dados %>% select(idade, tempo_pratica, ranking) %>%
                tbl_summary(label = names,
                            statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% as_gt() %>%
                gtsave(filename = "img/fig5.png")

#fig6
dados %>% select(idade, tempo_pratica, ranking) %>%
                tbl_summary(label = names) %>% as_gt() %>%
                gtsave(filename = "img/fig6.png")

#fig7
names2 <- list(idade ~ "Idade",
               ranking ~ "Ranking",
               parte_selecao ~ "Fez parte da Seleção")
dados %>% select(idade, ranking, parte_selecao) %>%
                tbl_summary(label = names2,
                            statistic = list(all_categorical() ~ "{n} / {N} ({p}%)")) %>% as_gt() %>%
                gtsave(filename = "img/fig7.png")


#fig8
names3 <- list(idade ~ "Idade",
               tempo_pratica ~ "Tempo de Prática",
               ranking ~ "Ranking",
               parte_selecao ~ "Fez parte da Seleção")
dados %>% select(idade, tempo_pratica, ranking, parte_selecao) %>%
                tbl_summary(label = names3,
                            statistic = list(all_categorical() ~ "{n} / {N} ({p}%)",
                                             all_continuous() ~ "{mean} ({sd})")) %>% as_gt() %>%
                gtsave(filename = "img/fig8.png")


#fig9 - by
dados %>% select(idade, ranking) %>%
                tbl_summary(label = idade ~ "Idade",
                            by = ranking) %>% as_gt() %>%
                gtsave(filename = "img/fig9.png")

#fig10 - digits
dados %>% select(idade, tempo_pratica, ranking, parte_selecao) %>%
                tbl_summary(label = names3,
                            statistic = list(all_categorical() ~ "{n} / {N} ({p}%)",
                                             all_continuous() ~ "{mean} ({sd})"),
                            digits = list(all_categorical() ~ 1,
                                          all_continuous() ~ 0)) %>% as_gt() %>%
                gtsave(filename = "img/fig10.png")

#fig11
dados %>% select(idade, tempo_pratica, ranking, parte_selecao) %>%
                tbl_summary(label = names3,
                            statistic = list(all_categorical() ~ "{n} / {N} ({p}%)",
                                             all_continuous() ~ "{mean} ({sd})"),
                            digits = list(all_categorical() ~ c(0, 0, 1),
                                          all_continuous() ~ c(1, 1))) %>% as_gt() %>%
                gtsave(filename = "img/fig11.png")


#fig12 - add_p
dados %>% select(idade, ranking) %>%
                tbl_summary(label = idade ~ "Idade",
                            by = ranking) %>% add_p() %>% 
                as_gt() %>%
                gtsave(filename = "img/fig12.png")

#fig13
dados %>% select(idade, parte_selecao, ranking) %>%
                tbl_summary(label = list(idade ~ "Idade",
                                         parte_selecao ~ "Fez parte da Seleção"),
                            by = ranking) %>% add_p(test = list(all_categorical() ~ "chisq.test.no.correct",
                                                                all_continuous() ~ "wilcox.test")) %>% 
                as_gt() %>%
                gtsave(filename = "img/fig13.png")

#fig14 - test
dados %>% select(idade, ranking) %>%
                tbl_summary(label = list(idade ~ "Idade"),
                            by = ranking) %>% add_p(pvalue_fun = function(x) style_pvalue(x, digits = 3, decimal.mark = ",")) %>% 
                as_gt() %>%
                gtsave(filename = "img/fig14.png")

#fig15 - add_overall
dados %>% select(idade, ranking) %>%
                tbl_summary(label = list(idade ~ "Idade"),
                            by = ranking) %>%
                add_overall() %>%
                as_gt() %>%
                gtsave(filename = "img/fig15.png")

#fig16
dados %>% select(idade, ranking) %>%
                tbl_summary(label = list(idade ~ "Idade"),
                            by = ranking) %>%
                add_overall(col_label = "**Total**, N = {N}") %>%
                as_gt() %>%
                gtsave(filename = "img/fig16.png")


#fig17 - funcoes extras
dados %>% select(idade, ranking) %>%
                tbl_summary(label = list(idade ~ "Idade",
                                         ranking ~ "Ranking")) %>% bold_labels() %>%
                as_gt() %>%
                gtsave(filename = "img/fig17.png")

#fig18
dados %>% select(idade, ranking) %>%
                tbl_summary(label = list(idade ~ "Idade",
                                         ranking ~ "Ranking")) %>% italicize_levels() %>%
                as_gt() %>%
                gtsave(filename = "img/fig18.png")

#fig19
dados %>% select(idade, tempo_pratica, ranking) %>%
                tbl_summary(label = names,
                            by = ranking) %>% add_p() %>% bold_p() %>%
                as_gt() %>%
                gtsave(filename = "img/fig19.png")


#fig20 - theme
theme_gtsummary_language(language = "pt")
dados %>% select(idade) %>%
                tbl_summary(label = idade ~ "Idade") %>%
                as_gt() %>%
                gtsave(filename = "img/fig20.png")

#fig21
theme_gtsummary_language(language = "pt", decimal.mark = ",")
dados %>% select(idade) %>%
                tbl_summary(label = idade ~ "Idade") %>%
                as_gt() %>%
                gtsave(filename = "img/fig21.png")

