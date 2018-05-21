library(tidyverse)
library(lubridate)
install.packages("lubridate")

## Nesta atividade você deve utilizar o resultado do exercício 01 da Atividade da aula 03 (remuneração em dólares convertida para reais)
## Utilize o código daquele exercício como ponto de partida para esta atividade. 
## Sempre utilize o caminho relativo, não o caminho absoluto, pois não funcionará na correção do exercício.

SALARIO_FIM <- salarios

SALARIO_FIM$REMUNERACAO_NOVA <- SALARIO_FIM$REMUNERACAO_REAIS + (SALARIO_FIM$REMUNERACAO_DOLARES * 3.2428)

SALARIO_FIM %>% filter(SALARIO_FIM$REMUNERACAO_NOVA > 900) -> SALARIO_FIM2

### IMPORTANTE ###
## Se você utilizar alguma função própria ou do material de aula, o código da(s) função(ões) deve estar neste arquivo da atividade.


### 1 ####
## 
## Correlação de ano de ingresso por cargo
## - Determine o coeficiente de correlação entre o tempo em anos desde a DATA_INGRESSO_ORGAO e o tempo em anos desde a DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO
##   para todos os cargos que possuem no mínimo 200 servidores.
## - Crie uma coluna que determina se a correlação é positiva ou negativa, e outra coluna que define a força da correlação de acordo com 
##   o material visto em aula sobre interpretação do coeficiente.
## - O resultado desta atividade deve ser um Data Frame com as variáveis de Cargo, Coeficiente de Correlação, Direção da Correlação e Força da Correlação
## 
### # ####.

##EXERCICIO 1
##A)

##PRIMEIRA FORMA
setDT(SALARIO_FIM2)
SALARIO_FIM2[ , Count := .N, by = list(SALARIO_FIM2$DESCRICAO_CARGO)]
SALARIO_FIM2 %>% filter(SALARIO_FIM2$Count > 200) -> SALARIO_FIM3

table(SALARIO_FIM3$Count < 200)

cor(x = 2018 - year(SALARIO_FIM3$DATA_INGRESSO_ORGAO), y = 2018 - year( SALARIO_FIM3$DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO ))

count(SALARIO_FIM2$DESCRICAO_CARGO)



##B)
SALARIO_FIM3 %>% summarise (corrr = cor(x = 2018 - year(SALARIO_FIM3$DATA_INGRESSO_ORGAO), y = 2018 - year( SALARIO_FIM3$DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO ))
                 , Sinal_correlacao = ifelse( corrr > 0, "positivo", "negativo")
                 , forca_correlation = ifelse( corrr > 0.9, "muito_forte",ifelse(corrr > 0.7 && corrr < 0.9,"forte",ifelse(corrr > 0.5 && corrr < 0.7,"moderado", ifelse(corrr > 0.3 && corrr < 0.5, "fraca",ifelse(corrr < 0.3, "fraca","nenhuma"))))))

SALARIO_FIM4 <- summarise (corrr = cor(x = 2018 - year(SALARIO_FIM3$DATA_INGRESSO_ORGAO), y = 2018 - year( SALARIO_FIM3$DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO ))
                            , Sinal_correlacao = ifelse( corrr > 0, "positivo", "negativo")
                            , forca_correlation = ifelse( corrr > 0.9, "muito_forte",ifelse(corrr > 0.7 && corrr < 0.9,"forte",ifelse(corrr > 0.5 && corrr < 0.7,"moderado", ifelse(corrr > 0.3 && corrr < 0.5, "fraca",ifelse(corrr < 0.3, "fraca","nenhuma"))))))


#' - 0.9 para mais ou para menos indica uma correlação muito forte.
#' - 0.7 a 0.9 positivo ou negativo indica uma correlação forte.
#' - 0.5 a 0.7 positivo ou negativo indica uma correlação moderada.
#' - 0.3 a 0.5 positivo ou negativo indica uma correlação fraca.
#' - 0 a 0.3 positivo ou negativo indica uma correlação desprezível.

##C)
SALARIO_FIM3 %>% summarise (corrr = cor(x = 2018 - year(SALARIO_FIM3$DATA_INGRESSO_ORGAO), y = 2018 - year( SALARIO_FIM3$DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO ))
                            , Sinal_correlacao = ifelse( corrr > 0, "positivo", "negativo")
                            , forca_correlation = ifelse( corrr > 0.9, "muito_forte",ifelse(corrr > 0.7 && corrr < 0.9,"forte",ifelse(corrr > 0.5 && corrr < 0.7,"moderado", ifelse(corrr > 0.3 && corrr < 0.5, "fraca",ifelse(corrr < 0.3, "fraca","nenhuma")))))) -> RESULTADOS


### 2 ###
##
## - A partir do dataset do exercício anterior, selecione os 10 cargos de correlação mais forte (seja positiva ou negativa) e os 
##   10 cargos de correlação mais fraca (de novo, independente de ser positiva ou negativa)
## - Para estes 20 cargos, determine a Moda do órgão de lotação (ORGSUP_LOTACAO) e de exercício (ORGSUP_EXERCICIO)
## - Reponda se existe diferença entre as modas e se existe relação entre a Força da Correlação e a diferença entre as modas 
##   (caso haja diferença)
##
### # ###
##EXERCICIO 2
##A)
