### Atividade prática

## Vamos começar carregando o arquivo de dados preparado para esta aula

install.packages("tidyverse")
library(tidyverse)
install.packages("xlsx")
library(xlsx)

library(readxl)
library(data.table)
library(plyr)
library(xlsx)
library(MASS)
library(dplyr)
salarios <- read_csv("aula-03/data/201802_dados_salarios_servidores.csv.gz")

### 1 ####
## 
## O arquivo possui 2 colunas de Remuneração, uma em Reais e outra em Dólares. 
## Crie uma nova coluna de Remuneração Final que terá a soma entre a remuneração em Reais e as remuneração em Dólares convertida para Reais.
## Atenção: Para conversão, utilize a Cotação Comercial do Dólar no último dia útil de Fevereiro.
## Após criar esta coluna, descarte todos os registros cuja Remuneração Final for menor que R$ 900,00
## 
### # ####

##Exercício 1

SALARIO_FIM <- salarios

SALARIO_FIM$REMUNERACAO_NOVA <- SALARIO_FIM$REMUNERACAO_REAIS + (SALARIO_FIM$REMUNERACAO_DOLARES * 3.2428)


SALARIO_FIM %>% filter(SALARIO_FIM$REMUNERACAO_NOVA > 900) -> SALARIO_FIM2




### 2 ####
## 
## Neste dataset é possível identificar que alguns servidores estão lotados em órgãos diferentes do seu órgão de exercício.
## Identifique os 5 cargos com maior quantidade de servidores que estão lotados em um órgão diferente, listando a descrição do cargo e a quantidade de servidores por cargo.
## Além de listar os 5 cargos e as quantidades, crie um vetor com os nomes destes 5 cargos. Crie este vetor com o nome de cargos_diferente_lotacao.
## 
## Dica: a função pull() do dplyr extrai uma variável em formato de vetor.
salarios %>% count(UF_EXERCICIO) %>% pull(UF_EXERCICIO) -> ufs # EXEMPLO
## 
### # ####

##Teste do Exercício 2

SALARIO_FIM2 %>% count(DIFR=SALARIO_FIM2$ORGSUP_LOTACAO!=SALARIO_FIM2$ORGSUP_EXERCICIO)

setDT(SALARIO_FIM2)
SALARIO_FIM2[ , Count := .N, by = list(SALARIO_FIM2$DIFERACAO, SALARIO_FIM2$DESCRICAO_CARGO)]

SALARIO_FIM2 %>% group_by(SALARIO_FIM2$DESCRICAO_CARGO) %>%
                 count(DIFERACAO)
##Exercício 2

SALARIO_FIM2$DIFERACAO <- ifelse(SALARIO_FIM2$ORGSUP_LOTACAO!=SALARIO_FIM2$ORGSUP_EXERCICIO,1,0)

SALARIO_FIM2 %>% group_by(DESCRICAO_CARGO) %>%
                 summarise(quantidade = sum(DIFERACAO), SERVIDORES = n()) %>%
                 ungroup() %>%
                 arrange(desc(quantidade)) %>%
                 head(5)

SALARIO_FIM2 %>% group_by(DESCRICAO_CARGO) %>%
                 summarise(quantidade = sum(DIFERACAO), SERVIDORES = n()) %>%
                 ungroup() %>%
                 arrange(desc(quantidade)) %>%
                 head(5) %>%
                 pull(DESCRICAO_CARGO) -> cargos_diferente_lotacao

                

### 3 ####
## 
## Utilizando o vetor criado na atividade anterior, calcule a média e o desvio padrão de cada cargo, 
## separando entre aqueles que estão lotados em outro órgão e aqueles que estão lotados no mesmo órgão de exercício.
## O resultado deve conter:
##    - a descrição do cargo, 
##    - uma variável indicando se o servidor está lotado no mesmo órgão de exercício ou em um órgão diferente, 
##    - a média salarial
##    - o desvio padrão
##    - a mediana
##    - o desvio absoluto da mediana
##    - o menor salário
##    - o maior salário
## Analise os valores por lotação dentro de um mesmo cargo e comente ao final do exercício se você considera alguma diferença significativa.
## 
## Dica 1: o operador %in% testa se valores de uma variável pertencem ao conjunto de valores de um vetor. Lembre que deve ser utilizada a variável cargos_diferente_lotacao
salarios %>% filter(DESCRICAO_CARGO %in% c("MINISTRO DE PRIMEIRA CLASSE", "ANALISTA DE TEC DA INFORMACAO", "PESQUISADOR")) %>% count(DESCRICAO_CARGO) # EXEMPLO
## Dica 2: Será necessário agrupar (group_by) por mais de uma variável para calcular as estatísticas solicitadas. 
## A função group_by permite múltiplos nomes de variáveis na mesma chamada.
## 
### # ####

##Exercício 3
SALARIO_FIM3 <- SALARIO_FIM2

(dam_salario <- median( abs( SALARIO_FIM3$REMUNERACAO_NOVA - median( SALARIO_FIM3$REMUNERACAO_NOVA))))

(md_salario <- median( SALARIO_FIM3$REMUNERACAO_NOVA ))

dam_salario / md_salario

SALARIO_FIM3$NDIFERACAO <- ifelse(SALARIO_FIM3$DIFERACAO==1, "LOTADO EM UM DIFERENTE", "LOTADO NO MESMO")

SALARIO_FIM3 %>% group_by(DESCRICAO_CARGO, NDIFERACAO) %>%
  filter(DESCRICAO_CARGO %in% cargos_diferente_lotacao) %>%
  summarise(media = mean(REMUNERACAO_NOVA)
            , desvio_padrão = sd(REMUNERACAO_NOVA)
            , mediana_salario = median(REMUNERACAO_NOVA)
            , desvio_absoluto = dam_salario / md_salario
            , menor_salario = min(REMUNERACAO_NOVA)
            , maximo_salario = max(REMUNERACAO_NOVA)
            , quantidade_servidores = n()) %>%
  ungroup() %>%
  arrange(desc(quantidade_servidores)) %>% 
  head(10)  