

# Carregue a biblioteca tidyverse. Lembre que outras bibliotecas serão carregadas junto ao tidyverse

library(tidyverse)
library(tibble)
library(lubridate)


# Crie um dataframe com o conteúdo do arquivo ted_main.csv.gz. 

ted <- read.csv("aula-05/data/ted_main.csv.gz")

colnames(ted)
str(ted)


# Visualize o resumo dos dados do dataframe. Verifique os mínimos, máximos, médias e medianas das variáveis numéricas.
# As variáveis duration, film_date e published_date estão no tipo de dados apropriado?

summary(ted$comments)
summary(ted$duration)
summary(ted$languages)
summary(ted$num_speaker)
summary(ted$views)
summary(ted$published_date)
summary(ted$film_date)


##As variaveis film_date e published_data devem estar em formato data.


# Converta as seguintes variáveis utilizando o pacote Lubridate:
#     * duration, para duração (em segundos). Experimente utilizar as funções as.duration e duration. Mantenha aquela que considerar mais apropriada.
#     * film_date, para data, com a função as_datetime.
#     * published_date, para data, com a função as_datetime..

ted %>% mutate(duração = as.duration(duration)) %>%
        mutate(filme_data = as_datetime(film_date)) %>%
        mutate(publicação_data = as_datetime(published_date)) -> ted_fim2

# Converta as seguintes variáveis character para variáveis categóricas com a função factor.
#     * event
#     * speaker_occupation

ted_fim2 %>% mutate(evento = factor(event)) %>%
  mutate(ocupação_falante = factor(speaker_occupation)) -> ted_fim3


# Retire do dataframe a variável name

ted_fim4 = subset(ted_fim3, select = -name )


# Visualize novamente o resumo dos dados do dataframe. Verifique os mínimos, máximos, médias e medianas das variáveis numéricas. Verifique as contagens das variáveis categóricas

summary(ted_fim4$comments)
summary(ted_fim4$duration)
summary(ted_fim4$languages)
summary(ted_fim4$num_speaker)
summary(ted_fim4$views)


# Verifique quais registros possuem a menor quantidade de línguas. Corrija para que possuam no mínimo 1 idioma.

table(ted_fim4$languages==0)

ted_fim4 %>% group_by(description) %>%
  filter(languages == 0 ) -> resultado2

ted_fim4 %>% mutate(languages = ifelse(languages == 0,1,ted_fim4$languages)) -> ted_fim5

table(ted_fim5$languages==0)

# Verifique os 15 registros com menor data de filmagem. 

ted_fim4 %>% arrange(filme_data) %>% head(15)




# Crie um dataframe com a contagem de apresentações por ano de filmagem e visualize todo o seu conteúdo

ted_fim4 %>% group_by(year(filme_data)) %>%
             count()  -> ted_fim5

##ou

ted_fim4 %>% mutate(ano_Apresentacao = year(filme_data)) %>%
             group_by(ano_Apresentacao) %>%
             summarise(quantidade = n()) %>%
            ungroup() -> resposta_2


# Analise os 10 quantis da quantidade de apresentações por ano.
# Descarte, do data frame de apresentações do TED Talks, aqueles cujo ano de filmagem tiver quantidade de apresentações menor ou igual à quantidade do quarto quantil.

quantile(ted_fim5$n, c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1))
quantile(ted_fim5$n)

ted_fim5<-ted_fim5[!(ted_fim5$n <= 33),]

##ou

ted_fim4 %>% mutate(ano_Apresentacao = year(filme_data)) %>%
  group_by(ano_Apresentacao) %>%
  summarise(quantidade = n()) %>%
  filter(quantidade > 33) %>%
  ungroup() -> resposta_3


# Verifique novamente o resumo dos dados do dataframe

View(ted_fim5)



# Verifique os 10 registros com maior duração.

ted_fim4 %>% group_by(title) %>%
  summarise(duracão = duração) %>%
  ungroup() %>%
  arrange(desc(duracão)) %>%
  head(10)

ted_fim4 %>% arrange(desc(duração)) %>% head(10)



# Existem apresentações com duração maior que 3 desvios padrão acima da média? Liste elas

(ted_sd <- sd(ted_fim4$duração))
(ted_sd3 <- 3 * (sd(ted_fim4$duração)))

(ted_mean <- mean(ted_fim4$duração))

tres_desvios_mais_media <- ted_mean + ted_sd3

(ted_fim4 %>% filter(duração > as.duration(tres_desvios_mais_media)) -> resposta)


# Calcule os 4 quartis e o IQR da duração das apresentações. Liste as apresentações cuja duração supera 1.5 * o IQR + o terceiro quartil

quantile(resposta$duração)
(ted_iqr <- IQR(resposta$duração))
(ted_3q <- quantile(resposta$duração, c(0.75)))
ted_3q_final <- as.numeric(ted_3q)

class(ted_iqr)

ted_final <- (1.5 * ted_iqr) + ted_3q_final

ted_fim4 %>% filter(as.numeric(duration) > ted_final) -> fim



# Visualize os 10 quantis da quantidade de visualizações
(quantile(ted_fim4$views, c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1)))




# Compare as seguintes estatísticas descritivas da quantidade de visualizações:
#   * Média e Mediana. Qual é maior?
#   * Desvio Absoluto da Mediana e Desvio Padrão. Qual é maior?
#   * Desvio Absoluto da Mediana e IQR. Quantas vezes o IQR é maior que o Desvio Absoluto da Mediana?
#   * Com base na média e na mediana, e na razão entre o IQR e o Desvio Absoluto da Mediana, 
#     você conclui que as quantidades de visualização estão distribuidas de forma simétrica em torno da média?

(mean_views <- mean(ted_fim4$views))
(meandian_views <- median(ted_fim4$views))

## média é maior

(sd_views <- sd(ted_fim4$views))

(dam_views <- median( abs( ted_fim4$views - median( ted_fim4$views ))))
(md_views <- median( ted_fim4$views ))
dam_views / md_views

## o desvio padrão é menor

(iqr_views <- IQR(ted_fim4$views ))

## o iqr é 2 vezes maior que

# Calcule a média, o desvio padrão, a mediana e o IQR da quantidade de línguas dos seguintes grupos:
#     * 10% de vídeos com maior número de visualizações
#     * 10% de vídeos com menor número de visualizações




# Determine a quantidade de apresentações por evento cujo nome inicie com TED. Utilize a função str_detect para este filtro

ted_fim4 %>% group_by(evento) %>%
             filter(str_detect(evento, "TED.*")) %>%
             summarise(qtd = n()) %>%
             ungroup()



# Determine, por evento cujo nome inicie com TED e que a quantidade de visualizações dos vídeos foi maior que a mediana calculada anteriormente.
#   * a quantidade de apresentações resultante do filtro, por evento
#   * o ano do evento (utilizar o menor ano da data de publicação)
#   * a quantidade média de línguas das apresentações
#   * o desvio padrão da quantidade de línguas
#   * o coeficiente de variação da quantidade de línguas
### EXIBA SOMENTE OS EVENTOS COM MAIS DE 10 APRESENTAÇÕES

ted_fim4 %>% group_by(evento) %>%
  filter(str_detect(evento, "TED.*"), views > mean_views) %>%
  summarise(qtd = n()
          , menor_data = min(year(publicação_data))
          , media_languages = mean(languages)
          , sd_languages = sd(languages)
          , coef_variação_languages = sd(languages)/mean(languages)) %>%
  ungroup()


# Calcule e classifique as seguintes correlações
#     * Quantidade de visualizações e Quantidade de línguas
#     * Quantidade de visualizações e Duração
#     * Quantidade de visualizações e Quantidade de Comentários
#     * Quantidade de Comentários e Quantidade de línguas

ted_fim4 %>% summarise(corelacao_view_languages = cor(views, languages)
                       , forca_correlation = ifelse( corelacao_view_languages > 0.9, "muito_forte",ifelse(corelacao_view_languages > 0.7 && corelacao_view_languages < 0.9,"forte",ifelse(corelacao_view_languages > 0.5 && corelacao_view_languages < 0.7,"moderado", ifelse(corelacao_view_languages > 0.3 && corelacao_view_languages < 0.5, "fraca",ifelse(corelacao_view_languages < 0.3, "fraca","nenhuma")))))
                       , corelacao_view_duration = cor(views, as.numeric(duration, "seconds"))
                       , forca_correlation_2 = ifelse( corelacao_view_duration > 0.9, "muito_forte",ifelse(corelacao_view_duration > 0.7 && corelacao_view_duration < 0.9,"forte",ifelse(corelacao_view_duration > 0.5 && corelacao_view_duration < 0.7,"moderado", ifelse(corelacao_view_duration > 0.3 && corelacao_view_duration < 0.5, "fraca",ifelse(corelacao_view_duration < 0.3, "fraca","nenhuma")))))
                       , corelacao_view_comments = cor(views, comments)
                       , forca_correlation_3 = ifelse( corelacao_view_comments > 0.9, "muito_forte",ifelse(corelacao_view_comments > 0.7 && corelacao_view_comments < 0.9,"forte",ifelse(corelacao_view_comments > 0.5 && corelacao_view_comments < 0.7,"moderado", ifelse(corelacao_view_comments > 0.3 && corelacao_view_comments < 0.5, "fraca",ifelse(corelacao_view_comments < 0.3, "fraca","nenhuma")))))
                       , correlacao_comments_languages = cor(comments, languages)
                       , forca_correlation_4 = ifelse( correlacao_comments_languages > 0.9, "muito_forte",ifelse(correlacao_comments_languages > 0.7 && correlacao_comments_languages < 0.9,"forte",ifelse(correlacao_comments_languages > 0.5 && correlacao_comments_languages < 0.7,"moderado", ifelse(correlacao_comments_languages > 0.3 && correlacao_comments_languages < 0.5, "fraca",ifelse(correlacao_comments_languages < 0.3, "fraca","nenhuma"))))))




# Descarte os vídeos cuja duração seja maior que 3 desvios padrões da média. Calcule novamente as 5 correlações solicitadas

ted_fim4 %>% mutate( tres_duracao = 3*sd(as.numeric(duration, "seconds")) + mean(as.numeric(duration, "seconds")))  %>% 
                    filter(as.numeric(duration) <= tres_duracao)  %>% 
                    summarise(corelacao_view_languages = cor(views, languages)
                       , forca_correlation = ifelse( corelacao_view_languages > 0.9, "muito_forte",ifelse(corelacao_view_languages > 0.7 && corelacao_view_languages < 0.9,"forte",ifelse(corelacao_view_languages > 0.5 && corelacao_view_languages < 0.7,"moderado", ifelse(corelacao_view_languages > 0.3 && corelacao_view_languages < 0.5, "fraca",ifelse(corelacao_view_languages < 0.3, "fraca","nenhuma")))))
                       , corelacao_view_duration = cor(views, as.numeric(duration, "seconds"))
                       , forca_correlation_2 = ifelse( corelacao_view_duration > 0.9, "muito_forte",ifelse(corelacao_view_duration > 0.7 && corelacao_view_duration < 0.9,"forte",ifelse(corelacao_view_duration > 0.5 && corelacao_view_duration < 0.7,"moderado", ifelse(corelacao_view_duration > 0.3 && corelacao_view_duration < 0.5, "fraca",ifelse(corelacao_view_duration < 0.3, "fraca","nenhuma")))))
                       , corelacao_view_comments = cor(views, comments)
                       , forca_correlation_3 = ifelse( corelacao_view_comments > 0.9, "muito_forte",ifelse(corelacao_view_comments > 0.7 && corelacao_view_comments < 0.9,"forte",ifelse(corelacao_view_comments > 0.5 && corelacao_view_comments < 0.7,"moderado", ifelse(corelacao_view_comments > 0.3 && corelacao_view_comments < 0.5, "fraca",ifelse(corelacao_view_comments < 0.3, "fraca","nenhuma")))))
                       , correlacao_comments_languages = cor(comments, languages)
                       , forca_correlation_4 = ifelse( correlacao_comments_languages > 0.9, "muito_forte",ifelse(correlacao_comments_languages > 0.7 && correlacao_comments_languages < 0.9,"forte",ifelse(correlacao_comments_languages > 0.5 && correlacao_comments_languages < 0.7,"moderado", ifelse(correlacao_comments_languages > 0.3 && correlacao_comments_languages < 0.5, "fraca",ifelse(correlacao_comments_languages < 0.3, "fraca","nenhuma"))))))



# Utilizando o data frame original, crie um dataframe com a mediana da duração dos vídeos por ano de filmagem. Calcule a correlação entre o ano e a mediana da duração
# e interprete o resultado

ted %>% mutate(filme_dataa = as_datetime(film_date), duracao = as.numeric(duration, "seconds")) %>%
        group_by(ano = year(filme_dataa)) %>%
        summarise(mediana_duracao = median(duracao)) %>%
        ungroup() %>%
        summarise(corr = cor(ano, mediana_duracao))

## A correlação é fraca, não é factivel afirmar que exista correlação entre ano e a mediana da duração
