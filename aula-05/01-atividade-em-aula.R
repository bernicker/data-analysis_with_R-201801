# Carregue a biblioteca tidyverse. Lembre que outras bibliotecas serão carregadas junto ao tidyverse




# Crie um dataframe com o conteúdo do arquivo ted_main.csv.gz. 

ted <- read_csv("aula-05/data/ted_main.csv.gz")

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
        mutate(publicação_data = as_datetime(published_date)) ->ted_fim2

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
             summarise(linguas = languages == 0) -> resultado1

ted_fim4 %>% group_by(description) %>%
            filter(languages == 0 ) -> resultado2

ted_fim4 %>% mutate(languages = ifelse(languages == 0,1,ted_fim4$languages)) -> ted_fim5
table(ted_fim5$languages==0)

# Verifique os 15 registros com menor data de filmagem. 

ted_fim4 %>% arrange(filme_data) %>% head(15)
                                         



# Crie um dataframe com a contagem de apresentações por ano de filmagem e visualize todo o seu conteúdo

ted_fim4 %>% group_by(year(filme_data)) %>%
             count()  -> ted_fim5


# Analise os 10 quantis da quantidade de apresentações por ano.
# Descarte, do data frame de apresentações do TED Talks, aqueles cujo ano de filmagem tiver quantidade de apresentações menor ou igual à quantidade do quarto quantil.

quantile(ted_fim5$n, c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1,00))
quantile(ted_fim5$n)

# Verifique novamente o resumo dos dados do dataframe




# Verifique os 10 registros com maior duração.




# Existem apresentações com duração maior que 3 desvios padrão acima da média? Liste elas




# Calcule os 4 quartis e o IQR da duração das apresentações. Liste as apresentações cuja duração supera 1.5 * o IQR + o terceiro quartil




# Visualize os 10 quantis da quantidade de visualizações




# Compare as seguintes estatísticas descritivas da quantidade de visualizações:
#   * Média e Mediana. Qual é maior?
#   * Desvio Absoluto da Mediana e Desvio Padrão. Qual é maior?
#   * Desvio Absoluto da Mediana e IQR. Quantas vezes o IQR é maior que o Desvio Absoluto da Mediana?
#   * Com base na média e na mediana, e na razão entre o IQR e o Desvio Absoluto da Mediana, 
#     você conclui que as quantidades de visualização estão distribuidas de forma simétrica em torno da média?




# Calcule a média, o desvio padrão, a mediana e o IQR da quantidade de línguas dos seguintes grupos:
#     * 10% de vídeos com maior número de visualizações
#     * 10% de vídeos com menor número de visualizações




# Determine a quantidade de apresentações por evento cujo nome inicie com TED. Utilize a função str_detect para este filtro




# Determine, por evento cujo nome inicie com TED e que a quantidade de visualizações dos vídeos foi maior que a mediana calculada anteriormente.
#   * a quantidade de apresentações resultante do filtro, por evento
#   * o ano do evento (utilizar o menor ano da data de publicação)
#   * a quantidade média de línguas das apresentações
#   * o desvio padrão da quantidade de línguas
#   * o coeficiente de variação da quantidade de línguas
### EXIBA SOMENTE OS EVENTOS COM MAIS DE 10 APRESENTAÇÕES




# Calcule e classifique as seguintes correlações
#     * Quantidade de visualizações e Quantidade de línguas
#     * Quantidade de visualizações e Duração
#     * Quantidade de visualizações e Quantidade de Comentários
#     * Quantidade de Comentários e Quantidade de línguas




# Descarte os vídeos cuja duração seja maior que 3 desvios padrões da média. Calcule novamente as 5 correlações solicitadas




# Utilizando o data frame original, crie um dataframe com a mediana da duração dos vídeos por ano de filmagem. Calcule a correlação entre o ano e a mediana da duração
# e interprete o resultado




