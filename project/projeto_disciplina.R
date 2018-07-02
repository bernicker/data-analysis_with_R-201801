# Descrição dos dados: https://tech.instacart.com/3-million-instacart-orders-open-sourced-d40d29ead6f2
# Estamos trabalhando com somente uma amostra do total de pedidos. O dataset abaixo não possui 3 milhões de pedidos ;)
library( tidyverse )
library(tidyverse)

departments <- read_csv("project/departments.csv")                   # Cadastro de Departamentos
aisles <- read_csv("project/aisles.csv")                             # Cadastro de "Corredores"
products <- read_csv("project/products.csv")                         # Cadastro de Produtos

insta_orders <- read_csv( "project/orders_instacart.csv" )           # Amostra de pedidos de usuários
insta_products <- read_csv( "project/order_products_instacart.csv" ) # Produtos que compõe os pedidos


#1 # Quantos dos produtos do cadastro nunca foram comprados?

products %>%
  anti_join(insta_products, by="product_id") %>%
  count()

#2 # Crie um dataframe com os dados combinados de produtos, corredores e departamentos. 

EXERCICIO_2 <-
  products %>%
  left_join(departments, by="department_id") %>%
  left_join(aisles, by="aisle_id")

#3 # Quais as 10 combinações corredor + departamento que possuem mais produtos cadastrados? Use o dataframe da atividade #2.

EXERCICIO_2 %>%
  count(aisle_id, department_id, aisle, department) %>%
  arrange(desc(n)) %>%
  head(10) -> RESPOSTA_EXERCICIO_3



#4 # Qual o percentual de pedidos que possuem algum produto dos pares 'corredor + departamento' da atividade anterior?

departments # Cadastro de Departamentos
aisles # Cadastro de "Corredores"
products  # Cadastro de Produtos

insta_orders  # Amostra de pedidos de usuários
insta_products # Produtos que compõe os pedidos

EXERCICIO_4_1 <- insta_products %>%
  left_join(products, by="product_id") %>%
  left_join(aisles, by="aisle_id") %>%
  left_join(departments, by="department_id")


RESPOSTA_EXERCICIO_3 %>% select(department_id) -> department_id
RESPOSTA_EXERCICIO_3 %>% select(aisle_id) -> aisle_id

PRIMEIRA_PARTE <- merge(EXERCICIO_2,department_id, id= "department_id")
SEGUNDA_PARTE <- merge(PRIMEIRA_PARTE,aisle_id, id= "aisle_id")

names(SEGUNDA_PARTE)

SEGUNDA_PARTE %>% distinct(product_id) %>% mutate(PEDIDO = 1) -> product_id
TERCEIRA_PARTE <- merge(insta_products, product_id, id="product_id", all = TRUE )

TERCEIRA_PARTE$NOVO_PEDIDO <- ifelse(is.na(TERCEIRA_PARTE$PEDIDO == ""),0,1)
class(TERCEIRA_PARTE)

names(TERCEIRA_PARTE)

TERCEIRA_PARTE %>% group_by(order_id) %>%
                   summarise(maximo = max(NOVO_PEDIDO)) -> QUARTA_PARTE

table(QUARTA_PARTE$maximo)
count(QUARTA_PARTE$maximo)


(resultado <- (82344/131210)*100)

#5 # Crie um novo dataframe de produtos em pedidos retirando aqueles produtos que não estão categorizados (usar resultado das atividades 3 e 4)

EXERCICIO_5 <- EXERCICIO_2 %>% 
  filter(aisle != 'missing' | department != 'missing') %>%
  left_join(insta_products, by="product_id") %>%
  left_join(insta_orders, by="order_id") %>%
  select(product_id, product_name, aisle_id, aisle, department_id, department, order_id, user_id)


#6 # Crie um dataframe que combine todos os dataframes através das suas chaves de ligação. Para produtos de pedidos, use o dataframe da atividade 4
# Transforme as variáveis user_id, department e aisle em factor
# Transforme a variável order_hour_of_day em um factor ordenado (ordered)

# Este dataframe deverá ser utilizado em todas as atividades seguintes

EXERCICIO_6 <- EXERCICIO_4_1 %>%
  left_join(insta_orders, by="order_id") %>%
  mutate(user_id = factor(user_id),
         department = factor(department),
         aisle = factor(aisle)) %>%
  mutate(order_hour_of_day = factor(order_hour_of_day, ordered = TRUE))


#7 # Identifique os 5 horários com maior quantidade de usuários que fizeram pedidos
EXERCICIO_6 %>%
  count(order_hour_of_day) %>%
  arrange(desc(n)) %>%
  head(5) %>%
  select(order_hour_of_day) -> resposta_7

#8 # Quais os 15 produtos mais vendidos nestes 5 horários? Identifique os produtos e a quantidade total nestes horários (total geral, não por hora)
EXERCICIO_6 %>% 
  filter(order_hour_of_day %in% pull(top_hours)) %>% 
  count(product_id, product_name) %>%
  arrange(desc(n)) %>%
  head(15) %>%
  mutate(quantidade = n) %>%
  select(product_id, product_name, quantidade) -> resposta_8

#9 # Calcule a média de vendas por hora destes 15 produtos ao longo do dia,
# e faça um gráfico de linhas mostrando a venda média por hora destes produtos. 
# Utilize o nome do produto para legenda da cor da linha.
# Você consegue identificar algum produto com padrão de venda diferente dos demais? 

EXERCICIO_6 %>%
  filter(product_id %in% pull(resposta_8 %>% select(product_id))) %>%
  count(product_name, order_hour_of_day, order_dow) %>%
  group_by(product_name, order_hour_of_day) %>%
  summarise(media = mean(n)) %>%
  ungroup() %>%
  ggplot(aes(x = order_hour_of_day, y = media, group = product_name, colour = product_name, shape = product_name)) +
  geom_line() +
  ggtitle("Média de Vendas por Hora") +
  xlab("Horas") +
  ylab("Média de vendas")




#10 # Calcule as seguintes estatísticas descritivas sobre a quantidade de pedidos por dia, para cada hora do dia. O resultado final deve ser exibido para cada hora do dia:
# Média, Desvio Padrão, Mediana, Mínimo e Máximo
# Considerando os valores calculados, você acredita que a distribuição por hora é gaussiana? 

EXERCICIO_6 %>%
  count(order_dow, order_hour_of_day) %>%
  group_by(order_hour_of_day) %>%
  summarise(media = mean(n),
            desvio_padrao = sd(n),
            mediana = median(n),
            minimo = min(n),
            maximo = max(n)) %>%
  ungroup() -> EXERCICIO_10

EXERCICIO_10 %>% View()

é percebido que os dados analisados possuem uma distribuição é gaussiana, pois a média tem apenas um pico e
 analisando o demais dados, aparenta gerar um curva normal.

#11 # Faça um gráfico da média de quantidade de produtos por hora, com 1 desvio padrão para cima e para baixo em forma de gráfico de banda
 EXERCICIO_10 %>%
  ggplot(aes(x = order_hour_of_day, y = media, group = "order_hour_of_day", ymin = media - desvio_padrao, ymax = media + desvio_padrao)) +
  geom_ribbon(fill = "lightgray", alpha = 0.7) +
  geom_line(color = "red") +
  ggtitle("Média/Quantidade de Produtos por Hora") +
  xlab("Horas") +
  ylab("Média")

#12 # Visualize um boxplot da quantidade de pedidos por hora nos 7 dias da semana. O resultado deve ter order_dow como eixo x.

 EXERCICIO_6 %>%
  distinct(order_id, order_dow, order_hour_of_day) %>%
  count(order_dow, order_hour_of_day) %>%
  ggplot(aes(x = order_dow, y = n, group = order_dow)) +
  geom_boxplot() + 
  scale_x_continuous( breaks = 0:6 ) +
  scale_y_continuous( breaks = seq(from = 0, to = 2500, by = 250 )) +
  labs( x = "Dias da Semana"
        , y = "Quantidade de Pedidos por Hora"
        , title = "Boxplot da Quantidade de Pedidos por Hora") +
  theme_bw()


#13 # Identifique, por usuário, o tempo médio entre pedidos

 EXERCICIO_6 %>%
  group_by(user_id) %>%
  summarise(tempo_medio = mean(days_since_prior_order)) %>%
  ungroup() -> exercicio_13


#14 # Faça um gráfico de barras com a quantidade de usuários em cada tempo médio calculado

 exercicio_13 %>%
  ggplot(aes(x = tempo_medio)) +
  geom_bar(fill = "blue", color = "blue", alpha = 0.6) +
  scale_x_continuous( breaks = seq( from = 0, to = 30, by = 3 ) ) +
  scale_y_continuous( breaks = seq( from = 0, to = 40000, by = 2500 )) +
  ggtitle("Quantidade de Usuários por Tempo Médio entre Compras") +
  xlab("Tempo Médio") +
  ylab("Quantidade de Usuários")

#15 # Faça um gráfico de barras com a quantidade de usuários em cada número de dias desde o pedido anterior. Há alguma similaridade entre os gráficos das atividades 14 e 15? 

 EXERCICIO_6 %>%
  distinct(user_id, days_since_prior_order) %>%
  ggplot(aes(x = days_since_prior_order)) +
  geom_bar(fill = "blue", color = "blue", alpha = 0.6) +
  ggtitle("Quantidade de Usuários por Tempo entre Compras") +
  scale_x_continuous( breaks = seq( from = 0, to = 30, by = 3 ) ) +
  scale_y_continuous( breaks = seq( from = 0, to = 40000, by = 2500 )) +
  xlab("Tempo (em Dias)") +
  ylab("Quantidade de Usuários")

Os gráficos analisados dos exercicios 14 e 15 possuem o mesmo padrão

#16 # Repita o gráfico da atividade 14 mantendo somente os usuários com no mínimo 5 pedidos. O padrão se mantém?

insta_orders %>% select(user_id) %>% count()
insta_orders %>% distinct(user_id) %>% count()

EXERCICIO_6 %>% 
  group_by(user_id) %>%
  summarise(qtd_pedidos = max(order_number)) %>%
  ungroup() %>%
  filter(qtd_pedidos >= 5) %>%
  select(user_id) -> usuario_mais_igual_5_pedidos

EXERCICIO_6 %>%
  filter(user_id %in% pull(usuario_mais_igual_5_pedidos)) %>%
  group_by(user_id) %>%
  summarise(tempo_medio = mean(days_since_prior_order)) %>%
  ungroup() %>%
  ggplot(aes(x = tempo_medio)) +
  geom_bar(fill = "green", color = "green", alpha = 0.6) +
  scale_x_continuous( breaks = seq( from = 0, to = 30, by = 3 ) ) +
  scale_y_continuous( breaks = seq( from = 0, to = 40000, by = 2500 )) +
  ggtitle("Quantidade de Usuários por Tempo Médio entre Compras") +
  xlab("Tempo Médio") +
  ylab("Quantidade de Usuários")

o padrão se mantem

#17 # O vetor abaixo lista todos os IDs de bananas maduras em seu estado natural.
# Utilizando este vetor, identifique se existem pedidos com mais de um tipo de banana no mesmo pedido.
bananas <- c(24852, 13176, 39276, 37067, 29259)

EXERCICIO_6 %>%
  filter(product_id %in% bananas) %>%
  count(order_id) %>%
  filter(n > 1) %>%
  select(order_id)-> orders_bananas

#18 # Se existirem, pedidos resultantes da atividade 17, conte quantas vezes cada tipo de banana aparece nestes pedidos com mais de um tipo de banana.
# Após exibir os tipos de banana, crie um novo vetor de id de bananas contendo somente os 3 produtos de maior contagem de ocorrências

EXERCICIO_6 %>%
  filter(order_id %in% pull(orders_bananas)) %>%
  filter(product_id %in% bananas) %>%
  group_by(product_id) %>%
  summarise(qtd = n()) %>%
  ungroup() %>%
  arrange(desc(qtd)) %>%
  head(3) %>% 
  select(product_id) -> top_3_bananas

top_3_bananas <- pull(top_3_bananas)

#19 # Com base no vetor criado na atividade 18, conte quantos pedidos de, em média, são feitos por hora em cada dia da semana. 

EXERCICIO_6 %>%
  filter(product_id %in% top_3_bananas) %>%
  group_by(order_dow, order_hour_of_day) %>%
  summarise(media = mean(n())) %>%
  ungroup() -> media_bananas

#20 # Faça um gráfico dos pedidos de banana da atividade 19. O gráfico deve ter o dia da semana no eixo X, a hora do dia no eixo Y, 
# e pontos na intersecção dos eixos, onde o tamanho do ponto é determinado pela quantidade média de pedidos de banana 
# nesta combinação de dia da semana com hora

media_bananas %>%
  ggplot(aes(x = order_dow, y = order_hour_of_day, size = media)) +
  geom_point(colour = "blue", alpha = 0.7) +
  ggtitle("Média de Pedidos de Bananas") +
  xlab("Dia da Semana") +
  ylab("Hora do Dia") + 
  scale_x_continuous( breaks = 0:6 )


#21 # Faça um histograma da quantidade média calculada na atividade 19, facetado por dia da semana

media_bananas %>%
  ggplot(aes(x = media)) +
  geom_histogram(bins = 150) +
  facet_wrap( ~ order_dow, ncol = 3 ) +
  theme_bw()


#22 # Teste se há diferença nas vendas por hora entre os dias 3 e 4 usando o teste de wilcoxon e utilizando a simulação da aula de testes

wilcox.test(media ~ order_dow, 
            data = media_bananas, 
            alternative = "two.sided", 
            subset = order_dow %in% c(3, 4), 
            conf.int = TRUE)
