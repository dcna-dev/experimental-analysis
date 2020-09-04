# Instala e carrega as bibliotecas necessárias
install.packages('dplyr', 'googlesheets4', 'janitor')
library(dplyr)
library(googlesheets4)
library(janitor)

# Lê aba Piloto da planilha do Google e salva na variável experimental_data.
# No primeiro acesso vai pedir permissão e depois essa autenticação fica em cache
experimental_data = read_sheet(GOOGLESHEET_URL, sheet = SHEET )

# Trata nomes das colunas
experimental_data <- experimental_data %>% clean_names()
names(experimental_data)[names(experimental_data) == "please_give_an_assessment_of_the_clarity_of_the_abstract_choosing_a_number_on_the_scale_of_1_10_below_where_a_value_of_1_represents_very_obscure_and_10_represents_extremely_clearly_written"] <- "clarity_of_the_abstract"

# Armazena os nomes das colunas na lista questions
questions = colnames(experimental_data)

# Converte os dados das colunas 6 a 15 (questões sobre Completeness) para valores numéricos
# de acordo com o definido no artigo
for (i in seq(6, 15)) {
	experimental_data[questions[i]][experimental_data[questions[i]] == "Yes"] <- '1'
	experimental_data[questions[i]][experimental_data[questions[i]] == "No"] <- '0'
	experimental_data[questions[i]][experimental_data[questions[i]] == "Partly"] <- '0.50'
	experimental_data[questions[i]][experimental_data[questions[i]] == "Not applicable"] <- NA
}

# Converte os dados numerico para o tipo numero (estão como character)
experimental_data[,6:16] <- lapply(experimental_data[,6:16], function(x) as.numeric(as.character(x)))

# Calcula a média das respostas para completeness para cada juiz e paper e salva em uma
# nova coluna chamada mean_completeness_specific_judge na variável experimental_data
experimental_data$mean_completeness_specific_judge <- rowMeans(experimental_data[,6:15], na.rm=TRUE)

# Salva os IDs únicos dos abstracts em uma lista
abstracts_ids <- unique(experimental_data$add_abstract_id)

# Cria um novo dataframe para armazenar os resultados que serão analisados
results <- data.frame(
    Abstract=character(),
    Treatment=character(),
    Journal=character(),
    Timeperiod=integer(),
    MeanCompleteness=double(),
    MedianCompleteness=double(),
    MedianClarity=double(),
    MeanClarity=double(),
    VarCompleteness=double(),
    VarClarity=double()
)

# Faz um loop usando o ID do abstract para filtrar os resultados e salva na variavel z,
# cria um novo datafram chamado abstract_results para armazenar os dados consolidados
# de cada abstract em cada iteração. Ao final, insere os dados do abstract no dataframe
# results.
# z <- experimental_data %>% filter(add_abstract_id == abstract) - Filtra os dados pelo ID do abstract,
# cada abstract tem 4 linhas de dados.
# A função mean calcula a média dos valores de uma coluna
# A função median calcula a mediana dos valores de uma coluna
# A função var calcula a variância dos valores de uma coluna
for (abstract in abstracts_ids) {
    z <- experimental_data %>% filter(add_abstract_id == abstract)
	abstract_results <- data.frame(
        Abstract=abstract,
	    Treatment=NA,
	    Journal=NA,
	    Timeperiod=NA,
	    MeanCompleteness=mean(z$mean_completeness_specific_judge),
	    MedianCompleteness=median(z$mean_completeness_specific_judge),
	    MedianClarity=median(z$clarity_of_the_abstract),
	    MeanClarity=mean(z$clarity_of_the_abstract),
	    VarCompleteness=var(z$mean_completeness_specific_judge),
	    VarClarity=var(z$clarity_of_the_abstract)
    )
	results <- rbind(results, abstract_results)
}