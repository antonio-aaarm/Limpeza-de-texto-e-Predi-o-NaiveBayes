#############
#pacotes
#############

library(tm)
library(SnowballC)
library(wordcloud)
library(gmodels)
library(e1071)

########################
# TRATAMENTO DOS TEXTOS
########################
#Semente aleatoria
set.seed(42)


TMG <- TMG[sample(1:nrow(TMG)),replace=FALSE ]

TMG_corpus <- VCorpus(VectorSource(TMG$Text))

##INFO DO CORPUS##

print(TMG_corpus)


## VER MENSAGEM ESPECIFICA ##

as.character(TMG_corpus_limpo[[16]])


#1) LOWER CASE CONVERT ##

TMG_corpus_limpo <- tm_map(TMG_corpus, content_transformer(tolower))

#2) Remoção de StopWords

TMG_corpus_limpo <- tm_map(TMG_corpus_limpo, removeWords ,stopwords(kind = "pt-br"))
TMG_corpus_limpo <- tm_map(TMG_corpus_limpo, removeWords ,stopwords())
## kind = language

stopwords(kind = "pt-br")

#3) Remoção de Pontuação

TMG_corpus_limpo <- tm_map(TMG_corpus_limpo, removePunctuation)

#4) Remove espaços brancos

TMG_corpus_limpo <- tm_map(TMG_corpus_limpo, stripWhitespace)

#5) Remoção das ramificações de palavras

TMG_corpus_limpo <- tm_map(TMG_corpus_limpo, stemDocument)


wordcloud(TMG_corpus_limpo, min.freq =  50 , random.order = FALSE)

#################################
# FAZENDO O DTM
#################################

TMG_dtm <- DocumentTermMatrix(TMG_corpus_limpo)

#Divisão Treino / Teste

TMG_dtm_treino <- TMG_dtm[1:floor(0.8*nrow(TMG)),]

TMG_dtm_teste  <- TMG_dtm[ceiling(0.8*nrow(TMG)):nrow(TMG),]

#Seprando os rotulos "RESPOSTAS"

TMG_treino_rotulos <- TMG[1:floor(0.8*nrow(TMG)),]$Classificacao

TMG_teste_rotulos  <- TMG[ceiling(0.8*nrow(TMG)):nrow(TMG),]$Classificacao

#filtro para palavras mais frequentes

#Criando lista para palavras mais frequentes

lista_frequentes <- findFreqTerms(TMG_dtm, 5)

#Filtrando dados de treino com as palavras mais frequentes
#remove palavras FORA de LISTRA_FREQUENTES

TMG_dtm_treino <- TMG_dtm_treino[,lista_frequentes]
TMG_dtm_teste <- TMG_dtm_teste[,lista_frequentes]

#Colocando sim e não para palavras com no minimo de uma palavra

converte_dtm <- function(x){
  x <- ifelse(x > 0, "sim", "não")
}

#apply aplica uma função em tudo de uma lista MARGIN=1 Linha MARGIN=2 Coluna

TMG_dtm_treino <- apply(TMG_dtm_treino, MARGIN = 2,converte_dtm)

TMG_dtm_teste <- apply(TMG_dtm_teste, MARGIN = 2,converte_dtm)

##############################
#Naive-Base
##############################
#classificador
classificador_nb <- naiveBayes(TMG_dtm_treino, TMG_treino_rotulos)

#Teste:

TMG_teste_predicoes <- predict(classificador_nb, TMG_dtm_teste)

#analise dos resultados

CrossTable(TMG_teste_predicoes, TMG_teste_rotulos, 
           prop.chisq = FALSE, 
           prop.c = FALSE, 
           prop.r = FALSE)









