

install.packages("caret")
install.packages("ISLR")
install.packages("randomForest")
library(caret)
library(ISLR)
library(randomForest)

set.seed(123)

dados <- Smarket[,c(2:6,9)]

indices <- sample(1:nrow(dados), round(0.8*nrow(dados)),replace = FALSE)

dados_treino <-dados[indices,]
dados_teste <-dados[-indices,]


#####################
#Floresta Ramdomica#
#####################

set.seed(123)

floresta <- randomForest(Direction ~ .,
                         data = dados_treino,
                         ntree=500)

predições <- predict(floresta,dados_teste)

confusionMatrix(predições, dados_teste$Direction)


