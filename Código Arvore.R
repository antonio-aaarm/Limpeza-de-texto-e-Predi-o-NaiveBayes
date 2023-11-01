

library(caret)
library(MASS)
library(rpart.plot)



set.seed(123)

dados <- iris

indicies_treino <- createDataPartition(dados$Species, p = 0.8, list = FALSE)

dados_treino <-dados[indicies_treino,]
dados_teste <-dados[-indicies_treino,]


########
#ARVORE#
########
controle <- trainControl(method = "cv", classProbs = TRUE)

set.seed(123)
DTModel <- train(Species ~.,
                 data =  dados_treino,
                 method = "rpart",
                 metric = "ROC",
                 parms = list(split = "gini"),
                 trControl = controle)


DTModel


#graficamente DALE

prp(DTModel$finalModel, box.palette = "Reds", tweak = 1.2, varlen = 20)





