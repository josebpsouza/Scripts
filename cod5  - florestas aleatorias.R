#carregando arquivo do banco de dados
setwd("C:/Users/Jose Bruno/Desktop/mestrado")
treino<-fread("treino-at.csv",na.strings = "")
teste<-fread("teste-at.csv",na.strings = "")

treino$var_resp<-recode_factor(treino$var_resp,"cura"="0", "obito"="1")
teste$var_resp<-recode_factor(teste$var_resp,"cura"="0", "obito"="1")

treino<-stats::na.omit(treino)
teste<-stats::na.omit(teste)


glimpse(teste)
glimpse(treino)

#levels(teste$var_resp)<-c("0","1")
#levels(treino$var_resp)<-c("0","1")
########################usando metodo de selecao manual para observacao de melhor mtry e ntree#############################
# Definindo a semente
set.seed(123)

####### Ajustar manualmente mtry e ntree ##################
customRF <- list(type = "Classification",#modelo de classificacao
                 library = "randomForest",
                 loop = NULL)

customRF$parameters <- data.frame(parameter = c("mtry", "ntree"),
                                  class = rep("numeric", 2),
                                  label = c("mtry", "ntree"))

customRF$grid <- function(x, y, len = NULL, search = "grid") {}

customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs) {
  randomForest(x, y,
               mtry = param$mtry,
               ntree=param$ntree)}

# Predizer a classe
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)

# Predizer a probabilidade
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")

customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes
###################################
# Vamos utilizar uma validação-cruzada 10-fold
set.seed(123)
ctrl <- trainControl(method = "cv", 
                     number = 10,
                     allowParallel = T) # paralelizar a avaliacao

grid <- expand.grid(.mtry = c(1:20), # número de variáveis
                    .ntree = c(500,1000,1500)) # número de árvores

rfFit <- train(var_resp ~ .,
               method = customRF, # metodo gerado para avaliar mtrey ntree
               tuneGrid = grid, # as variaveis e arvores quem e quantas avaliar
               trControl = ctrl,
               metric = "Accuracy",# parametro de escolha
               data = treino) 
rfFit
plot(rfFit)
plot(rfFit$finalModel)
legend("topright", colnames(rfFit$finalModel$err.rate), col = 1:3, cex = 0.8, fill = 1:3)


#summary(rfFit)

########### Importância das Variáveis ########
#com base no codigo anterior temos que o melhor modelo foi com mtry=4 e ntree=500
set.seed(123)
rf <- randomForest(var_resp ~ ., data = treino, 
                   importance = T, mtry = 3, ntree = 500)
rf

# Gráfico da importância das variáveis pelo gini
varImpPlot(rf, sort = T)

# Quantas vezes cada variável explicativa foi utilizada na construção das árvores
varUsed(rf, count = T)

########## Predicao para o conjunto de teste  ########

predrf <- predict(rfFit, teste, type = "prob") 
resultrf <- as.factor(ifelse(predrf[,2] > 0.5,"1","0"))

### Desempenho do modelo ###

# Matriz de confusão e medidas
library(caret)
confusionMatrix(resultrf, teste$var_resp, positive = "1")

### Curva ROC e AUC#####
library(pROC)
saida1 <- data.frame(p_hat=fitted(rfFit), y=treino$var_resp)

### Construcao da curva ROC


aucrf <- roc(teste$var_resp, predrf[,2])
r1<-plot.roc(aucrf, print.thres = T, xlab = "especifidade", ylab = "sensibilidade") # descobrimos o ponto de corte que fornece melhor soma de S e E

g1 <- ggroc(r1, legacy.axes=T, size=2, col='steelblue') + labs(x='1-Especificidade', y='Sensibilidade') + theme(axis.title=element_text(size=22), axis.text=element_text(size=18)) + annotate(geom="text", x=.6, y=.75, label='AUC = 0.76', color="black", size=9)
g1

# Usando o novo ponto de corte
coords(aucrf, "best")

resultrf2 <- as.factor(ifelse(predrf[,2] > 0.295 ,"1","0"))
confusionMatrix(resultrf2, teste$var_resp, positive = "1")


