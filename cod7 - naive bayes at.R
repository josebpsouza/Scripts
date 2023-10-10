##########diretorio de trabalho e banco de dados##########
setwd("C:/Users/Jose Bruno/Desktop/mestrado")
treino<-fread("treino-nb.csv",na.strings = "")
teste<-fread("teste-nb.csv",na.strings = "")


levels(treino$var_resp)<-c("cura","obito")
levels(teste$var_resp)<-c("cura","obito")

teste$var_resp<-recode_factor(teste$var_resp,"cura"="cura", "obito"="obito")
treino$var_resp<-recode_factor(treino$var_resp,"cura"="cura", "obito"="obito")


treinoNB$escolaridade<-factor(treinoNB$escolaridad,levels = c("Sem","Fundamental1","Fundamental2","Medio","Superior"))
testeNB$escolaridade<-factor(testeNB$escolaridad,levels = c("Sem","Fundamental1","Fundamental2","Medio","Superior"))


treino<-stats::na.omit(treino)
teste<-stats::na.omit(teste)

##########configurando o algoritimo##########
set.seed(000)
##controle com ten fold repetido 3 vezes
ctrl = trainControl(method = "repeatedcv",
                    number = 10,
                    repeats = 3,#quantidade de repetição
                    classProbs = TRUE,
                    summaryFunction = twoClassSummary)#como a var resp e ditocomica binaria
#tune
tuneGrid <- expand.grid(
  usekernel = TRUE,#tipo de distribuição (normal ou não)
  fL = 0:1,#suavizador de laplace (vou somar 1 ou não)
  adjust = 1:5)#largura de banda ( 1, 2, 3, 4 ou 5 desvios padroes)
#seleção do modelo
nbfit<- train( var_resp ~ sexo+raca+escolaridade+nosocomial+puerpera+cardiopati+hematologi+sind_down+hepatica+asma+Diabetes+neurologic+pneumopati+imunodepre+renal+obesidade+vacina_cov+uti+suport_ven+faixa,
               method= "nb",
               preProcess = c("center","scale"),#pre processo para var continuas
               tuneLength = 3,#quantas vezes vou avaliar
               trControl = ctrl,
               tuneGrid = tuneGrid,
               metric = "accuracy",#metrica de avaliação do desempenho do modelo (auc)
               data = treino)
#estimativas para escolha do melhor ajuste para o modelo
nbfit

plot(nbfit)

##########aplicando o modelo para o banco de teste#########
prednb<-predict(nbfit, teste, type = "prob")

resultnb<- as.factor(ifelse(prednb[,2]>0.5,"obito", "cura"))

confusionMatrix(resultnb, teste$var_resp, positive = "obito")

##########selecionando o melhor ponto de corte para as probabilidades estimadas##########
aucnb<-roc(teste$var_resp, prednb[,2])


g1 <- ggroc(r3, legacy.axes=T, size=2, col='steelblue') + labs(x='1-Especificidade', y='Sensibilidade') + theme(axis.title=element_text(size=22), axis.text=element_text(size=18)) + annotate(geom="text", x=.6, y=.75, label='AUC = 0.59', color="black", size=9)
g1


r3<-plot.roc(aucnb, print.thres = T)

resultnb2<- as.factor(ifelse(prednb[,2]>0.523,"obito", "cura"))
confusionMatrix(resultnb2, teste$var_resp, positive = "obito")#melhor modelo
