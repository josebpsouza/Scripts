#carregando arquivo do banco de dados
setwd("C:/Users/Jose Bruno/Desktop/mestrado")
treino<-fread("treinosvm-at.csv",na.strings = "")
teste<-fread("testesvm-at.csv",na.strings = "")
#omissao de obs com na
treino<-stats::na.omit(treino)
teste<-stats::na.omit(teste)

treino$var_resp = factor(treino$var_resp)
teste$var_resp = factor(teste$var_resp) 

y<-teste$var_resp
#validacao cruzada 
set.seed(123)
tune.out<-tune(svm,var_resp~., data= treino, kernel = "radial",
               ranges= list(cost= c(0.001, 0.01, 0.1, 1, 10, 100, 1000),gamma=c(0.5, 1,2)))#ver e fazer com todos os kernels possiveis
summary(tune.out)
plot(tune.out)
print(tune.out)

#######################testando kernels variando os mesmos coast e gamma ###########################
set.seed(123)
tune.outr<-tune(svm,var_resp~., data= treino, kernel = "radial",
               ranges= list(cost= c(0.001, 0.01, 0.1, 1, 10, 100, 1000),gamma=c(0.5, 1,2)))#menor erro 0.2714012 gama = 0,5 cost = 1 
summary(tune.outr)
plot(tune.outr)
print(tune.outr)

tune.outl<-tune(svm,var_resp~., data= treino, kernel = "linear",
               ranges= list(cost= c(0.001, 0.01, 0.1, 1, 10, 100, 1000),gamma=c(0.5, 1,2)))#menor erro 0.1949808 gama = 0,5 cost = 1000 tentar com (cost 1000, 1010, 1100)
summary(tune.outl)
plot(tune.outl)
print(tune.outl)

tune.outs<-tune(svm,var_resp~., data= treino, kernel = "sigmoid",
               ranges= list(cost= c(0.001, 0.01, 0.1, 1, 10, 100, 1000),gamma=c(0.5, 1,2)))#menor erro 0.2068534 gama = 2 cost = 0,01
summary(tune.outs)
plot(tune.outs)
print(tune.outs)

tune.outp<-tune(svm,var_resp~., data= treino, kernel = "polynomial",
               ranges= list(cost= c(0.001, 0.01, 0.1, 1, 10, 100, 1000),gamma=c(0.5, 1,2)))#menor erro 0.2140859 gama = 0,5 cost = 0,001
summary(tune.outp)
plot(tune.outp)
print(tune.outp)
################################################

set.seed(123)




tune.outl<-tune(svm,var_resp~., data= treino, kernel = "linear",
                ranges= list(cost= c(1000, 1010, 1100),gamma=c(0.5)))#menor erro 0.1949808 gama = 0,5 cost = 1000 tentar com (cost 1000, 1010, 1100) 0.1983182  1010 0,5 não adiantou
summary(tune.outl)
plot(tune.outl)
print(tune.outl)

tune.outs<-tune(svm,var_resp~., data= treino, kernel = "sigmoid",
                ranges= list(cost= c(0.01),gamma=c(2, 5, 10)))#menor erro 0.2068534 gama = 2 cost = 0,01
summary(tune.outs)
plot(tune.outs)
print(tune.outs)

tune.outp<-tune(svm,var_resp~., data= treino, kernel = "polynomial",
                ranges= list(cost= c(0.001, 0.0001),gamma=c(0.5, 0.05)))#menor erro 0.2140859 gama = 0,5 cost = 0,001
summary(tune.outp)
plot(tune.outp)
print(tune.outp)
#####################################################################################################################
#tune_svm <- tune(svm, var_resp~., data= treino, kernel = "radial", ranges = list(cost=10^(-1:2)), gamma=c(0.5, 1,2))
#summary(tune_svm)
#plot(tune_svm)                 



# de acordo com a validacao cruzada obtemos o melhor valor para coast=1 para o kernel radial que traz um erro de 0.208814, desta forma construimos o modelo 
classif<-svm(formula = var_resp~.,
             data = treino,
             type = "C-classification",
             kernel = "sigmoid",
             cost = 0.01,
             gamma = 1)
classif

table(treino$var_resp,fitted(classif))
pred1<-predict(classif, teste)
mc<-table(teste$var_resp,pred1)

confusionMatrix(mc)
plot(pred1)

#acuracia obtida 78%

