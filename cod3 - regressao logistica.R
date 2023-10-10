###########################################################carregadno pacotes########
require(Rmisc)
require(dplyr)
require(ggplot2)
require(ggformula)
require(hnp)
require(data.table)
require(GGally)
require(survival) ### Pacotes para An. Sobrevivencia
require(lubridate) ### Pacote para trabalhar com datas
require(fpp2)
require(pROC)
require(ggplot)
require(statmod)
require(MuMIn)
require(pROC)
require(caret)
#################################################################################
#carregar diretorio e conjunto de dados
setwd("C:/Users/Jose Bruno/Desktop/mestrado")
treino<-fread("treino-at.csv",na.strings = "")
teste<-fread("teste-at.csv",na.strings = "")

table(treino$var_resp)

#ifelse
treino$var_resp<-ifelse(treino$var_resp == "cura" ,0,1 )
teste$var_resp<-ifelse(teste$var_resp == "cura" ,0,1)

table(treino$var_resp)
class(treino$var_resp)


#treino$var_resp<-as.numeric(treino$var_resp)
#teste$var_resp<-as.numeric(teste$var_resp)

treino$escolaridade = recode_factor(treino$escolaridade, '0'='Sem', '1'='Fundamental 1', '2'='Fundamental 2', '3'='Medio', '4'='Superior', '5'=NULL, '9'=NULL)#organizando niveis de escolaridade
teste$escolaridade = recode_factor(teste$escolaridade, '0'='Sem', '1'='Fundamental 1', '2'='Fundamental 2', '3'='Medio', '4'='Superior', '5'=NULL, '9'=NULL)#organizando niveis de escolaridade

#treino$var_resp<-recode_factor(treino$var_resp,"obito"="1","cura"="0" )
#teste$var_resp<-recode_factor(teste$var_resp,"obito"="1","cura"="0")

treino<-stats::na.omit(treino)
teste<-stats::na.omit(teste)


####################################### analise descritiva do conjunto de todos os dados ################################################
#glimpse(dados)#tipo de variavel(fct, chr, etc)
table(DATA1$sexo)#tabela de frequencia absoluta

#####tabela de frequecia relativa
prop.table(table(dados$sexo))#para uma unica variavel
prop.table(table(dados$sexo,dados$raca))#com mais de uma variavel

#####tabela de frequencia cruzada
table(dados$sexo,dados$raca)#cruza frequencia de duas variaveis 
table(dados$sexo,dados$raca,dados$Diabetes)#duas tabelas se terceira variavel sim e se nao

###################################### graficos descritivos ##########################################
#grafico de barras
p1<-ggplot(dados, aes(x=sexo)) + geom_bar(width=0.7, col='black', fill="steelblue") + labs(x='sexo', y='Frequencia') + theme(axis.title=element_text(size=22), axis.text=element_text(size=18))
p1
#grafico de pontos dispersao
p2<-ggplot(dados, aes(x=sexo, y=var_resp)) + geom_jitter(size=3, col='#FC4E07') + xlab('sexo') + ylab('variavel resposta') + theme(axis.title=element_text(size=22), axis.text=element_text(size=18))
p2

################################################################################
#--------------------------------------------------#
#---- Modelo de RegressÃo Logística ---------------#
#--------------------------------------------------#
#selecionando variaveis para o modelo
#dados1<-dplyr::select(treino,var_resp,sexo,raca,escolaridade,nosocomial,puerpera,cardiopati,hematologi,
#                      sind_down,hepatica,asma,Diabetes,neurologic,pneumopati,imunodepre,renal,obesidade,
#                      vacina_cov,uti,suport_ven,faixa)
#retirando as obs com Na
#dados2<-stats::na.omit(dados1)
#teste<-stats::na.omit(teste)

set.seed(123)
fit0<-glm(var_resp~sexo+raca+escolaridade+nosocomial+puerpera+cardiopati+hematologi+
            sind_down+hepatica+asma+Diabetes+neurologic+pneumopati+imunodepre+renal+obesidade+
            vacina_cov+uti+suport_ven+faixa, data = treino,family = binomial); 

summary(fit0)
#######################analise de residuos############################
#aplicar razao de chances exp dos betas 
#residuos vs preditores
xx<-qresiduals(fit0)

pred<-fitted(fit0, type='response')

dd <- data.frame(residuals=xx, preditores=pred)

p1 <- ggplot(dd, aes(x=pred, y=xx)) + geom_point(size=2) + geom_hline(yintercept=c(-2,2), linetype=2) + xlab("Preditos") + ylab("Residuos")
p1

hnp(xx, half=F, pch=16, cex=1.1,print=T)

############################usando a funçao step wise para selecionar apenas variaveis esta. significativas (selecao automatica)###############
require(MASS)
fit1 <- step(fit0)

summary(fit1)

exp(cbind(or = coef(fit1), confint.default(fit1)))

###############################analise de residuos do novo modelo###################################
set.seed(123)
xx1<-qresiduals(fit1)

pred1<-fitted(fit1, type='response')

dd1 <- data.frame(residuals=xx1, preditores=pred1)

p2 <- ggplot(dd1, aes(x=pred1, y=xx1)) + geom_point(size=2) + geom_hline(yintercept=c(-2,2), linetype=2) + xlab("Preditos") + ylab("Residuos")
p2

hnp(xx1, half=F, pch=16, cex=1.1,print=T)


# which(abs(xx1)>3) # ver as obs que aparececem fora do intervalo

######matriz de confusao
#saida <- data.frame(p_hat=fitted(fit1), y=treino$var_resp)

#treino$pdata <- as.factor(
#  ifelse(
#    predict(fit1, 
#            newdata = treino, 
#            type = "response")
#    >0.5,"1","0"))

#treino$pdata
#treino$var_resp

#summary(treino$pdata)

#levels(treino$pdata)
#levels(treino$var_resp)

#levels(treino$pdata)<-c("1","0")

#mc1<-confusionMatrix(treino$pdata, treino$var_resp, positive = "1")
#mc1
#--------------------------------------------------#
#---- ConstruÃ§Ã£o da Curva ROC ---------------------#
#--------------------------------------------------#

### Base com valores ajustados e observados 
saida1 <- data.frame(p_hat=fitted(fit1), y=treino$var_resp)

### Construcao da curva ROC
r1 <- roc(y ~ p_hat, data=saida1)
g1 <- ggroc(r1, legacy.axes=T, size=2, col='steelblue') + labs(x='1-Especificidade', y='Sensibilidade') + theme(axis.title=element_text(size=22), axis.text=element_text(size=18)) + annotate(geom="text", x=.6, y=.75, label='Teste 1', color="black", size=9)
g1

### Tabela com ponto de corte
coords(r1, 'all')  ### todos pontos de corte possiveis
coords(r1, 'best') ### melhor ponto de corte

### Medidas de qualidade preditiva
pc <- coords(r1, 'best')$threshold
pc
saida1$y_hat <- ifelse(saida1$p_hat<pc, 0, 1)

ta <- table(saida1$y_hat, saida1$y, dnn=c('Predito', 'Observado'))
ta


sen <- 	ta[2,2]/(ta[1,2] + ta[2,2])		### P(Y_hat=1 | Y=1)
esp <-	ta[1,1]/(ta[1,1] + ta[2,1])		### P(Y_hat=0 | Y=0)
vpp <- 	ta[2,2]/(ta[2,1] + ta[2,2])		### P(Y=1 | Y_hat=1)
vpn <- 	ta[1,1]/(ta[1,1] + ta[1,2])		### P(Y=0 | Y_hat=0)

round(cbind(sen, esp, vpp, vpn)*100,2)

round(coords(r1, 'best', ret=c('thershold', 'sensitivity', 'specificity', 'ppv', 'npv' ), transpose=T)*100,2)

auc(r1)#area sobre a curva



###############################modelo aplicado ao grupo teste###################################################

pre3<-predict(fit1, type='response', newdata = teste)
pre3


### Construcao da curva ROC teste
saida3 <- data.frame(p_hat=pre3, y=teste$var_resp)
auc(r3)
r3 <- roc(y ~ p_hat, data=saida3)	
g3 <- ggroc(r3, legacy.axes=T, size=2, col='steelblue') + labs(x='1-Especificidade', y='Sensibilidade') + theme(axis.title=element_text(size=22), axis.text=element_text(size=18)) + annotate(geom="text", x=.6, y=.75, label='AUC = 0.78 ', color="black", size=9)
g3

### Tabela com ponto de corte
coords(r3, 'all')  ### todos pontos de corte possiveis
coords(r3, 'best') ### melhor ponto de corte

### Medidas de qualidade preditiva teste
pc3 <- coords(r3, 'best')$threshold
pc3

saida3$y_hat <- ifelse(saida3$p_hat<pc, 0, 1)

ta3 <- table(saida3$y_hat, saida3$y, dnn=c('Predito', 'Observado'))

sen <- 	ta3[2,2]/(ta3[1,2] + ta3[2,2])		### P(Y_hat=1 | Y=1)
esp <-	ta3[1,1]/(ta3[1,1] + ta3[2,1])		### P(Y_hat=0 | Y=0)
vpp <- 	ta3[2,2]/(ta3[2,1] + ta3[2,2])		### P(Y=1 | Y_hat=1)
vpn <- 	ta3[1,1]/(ta3[1,1] + ta3[1,2])		### P(Y=0 | Y_hat=0)
acc <- sum(diag(ta3))/sum(ta3)

round(cbind(sen, esp, vpp, vpn, acc)*100,2)

round(coords(r3, 'best', ret=c('thershold', 'sensitivity', 'specificity', 'ppv', 'npv' , 'accuracy'), transpose=T)*100,2)

acc
auc(r3)


### Tabela com ponto de corte teste
coords(r3, 'all')  ### todos pontos de corte possiveis
coords(r3, 'best') ### melhor ponto de corte

### Medidas de qualidade preditiva teste
pc3<- coords(r3, 'best')$threshold
pc3

 ######matriz de confusao teste#####
teste$pdata <- as.factor(
  ifelse(pre3 > pc3,"1","0"))

levels(teste$pdata)
levels(teste$var_resp)

levels(teste$pdata)<-c("1","0")

teste$pdata
teste$var_resp

summary(teste$pdata)

cm3<-confusionMatrix(teste$pdata, teste$var_resp, positive = "1")
cm3




#############################################################


#dados2$observ <- as.factor(
#  ifelse(
#    dados2$var_resp
#    >0.5,"1","0"))


dados1$pdata <- as.factor(
  ifelse(
    predict(fit1, 
           newdata = dados1, 
            type = "response")
   >0.5,"1","0"))


cmat <- confusionMatrix(dados1$pdata, dados1$var_resp, positive = "1")
fourfoldplot(cmat$table, color = c("cyan", "pink"),
             conf.level = 0, margin = 1, main = "Matriz de confusão")

