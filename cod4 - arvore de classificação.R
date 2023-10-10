#carregando arquivo do banco de dados
setwd("C:/Users/Jose Bruno/Desktop/mestrado")
treino<-fread("treino-at.csv",na.strings = "")
teste<-fread("teste-at.csv",na.strings = "")


# Ajustar a árvore:
set.seed(1234)#10
fit <- rpart(formula=treino$var_resp ~ sexo+raca+escolaridade+nosocomial+puerpera+cardiopati+hematologi+sind_down+hepatica+asma+
               Diabetes+neurologic+pneumopati+imunodepre+renal+obesidade+vacina_cov+
               uti+suport_ven+faixa,
             method = "class",
             data = treino,
             parms = list(split = "gini"),#gini metrica para geracao dos nos
             cp = 0.001#diminui o parametro de complexidade pois com 0,0001 a arvore estava ficando com 80 nos mais durante o processo de poda o melhor_cp foi o mesmo
)#cp nivel de complexidade de cada interacao (no)

rpart.plot(fit,type = 2,extra = 104, box.palette = "GnBu",branch.lty=3,shadow.col = "gray",nn = TRUE, cex=1)

#gerando grafico com as variaveis mais importantes (na propria construcao da arvore ja sao realizado a selecao das mesmas)
fit$variable.importance
barplot(fit$variable.importance)# calculada pela sua contribuicao no gini, essa selecao e automatica da arvore, para vars que nao contribbuiem com o parametro gini a mesma e descartada

#estimativas destas variaveis 
fit$cptable#historico de crescimento da arvore observar qntidade de no em que o xerror vai se tornando constante

#verificar o erro cruzado(xerror) durante cada no na construcao da arvore
plotcp(fit)

# poda:
#parametro complexo com menor valor e menor erro cruzado
melhor_cp <- fit$cptable[which.min(fit$cptable[, "xerror"]),
                         "CP"]#metodo automatico escolhe deacordo com a relacao no xerror
melhor_cp

# contruindo a arvore com o parametro de complexidade que apresentou menor erro
p1fit<-rpart::prune(fit, cp= melhor_cp)
rpart.plot(p1fit,type = 2,extra = 104, box.palette = "GnBu",branch.lty=3,shadow.col = "gray",nn = TRUE, cex=0.6)

pdf('C:/Users/Jose Bruno/Desktop/arquivotest//plot.pdf', width=10, height=9)
rpart.plot(p1fit,type = 2,extra = 104, box.palette = "GnBu",branch.lty=3,shadow.col = "gray",nn = TRUE, cex=0.6)
dev.off()

barplot(p1fit$variable.importance)

#p2fit<-rpart::prune(fit, cp= 0.001493652)
#rpart.plot(p2fit,type = 2,extra = 104, box.palette = "GnBu",branch.lty=3,shadow.col = "gray",nn = TRUE, cex=0.7)


#pfit <- rpart::prune(fit, cp = 0.0016)#apartir da analise do grafico do cptable podemos notar que apartir do 10 no o valor do cp estabiliza
#plotcp(pfit)
#rpart.plot(pfit)

###############################################################
#avaliando o poder de predicao para o grupo de teste
pred1<-predict(object = p1fit,newdata = teste,type = 'class')#prob apresenta probabilidades class ja apresenta as classes
#pred2<-predict(object = p2fit,newdata = teste,type = 'class')


pred1
teste$var_resp

t1<-table(pred1,teste$var_resp)

sen1 <- 	t1[2,2]/(t1[1,2] + t1[2,2])		
esp1 <-	t1[1,1]/(t1[1,1] + t1[2,1])		
vpp1 <- 	t1[2,2]/(t1[2,1] + t1[2,2])	
vpn1 <- 	t1[1,1]/(t1[1,1] + t1[1,2])	
acc1 <- sum(diag(t1))/sum(t1)

round(cbind(sen1, esp1, vpp1, vpn1, acc1)*100,2)


#t2<-table(pred2,teste$var_resp)

#sen2 <- 	t2[2,2]/(t2[1,2] + t2[2,2])		
#esp2 <-	t2[1,1]/(t2[1,1] + t2[2,1])		
#vpp2 <- 	t2[2,2]/(t2[2,1] + t2[2,2])	
#vpn2 <- 	t2[1,1]/(t2[1,1] + t2[1,2])	
#acc2 <- sum(diag(t2))/sum(t2)

#round(cbind(sen2, esp2, vpp2, vpn2, acc2)*100,2)

