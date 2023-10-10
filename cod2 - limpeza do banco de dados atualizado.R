#selecionando diretorio de trabalho e conjunto de dados 
setwd("C:/Users/Jose Bruno/Desktop/mestrado")
fread(input="INFLUD22-12-09-2022.csv")
dados <- fread('INFLUD22-12-09-2022.csv')

table(dados$UTI, dados$SG_UF)
#selecionado os criterios de inclusao para o conjunto de dados
# Brasileiros (residentes no Brasil)
# PR
# Hospitalizados
# Covid
# Adultos (18 anos ou mais)
# Casos Notificados em 2022

dados1 <- subset(dados, CLASSI_FIN==5 & ESTRANG==2)
dados1 <- subset(dados, CLASSI_FIN==5 & HOSPITAL==1 & SG_UF=='PR' & ESTRANG==2 & TP_IDADE==3 & NU_IDADE_N>=18)

### variaveis de interesse
# Obito (desfecho; variavel resposta)
# Vacina (sim ou nao)
# Cadastrais(caracteristicas)
# Comorbidades
# Sintomas**************
# UTI

Dados<-dplyr::select(dados1, TP_IDADE, CS_SEXO, NU_IDADE_N, CS_RACA, CS_ESCOL_N,
                     ID_MN_RESI, CS_ZONA, NOSOCOMIAL, FEBRE, TOSSE, GARGANTA, DISPNEIA, DESC_RESP, SATURACAO, DIARREIA, VOMITO, DOR_ABD,
                     FADIGA, PERD_OLFT, PERD_PALA, OUTRO_SIN, FATOR_RISC, PUERPERA, CARDIOPATI, HEMATOLOGI, SIND_DOWN, HEPATICA, ASMA,
                     DIABETES, NEUROLOGIC, PNEUMOPATI, IMUNODEPRE, RENAL, OBESIDADE, OBES_IMC, VACINA_COV, DOSE_1_COV, DOSE_2_COV, DOSE_REF,
                     VACINA, DT_UT_DOSE, ANTIVIRAL, TP_ANTIVIR, HOSPITAL, UTI, SUPORT_VEN, EVOLUCAO)#para selecionar as variaveis de interesse
table(Dados$EVOLUCAO)

#####RENOMEANDO AS RESPOSTAS DAS VARIAVEIS
dados2 <- with(Dados, data.frame(
  sexo  = recode_factor(Dados$CS_SEXO, M='M', F='F', I=NULL),
  rac  = recode_factor(Dados$CS_RACA, '1'='Branca', '2'='Preta', '3'='Amarela', '4'='Parda', '5'='Indigena', '9'=NULL),#9=null ou 9=nao informado
  escolaridad = recode_factor(Dados$CS_ESCOL_N, '0'='Sem', '1'='Fundamental1', '2'='Fundamental2', '3'='Medio', '4'='Superior', '5'=NULL, '9'=NULL),#ver com o prof
  zona =  recode_factor(Dados$CS_ZONA, '1'='urbana', '2'='rural', '3'='periurbana', '9'=NULL),
  nosocomial =  recode_factor(Dados$NOSOCOMIAL, '1'='sim', '2'='não', '9'='não', .missing = 'não'),
  febre =  recode_factor(Dados$FEBRE, '1'='sim', '2'='não', '9'='não', .missing = 'não'),
  tosse =  recode_factor(Dados$TOSSE, '1'='sim', '2'='não', '9'='não', .missing = 'não'),
  garganta =  recode_factor(Dados$GARGANTA, '1'='sim', '2'='não', '9'='não', .missing = 'não'),
  dispneia =  recode_factor(Dados$DISPNEIA, '1'='sim', '2'='não', '9'='não', .missing = 'não'),
  desc_resp =  recode_factor(Dados$DESC_RESP, '1'='sim', '2'='não', '9'='não', .missing = 'não'),
  saturacao =  recode_factor(Dados$SATURACAO, '1'='sim', '2'='não', '9'='não', .missing = 'não'),
  diarreia =  recode_factor(Dados$DIARREIA, '1'='sim', '2'='não', '9'='não', .missing = 'não'),
  vomito =  recode_factor(Dados$VOMITO, '1'='sim', '2'='não', '9'='não', .missing = 'não'),
  dor_abd =  recode_factor(Dados$DOR_ABD, '1'='sim', '2'='não', '9'='não', .missing = 'não'),
  fadiga =  recode_factor(Dados$FADIGA, '1'='sim', '2'='não', '9'='não', .missing = 'não'),
  perd_olft =  recode_factor(Dados$PERD_OLFT, '1'='sim', '2'='não', '9'='não', .missing = 'não'),
  perd_pala =  recode_factor(Dados$PERD_PALA, '1'='sim', '2'='não', '9'='não', .missing = 'não'),
  outro_sin =  recode_factor(Dados$OUTRO_SIN, '1'='sim', '2'='não', '9'='não', .missing = 'não'),
  fator_risc =  recode_factor(Dados$FATOR_RISC, '1'='sim', '2'='não', '9'='não', .missing = 'não'),
  puerpera =  recode_factor(Dados$PUERPERA, '1'='sim', '2'='não', '9'='não', .missing = 'não'),
  cardiopati =  recode_factor(Dados$CARDIOPATI, '1'='sim', '2'='não', '9'='não', .missing = 'não'),
  hematologi =  recode_factor(Dados$HEMATOLOGI, '1'='sim', '2'='não', '9'='não', .missing = 'não'),
  sind_down =  recode_factor(Dados$SIND_DOWN, '1'='sim', '2'='não', '9'='não', .missing = 'não'),
  hepatica =  recode_factor(Dados$HEPATICA, '1'='sim', '2'='não', '9'='não', .missing = 'não'),
  asma =  recode_factor(Dados$ASMA, '1'='sim', '2'='não', '9'='não', .missing = 'não'),
  Diabetes =  recode_factor(Dados$DIABETES, '1'='sim', '2'='não', '9'='não', .missing = 'não'),
  neurologic =  recode_factor(Dados$NEUROLOGIC, '1'='sim', '2'='não', '9'='não', .missing = 'não'),
  pneumopati =  recode_factor(Dados$PNEUMOPATI, '1'='sim', '2'='não', '9'='não', .missing = 'não'),
  imunodepre =  recode_factor(Dados$IMUNODEPRE, '1'='sim', '2'='não', '9'='não', .missing = 'não'),
  renal =  recode_factor(Dados$RENAL, '1'='sim', '2'='não', '9'='não', .missing = 'não'),
  obesidade =  recode_factor(Dados$OBESIDADE, '1'='sim', '2'='não', '9'='não', .missing = 'não'),
  vacina_cov = recode_factor(Dados$VACINA_COV, '1'='sim', '2'='não', '9'=NULL , .default = NULL ),
  vacina =  recode_factor(Dados$VACINA, '1'='sim', '2'='não', '9'=NULL),
  antiviral =  recode_factor(Dados$ANTIVIRAL, '1'='sim', '2'='não', '9'=NULL),
  tp_antivir =  recode_factor(Dados$TP_ANTIVIR, '1'='oseltamivir', '2'='zanamivir', '9'=NULL),
  uti =  recode_factor(Dados$UTI, '1'='sim', '2'='não', '9'='não', .missing = 'não'),
  suport_ven =  recode_factor(Dados$SUPORT_VEN, '1'='invasivo', '2'='não_ivasivo', '3'='não',  '9'='não', .missing = 'não'),
  var_resp = recode_factor(Dados$EVOLUCAO, '1'='0', '2'='1', '3'='1', '9'='1', .missing = '1'),#'1'='cura', '2'='obito', '3'='obito por outras causas', '9'=NULL(para os dados 0 e cura 1 obito)
  faixa = case_when(NU_IDADE_N<60 & NU_IDADE_N>17 & TP_IDADE==3 ~ 'Adulto', NU_IDADE_N>59 & TP_IDADE==3 ~ 'Idoso')
))

#########NOS SINTOMAS QUANDO A RESPOSTA FOR NULL ASSUMIR QUE NÃO TEVE AQUELE SINTOMA pois se nao foi colocado nao foi relevante para se relatar na ficha
#caso contrario manter Nas
table(dados$vacina_cov,useNA='always')

#alteracao na ordem de organizacao das respostas das variaveis raca e escolaridade
dados2$raca<-factor(dados2$rac,levels = c("Branca","Amarela","Parda","Preta","Indigena"))
dados2$escolaridade<-factor(dados2$escolaridad,levels = c("Sem","Fundamental1","Fundamental2","Medio","Superior"))

DATA<-with(dados2, data.frame(sexo,raca,escolaridade,nosocomial,puerpera,cardiopati,hematologi,sind_down,hepatica,asma,
                              Diabetes,neurologic,pneumopati,imunodepre,renal,obesidade,vacina_cov,uti,suport_ven,
                              var_resp,faixa))
DATA1<-stats::na.omit(DATA)
set.seed(123)
aux <- sample(1:8421,3609)
teste<- DATA[aux, ]
treinamento<- DATA[-aux, ]

treinamento$var_resp<-recode_factor(treinamento$var_resp,"0"="cura", "1"="obito")
teste$var_resp<-recode_factor(teste$var_resp,"0"="cura", "1"="obito")

#exportar base de dados#
fwrite(DATA,file = 'dados-atualizado2.csv',sep=';')
fwrite(treinamento,file = 'treino-nb.csv',sep=';')
fwrite(teste,file = 'teste-nb.csv',sep=';')

