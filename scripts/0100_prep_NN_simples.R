
df_nn<-train[,c("lota","arte_eu","ETAMANHO","EESPECIE","VVENDA","QVENDA")]
# 
# ,
#                 "HOM","HKE","JOD","EOI","BRB",
#                 "PLE","BIB","BSS", "SBA", "BLL",
#                 "SOL", "MUR", "CET", "BRF", "CTC"

# y_nn<-teste[teste$EESPECIE%in%c("LEP","GUU","GUR","GUN","LDV"),]$p.GUU
y_nn<-train$prop.GUU
# y_nn<-log(y_nn+0.01)

# Temos de normalizar os inputs:
# 
# Categóricos: lota,arte\_eu,ETAMANHO,EESPECIE
encoder<-caret::dummyVars("~.",data=df_nn)
funcionou<-predict(encoder,newdata=df_nn)

# No final, ficamos com 66 variáveis
# 
# Agora, só nos resta escalar as variaveis numericas: atenção ao efieto de correcção do zero

#VVENDA SERVE COMO PONTO DE REFERENCIA PARA TRANSFORMAÇOES NUMERICAS - tem quer ser a primeira variavel numerica
# names(data.frame(funcionou))# para ver os indices da numerica
funcionou<-data.frame(funcionou)

first_num<-which(names(funcionou)=="VVENDA")

#necessario forçar a conversao para data.frame porque pode dar-se o caso de so haver uma variavel numerica e a coluna e tratada como um vector
for(i in first_num:dim(funcionou)[2]){
  funcionou[,i]<-log(funcionou[,i]+0.01)}

norm<-caret::preProcess(data.frame(funcionou[,first_num:dim(funcionou)[2]]),method="range")

funcionou[,first_num:dim(funcionou)[2]]<-predict(norm,newdata = data.frame(funcionou[,first_num:dim(funcionou)[2]]))

inp<-as.matrix(funcionou)

# Portanto o nosso data.frame original foi codificado pelo objecto **encoder** e as variaveis foram centradas pelo objecto **norm**. Estamos prontos para reestruturar os dados
# 
# Notas sobre optimização da rede:
# 
# -   Principio da parcimonia é sempre apreciado;
# -   Uma abordagem inicial é a de afunilamento: Uma layer com N, a segunda com N/2, a terceira com N/4...

### Teste de validacao
x_test<-test[,c("lota","arte_eu","ETAMANHO","EESPECIE","VVENDA","QVENDA")]

# ,
#                 "HOM","HKE","JOD","EOI","BRB",
#                 "PLE","BIB","BSS", "SBA", "BLL",
#                 "SOL", "MUR", "CET", "BRF", "CTC"

y_test<-test$prop.GUU

#codifica categoricas:
x_test<-predict(encoder,newdata=x_test)

#recentra numericas
x_test<-data.frame(x_test)
first_num_test<-which(names(x_test)=="VVENDA")

#necessario forçar a conversao para data.frame porque pode dar-se o caso de so haver uma variavel numerica e a coluna e tratada como um vector
for(i in first_num_test:dim(x_test)[2]){
  x_test[,i]<-log(x_test[,i]+0.01)}

x_test[,first_num_test:dim(x_test)[2]]<-predict(norm,newdata = data.frame(x_test[,first_num_test:dim(x_test)[2]]))

x_test<-as.matrix(x_test)

alfa<-2
# dim(inp)[1]/(alfa * dim(inp)[2] * 1)