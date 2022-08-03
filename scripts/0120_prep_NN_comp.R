
df_nn_c<-train[,c("lota","arte_eu","ETAMANHO","EESPECIE","VVENDA","QVENDA",
                  "HOM","HKE","JOD","EOI","BRB",
                  "PLE","BIB","BSS", "SBA", "BLL",
                  "SOL", "MUR", "CET", "BRF", "CTC")]


# y_nn<-teste[teste$EESPECIE%in%c("LEP","GUU","GUR","GUN","LDV"),]$p.GUU
# y_nn_c<-train$prop.GUU
y_nn_c<-train$prop.GUU

# Temos de normalizar os inputs:
# 
# Categóricos: lota,arte\_eu,ETAMANHO,EESPECIE
encoder_c<-caret::dummyVars("~.",data=df_nn_c)
funcionou_c<-predict(encoder_c,newdata=df_nn_c)

# No final, ficamos com 66 variáveis
# 
# Agora, só nos resta escalar as variaveis numericas: atenção ao efieto de correcção do zero

#VVENDA SERVE COMO PONTO DE REFERENCIA PARA TRANSFORMAÇOES NUMERICAS - tem quer ser a primeira variavel numerica
# names(data.frame(funcionou))# para ver os indices da numerica
funcionou_c<-data.frame(funcionou_c)

first_num_c<-which(names(funcionou_c)=="VVENDA")

#necessario forçar a conversao para data.frame porque pode dar-se o caso de so haver uma variavel numerica e a coluna e tratada como um vector
for(i in first_num_c:dim(funcionou_c)[2]){
  funcionou_c[,i]<-log(funcionou_c[,i]+0.01)}

norm_c<-caret::preProcess(data.frame(funcionou_c[,first_num_c:dim(funcionou_c)[2]]),method="range")

funcionou_c[,first_num_c:dim(funcionou_c)[2]]<-predict(norm_c,newdata = data.frame(funcionou_c[,first_num_c:dim(funcionou_c)[2]]))

inp_c<-as.matrix(funcionou_c)

# Portanto o nosso data.frame original foi codificado pelo objecto **encoder** e as variaveis foram centradas pelo objecto **norm**. Estamos prontos para reestruturar os dados
# 
# Notas sobre optimização da rede:
# 
# -   Principio da parcimonia é sempre apreciado;
# -   Uma abordagem inicial é a de afunilamento: Uma layer com N, a segunda com N/2, a terceira com N/4...



### Teste de validacao
x_test_c<-test[,c("lota","arte_eu","ETAMANHO","EESPECIE","VVENDA","QVENDA",
                  "HOM","HKE","JOD","EOI","BRB",
                  "PLE","BIB","BSS", "SBA", "BLL",
                  "SOL", "MUR", "CET", "BRF", "CTC")]

y_test_c<-test$prop.GUU

#codifica categoricas:
x_test_c<-predict(encoder_c,newdata=x_test_c)

#recentra numericas
x_test_c<-data.frame(x_test_c)
first_num_test_c<-which(names(x_test_c)=="VVENDA")

#necessario forçar a conversao para data.frame porque pode dar-se o caso de so haver uma variavel numerica e a coluna e tratada como um vector
for(i in first_num_test_c:dim(x_test_c)[2]){
  x_test_c[,i]<-log(x_test_c[,i]+0.01)}

x_test_c[,first_num_test_c:dim(x_test_c)[2]]<-predict(norm_c,newdata = data.frame(x_test_c[,first_num_test_c:dim(x_test_c)[2]]))

x_test_c<-as.matrix(x_test_c)

alfa_c<-2
# dim(inp)[1]/(alfa * dim(inp)[2] * 1)