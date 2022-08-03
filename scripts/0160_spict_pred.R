vd_pred <-
  vd[vd$EESPECIE%in%c("GUU","GUM","GUN","GUR","LDV","GUG","LEP","CTZ","GUX","GUY") & 
       year_sale %in% c(2009:2019)] %>%
  select(IEMBARCA,IDATVEND,EESPECIE,QVENDA,PORTO,zona,EGRUPART,VVENDA,ETAMANHO,year_sale) 

summary(vd_pred$QVENDA)

#observaçoes que saem dos limits do modelo
obs <- which(vd_pred$QVENDA > 12)

vd_split <- list()

#data frame so com essas observaçoes
vd_max <- vd_pred[obs,]

for(j in 1:nrow(vd_max)){
  #divisor que vamos aplicar a cada uma dessas observações
  divisor <- trunc(vd_max$QVENDA[j]/12)+1
  
  vd_j <- vd_max[rep(j, divisor),]
  vd_j$QVENDA <- vd_j$QVENDA/divisor
  vd_j$VVENDA <- vd_j$VVENDA/divisor
  
  vd_split[[j]] <- vd_j
}

## fechar
vd_pred <-
  rbind(
    vd_pred[-obs,],
    do.call(rbind,vd_split)
  )

#Arrumaçao dos factores

##arte_eu
names(vd_pred)[grepl("EGRUPART",names(vd_pred))]<-"arte_eu"
levels(vd_pred$arte_eu)<-c("OTB","PS","MIS_MIS")
vd_pred<-vd_pred[vd_pred$arte_eu%in%encoder$lvls$arte_eu,]
vd_pred$arte_eu<-droplevels(vd_pred$arte_eu)

##lota
names(vd_pred)[grepl("PORTO",names(vd_pred))]<-"lota"
vd_pred<-vd_pred[vd_pred$lota%in%unique(encoder$lvls$lota),]
vd_pred$lota<-droplevels(vd_pred$lota)
vd_pred$lota<-factor(vd_pred$lota,ordered=F)

##EESPECIE
vd_pred<-vd_pred[vd_pred$EESPECIE%in%encoder$lvls$EESPECIE,]

vd_ref<-vd_pred %>% select(IEMBARCA,lota,IDATVEND,year_sale)
vd_pred<-vd_pred %>% select(IEMBARCA,EESPECIE,QVENDA,lota,zona,arte_eu,VVENDA,ETAMANHO)

#one hot encoding:
vd_ohe<-data.frame(predict(encoder,newdata=vd_pred))

#normalizar:
first_num2<-which(names(vd_ohe)=="VVENDA")

#necessario forçar a conversao para data.frame porque pode dar-se o caso de so haver uma variavel numerica e a coluna e tratada como um vector
for(i in first_num2:dim(vd_ohe)[2]){
  vd_ohe[,i]<-log(vd_ohe[,i]+0.0001)}
norm<-caret::preProcess(data.frame(vd_ohe[,first_num2:dim(vd_ohe)[2]]),method="range")
vd_ohe[,first_num2:dim(vd_ohe)[2]]<-predict(norm,newdata = data.frame(vd_ohe[,first_num2:dim(vd_ohe)[2]]))

vd_ohe<-as.matrix(vd_ohe)

#Previsoes da NN
vd_yhat<-
  model %>% predict(vd_ohe)

#Quantidades previstas
vd_ref$GUU_hat<-vd_yhat


####
vd_ref[vd_ref$GUU_hat<0,"GUU_hat"] <- 0
####

#aqui estabelecemos um novo conjunto de embarcaçoes

index_pred<-
  vd_ref %>%
  group_by(IEMBARCA) %>%
  summarize(QVENDA = sum(GUU_hat))

#procurando o ponto de inflexao
temp_pred<-
  index_pred[order(index_pred$QVENDA,decreasing = T),][1:200,]

ggplot(temp_pred,aes(x=reorder(IEMBARCA,-QVENDA),y=QVENDA))+
  geom_line(aes(group=1))+
  theme(axis.text.x = element_text(angle=90))

#grosso modo, desembarques totais acima de 25T
embs_pred<-
  index_pred[index_pred$QVENDA>15000,] %>% select(IEMBARCA)

vd_land2<-
  vd_ref %>% 
  group_by(year_sale) %>%
  summarise(QVENDA=sum(GUU_hat))

lpue2<-
  vd_ref %>% 
  filter(IEMBARCA%in%t(embs_pred)) %>%
  group_by(year_sale,IEMBARCA) %>%
  summarize(QVENDA = sum(GUU_hat),
            dias = length(unique(IDATVEND))) %>%
  summarize(QVENDA = sum(QVENDA),
            dias= sum(dias),
            lpue=QVENDA/dias)
