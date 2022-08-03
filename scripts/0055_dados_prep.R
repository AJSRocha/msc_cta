
#Agregacao de dados para o trabalho

# conseguimos acrescentar o campo vvenda?

guitos<-vd[,.(IEMBARCA,IDATVEND,EESPECIE,ETAMANHO,QVENDA,VVENDA)]

naut2$peso_total_caixa<-signif(naut2$peso_total_caixa)

guitos$QVENDA<-signif(guitos$QVENDA)

#agrega à caixa
# df_prop<-
# naut %>% 
#   dcast(.,cfr+data_venda+id_caixa+peso_total_caixa+peso_am_caixa+lota+arte_eu+cat_com+cod_fao_venda~cod_fao,
#                fun.aggregate = mean,na.rm=T,
#                value.var="peso_am_spp",
#                fill=0)

#proporçao de triglas
df_prop<-
  naut2[naut2$especie_am%in%c("GUU","GUR","GUM","GUG","GUN","CTZ","LEP","LDV","GUX"),] %>%  
  droplevels %>%
  dcast(.,cfr.x+data_venda+id_caixa+peso_total_caixa+peso_am_caixa+lota+arte_eu+cat_com+cod_fao_venda~especie_am,
        fun.aggregate = mean,na.rm=T,
        value.var="peso_am_spp_comp",
        fill=0)

df_prop$p.GUU<-df_prop$GUU/(df_prop$GUU+df_prop$CTZ+df_prop$GUG+df_prop$GUM+df_prop$GUN+df_prop$GUR+df_prop$GUX+df_prop$LDV)

index<-which(names(df_prop)%in%c("GUU","GUR","GUM","GUG","GUN","CTZ","LEP","LDV","GUX"))
names(df_prop)[index]<-paste("prop",names(df_prop)[index],sep=".")
rm(index)

#dados de desembarques
df_land<-
  naut2 %>% dcast(.,cfr.x+data_venda+lota~cod_fao_venda,
                  fun.aggregate = mean,na.rm=T,
                  value.var="peso_total_dom",
                  fill=0)

df<-merge(df_prop,df_land,
          by=c("cfr.x","data_venda","lota"),all.x=T,all.y=F)

teste<-
  guitos[df,on=c(IEMBARCA="cfr.x",
                 IDATVEND="data_venda",
                 EESPECIE="cod_fao_venda",
                 ETAMANHO="cat_com",
                 QVENDA="peso_total_caixa"),]

summary(teste$VVENDA)

miss_info<-teste[is.na(teste$VVENDA)]
teste<-teste[!is.na(teste$VVENDA),]
teste<-teste[teste$EESPECIE %in% c("LEP","GUU","GUR","GUN","LDV"),]
teste$EESPECIE<-droplevels(teste$EESPECIE)
teste$ETAMANHO<-factor(teste$ETAMANHO,ordered=F)
teste<-teste %>% filter(!lota %in% c("SINES","OLHAO","VRSA") & arte_eu!="PS")

# limpeza
rm(df_prop,df_land,guitos,df,matriculas_naut)

