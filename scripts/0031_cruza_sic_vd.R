#
#script que cruza as vendas-dia com os dados SIC. Pressupoe que os objectos 'vd' e 'sic_vd' e 'fleet' estao na memoria
load("C://Google Drive//Ruivices/dados//fleet.Rdata")

#
fleet_ref<-fleet[,c("Ext.Marking","Event.Start.Date","Event.End.Date","CFR")]
sic_ref<-unique(sic[,c("ID_VIAGEM","DATA_AM","PORTO_NOME","MATRICULA","CAT")])

sic_ref<-
sqldf::sqldf("SELECT *
                FROM sic_ref
           LEFT JOIN fleet_ref
                  ON sic_ref.MATRICULA = fleet_ref.[Ext.Marking] AND
                     sic_ref.DATA_AM BETWEEN fleet_ref.[Event.Start.Date] AND fleet_ref.[Event.End.Date]
                      ")
rm(fleet,fleet_ref)

#cria vendas-dia horizontal
vd_land<-
  vd %>%
  select(IEMBARCA,IDATVEND,EESPECIE,ETAMANHO,QVENDA,PORTO,zona,EGRUPART) %>%
  dcast(IEMBARCA+IDATVEND+zona+PORTO+EGRUPART+ETAMANHO~EESPECIE,
        fun.aggregate=sum,
        value.var="QVENDA",
        fill=0)


teste_sic<-
  sqldf::sqldf("SELECT *
                FROM sic_ref
           LEFT JOIN vd_land
                  ON sic_ref.CFR = vd_land.IEMBARCA AND
                     sic_ref.DATA_AM = vd_land.IDATVEND AND
                     sic_ref.CAT = vd_land.ETAMANHO
                      ")

#IDs viagem duplicados
dup<-teste_sic$ID_VIAGEM[duplicated(teste_sic$ID_VIAGEM)] # 4 viagens que venderam em 2 portos; facilmente se resolve juntando as viagens ao porto que efectivamente vendeu GUU. Podemos corrigir isto a pata.

#NAs - #394 em 2301
teste_sic[is.na(teste_sic$IEMBARCA),] %>% View

#df auxiliar sem especies acessorias
sic_gux<-teste_sic %>% select(ID_VIAGEM,PORTO_NOME,EGRUPART,ETAMANHO,LDV,LEP,GUU,GUR,GUN)

#Proporções da sic
bubunga<-
  sic %>% 
  dcast(.,ID_VIAGEM+MATRICULA+PORTO_NOME+EGRUPART+CAT~COD_FAO,
        fun.aggregate = mean,na.rm=T,
        value.var="PESO_D_C",
        fill=0) %>%
  mutate(p.GUU=GUU/(CTZ+GUG+GUM+GUN+GUR+GUU+GUX+LDV+LEP)) 

#conseguimos fundir sic_gux e bubunga?
head(sic_gux)

cabunga<-merge(bubunga,sic_gux,
               by.x=c("ID_VIAGEM","CAT"),
               by.y=c("ID_VIAGEM","ETAMANHO"),
               all.x=F,all.y=F)

catastrofe<-
cabunga[
(rowSums(cabunga[,c("LDV.y","LEP.y","GUU.y","GUR.y","GUN.y")])> rowSums(cabunga[,c("LDV.x","LEP.x","GUU.x","GUR.x","GUN.x","CTZ","GUM","GUX","GUG")])),]

index_sic_unico<-
rowSums(                                                                      
apply(catastrofe[,c("LDV.y","LEP.y","GUU.y","GUR.y","GUN.y")],2,function(x){
  x>0
  }))

catastrofe<-
catastrofe[index_sic_unico==1,]  

catastrofe_melt<-
melt(catastrofe,
     measure.vars = c("LDV.y","LEP.y","GUU.y","GUR.y","GUN.y"),
     variable.name = "EESPECIE")

catastrofe_melt<- catastrofe_melt[catastrofe_melt$value>0,]  

catastrofe_melt<-
  merge(catastrofe_melt,unique(sic[,c("ID_VIAGEM","DATA_AM")]),
        by = "ID_VIAGEM",all.x=T,all.y=F)

teste_sic<-catastrofe_melt[,c("DATA_AM","GUU.x","PORTO_NOME.y","EGRUPART.y","CAT","EESPECIE","value")]
names(teste_sic)<-c("IDATVEND","prop.GUU","lota","arte_eu","ETAMANHO","EESPECIE","QVENDA")

vd_crux<- guitos %>% filter(EESPECIE %in% c("GUU","GUR","GUN","GUX","LEP","LDV")) %>%
  group_by(IDATVEND,EESPECIE,ETAMANHO) %>%
  summarise(QVENDA = sum(QVENDA),
            VVENDA = sum(VVENDA))


# vd_crux$IDATVEND <-
#   vd_crux$IDATVEND %>%
#     trunc(units=c("days")) %>%
#     as.POSIXct()
levels(teste_sic$EESPECIE)<-c("LDV","LEP","GUU","GUR","GUN")

temp<-
  merge(teste_sic,vd_crux,by.x=c("IDATVEND","EESPECIE","ETAMANHO"),
        by.y=c("IDATVEND","EESPECIE","ETAMANHO"))

