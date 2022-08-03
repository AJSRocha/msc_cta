
#Este script importa os dados do naut2ilus que incluem as especies todas das viagens de ruivo, tal como foram sacadas pela query do bernardo

load("C:/Google Drive/MSc CTA/dados/gux_all.RData")

naut2<-gux_all_am
rm(gux_all_am)

# load("C:/Google Drive//Ruivices/dados/wgwide_2009_2018_cmp_gux_ 201907041810 .RData")
# dados<-rbind(gux_2017_all_am2,gux_2018_all_am2)
# dados<-read.csv("C:\\Dropbox\\Ruivices\\dados_ruivos_comp20172018_091120181557.csv")

#corrigir estrutura dos dados 2017-2918
naut2$id_viagem<-factor(naut2$id_viagem)
naut2$id_venda<-factor(naut2$id_venda)
naut2$id_denominacao<-factor(naut2$id_denominacao)
naut2$id_caixa<-factor(naut2$id_caixa)
naut2$id_spp<-factor(naut2$id_spp)
naut2$id_comp<-factor(naut2$id_comp)
naut2$lota<-factor(naut2$lota)
naut2$cod_fao_venda<-factor(naut2$cod_fao_venda)
naut2$cod_fao<-factor(naut2$cod_fao)

naut2$data_fin<-as.POSIXct(naut2$data_fin,format="%Y-%m-%d")
naut2$data_venda<-as.POSIXct(naut2$data_venda,format="%Y-%m-%d")

levels(naut2$lota)<-list("VIANA DO CASTELO" = "Viana do Castelo",
                        "POVOA DO VARZIM" = "Póvoa de Varzim",
                        "MATOSINHOS" = "Matosinhos",
                        "AVEIRO" = "Aveiro",
                        "FIGUEIRA DA FOZ" = "Figueira da Foz",
                        "NAZARE" = "Nazaré",
                        "PENICHE" = "Peniche",
                        "COSTA DA CAPARICA" = "Costa da Caparica",
                        "SESIMBRA" = "Sesimbra",
                        "SETUBAL" = "Setúbal",
                        "SINES" = "Sines",
                        "SAGRES" = "Sagres",
                        "PORTIMAO" = "Portimão",
                        "OLHAO" = "Olhão",
                        "VRSA" = "Vila Real de Santo António")

naut2$codporto<-factor(naut2$codporto)
naut2$codigo_slv<-factor(naut2$codigo_slv)
naut2$mes<-factor(naut2$mes)
naut2$ano<-factor(naut2$ano)
naut2$arte_eu<-factor(naut2$arte_eu)

levels(naut2$arte_eu)[grepl("OTB",levels(naut2$arte_eu))]<-"OTB"
levels(naut2$arte_eu)[grepl("PS",levels(naut2$arte_eu))]<-"PS"
levels(naut2$arte_eu)[!levels(naut2$arte_eu)%in%c("OTB","PS")]<-"MIS_MIS"

naut2$zona <- factor(naut2$zona, levels=c("NW","SW","Sul"), ordered=T)
levels(naut2$zona)[grepl("Sul",levels(naut2$zona))]<-"S"

#apagamos os ts que nao sao de ruivo, nao os vamos usar para modelacao
naut2[naut2$cat_com%in%c("G","M","P","T6"),]$cat_com<-"T0"
naut2$cat_com <- factor(naut2$cat_com,
                       levels=c("T0","T1","T2","T3","T4","T5",""), ordered=T)
naut2$cat_com<-droplevels(naut2$cat_com)

# naut2$peso_total_dom<-as.numeric(as.character(naut2$peso_total_dom))
# naut2$peso_total_spp<-as.numeric(as.character(naut2$peso_total_spp))

# ACRESCENTAR COLUNAS COM ABREVIATURAS
# abbreviate(naut2$lota,minlength = 3)

#reformatacao
load("C:/Google Drive/MSc CTA/dados/matriculas_naut.rdata")
matriculas_naut$id<-factor(matriculas_naut$id)

naut2<-merge(naut2,matriculas_naut,by.x="id_viagem",by.y="id")
