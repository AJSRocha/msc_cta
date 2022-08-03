
#Carrega e prepara os dados da base de dados do nautilus
load("C:/Google Drive/MSc CTA/dados/gux_2017_2019.RData")

naut<-gux_2019_cmp
rm(gux_2019_cmp)

# load("C:/Google Drive//Ruivices/dados/wgwide_2009_2018_cmp_gux_ 201907041810 .RData")
# dados<-rbind(gux_2017_all_am2,gux_2018_all_am2)
# dados<-read.csv("C:\\Dropbox\\Ruivices\\dados_ruivos_comp20172018_091120181557.csv")

#corrigir estrutura dos dados 2017-2918
naut$id_viagem<-factor(naut$id_viagem)
naut$id_venda<-factor(naut$id_venda)
naut$id_denominacao<-factor(naut$id_denominacao)
naut$id_caixa<-factor(naut$id_caixa)
naut$id_spp<-factor(naut$id_spp)
naut$id_comp<-factor(naut$id_comp)
naut$lota<-factor(naut$lota)
naut$cod_fao_venda<-factor(naut$cod_fao_venda)
naut$cod_fao<-factor(naut$cod_fao)

naut$data_fin<-as.POSIXct(naut$data_fin,format="%Y-%m-%d")
naut$data_venda<-as.POSIXct(naut$data_venda,format="%Y-%m-%d")

levels(naut$lota)<-list("VIANA DO CASTELO" = "Viana do Castelo",
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

naut$codporto<-factor(naut$codporto)
naut$esp_slv<-factor(naut$esp_slv)
naut$mes<-factor(naut$mes)
naut$ano<-factor(naut$ano)
naut$arte_eu<-factor(naut$arte_eu)

levels(naut$arte_eu)[grepl("OTB",levels(naut$arte_eu))]<-"OTB"
levels(naut$arte_eu)[grepl("PS",levels(naut$arte_eu))]<-"PS"
levels(naut$arte_eu)[!levels(naut$arte_eu)%in%c("OTB","PS")]<-"MIS_MIS"

naut$zona <- factor(naut$zona, levels=c("NW","SW","Sul"), ordered=T)
levels(naut$zona)[grepl("Sul",levels(naut$zona))]<-"S"


naut$cat_com <- factor(naut$cat_com,
                       levels=c("T0","T1","T2","T3","T4","T5",""), ordered=T)
naut$cat_com<-droplevels(naut$cat_com)

# naut$peso_total_dom<-as.numeric(as.character(naut$peso_total_dom))
# naut$peso_total_spp<-as.numeric(as.character(naut$peso_total_spp))

# ACRESCENTAR COLUNAS COM ABREVIATURAS
# abbreviate(naut$lota,minlength = 3)

#acrescenta matriculas ao naut
load("C:/Google Drive/MSc CTA/dados/matriculas_naut.rdata")
matriculas_naut$id<-factor(matriculas_naut$id)

naut<-merge(naut,matriculas_naut,by.x="id_viagem",by.y="id")