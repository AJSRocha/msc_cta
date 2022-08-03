
# Importa e prepara dados da SIC
load("C:/Google Drive//Ruivices/dados/wgwide_2009_2018_cmp_gux_ 201907041810 .RData")
sic<-gux_2009_2016_cmp
sic_vd<-gux_2009_2016_vnd

rm(list=ls(pattern="gux"));rm(a2018allcont,a2018cmpcont,fleet_frg2)


#corrigir estrutura dos dados 2009-2016
sic$ARTE_EU<-droplevels(factor(sic$ARTE_EU))
sic$EGRUPART<-"MIS_MIS"
sic[sic$ARTE_EU%in%c("PSEINERS","DSEINER","PS_SPF_>=16_0_0"),"EGRUPART"]<-"PS"
sic[sic$ARTE_EU%in%c("OTB_CRU_55-59_0_0","OTB_CRU_>=55_0_0","BTRAWL","DTRAWL","OTB_DEF_>=55_0_0","OTB_DEF_65-69_0_0"),"EGRUPART"]<-"OTB"

#limpa datas
sic$DATA_AM<-as.POSIXct(sic$DATA_AM,format="%d-%m-%Y")
sic$MES<-format(sic$DATA_AM,format="%m")

sic$REGIAO<-factor(sic$REGIAO,levels=c("N","C","S"),ordered=T)
levels(sic$REGIAO)<-c("NW","SW","S")

#correcoes que Alberto e Diana detectaram na SIC
sic$MATRICULA<-as.character(sic$MATRICULA)#permite a adi??o de novos niveis
sic$MATRICULA[sic$ID_VIAGEM==109120]<-"A-3570-C"
sic$MATRICULA[sic$ID_VIAGEM==111804]<-"PE-2249-C"
sic$MATRICULA[sic$ID_VIAGEM==108132]<-"VC-288-C"
sic$MATRICULA[sic$ID_VIAGEM==107750]<-"A-2958-L"
sic$MATRICULA[sic$ID_VIAGEM==107384]<-"A-3602-C"
sic$MATRICULA[sic$ID_VIAGEM==108730]<-"A-3608-C"
sic$MATRICULA[sic$ID_VIAGEM==108348]<-"ES-350-C"
sic$MATRICULA[sic$ID_VIAGEM==112560]<-"O-2218-C"
sic$MATRICULA[sic$ID_VIAGEM==108905]<-"O-2200-C"
sic$MATRICULA[sic$ID_VIAGEM==108909]<-"O-2200-C"
sic$MATRICULA[sic$ID_VIAGEM==112629]<-"PV-246-C"
sic$MATRICULA[sic$ID_VIAGEM==110154]<-"PE-2443-L"
sic$MATRICULA[sic$ID_VIAGEM==109784]<-"SB-1188-L"
sic$MATRICULA[sic$ID_VIAGEM==109731]<-"SB-1226-L"
sic$MATRICULA[sic$ID_VIAGEM==109721]<-"SB-1282-L"
sic$MATRICULA[sic$ID_VIAGEM==109726]<-"SB-1287-L"
sic$MATRICULA[sic$ID_VIAGEM==109777]<-"SB-1287-L"
sic$MATRICULA[sic$ID_VIAGEM==107267]<-"VC-250-C"
sic$MATRICULA[sic$ID_VIAGEM==107384]<-"VC-145-L"
sic$MATRICULA[sic$ID_VIAGEM==107690]<-"VC-145-L"
sic$MATRICULA[sic$ID_VIAGEM==107834]<-"A-3602-C"
#Inconsistencias entre o file do bernardo e o fleet register
sic$MATRICULA[sic$ID_VIAGEM==112186]<-"PE-2475-C"
sic$MATRICULA[sic$ID_VIAGEM==109383]<-"PV-278-L"
sic$MATRICULA[sic$ID_VIAGEM==112222]<-"P-2175-L"
sic$MATRICULA[sic$ID_VIAGEM==106978]<-"PV-298-C"
sic$MATRICULA[sic$ID_VIAGEM==107201]<-"A-3585-C"
sic$MATRICULA[sic$ID_VIAGEM==110166]<-"PE-2463-L"
sic$MATRICULA[sic$ID_VIAGEM==112656]<-"O-2038-L"
sic$MATRICULA[sic$ID_VIAGEM==109386]<-"V-1046-C"
sic$MATRICULA[sic$ID_VIAGEM==112991]<-"O-1904-C"
sic$MATRICULA[sic$ID_VIAGEM==110613]<-"FF-1251-C"
sic$MATRICULA[sic$ID_VIAGEM==111868]<-"VC-271-C"
sic$MATRICULA[sic$ID_VIAGEM==107098]<-"PE-2229-C"
sic$MATRICULA[sic$ID_VIAGEM==109719]<-"SB-1335-L"
sic$MATRICULA[sic$ID_VIAGEM==107109]<-"VC-285-C"
sic$MATRICULA[sic$ID_VIAGEM==109563]<-"PE-2463-L"
sic$MATRICULA[sic$ID_VIAGEM==113009]<-"VC-295-C"
sic$MATRICULA[sic$ID_VIAGEM==111657]<-"PE-2475-C"
sic$MATRICULA[sic$ID_VIAGEM==110354]<-"PE-2157-C"
sic$MATRICULA[sic$ID_VIAGEM==113002]<-"A-3611-C"
sic$MATRICULA[sic$ID_VIAGEM==111536]<-"FF-1251-C"
sic$MATRICULA[sic$ID_VIAGEM==112018]<-"PE-2463-L"
sic$MATRICULA[sic$ID_VIAGEM==108901]<-"L-2073-C"
sic$MATRICULA[sic$ID_VIAGEM==112603]<-"O-1904-C"
sic$MATRICULA[sic$ID_VIAGEM==106946]<-"VC-286-C"
sic$MATRICULA[sic$ID_VIAGEM==109125]<-"A-3583-L"
sic$MATRICULA[sic$ID_VIAGEM==109380]<-"A-3585-C"
sic$MATRICULA[sic$ID_VIAGEM==109805]<-"S-714-L"
sic$MATRICULA[sic$ID_VIAGEM==108463]<-"A-3314-C"#Transferencia de pescado?
sic$MATRICULA[sic$ID_VIAGEM==110399]<-"S-2152-L"
sic$MATRICULA[sic$ID_VIAGEM==109809]<-"SB-1334-L"
sic$MATRICULA[sic$ID_VIAGEM==109130]<-"A-3346-C"
sic$MATRICULA[sic$ID_VIAGEM%in%c(112408,113184,114850,112154)]<-"N-2612-C"
sic$MATRICULA<-factor(sic$MATRICULA)

#duas viagens que na verdade s?o a mesma
sic_vd$ID_VIAGEM[sic_vd$ID_VIAGEM==107005]<-107004

##como estao os tamanhos: CAT
sic$CAT<-factor(sic$CAT)
levels(sic$CAT)<-c("T0","T0","T0","T0","T0","T0","T0","T0","T0","T0","T0","T0","T0",
                   "T1","T2","T2","T2","T3","T4","T5")

##EGRUPART
sic$EGRUPART<-factor(sic$EGRUPART)
levels(sic$EGRUPART)

##PORTO_NOME
sic$PORTO_NOME<-factor(sic$PORTO_NOME)
levels(sic$PORTO_NOME)

##ANO
sic$ANO<-format(sic$DATA_AM,format="%Y")
sic$ANO<-factor(sic$ANO)




#PRT que ficou mal cruzado por culpa da diana
# sic_vd$CFR[sic_vd$ID_VIAGEM==109130]<-"PRT000019696"
