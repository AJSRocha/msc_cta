
#restringe o vendas-dia a viagens que desembarcaram triglideos
source('C://repos/path.R'); path('local')


load("C://Dropbox//Ruivices//dados//vd.rdata")

vd<-split(vd,vd$year_sale)

# lapply(vd,function(x)
#   {x$id<-paste(x$IEMBARCA,x$PORTO,x$IDATVEND,sep="_")})

for(i in 1:length(vd)){
  vd[[i]]$id<-paste(vd[[i]]$IEMBARCA,vd[[i]]$PORTO,vd[[i]]$IDATVEND,sep="_")
}

vd<-do.call(rbind,vd)

fao=c("GUU","GUR","GUM","GUG","GUN","CTZ","LEP","LDV","GUX")

ref<-unique(vd[vd$EESPECIE%in%fao,"id"])

vd<-vd[vd$id%in%ref,]

#* DGRM altera caixas de LEP para LDV nos seguintes portos: **VIANA DO CASTELO** **POVOA DO VARZIM** **AVEIRO**
vd$EESPECIE<-as.character(vd$EESPECIE)
vd[vd$EESPECIE=="LDV"&vd$PORTO%in%c("VIANA DO CASTELO","POVOA DO VARZIM","AVEIRO"),"EESPECIE"]<-"LEP"
vd$EESPECIE<-factor(vd$EESPECIE)


#corrige tamanhos
levels(vd$ETAMANHO)<-c("T1","T2","T3","T4","T5","T6","T7","T8","T0")

#grava
save(vd,file="dados//vd_gux.Rdata")
