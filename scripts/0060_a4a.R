#A4A - Trabalhando com catch at length e catch at age

#fonte
"https://flr-project.org/doc/Modelling_growth_and_its_uncertainty_in_FLa4a.html?fbclid=IwAR3dRIus-Vis1f68W7S3vBAN9qLzw0H2KgkTLP_z-chMNFmt99OgHD3vD8E"

# install.packages(c("copula","triangle", "coda", "XML","latticeExtra"))
# # from FLR
# install.packages(c("FLCore", "FLa4a"), repos="http://flr-project.org/R")
# install.packages(c("ggplotFL"), repos="http://flr-project.org/R")
# install.packages(c("FLFleet"), repos="http://flr-project.org/R")

# Load all necessary packages and datasets; trim pkg messages
library(FLFleet)
library(ggplotFL)
library(FLa4a)
library(XML)
library(reshape2)
library(latticeExtra)
data(ple4)
data(ple4.indices)
data(ple4.index)
data(rfLen)

#Importing data

##FLQuant objects accept ‘vector’, ‘array’ or ‘matrix’. We can convert the object catch.n to a matrix.

##Example: landings: one column per year

##An FLQuant object is made of six dimensions. The name of the first dimension can be altered by the user from its default, ‘quant’. This could typically be ‘age’ or ‘length’ for data related to natural populations. The only name not accepted is ‘cohort’, because data structured along cohort should be stored using the FLCohort class instead. Other dimensions are always named as follows: ‘year’ for the calendar year of the data point; ‘unit’ for any kind of division of the population, e.g. by sex; ‘season’ for any temporal strata shorter than year; ‘area’ for any kind of spatial stratification; and ‘iter’ for replicates obtained through bootstrap, simulation or Bayesian analysis

##When importing catch numbers, for example, the input object needs to be formatted as such: age or length in the first dimension and year in the second dimension. If the object is not formatted in the right way, you can use the reshape functions from the package reshape2.


## ampliar classes de comprimento aos desembarques
vd_amplia<-
  vd %>% filter(EESPECIE == "GUU" & PORTO %in% unique(naut$lota)) %>% 
  group_by(year_sale,month_sale,PORTO) %>%
  summarise(QVENDA = sum(QVENDA))

###cobaia para ampliacoes
naut_amp<-naut

###NAs
naut[is.na(naut$peso_total_dom),]$peso_total_dom<-
  naut[is.na(naut$peso_total_dom),]$peso_amostrado_dom

naut[is.na(naut$peso_amostrado_dom),]#0

naut[is.na(naut$peso_total_caixa),]$peso_total_caixa<-
  naut[is.na(naut$peso_total_caixa),]$peso_am_caixa

naut[is.na(naut$peso_am_caixa),]#0

naut[is.na(naut$peso_total_spp),]#135

naut[is.na(peso_total_spp),]$peso_total_spp<-
  naut[is.na(peso_total_spp),]$peso_am_spp*
  naut[is.na(peso_total_spp),]$peso_total_caixa/
  naut[is.na(peso_total_spp),]$peso_am_caixa

naut[is.na(naut$peso_am_spp),]#0

###amplia denominacao
(sapply(naut$peso_total_dom/naut$peso_amostrado_dom,function(x){max(x,1)})) %>% summary

###amplia caixa
(naut$peso_total_caixa/naut$peso_am_caixa) %>% summary

###amplia spp
(naut$peso_total_spp/naut$peso_am_spp) %>% summary

###amplia classe
naut$n_amplia<-
  naut$n_nao_observados*(naut$peso_total_spp/naut$peso_am_spp)*
                    (naut$peso_total_caixa/naut$peso_am_caixa)*
                    (sapply(naut$peso_total_dom/naut$peso_amostrado_dom,function(x){max(x,1)}))

###agrega ao maximo

temp<-
naut %>% filter(cod_fao=="GUU")%>% 
         group_by(lota,ano,mes,classe_comp) %>%
         summarise(n = sum(n_amplia,na.rm=T),
                   kg = sum(peso_total_dom,na.rm=T))

temp<-
merge(temp,vd_amplia,
      by.x = c("lota","ano","mes"),
      by.y = c("PORTO","year_sale","month_sale"),
      all.x=T,all.y=F)

#estimativa de n.GUU desembarcada por ano,mes e lota
temp$n_amplia<-temp$n*temp$QVENDA/temp$kg

#agregado ao ano
guu.a4a<-
temp %>% group_by(ano,classe_comp) %>%
         summarise(n = sum(n_amplia,na.rm=T))

guu.a4a<-
guu.a4a %>% dcast(classe_comp ~ ano,
                  fill=0)

# Nota importante: ao usar a designacao 'GUU' no vendas dia, faltam desembarques de mais para extrapolar correctamente




#criando o objecto 
vbObj <- a4aGr(
grMod=~linf*(1-exp(-k*(t-t0))),      
grInvMod=~t0-1/k*log(1-len/linf),      
params=FLPar(linf=58.5, k=0.086, t0=0.001, units=c('cm','year-1','year')))

# Check the model and its inverse
lc=20
c(predict(vbObj, len=lc))