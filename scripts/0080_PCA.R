
# Script que executa a analise de componentes principais

## ranking das especies
vd_land<-
  vd %>% filter(EGRUPART!=5 & PORTO %in% levels(teste$lota)) %>%
  select(IEMBARCA,IDATVEND,EESPECIE,QVENDA,PORTO,zona,EGRUPART) %>%
  dcast(IEMBARCA+IDATVEND+zona+PORTO+EGRUPART~EESPECIE,
        fun.aggregate=sum,
        value.var="QVENDA",
        fill=0)

# index_pca <- which(vd_land$PORTO %in% levels(teste$lota) &
#                      vd_land$EGRUPART!=5)

rank_ton<-
  vd %>% 
  select(EESPECIE,QVENDA) %>%
  group_by(EESPECIE) %>%
  summarise(QVENDA=sum(QVENDA)/1000)

rank_freq<-
  vd %>% 
  select(IEMBARCA,PORTO,IDATVEND,EESPECIE,QVENDA) %>%
  group_by(IEMBARCA,PORTO,IDATVEND,EESPECIE) %>%
  summarise(QVENDA=sum(QVENDA)) %>%
  select(IEMBARCA,PORTO,IDATVEND,EESPECIE) 

species<-c(  
  (rank_ton[order(rank_ton$QVENDA,decreasing = T),][1:50,1] %>% t),
  ((table(rank_freq$EESPECIE) %>% sort(decreasing=T))[1:50] %>% names))

species<-species[!duplicated(species)]
paste(species[!duplicated(species)],collapse = "+")

## PCA com especies mais frequentes/importantes

### valores absolutos
b1<-vd_land %>% select (species)

### valores absolutos, sem transformacao: dim1 = 6.9% dim2 = 4.2%
# b1<- preProcess(b1,method = c("center","scale")) %>% predict(newdata = b1)
### log transform: dim 1 = 19.6% dim2 = 8%
b1<- apply(b1,2,function(x){log(x+0.01)})
### transformaçao YeoJohnson - vamos esquecer
# b1<- preProcess(b1,method = c("YeoJohnson")) %>% predict(newdata = b1)
###

### PCA
b1_pca <- princomp(b1)

summary(b1_pca)

### Plots
# fviz_pca_ind(b1_pca,col.ind = vd_land$GUU>0,label="none",legend.title = "GUU")
# fviz_pca_ind(b1_pca,col.ind = vd_land$GUR>0,label="none",legend.title = "GUR")
# fviz_pca_ind(b1_pca,col.ind = vd_land$LEP>0,label="none",legend.title = "LEP")
# fviz_pca_ind(b1_pca,col.ind = vd_land$LDV>0,label="none",legend.title = "LDV")
# 
# 
# fviz_pca_var(b1_pca, col.var="contrib",
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) 
# 
# fviz_pca_biplot(b1_pca,col.ind = vd_land$EGRUPART)
# fviz_eig(b1_pca, addLabels=TRUE)
# 
# corrplot::corrplot(get_pca_var(b1_pca)$contrib, is.corr=FALSE)
# fviz_pca_ind(b1_pca,
#              geom.ind = "point", # show points only (nbut not "text")
#              col.ind = vd_land$EGRUPART, # color by groups
#              palette = c("#00AFBB", "#E7B800","#FC4E07"),
#              addEllipses = TRUE, # Concentration ellipses
#              legend.title = "spp")
# 
# 
# 
# b1_pca$loadings
# 



# 
# ### todas as especies
# b1<-vd_land[index_pca,c(-1:-5)]
# b3<-
# data.frame(apply(b1,2,function(x){x>0})) 
# b3 <- b3*1
# 
# b3_pca <- logisticPCA::logisticPCA(b3, k = 2)
# 
# Ti<-Sys.time()
# # plot(b3_pca,type="scores") + geom_point(aes(color=names(b3)[(b3 %>% apply(1,which.max))] ))
# plot(b3_pca,type="scores") + geom_point(aes(color=vd_land[index_pca,]$zona))
# Tf<-Sys.time()
# Tf-Ti
# 
# #ordenacao scores pca log
# names(b3)[order(b3_pca$U[,1],decreasing=T)]
# 
# names(b3)[order(b3_pca$U[,2],decreasing=T)]
# 
# 
# ####################################
# save(a3,b3,file="dados//pca_bin.Rdata")
save(vd_land,b1_pca,species,file="dados//pca.Rdata")
# ####################################







# usado para obter as especies de forma acessivel
# 
# paste(names(vd_land),collapse = "+")
