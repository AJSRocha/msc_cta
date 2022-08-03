"
https://www.datanovia.com/en/lessons/clara-in-r-clustering-large-applications/#:~:text=CLARA%20(Clustering%20Large%20Applications%2C%20(,time%20and%20RAM%20storage%20problem.
"



df_clara<-t(teste[,c(-1:-22)])
#
df_clara<-data.frame(apply(df_clara,2,function(x){log(x+0.01)}))
df_clara<-setDT(df_clara)

#esta linha so faz sentido com dados brutos
# clara_zero<-which(colSums(df_clara)>0)

#esta e a versao para dados logaritmizados que exclui colunas com variancia nula
clara_zero<-names(apply(df_clara,2,function(x){which(var(x)!=0)}))
df_clara<-df_clara[,..clara_zero]

clara_norm<-caret::preProcess(df_clara,method=c("center","scale"))
df_clara<-predict(clara_norm,newdata=df_clara)


"tentativa de detectar outliers automaticamente"


dbscan::kNNdistplot(df_clara, k =  5)
abline(h = 15, lty = 2)



cl_out<-
  dbscan::dbscan(df_clara,
                 eps=30) 

fviz_cluster(cl_out, df_clara, stand = FALSE, frame = FALSE, geom = "point")
print(cl_out)


fviz_nbclust(df_clara[cl_out$cluster>0,],
             FUNcluster= cluster::clara,
             method="silhouette")


#number of clusters
k = 7

clara1<-
  cluster::clara(df_clara,
                 k, #number of clusters
                 metric = "euclidean",
                 samples = 500,
                 pamLike = T)
