
# Analise de correlaçoes

a1 <- vd_land %>% select("GUU","GUR","GUN","LDV","LEP","GUX")
a2 <- log(a1+0.01)


cor_raw <- vd_land %>% select(c("GUN",species)) %>% cor %>% data.frame

cor_log <- vd_land %>% select(c("GUN",species)) %>% apply(2,function(x){log(x+0.01) }) %>% cor %>% data.frame

cor_log["GUU",] %>% sort(decreasing = T)
cor_log["GUR",] %>% sort(decreasing = T)
cor_log["LEP",] %>% sort(decreasing = T)
cor_log["LDV",] %>% sort(decreasing = T)
cor_log["GUN",] %>% sort(decreasing = T)

# 
# data.frame(apply(a1,2,function(x){x>0})) -> a3
# 
# a3 <- a3*1
# 
# a3_pca<-
# logisticPCA::logisticPCA(a3, k = 2)
# 
# plot(a3_pca)
# 
# Ti<-Sys.time()
# plot(a3_pca,type="scores") + geom_point(aes(color=names(a3)[(a3 %>% apply(1,which.max))] ))
# Tf<-Sys.time()
# Tf-Ti
# 
# a3_cor<-  
# polycor::polychor(a3)
# #
# 

save(a1,a2,cor_raw,cor_log, file = "dados/ead.Rdata")
