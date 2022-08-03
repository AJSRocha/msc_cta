
# paste(names(test)[c(-1,-2,-5,-8,-9,-11:-20)],collapse = "+")

# modelo so com dados de vendas-dia
# modelo1<-as.formula(p.GUU ~ EESPECIE + ETAMANHO + QVENDA + VVENDA + lota + arte_eu)

# modelo so com dados de especies concurrentes
# modelo2<-as.formula(prop.GUU ~ AKL+ANE+ANK+ASD+ATP+BAS+BBS+BHD+BIB+BLB+BLL+BLT+BLU+BOG+BON+BRB+BRF+BSC+BSH+BSS+BUX+BXD+CBC+CET+CIL+COE+CPR+CRE+CTB+CTC+DEC+DGS+ENX+EOI+EQI+FLE+FOR+FRZ+GAR+GFB+GPW+GUM+GUN+GUR+GUU+GUX+HKE+HKW+HMM+HOM+HPR+JAA+JOD+JOS+KDH+KLK+KRJ+LBE+LDB+LDV+LEF+LEP+LIO+LTA+MAC+MAS+MEG+MGA+MGC+MGR+MIA+MKG+MLR+MMH+MON+MSF+MUE+MUF+MUI+MUL+MUR+MUT+MUX+OAL+OCC+OCM+OFJ+OMZ+OUB+OUL+OUM+OXY+PAC+PIC+PIL+PLE+POA+POI+POL+REA+REB+RJC+RJE+RJH+RJM+RJN+RJU+ROL+RPG+RSE+SAN+SBA+SBG+SBR+SBZ+SCL+SCM+SCR+SCS+SFS+SHO+SHR+SKJ+SLM+SLO+SMD+SOL+SOS+SOX+SPU+SQC+SQE+SQI+SQR+SQU+SRG+STG+SWA+SWM+SWO+SYC+SYT+TDF+TDQ+THS+TOE+TRG+TRI+TRO+TSD+TTR+TTV+TUR+URA+USI+WEG+WEX+WHB+WHG+WIT+WRA+WRF+YFM+YTC)

# modelo combinado
# modelo3 <- as.formula(p.GUU~EESPECIE+ETAMANHO+VVENDA+lota+arte_eu+V1+AKL+ANE+ANK+ASD+ATP+BAS+BBS+BHD+BIB+BLB+BLL+BLT+BLU+BOG+BON+BRB+BRF+BSC+BSH+BSS+BUX+BXD+CBC+CET+CIL+COE+CPR+CRE+CTB+CTC+DEC+DGS+ENX+EOI+EQI+FLE+FOR+FRZ+GAR+GFB+GPW+HKE+HKW+HMM+HOM+HPR+JAA+JOD+JOS+KDH+KLK+KRJ+LBE+LDB+LEF+LIO+LTA+MAC+MAS+MEG+MGA+MGC+MGR+MIA+MKG+MLR+MMH+MON+MSF+MUE+MUF+MUI+MUL+MUR+MUT+MUX+OAL+OCC+OCM+OFJ+OMZ+OUB+OUL+OUM+OXY+PAC+PIC+PIL+PLE+POA+POI+POL+REA+REB+RJC+RJE+RJH+RJM+RJN+RJU+ROL+RPG+RSE+SAN+SBA+SBG+SBR+SBZ+SCL+SCM+SCR+SCS+SFS+SHO+SHR+SKJ+SLM+SLO+SMD+SOL+SOS+SOX+SPU+SQC+SQE+SQI+SQR+SQU+SRG+STG+SWA+SWM+SWO+SYC+SYT+TDF+TDQ+THS+TOE+TRG+TRI+TRO+TSD+TTR+TTV+TUR+URA+USI+WEG+WEX+WHB+WHG+WIT+WRA+WRF+YFM+YTC)

# Fiz batota e troquei SKA por RJC e retirei OCT e MNZ e SOO. OUW
modelo <- as.formula(prop.GUU ~ HOM+BIB+HKE+OCC+JAA+MAS+WHB+MAC+SBA+SYT+
                                RJC+PIL+COE+CTC+SOL+JOD+SQR+RJC+RJH+
                                EOI+CET+SCL+GUR+THS+LEF+MUR+MON+
                                BRB+BRF+CTB+SQC+BSS+PLE+ANK+SOS+
                                FOR+MEG+MKG+SBR+RJM+WRF+OMZ+
                                BLL+TUR+RPG+PAC+SWA+TOE+MUX)

# Criamos a grelha de numero de variaveis a testar
grelha <- expand.grid(mtry = c(1,2,5,10,20,30,40,50))

# corremos a simulação
forest<-caret::train(modelo,
                     method="rf",
                     data=train,
                     trControl=control,
                     tuneGrid=grelha,
                     preProc=c("BoxCox"),
                     ntree=500,
                     metric="RMSE")



print(forest)
forest$finalModel
forest$results

ggplot(data.frame(variables=rownames(forest$finalModel$importance),importance=forest$finalModel$importance))+
  geom_bar(stat="identity",aes(x=variables,y=IncNodePurity))+
  theme(axis.text.x = element_text(angle=90))


# Vamos averiguar quais as variaveis mais significativas para este modelo

var_rank<-data.frame(variables=rownames(forest$finalModel$importance),importance=forest$finalModel$importance)

var_rank[order(var_rank$IncNodePurity,decreasing=T),][1:20,] %>% View

# E vamos rapidamente avaliar a performance do modelo

gridExtra::grid.arrange(
  ggplot(data.frame(y=train$prop.GUU,y_hat=predict(forest,newdata = train)))+
    geom_point(aes(x=y,y=y_hat))+
    geom_abline(slope=1,intercept=0)
  ,
  ggplot(data.frame(y=test$prop.GUU,y_hat=predict(forest,newdata = test)))+
    geom_point(aes(x=y,y=y_hat),col="green")+
    geom_abline(slope=1,intercept=0)
  ,ncol=2)

save(modelo,grelha,forest,var_rank, file = "dados/rf.Rdata")

tree_func <- function(final_model, 
                      tree_num) {
  
  # get tree by index
  tree <- randomForest::getTree(final_model, 
                                k = tree_num, 
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # make leaf split points to NA, so the 0s won't get plotted
    mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
  
  # prepare data frame for graph
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))
  
  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame(graph_frame) %>%
    delete_vertices("0")
  
  # set node labels
  V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
  V(graph)$leaf_label <- substr(as.character(tree$prediction), 1, 1)
  V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
  
  # plot
  plot <- ggraph(graph, 'dendrogram') + 
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
    geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
                    repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18))
  
  print(plot)
}

tree_func(forest$finalModel,tree_num)
