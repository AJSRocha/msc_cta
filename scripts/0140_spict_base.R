
##desembarques
land<-
  vd %>% 
  filter(EESPECIE=="GUU" & 
           year_sale %in% c(2009:2019) &
           PORTO %in% unique(encoder$lvls$lota)) %>%
  group_by(year_sale) %>%
  summarise(QVENDA= sum(QVENDA))

##frota de referencia
index<-
  vd %>% 
  filter(EESPECIE%in%c("GUU")  & 
           year_sale %in% c(2009:2019)) %>%
  group_by(IEMBARCA) %>%
  summarize(QVENDA = sum(QVENDA)) 

#procurando o ponto de inflexao
temp<-
  index[order(index$QVENDA,decreasing = T),][1:200,]

ggplot(temp,aes(x=reorder(IEMBARCA,-QVENDA),y=QVENDA))+
  geom_line(aes(group=1))+
  theme_light() + 
  theme(axis.text.x = element_blank(),
  ) + 
  
  labs(x = "embarcações ordenadas", y = "desembarques (ton)") + 
  geom_hline(aes(yintercept = 5000), col = "red")

#grosso modo, desembarques totais acima de 25T
embs<-
  index[index$QVENDA>5000,] %>% select(IEMBARCA)

#podemos calcular esforço?

lpue<-
  vd %>% 
  filter(EESPECIE%in%c("GUU")&IEMBARCA%in%t(embs) & year_sale %in% c(2009:2019)) %>%
  group_by(year_sale,IEMBARCA) %>%
  summarize(QVENDA = sum(QVENDA),
            dias = length(unique(IDATVEND))) %>%
  summarize(QVENDA = sum(QVENDA),
            dias= sum(dias),
            lpue=QVENDA/dias)