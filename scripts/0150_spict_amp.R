

## desembarques
vd_amp <-
vd %>% filter(year_sale %in% c(2009:2019) &
                PORTO %in% unique(encoder$lvls$lota)) %>% 
        filter(EESPECIE%in%c("GUU","LDV","LEP","GUR","GUM","GUN","GUG","CTZ","GUX","GUY"))

amplia_final$mes <- factor((amplia_final$mes))
levels(vd_amp$EGRUPART) <- c("OTB","PS","MIS_MIS")


conas <-
merge(vd_amp,
      unique(amplia_final[,c("PORTO","EGRUPART","EESPECIE","mes","prop_m")]),
      by.x = c("PORTO","EGRUPART","EESPECIE","month_sale"),
      by.y = c("PORTO","EGRUPART","EESPECIE","mes"),
      all.x = T,
      all.y = F)

land_amp <-
conas %>%
mutate(QVENDA_amp = QVENDA * prop_m) %>%
  group_by(year_sale) %>%
  summarise(GUU = sum(QVENDA, na.rm = T))



## frota de referencia
index_amp <-
conas %>%
  filter() %>%
  group_by(IEMBARCA) %>%
  summarize(QVENDA = sum(QVENDA, na.rm = T))

#procurando o ponto de inflexao
temp_amp <-
index_amp[order(index_amp$QVENDA,decreasing = T),][1:200,]

ggplot(temp_amp,aes(x=reorder(IEMBARCA,-QVENDA),y=QVENDA))+
  geom_line(aes(group=1))+
  theme(axis.text.x = element_text(angle=90))

#grosso modo, desembarques totais acima de 25T
embs_amp<-
index_amp[index_amp$QVENDA>15000,] %>% select(IEMBARCA)

#podemos calcular esforço?

lpue_amp <-
conas %>%
  filter(IEMBARCA %in% t(embs_amp)) %>%
  group_by(year_sale, IEMBARCA) %>%
  summarize(QVENDA = sum(QVENDA, na.rm = T),
            dias = length(unique(IDATVEND))) %>%
  summarize(QVENDA = sum(QVENDA, na.rm = T),
            dias= sum(dias),
            lpue=QVENDA/dias)




