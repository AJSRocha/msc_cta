
# Ampliacoes de proporçoes

## Agregamos vendas-dia:

### Ao mes
vd_amp_month_y <-
vd_land %>% select(IEMBARCA,IDATVEND,zona,PORTO,EGRUPART,GUU,GUN,GUR,GUG,GUX,LEP,LDV) %>% 
  mutate(mes = format(IDATVEND,format = "%m")) %>%
  group_by(zona,PORTO,EGRUPART,mes) %>%
  summarise(GUU = sum(GUU),
            GUR = sum(GUR),
            GUN = sum(GUN),
            GUG = sum(GUG),
            GUX = sum(GUX),
            LEP = sum(LEP),
            LDV = sum(LDV))

vd_amp_month_y <-
vd_amp_month_y %>% melt(id = 1:4, variable.name = "EESPECIE", value.name = "QVENDA")

## Agregamos nautilus

### ao mes
df_prop_y<-
  naut2[naut2$especie_am%in%c("GUU","GUR","GUM","GUG","GUN","CTZ","LEP","LDV","GUX")&
          naut2$cod_fao_venda%in%c("GUU","GUR","GUM","GUG","GUN","CTZ","LEP","LDV","GUX"),] %>%  
  droplevels %>%
  dcast(.,cfr.x+data_venda+id_viagem+id_caixa+peso_total_dom + peso_amostrado_dom + 
           peso_total_caixa+peso_am_caixa+lota+arte_eu+cat_com+cod_fao_venda~especie_am,
        fun.aggregate = mean,na.rm=T,
        value.var="peso_am_spp_comp",
        fill=0) %>%
  mutate(mes = format(data_venda,format = "%m"))

naut_amp_month_y <-
df_prop_y %>% group_by(mes,lota,arte_eu,cod_fao_venda,cat_com,id_viagem) %>%
            summarise(GUU = sum(GUU),
                      peso_total_dom = unique(peso_total_dom)) %>%
            group_by(mes,lota,arte_eu,cod_fao_venda,id_viagem) %>%
            summarise(GUU = sum(GUU),
                      peso_total_dom = sum(peso_total_dom)) %>%
            group_by(mes,lota,arte_eu,cod_fao_venda) %>%
            summarise(GUU = sum(GUU),
                      peso_total_dom = sum(peso_total_dom))
            
naut_amp_month_y$prop <- ifelse(naut_amp_month_y$peso_total_dom == 0,
                        0,
                        naut_amp_month_y$GUU/naut_amp_month_y$peso_total_dom)



### join
levels(vd_amp_month_y$EGRUPART) <- c("OTB","PS","MIS_MIS")

amplia_month_y <-
merge(vd_amp_month_y, naut_amp_month_y,
      by.x = c("PORTO","EGRUPART","mes","EESPECIE"),
      by.y = c("lota","arte_eu","mes","cod_fao_venda"),
      all.x=T)

amplia_final <-
  merge(amplia_month,
        amplia_month_y[,c("PORTO","EGRUPART","mes","EESPECIE","prop")],
        by = c("PORTO","EGRUPART","mes","EESPECIE"),
        all.x = T,
        all.y = F)

names(amplia_final)[grepl("prop.x",names(amplia_final))] <- "prop"
names(amplia_final)[grepl("prop.y",names(amplia_final))] <- "prop_m"

## limpeza
rm(df_prop_y, vd_amp_month_y)




  
  

