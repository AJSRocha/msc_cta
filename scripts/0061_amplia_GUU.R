
# Ampliacoes de proporçoes

## Agregamos vendas-dia:

### Ao mes
vd_amp_month <-
vd_land %>% select(IEMBARCA,IDATVEND,zona,PORTO,EGRUPART,GUU,GUN,GUR,GUG,GUX,LEP,LDV) %>% 
  mutate(mes = format(IDATVEND,format = "%m"),
         ano = format(IDATVEND,format = "%Y")) %>%
  group_by(zona,PORTO,EGRUPART,mes,ano) %>%
  summarise(GUU = sum(GUU),
            GUR = sum(GUR),
            GUN = sum(GUN),
            GUG = sum(GUG),
            GUX = sum(GUX),
            LEP = sum(LEP),
            LDV = sum(LDV))

vd_amp_month <-
vd_amp_month %>% melt(id = 1:5, variable.name = "EESPECIE", value.name = "QVENDA")

### ao trimestre
vd_amp_trim <-
  vd_land %>% select(IEMBARCA,IDATVEND,zona,PORTO,EGRUPART,GUU,GUN,GUR,GUG,GUX,LEP,LDV) %>% 
  mutate(trim = quarters(IDATVEND),
         ano = format(IDATVEND,format = "%Y")) %>%
  group_by(zona,PORTO,EGRUPART,trim,ano) %>%
  summarise(GUU = sum(GUU),
            GUR = sum(GUR),
            GUN = sum(GUN),
            GUG = sum(GUG),
            GUX = sum(GUX),
            LEP = sum(LEP),
            LDV = sum(LDV))

vd_amp_trim <-
  vd_amp_trim %>% melt(id = 1:5, variable.name = "EESPECIE", value.name = "QVENDA")

### ao semestre
vd_amp_sem <-
  vd_land %>% select(IEMBARCA,IDATVEND,zona,PORTO,EGRUPART,GUU,GUN,GUR,GUG,GUX,LEP,LDV) %>% 
  mutate(sem = lubridate::semester(IDATVEND),
         ano = format(IDATVEND,format = "%Y")) %>%
  group_by(zona,PORTO,EGRUPART,sem,ano) %>%
  summarise(GUU = sum(GUU),
            GUR = sum(GUR),
            GUN = sum(GUN),
            GUG = sum(GUG),
            GUX = sum(GUX),
            LEP = sum(LEP),
            LDV = sum(LDV))

vd_amp_sem <-
  vd_amp_sem %>% melt(id = 1:5, variable.name = "EESPECIE", value.name = "QVENDA")

## Agregamos nautilus

### ao mes
df_prop<-
  naut2[naut2$especie_am%in%c("GUU","GUR","GUM","GUG","GUN","CTZ","LEP","LDV","GUX")&
          naut2$cod_fao_venda%in%c("GUU","GUR","GUM","GUG","GUN","CTZ","LEP","LDV","GUX"),] %>%  
  droplevels %>%
  dcast(.,cfr.x+data_venda+id_viagem+id_caixa+peso_total_dom + peso_amostrado_dom + 
           peso_total_caixa+peso_am_caixa+lota+arte_eu+cat_com+cod_fao_venda~especie_am,
        fun.aggregate = mean,na.rm=T,
        value.var="peso_am_spp_comp",
        fill=0) %>%
  mutate(mes = format(data_venda,format = "%m"),
         ano = format(data_venda,format = "%Y"))

naut_amp_month <-
df_prop %>% group_by(ano,mes,lota,arte_eu,cod_fao_venda,cat_com,id_viagem) %>%
            summarise(GUU = sum(GUU),
                      peso_total_dom = unique(peso_total_dom)) %>%
            group_by(ano,mes,lota,arte_eu,cod_fao_venda,id_viagem) %>%
            summarise(GUU = sum(GUU),
                      peso_total_dom = sum(peso_total_dom)) %>%
            group_by(ano,mes,lota,arte_eu,cod_fao_venda) %>%
            summarise(GUU = sum(GUU),
                      peso_total_dom = sum(peso_total_dom))
            
naut_amp_month$prop <- ifelse(naut_amp_month$peso_total_dom == 0,
                        0,
                        naut_amp_month$GUU/naut_amp_month$peso_total_dom)

### ao trimestre
df_prop<-
  naut2[naut2$especie_am%in%c("GUU","GUR","GUM","GUG","GUN","CTZ","LEP","LDV","GUX")&
          naut2$cod_fao_venda%in%c("GUU","GUR","GUM","GUG","GUN","CTZ","LEP","LDV","GUX"),] %>%  
  droplevels %>%
  dcast(.,cfr.x+data_venda+id_viagem+id_caixa+peso_total_dom + peso_amostrado_dom + 
          peso_total_caixa+peso_am_caixa+lota+arte_eu+cat_com+cod_fao_venda~especie_am,
        fun.aggregate = mean,na.rm=T,
        value.var="peso_am_spp_comp",
        fill=0) %>%
  mutate(trim = quarters(data_venda),
         ano = format(data_venda,format = "%Y"))

naut_amp_trim <-
  df_prop %>% group_by(ano,trim,lota,arte_eu,cod_fao_venda,cat_com,id_viagem) %>%
  summarise(GUU = sum(GUU),
            peso_total_dom = unique(peso_total_dom)) %>%
  group_by(ano,trim,lota,arte_eu,cod_fao_venda,id_viagem) %>%
  summarise(GUU = sum(GUU),
            peso_total_dom = sum(peso_total_dom)) %>%
  group_by(ano,trim,lota,arte_eu,cod_fao_venda) %>%
  summarise(GUU = sum(GUU),
            peso_total_dom = sum(peso_total_dom))

naut_amp_trim$prop <- ifelse(naut_amp_trim$peso_total_dom == 0,
                        0,
                        naut_amp_trim$GUU/naut_amp_trim$peso_total_dom)

### ao semestre
df_prop<-
  naut2[naut2$especie_am%in%c("GUU","GUR","GUM","GUG","GUN","CTZ","LEP","LDV","GUX")&
          naut2$cod_fao_venda%in%c("GUU","GUR","GUM","GUG","GUN","CTZ","LEP","LDV","GUX"),] %>%  
  droplevels %>%
  dcast(.,cfr.x+data_venda+id_viagem+id_caixa+peso_total_dom + peso_amostrado_dom + 
          peso_total_caixa+peso_am_caixa+lota+arte_eu+cat_com+cod_fao_venda~especie_am,
        fun.aggregate = mean,na.rm=T,
        value.var="peso_am_spp_comp",
        fill=0) %>%
  mutate(sem = lubridate::semester(data_venda),
         ano = format(data_venda,format = "%Y"))

naut_amp_sem <-
  df_prop %>% group_by(ano,sem,lota,arte_eu,cod_fao_venda,cat_com,id_viagem) %>%
  summarise(GUU = sum(GUU),
            peso_total_dom = unique(peso_total_dom)) %>%
  group_by(ano,sem,lota,arte_eu,cod_fao_venda,id_viagem) %>%
  summarise(GUU = sum(GUU),
            peso_total_dom = sum(peso_total_dom)) %>%
  group_by(ano,sem,lota,arte_eu,cod_fao_venda) %>%
  summarise(GUU = sum(GUU),
            peso_total_dom = sum(peso_total_dom))

naut_amp_sem$prop <- ifelse(naut_amp_sem$peso_total_dom == 0,
                        0,
                        naut_amp_sem$GUU/naut_amp_sem$peso_total_dom)

### join
levels(vd_amp_month$EGRUPART) <- c("OTB","PS","MIS_MIS")
levels(vd_amp_trim$EGRUPART) <- c("OTB","PS","MIS_MIS")
levels(vd_amp_sem$EGRUPART) <- c("OTB","PS","MIS_MIS")

amplia_month <-
merge(vd_amp_month, naut_amp_month,
      by.x = c("PORTO","EGRUPART","mes","ano","EESPECIE"),
      by.y = c("lota","arte_eu","mes","ano","cod_fao_venda"),
      all.x=T)

amplia_trim <-
  merge(vd_amp_trim, naut_amp_trim,
        by.x = c("PORTO","EGRUPART","trim","ano","EESPECIE"),
        by.y = c("lota","arte_eu","trim","ano","cod_fao_venda"),
        all.x=T)

amplia_sem <-
  merge(vd_amp_sem, naut_amp_sem,
        by.x = c("PORTO","EGRUPART","sem","ano","EESPECIE"),
        by.y = c("lota","arte_eu","sem","ano","cod_fao_venda"),
        all.x=T)


## limpeza
rm(df_prop, vd_amp_month, vd_amp_trim, vd_amp_sem, naut_amp_month, naut_amp_trim, naut_amp_sem)




  
  

