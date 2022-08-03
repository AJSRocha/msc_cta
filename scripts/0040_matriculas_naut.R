
#saca os CFR das nossas viagens da bd

matricula<-
dbGetQuery(con,"
                select viagem.id, viagem.embarcacao,  embarcacao.cfr, embarcacao.matricula
                from viagem
                inner join embarcacao on viagem.embarcacao=embarcacao.id
                where embarcacao.activa=1")

save(matricula,file="C://Google Drive//MSc CTA//matriculas.rdata")
           