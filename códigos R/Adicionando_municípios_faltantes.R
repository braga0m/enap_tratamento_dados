#
library(readxl)
library(dplyr)
library(writexl)


#
setwd('/home/matheus/Documentos/ENAP')
getwd()

health <- read_excel('Dataset_saúde.xlsx')
health2 <- health #dataframes não são 'espelhos'!
glimpse(health)

IBGE <- read_excel('RELATORIO_DTB_BRASIL_MUNICIPIO.xls')

#
municipios <- data.frame(health$COD.IBGE7,health$MUNICÍPIO)
colnames(municipios) <- c('COD','NOME')

municipios.IBGE <- data.frame(IBGE$`Código Município Completo`,IBGE$Nome_Município)
colnames(municipios.IBGE) <- c('COD','NOME')

#
#munipios faltantes na base health
setdiff(municipios.IBGE$COD,municipios$COD)#o que tem em (1) que não tem em (2)

faltantes <- matrix(c(150475,1504752,'1 - Nort', 15, "3 - 10001 até 20000", 'Mojuí dos Campos',NA,NA,NA,NA,NA,NA,
                      422000,4220000,"4 - Sul", 42, "3 - 10001 até 20000", 'Balneário Rincão',NA,NA,NA,NA,NA,NA, 
                      421265,4212650,"4 - Sul", 42, "3 - 10001 até 20000",'Pescaria Brava',NA,NA,NA,NA,NA,NA,
                      431454,4314548,"4 - Sul", 43, "1 - Até 5000",'Pinto Bandeira',NA,NA,NA,NA,NA,NA,
                      500627,5006275,"5 - Cent", 50, "2 - 5001 até 10000", "Paraíso das Águas",NA,NA,NA,NA,NA,NA,
                      530010,5300108,"5 - Cent", 53, "7 - Maior que 500000", "Brasília",NA,NA,NA,NA,NA,NA),
                      byrow = T,ncol = 12)
faltantes <- data.frame(faltantes)
colnames(faltantes) <- colnames(health)
faltantes

#adicionando dados faltantes
#Mojuí dos Campos
#Balneário Rincão
#Pescaria Brava
#Pinto Bandeira
#Paraíso das Águas
#Brasília
health2 <- rbind(health2,faltantes)
tail(health2, n = 6)

health2 <- health2%>%
  arrange(UF,MUNICÍPIO)

educacao <- health2%>%
  select(-c(psf,ano_ado_psf,ato_psf,redceg,ano_ado_redceg,ato_redceg))

#exportando dados
write_xlsx(health2,'/home/matheus/Documentos/ENAP\\Dataset_saúde.xlsx')
write_xlsx(educacao,'/home/matheus/Documentos/ENAP\\Dataset_educação.xlsx')


