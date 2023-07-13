#
library(readxl)
library(dplyr)
library(writexl)


#
setwd('/home/matheus/Documentos/ENAP')
getwd()

#importação/modificação
IBGE <- read_excel('RELATORIO_DTB_BRASIL_MUNICIPIO.xls')
IBGE <- data.frame(IBGE$UF,IBGE$Nome_UF,IBGE$`Código Município Completo`,IBGE$Nome_Município)
colnames(IBGE) <- c('COD/UF','NOME/UF','COD/MUN','NOME/MUN')

educacao <- read_excel('Dataset_educação.xlsx')
saude <- read_excel('Dataset_saúde2.xlsx')

#ajuste-educação
educacao <- educacao%>%
  left_join(IBGE, by = c('COD.IBGE7' = 'COD/MUN'))%>%
  select(c(COD.IBGE6,COD.IBGE7,REGIÃO,UF,`NOME/UF`,PORTE,MUNICÍPIO))

educacao$REGIÃO <- gsub('1 - Nort','1 - Norte',
                   gsub('2 - Nord','2 - Nordeste',
                   gsub('3 - Sude','3 - Sudeste',
                   gsub('5 - Cent','5 - CentroOeste',educacao$REGIÃO
                        ))))
  
  
unique(educacao$REGIÃO)


#ajuste-saúde
saude <- saude%>%
  left_join(IBGE, by = c('COD.IBGE7' = 'COD/MUN'))%>%
  select(-c(13,15))

saude$REGIÃO <- gsub('1 - Nort','1 - Norte',
                gsub('2 - Nord','2 - Nordeste',
                gsub('3 - Sude','3 - Sudeste',
                gsub('5 - Cent','5 - CentroOeste',saude$REGIÃO
                    ))))


unique(saude$REGIÃO)

#exportando dados
write_xlsx(saude,'/home/matheus/Documentos/ENAP\\Dataset_saúde2_mod.xlsx')
write_xlsx(educacao,'/home/matheus/Documentos/ENAP\\Dataset_educação_mod.xlsx')
