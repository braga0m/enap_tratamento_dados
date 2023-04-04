###################################################### pacotes
if (!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse','readxl','data.table','writexl')

setwd('/home/matheus/Documentos/ENAP')
getwd()

dados <- read_xlsx('/home/matheus/Documentos/ENAP/dados_tipo_DEFINITIVO_FINAL_RESTO.xlsx')

dados%>%
  group_by(Área)%>%
  summarise(n = n())

################################################### incluindo o nome das políticas
pasta_datasets <- list.files('../ENAP/Datasets')
nome_politicas <- function(x,area){
  ministerio <- read_xlsx(str_c('../ENAP/Datasets/',pasta_datasets[x]),sheet = 2)%>%
    filter(str_detect(variables,'atogf'))%>%
    mutate(description = str_sub(description,46))%>%
    mutate(type = rep(area,nrow(.)))
}

sau <- nome_politicas(19,'Saúde')
edu <- nome_politicas(11,'Educação')
x1 <- nome_politicas(3,"Assistência Social")
x2 <- nome_politicas(2,"Agropecuária e Agrária")
x3 <- nome_politicas(4,"Ciência e Tecnologia")
x4 <- nome_politicas(5,'Comunicação')
x5 <- nome_politicas(6,'Cultura')
x6 <- nome_politicas(7,'Defesa Nacional')
x7 <- nome_politicas(10,"Economia")
x8 <- nome_politicas(12,"Habitação e Urbanismo")
x9 <- nome_politicas(15,"Justiça e Segurança Pública")#
x10 <- nome_politicas(16,"Meio Ambiente")
x11 <- nome_politicas(17,"Previdência")
x12 <- nome_politicas(21,"Trabalho e Emprego")

x13 <- nome_politicas(1,"Administração Pública")
x14 <- nome_politicas(8,"Desenvolvimento Regional")
x15 <- nome_politicas(9,"Direitos Humanos")
x16 <- nome_politicas(14,"Infraestrutura")
x17 <- nome_politicas(18,"Relações Exteriores")
x18 <- nome_politicas(22,"Turismo, Desporto e Lazer")
x19 <- nome_politicas(20,"Segurança Alimentar")
x20 <- nome_politicas(13,"Indústria e Comércio")


nomes <- x13%>%
  bind_rows(x14,x15,x16,x17,x18,x19,x20)

dados2 <- dados%>%
  left_join(nomes,by = c('Política' = 'variables','Área' ='type'),keep = F)


########################################################
