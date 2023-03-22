#pacotes e diretório
if (!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse','readxl','data.table','writexl')

#leitura dos dados
#alguns datasets estão com a data em formato de caractere
dados <- function(arquivo_xlsx,campo){
  area <- read_xlsx(str_c('../enap_manipulacao/datasets_enap_padrao/',arquivo_xlsx))%>%
    select(seq(8,ncol(.),3))%>%
    head(1)%>%
    select_if(~any(!is.na(.)))%>%
    gather(1:ncol(.),key = 'Política', value = 'Data')%>%
    #mutate(Data = as.Date.character(Data,format = '%d/%m/%Y'))%>%
    mutate(Ano = format(Data,format = '%Y'))%>%
    mutate(Área = rep(campo, nrow(.)))%>%
    drop_na(Ano, Data)
  
  return(area)
}

saude <- dados('Dataset_saúde.xlsx','Saúde')
educacao <- dados('Dataset_educação.xlsx','Educação')

