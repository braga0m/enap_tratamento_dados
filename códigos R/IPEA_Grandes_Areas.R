#pacotes
if (!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse','data.table','writexl',
               'lubridate','vroom','readxl')


#dados
ipea <- vroom('C:\\Users\\Matias\\Documents\\Projetos_e_Estudos\\ENAP\\Dados\\Lista_Completa_Políticas_IPEA.csv')%>%
  select(politica,grande_area)

ipea <- ipea%>%distinct(politica,grande_area)

dados <- read_xlsx('C:\\Users\\Matias\\Documents\\Projetos_e_Estudos\\ENAP\\Dados\\LISTA_POLITICAS_20_04_2023.xlsx')



#sub areas e grande areas
#https://www.gov.br/mcti/pt-br/composicao/rede-mcti/laboratorio-nacional-de-computacao-cientifica
#motivo da LNCC ser categorizada como Social - inserida pelo Excel
unique(ipea$subareas)

unique(ipea$grande_area)

unique(ipea$politica)

ipea%>%filter(is.na(grande_area))



#lista de politicas que o grupo mapeaou antes da lista do IPEA
#Educação e Saúde
pre_lista_ipea <- dados%>%
  left_join(ipea, by = c('Nome' = 'politica'))%>%
  filter(is.na(grande_area))%>%
  mutate(grande_area = replace_na(grande_area,'Social'))


dados <- dados%>%
  left_join(ipea, by = c('Nome' = 'politica'))%>%
  mutate(grande_area = replace_na(grande_area,'Social'))
