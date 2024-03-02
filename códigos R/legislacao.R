#pacotes
if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse","rvest","writexl",'readxl','lubridate','vroom')

getwd()

#dados
dados <- read_xlsx('C:\\Users\\Matias\\Documents\\Projetos_e_Estudos\\ENAP\\Dados\\lista_políticas_grupo_ENAP.xlsx')

ipea <- vroom('C:\\Users\\Matias\\Documents\\Projetos_e_Estudos\\ENAP\\Dados\\Lista_Completa_Políticas_IPEA.csv',
              col_select = c('politica','vigencia_inicio','ano'))%>%
  distinct(politica, .keep_all = T)

legis <- read_xlsx('C:\\Users\\Matias\\Documents\\Projetos_e_Estudos\\ENAP\\Dados\\merge.xlsx')%>%
  select(c(Nome,legislacao))
  #distinct(Nome, .keep_all = T)


#match de datas do IPEA com os do grupo
datas_diferentes <- y%>%
  mutate(Data = as.Date(Data,format = '%d/%m/%Y'))%>%
  mutate(vigencia_inicio = as.Date(vigencia_inicio,format = '%d/%m/%Y'))%>%
  filter(Data != vigencia_inicio)


#politicas repetidas
politicas_repetidas <- dados%>%
  group_by(Nome) %>% 
  filter(n()>1)


#fazendo os joins necessários
dados%>%
  left_join(ipea,by = c('Nome' = 'politica'))%>%
  left_join(legis2, by = 'Nome')


#arrumando database
enviar <- dados%>%
  mutate(legislacao = legis$legislacao)%>%
  mutate(legislacao = if_else(legislacao == 'NULL', NA, legislacao))%>%
  mutate(legislacao = if_else(legislacao == 'character(0)', NA, legislacao))%>%
  mutate(medida_provisoria = rep(NA,531))


#importando novamente após ajuste da área de relações internacionais e previdência
enviar <- read_xlsx('enviar.xlsx')


#regex
teste <- enviar%>%
  filter(grepl('"',legislacao))%>%
  mutate(legislacao = substr(legislacao,3,nchar(legislacao)))%>%
  mutate(medida_provisoria = gsub("(.*),.*", "\\1", legislacao))%>%
  mutate(medida_provisoria = str_remove_all(medida_provisoria,'["]'))%>%
  mutate(legislacao = gsub(".*,(.*)", "\\1", legislacao))%>%
  mutate(legislacao = str_remove_all(legislacao,'["]'))


enviar <- enviar%>%
  mutate(legislacao = if_else(grepl("c\\(",legislacao), substr(legislacao,3,nchar(legislacao) - 1),legislacao))%>%
  mutate(medida_provisoria = if_else(grepl('"', legislacao), gsub("(.*),.*", "\\1",legislacao), NA))%>%
  mutate(medida_provisoria = str_remove_all(medida_provisoria,'["]'))%>%
  mutate(legislacao = if_else(grepl('"', legislacao), gsub(".*,(.*)", "\\1",legislacao),legislacao))%>%
  mutate(legislacao = gsub(".*,(.*)", "\\1", legislacao))%>%
  mutate(legislacao = str_remove_all(legislacao,'["]'))



#write_xlsx(x,'C:\\Users\\Matias\\Documents\\Projetos_e_Estudos\\ENAP\\enap_tratamento_dados\\confirmar.xlsx')


#politicas sem legislacao - jogar no grupo
legis%>%
  group_by(Área)%>%
  summarise(n = n())

dados_legislacao <- read_xlsx('C:\\Users\\Matias\\Documents\\Projetos_e_Estudos\\ENAP\\Dados\\Lista_com_legislacoes.xlsx')

divisao <- c(rep('Matheus',23),rep('Carol',23),rep('Isabelle',22),rep('Fernanda',22))
dados_legislacao <- dados_legislacao%>%
  filter(!is.na(legislacao))%>%
  mutate(divisao = divisao)


#juntando os datasets com legislacao e sem legislacao
dados_legislacao <- read_xlsx('C:\\Users\\Matias\\Documents\\Projetos_e_Estudos\\ENAP\\Dados\\Lista_com_legislacoes.xlsx')

faltantes <- read_xlsx('C:\\Users\\Matias\\Documents\\Projetos_e_Estudos\\ENAP\\Dados\\faltantes.xlsx')%>%
  select(-c(divisao))

dados_legislacao <- dados_legislacao%>%
  filter(!is.na(legislacao))

dados_legislacao <- dados_legislacao%>%
  bind_rows(faltantes)%>%
  arrange(Área, desc(Ano))

write_xlsx(dados_legislacao,'C:\\Users\\Matias\\Documents\\Projetos_e_Estudos\\ENAP\\enap_tratamento_dados\\Lista_Politicas_Legislacoes_17_08.xlsx')

