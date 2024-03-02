###
if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse","readxl","data.table","writexl","openxlsx","stringi")
















###############################################################################################################
#######################DIVISÃO COM A LETÍCIA
###
dados <- read_excel('C:\\Users\\Matias\\Documents\\Projetos_e_Estudos\\ENAP\\output_federal_atual.xlsx')

###
dados <- dados%>%
  filter(Estado == 'UN')

###divisão
impar <- dados %>% slice(seq(1, n(), by = 2))
par <- dados %>% slice(seq(2, n(), by = 2))

###exportando
write_xlsx(par,"par.xlsx")
write_xlsx(impar,"impar.xlsx")
###############################################################################################################
###############################################################################################################



















###############################################################################################################
#######################       MATCH COM CÓDIGOS IBGE E SIGLA
###
info_ibge <- read_excel('..\\Dados\\datasets_enap_padrao\\Dataset_saúde.xlsx')%>%
  select(c('COD.IBGE6','COD.IBGE7','UF',`NOME/UF`,'MUNICÍPIO'))

###
siglas_estados <- tibble(
  sigla = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", 
            "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"),
  estado_completo = c("Acre", "Alagoas", "Amazonas", "Amapá", "Bahia", "Ceará", "Distrito Federal", 
                      "Espírito Santo", "Goiás", "Maranhão", "Minas Gerais", "Mato Grosso do Sul", 
                      "Mato Grosso", "Pará", "Paraíba", "Pernambuco", "Piauí", "Paraná", "Rio de Janeiro", 
                      "Rio Grande do Norte", "Rondônia", "Roraima", "Rio Grande do Sul", "Santa Catarina", 
                      "Sergipe", "São Paulo", "Tocantins"))
###
info_ibge <- info_ibge%>%
  left_join(siglas_estados, by = c(`NOME/UF` = 'estado_completo'))%>%
  mutate(MUNICÍPIO = toupper(MUNICÍPIO))%>%
  mutate(municipio_sem_acento = stri_trans_general(MUNICÍPIO,"Latin-ASCII"))

###############################################################################################################
###############################################################################################################




















###############################################################################################################
#######################       COBRINDO OS ERROS DE DIGITAÇÃO E JUNTANDO OS DADOS

###todas as linhas da planilha
parte1 <- read_excel('C:\\Users\\Matias\\Documents\\Projetos_e_Estudos\\ENAP\\output_federal_atual.xlsx')%>%
  select(c(Estado,Cidade,DataFrame_Name,`Data de publicação`))%>%
  rename(estado = Estado, data = `Data de publicação`, cidade = Cidade, pol = DataFrame_Name)%>%
  filter(!is.na(cidade))%>%
  mutate(cidade = toupper(cidade))

impar <- read_excel('C:\\Users\\Matias\\Documents\\Projetos_e_Estudos\\ENAP\\impar.xlsx')%>%
  select(c(Estado,Cidade,DataFrame_Name,`Data de publicação`))%>%
  rename(estado = Estado, data = `Data de publicação`, cidade = Cidade, pol = DataFrame_Name)%>%
  mutate(cidade = toupper(cidade))

dados <- rbind(parte1, impar)%>%
  arrange(desc(pol))%>%
  mutate(cidade_sem_acento = stri_trans_general(cidade,"Latin-ASCII"))%>%
  filter(cidade != 'ALAGOAS')

###identificando os erros de digitação
erro_digitacao <- anti_join(dados, info_ibge, by = c("cidade_sem_acento" = "municipio_sem_acento"))%>%
  select(c('estado', 'cidade'))%>%
  distinct(cidade)%>%
  arrange(cidade)

###corrigindo com case_when
dados%>%
  mutate(cidade_corrigida = case_when(
    cidade == 'AFONSO CLAUDIO'~'AFONSO CLÁUDIO',cidade == 'AFUA'~'AFUÁ',cidade == 'AGUAS DA PRATA'~'ÁGUAS DA PRATA',
    cidade == 'AGUAS DE SANTA BARBARA'~'ÁGUAS DE SANTA BÁRBARA',cidade == 'ALCANTARA'~'ALCÂNTARA', cidade == 'ALMIRANTE DE TAMANDARÉ'~'ALMIRANTE TAMANDARÉ',
    cidade == 'ALMIRANTE TAMANDARÉ DO SUL'~'ALMIRANTE TAMANDARÉ DO SUL',cidade == 'ALTA FLORESTA DOESTE'~"ALTA FLORESTA D'OESTE",
    cidade == 'ALTO ALEGRE DO PARECIS'~'ALTO ALEGRE DOS PARECIS',
    cidade == 'AFONSO CLAUDIO'~'AFONSO CLÁUDIO',cidade == 'AFONSO CLAUDIO'~'AFONSO CLÁUDIO',cidade == 'AFONSO CLAUDIO'~'AFONSO CLÁUDIO',
    cidade == 'AFONSO CLAUDIO'~'AFONSO CLÁUDIO',cidade == 'AFONSO CLAUDIO'~'AFONSO CLÁUDIO',cidade == 'AFONSO CLAUDIO'~'AFONSO CLÁUDIO',
    cidade == 'AFONSO CLAUDIO'~'AFONSO CLÁUDIO',cidade == 'AFONSO CLAUDIO'~'AFONSO CLÁUDIO',cidade == 'AFONSO CLAUDIO'~'AFONSO CLÁUDIO',
    cidade == 'AFONSO CLAUDIO'~'AFONSO CLÁUDIO',cidade == 'AFONSO CLAUDIO'~'AFONSO CLÁUDIO',cidade == 'AFONSO CLAUDIO'~'AFONSO CLÁUDIO',
    
  ))

###############################################################################################################
###############################################################################################################

























###############################################################################################################
#######################       MATCH NOME DOS MUNICÍPIOS

#merge info do ibge e dados do diário
dados_ibge <- dados%>%
  inner_join(info_ibge, by = c("estado" = "sigla", "cidade" = "MUNICÍPIO"))%>%
  #mutate(`Data de publicação` = as.Date(`Data de publicação`, format = '%Y-%m-%d'))%>%
  select(c(COD.IBGE7,data,pol,cidade))
 
  

#filtrando pela data mais antiga
politica <- dados_ibge%>%
  filter(DataFrame_Name == "Programa Criança Feliz")%>%
  arrange(Cidade,`Data de publicação`)%>%
  group_by(Cidade)%>%
  filter(row_number() == 1)%>%
  select(c("COD.IBGE7",`Data de publicação`))
###############################################################################################################
###############################################################################################################
  
























###############################################################################################################
#######################       ADICIONANDO NOS DADOS DO PROJETO - PROGRAMA CRIANÇA FELIZ
dataset_saude <- read_excel('..\\Dados\\datasets_enap_padrao\\Dataset_saúde.xlsx')

dataset_saude <- dataset_saude%>%
  left_join(politica, by = "COD.IBGE7")%>%
  mutate(ano_ado_pcf = `Data de publicação`)%>%
  mutate(pcf = if_else(is.na(ano_ado_pcf), 0, 1))%>%
  select(-c(`Data de publicação`, "Cidade"))
  
###############################################################################################################
###############################################################################################################
