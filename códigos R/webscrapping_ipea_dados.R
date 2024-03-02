#pacotes
if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse","rvest","writexl",'readxl')

getwd()

###########################################SAÚDE
link <- 'https://catalogo.ipea.gov.br/area-tematica/1/saude'
page <- read_html(link)

nome_politica <- page%>%
  html_nodes('br+ strong')%>%
  html_text()

ano <- page%>%
  html_nodes('.m-0 h2')%>%
  html_text()%>%
  as.numeric()

link_politica <- page%>%
  html_nodes('.linsLeft a')%>%
  html_attr('href')%>%
  str_c('https://catalogo.ipea.gov.br/',.)

coleta_data <- function(i){
  page_politica = read_html(i)
  data = page_politica%>%html_nodes('.col-md-8 div div:nth-child(1) h2 strong')%>%html_text()%>%str_trim(side = 'both')
  return(data)
}
datas <- sapply(link_politica,FUN = coleta_data, USE.NAMES = F)

coleta_legis <- function(i){
  page_politica = read_html(i)
  data = page_politica%>%html_nodes('hr+ div h2 strong')%>%html_text()%>%str_trim(side = 'both')
  return(data)
}
legislacao <- sapply(link_politica,FUN = coleta_legis, USE.NAMES = F)

legislacao2 <- map(.x = legislacao, .f = ~str_c(.x,collapse = ''))%>%
  str_remove_all(pattern = c('[-0123456789./]'))%>%
  str_trim(side = 'both')

tabela_saude <- tibble(nome_politica,ano,datas,legislacao)
write_xlsx(tabela_saude,"/home/matheus/Documentos/ENAP\\ipea_saúde_final.xlsx")




################################################EDUCAÇÃO
link <- 'https://catalogo.ipea.gov.br/area-tematica/19/educacao'
page <- read_html(link)

nome_politica <- page%>%
  html_nodes('br+ strong')%>%
  html_text()

ano <- page%>%
  html_nodes('.m-0 h2')%>%
  html_text()%>%
  as.numeric()

link_politica <- page%>%
  html_nodes('.linsLeft a')%>%
  html_attr('href')%>%
  str_c('https://catalogo.ipea.gov.br/',.)

coleta_data <- function(i){
  page_politica = read_html(i)
  data = page_politica%>%html_nodes('.col-md-8 div div:nth-child(1) h2 strong')%>%html_text()%>%str_trim(side = 'both')
  return(data)
}
datas <- sapply(link_politica,FUN = coleta_data, USE.NAMES = F)

tabela_educacao <- tibble(nome_politica,ano,datas)
write_xlsx(tabela_educacao,"/home/matheus/Documentos/ENAP\\ipea_educação_final.xlsx")

##########################################MODIFICANDO O DATASET
saude_restante <- tabela_saude%>%
  #filter(ano >= 1990)%>%
  filter(!str_detect(nome_politica,'Política Nacional'))

saude_politica_nacional <- tabela_saude%>%
  #filter(ano >= 1990)%>%
  filter(str_detect(nome_politica,'Política Nacional'))

write_xlsx(saude_restante,"/home/matheus/Documentos/ENAP\\ipea_saude_pnacional.xlsx")
write_xlsx(saude_politica_nacional,"/home/matheus/Documentos/ENAP\\ipea_saude_restante.xlsx")



##########################################CRIANDO A FUNÇÃO
coleta_politica_ipea <- function(link){
  link <- link
  page <- read_html(link)
  
  nome_politica <- page%>%
    html_nodes('br+ strong')%>%
    html_text()
  
  #nome_politica <- nome_politica[-3]
  
  ano <- page%>%
    html_nodes('.m-0 h2')%>%
    html_text()%>%
    as.numeric()
  
  #ano <- ano[-3]
  
  link_politica <- page%>%
    html_nodes('.linsLeft a')%>%
    html_attr('href')%>%
    str_c('https://catalogo.ipea.gov.br/',.)
  
  #link_politica <- link_politica[-3]
  
  coleta_data <- function(i){
    page_politica = read_html(i)
    data = page_politica%>%html_nodes('.col-md-8 div div:nth-child(1) h2 strong')%>%html_text()%>%str_trim(side = 'both')
    return(data)
  }
  
  coleta_legis <- function(i){
    page_politica = read_html(i)
    data = page_politica%>%html_nodes('hr+ div h2 strong')%>%html_text()%>%str_trim(side = 'both')
    return(data)
  }
  
  datas <- sapply(link_politica,FUN = coleta_data, USE.NAMES = F)
  legislacao <- sapply(link_politica,FUN = coleta_legis, USE.NAMES = F)
  
  #legislacao2 <- map(.x = legislacao, .f = ~str_c(.x,collapse = ''))%>%
    #str_remove_all(pattern = c('[-0123456789./]'))%>%
    #str_trim(side = 'both')
  
  
  return(tibble(nome_politica,ano,datas,legislacao))
}



#uma politica da área de ciencia e tecnologia tem um site quebrado
#LNCC, terceira politica
x1 <- coleta_politica_ipea('https://catalogo.ipea.gov.br/area-tematica/1/saude')
x2 <- coleta_politica_ipea('https://catalogo.ipea.gov.br/area-tematica/19/educacao')
x3 <- coleta_politica_ipea('https://catalogo.ipea.gov.br/area-tematica/6/assistencia-social')
x4 <- coleta_politica_ipea('https://catalogo.ipea.gov.br/area-tematica/11/habitacao-e-urbanismo')
x5 <- coleta_politica_ipea('https://catalogo.ipea.gov.br/area-tematica/3/direitos-humanos')
x6 <- coleta_politica_ipea('https://catalogo.ipea.gov.br/area-tematica/4/agropecuaria-e-agraria')
x7 <- coleta_politica_ipea('https://catalogo.ipea.gov.br/area-tematica/5/desenvolvimento-regional')
x8 <- coleta_politica_ipea('https://catalogo.ipea.gov.br/area-tematica/2/defesa-nacional')
x9 <- coleta_politica_ipea('https://catalogo.ipea.gov.br/area-tematica/7/justica-e-seguranca-publica')
x10 <- coleta_politica_ipea('https://catalogo.ipea.gov.br/area-tematica/8/administracao-publica')
x11 <- coleta_politica_ipea('https://catalogo.ipea.gov.br/area-tematica/9/ciencia-e-tecnologia')
x12 <- coleta_politica_ipea('https://catalogo.ipea.gov.br/area-tematica/10/comunicacao')
x13 <- coleta_politica_ipea('https://catalogo.ipea.gov.br/area-tematica/18/cultura')
x14 <- coleta_politica_ipea('https://catalogo.ipea.gov.br/area-tematica/12/industria-e-comercio')
x15 <- coleta_politica_ipea('https://catalogo.ipea.gov.br/area-tematica/13/infraestrutura')
x16 <- coleta_politica_ipea('https://catalogo.ipea.gov.br/area-tematica/14/politica-economica')
x17 <- coleta_politica_ipea('https://catalogo.ipea.gov.br/area-tematica/15/trabalho-e-emprego')
x18 <- coleta_politica_ipea('https://catalogo.ipea.gov.br/area-tematica/17/meio-ambiente')
x19 <- coleta_politica_ipea('https://catalogo.ipea.gov.br/area-tematica/20/previdencia')
x20 <- coleta_politica_ipea('https://catalogo.ipea.gov.br/area-tematica/21/seguranca-alimentar')
x21 <- coleta_politica_ipea('https://catalogo.ipea.gov.br/area-tematica/22/relacoes-exteriores')
x22 <- coleta_politica_ipea('https://catalogo.ipea.gov.br/area-tematica/16/turismo-desporto-e-lazer')





#transformando a variável em lista, para ficar do mesmo tipo dos outros datasets
x19 <- x19%>%
  mutate(legislacao = list(legislacao))

x21 <- x21%>%
  mutate(legislacao = list(legislacao))



#recriando e organizando a lista do IPEA
catalogo_ipea <- bind_rows(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,
          x18,x19,x20,x21,x22)

catalogo_ipea <- catalogo_ipea%>%
  select(nome_politica,legislacao)

catalogo_ipea <- catalogo_ipea%>%distinct(nome_politica,.keep_all = T)


#juntando os datasets
dados <- read_xlsx('C:\\Users\\Matias\\Documents\\Projetos_e_Estudos\\ENAP\\Dados\\LISTA_POLITICAS_20_04_2023.xlsx')

dados2 <- dados%>%
  left_join(catalogo_ipea, by = c('Nome' = 'nome_politica'))

dados2 <- dados2%>%
  mutate(legislacao = as.character(legislacao))

write_xlsx(dados2,'C:\\Users\\Matias\\Documents\\Projetos_e_Estudos\\ENAP\\enap_tratamento_dados\\merge.xlsx')



