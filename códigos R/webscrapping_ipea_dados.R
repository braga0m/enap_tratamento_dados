#pacotes
if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse","rvest","writexl")

setwd('/home/matheus/Documentos/ENAP')
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

tabela_saude <- tibble(nome_politica,ano,datas,legislacao2)
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
  
  coleta_legis <- function(i){
    page_politica = read_html(i)
    data = page_politica%>%html_nodes('hr+ div h2 strong')%>%html_text()%>%str_trim(side = 'both')
    return(data)
  }
  
  datas <- sapply(link_politica,FUN = coleta_data, USE.NAMES = F)
  legislacao <- sapply(link_politica,FUN = coleta_legis, USE.NAMES = F)
  
  legislacao2 <- map(.x = legislacao, .f = ~str_c(.x,collapse = ''))%>%
    str_remove_all(pattern = c('[-0123456789./]'))%>%
    str_trim(side = 'both')
  
  
  return(tibble(nome_politica,ano,datas,legislacao2))
}

#ciencia e tec nao funcionou
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


#ciencia e tecnologia n funcionou
write_xlsx(x2,"/home/matheus/Documentos/ENAP\\ipea_politicas_educacao.xlsx")
write_xlsx(x3,"/home/matheus/Documentos/ENAP\\ipea_politicas_assistencia_social.xlsx")
write_xlsx(x4,"/home/matheus/Documentos/ENAP\\ipea_politicas_habitacao_e_urbanismo.xlsx")
write_xlsx(x5,"/home/matheus/Documentos/ENAP\\ipea_politicas_direitos_humanos.xlsx")
write_xlsx(x6,"/home/matheus/Documentos/ENAP\\ipea_politicas_agropecuaria_agraria.xlsx")
write_xlsx(x7,"/home/matheus/Documentos/ENAP\\ipea_politicas_desenvolvimento_regional.xlsx")
write_xlsx(x8,"/home/matheus/Documentos/ENAP\\ipea_politicas_defesa_nacional.xlsx")
write_xlsx(x9,"/home/matheus/Documentos/ENAP\\ipea_politicas_justica_seguranca_pub.xlsx")
write_xlsx(x10,"/home/matheus/Documentos/ENAP\\ipea_politicas_administracao_pub.xlsx")
#write_xlsx(x11,"/home/matheus/Documentos/ENAP\\ipea_politicas_ciencia_tecnologia.xlsx")
write_xlsx(x12,"/home/matheus/Documentos/ENAP\\ipea_politicas_comunicacao.xlsx")
write_xlsx(x13,"/home/matheus/Documentos/ENAP\\ipea_politicas_cultura.xlsx")
write_xlsx(x14,"/home/matheus/Documentos/ENAP\\ipea_politicas_industria_comercio.xlsx")
write_xlsx(x15,"/home/matheus/Documentos/ENAP\\ipea_politicas_infraestrutura.xlsx")
write_xlsx(x16,"/home/matheus/Documentos/ENAP\\ipea_politicas_economia.xlsx")
write_xlsx(x17,"/home/matheus/Documentos/ENAP\\ipea_politicas_trabalho_emprego.xlsx")
write_xlsx(x18,"/home/matheus/Documentos/ENAP\\ipea_politicas_meio_ambiente.xlsx")
write_xlsx(x19,"/home/matheus/Documentos/ENAP\\ipea_politicas_previdencia.xlsx")
write_xlsx(x20,"/home/matheus/Documentos/ENAP\\ipea_politicas_seguranca_alimentar.xlsx")
write_xlsx(x21,"/home/matheus/Documentos/ENAP\\ipea_politicas_relacoes_exteriores.xlsx")
write_xlsx(x22,"/home/matheus/Documentos/ENAP\\ipea_politicas_turismo_lazer.xlsx")



