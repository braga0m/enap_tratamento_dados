#
if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse",'writexl','readxl','openxlsx')

setwd('/home/matheus/Documentos/ENAP/Datasets/IPEA')
getwd()


#base do dataset
base <- read_excel('/home/matheus/Documentos/ENAP/Datasets/Dataset_educação.xlsx')%>%
  select(1:7)

base_dicio <- read_excel('/home/matheus/Documentos/ENAP/Datasets/X.xlsx')

#dados - contendo o nome das políticas
dados <- read_excel('/home/matheus/Documentos/ENAP/Datasets/IPEA/ipea_politicas_meio_ambiente.xlsx')%>%filter(ano >= 1990)
  
#modicando a base de dados - criando as siglas das políticas
dados2 <- dados%>%
  mutate(datas = na_if(datas,'Não encontrada'))%>%
  mutate(siglas = tolower(gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", nome_politica, perl=T)))%>% #EXTRAI PALAVRAS ENTRE PARÊNTESES!
  mutate(siglas = str_replace_all(siglas,c('[ -]'),'_'),siglas = na_if(siglas,""))%>% #SUBSTITUI (espaço E -) POR (_), DEIXA O RESTANTE COMO NA.
  mutate(siglas = tolower(if_else(is.na(siglas),
                                  gsub('\\b(\\pL)\\pL{5,}|.','\\U\\1',nome_politica,perl = TRUE),siglas))) # EXTRAI A PRIMEIRA LETRA DE CADA PALAVRA

#informações que serão utilizadas - geradas como vetores
sig <- dados2%>%
  pull(siglas)

npol <- dados2%>%
  pull(nome_politica)

##prefixos
prefixo <- c('atogf','','ano_ado')

frases <- c('ato normativo do governo federal da política',
            'adoção da política: 0 = não adotou; 1 = adotou.',
            'ano em que o município adotou a política.') 

##colunas do dataset
nomes_colunas <- function(nome_politica, prefixo, vetor){
  vetor <- c()
  for(i in 1:length(nome_politica)){
    vetor <- append(vetor,paste(prefixo[1],sep = '_', nome_politica[i]))
    vetor <- append(vetor,paste(prefixo[2],sep = '',nome_politica[i]))
    vetor <- append(vetor,paste(prefixo[3],sep = '_', nome_politica[i]))
  }
  return(vetor)
}

empty_colunas <- c(); colunas <- nomes_colunas(sig,prefixo,empty_colunas)

##dicionário
dicionario_c <- function(nome_politica,frases,vetor){
  vetor <- c()
  for(i in 1:length(nome_politica)){
    vetor <- append(vetor,paste(frases[1],sep = ' ', nome_politica[i]))
    vetor <- append(vetor, frases[2])
    vetor <- append(vetor, frases[3])
  }
  return(vetor)
}

empty_dicionario <- c(); dicionario <- dicionario_c(npol,frases,empty_dicionario)

##transformando o dicionário numa planilha
codebook <- tibble(variables = colunas, description = dicionario)

y <- base_dicio%>%
  bind_rows(codebook)

##inserindo as colunas no dataset
novas_colunas <- tibble()
novas_colunas[colunas] <- NA

x <- base%>%
  bind_rows(novas_colunas)

ordem <- c(seq(from = 8, to = ncol(x), by = 3))
for(i in 1:length(ordem)){
  x[ordem[c(i)]] <- rep(dts[i],5570)
}
#####criando a planilha
dataset_politica <- list('politicas' = x,'codebook' = y)

write.xlsx(dataset_politica, file = 'Dataset_x.xlsx')

