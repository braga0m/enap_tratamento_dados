#####pacotes
if (!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse','readxl','writexl','lubridate','openxlsx')

#####dados
dados <- read_xlsx('C:\\Users\\Matias\\Documents\\Projetos_e_Estudos\\ENAP\\Dados\\Lista de Políticas_FINAL.xlsx')

dados <- dados%>%
  select(c(Nome,Área, Data, Ano))%>%
  mutate(Ano = factor(Ano, levels = 1990:2022))

#####indice

#pesos
p_ano <- c()
for(i in 1:length(c(1990:2022))){
  p_ano[i] <- c(i/33)
}


#tabela com frequencia, pesos e indices anuais, da area selecionada
#e indice geral, da area selecionada
Indice_Inovacao <- function(area){
  dados_indice <- dados%>%
    filter(Área == area)%>%
    group_by(Ano)%>%
    summarise(n = n())%>%
    complete(Ano = unique(dados$Ano), fill = list(n = 0))%>%
    mutate(freq_ano = n/12, p_ano = p_ano, indice_ano = freq_ano*p_ano)
  
  indice_geral_area <- sum(dados_indice$indice_ano)
  
  return(list(dados_indice, indice_geral_area))
  
}

######organizando planilha por areas
areas <- dados%>%distinct(Área)%>%arrange(Área)%>%pull()

Indice_Geral_Area <- c()
for (i in 1:length(areas)) {
  Indice_Geral_Area[i] <- Indice_Inovacao(areas[i])[[2]]
}


tabela_area_indice <-tibble(area = areas, indice = Indice_Geral_Area)%>%
  arrange(desc(indice))


planilhas_indices_inovacao_ministerial <- list(
                        "Indices Geral por Área" = tabela_area_indice,
                        "Administração Pública" = Indice_Inovacao(areas[1])[[1]],
                        "Agropecuária e Agrária" = Indice_Inovacao(areas[2])[[1]],
                        "Assistência Social" = Indice_Inovacao(areas[3])[[1]],
                        "Ciência e Tecnologia"  = Indice_Inovacao(areas[4])[[1]],
                        "Comunicação"  = Indice_Inovacao(areas[5])[[1]],
                        "Cultura"  = Indice_Inovacao(areas[6])[[1]],
                        "Defesa Nacional" = Indice_Inovacao(areas[7])[[1]],
                        "Desenvolvimento Regional" = Indice_Inovacao(areas[8])[[1]],
                        "Direitos Humanos" = Indice_Inovacao(areas[9])[[1]],
                        "Economia"  = Indice_Inovacao(areas[10])[[1]],
                        "Educação" = Indice_Inovacao(areas[11])[[1]],
                        "Habitação e Urbanismo" = Indice_Inovacao(areas[12])[[1]],
                        "Indústria e Comércio" = Indice_Inovacao(areas[13])[[1]],
                        "Infraestrutura" = Indice_Inovacao(areas[14])[[1]],
                        "Justiça e Segurança Pública" = Indice_Inovacao(areas[15])[[1]],
                        "Meio Ambiente" = Indice_Inovacao(areas[16])[[1]],
                        "Previdência" = Indice_Inovacao(areas[17])[[1]],
                        "Relações Exteriores" = Indice_Inovacao(areas[18])[[1]],
                        "Saúde" = Indice_Inovacao(areas[19])[[1]],
                        "Segurança Alimentar" = Indice_Inovacao(areas[20])[[1]],
                        "Trabalho e Emprego"  = Indice_Inovacao(areas[21])[[1]],
                        'Turismo, Desporto e Lazer' = Indice_Inovacao(areas[22])[[1]]
                        )

write.xlsx(planilhas_indices_inovacao_ministerial, file = 'indices_inovacao_ministerial.xlsx')
