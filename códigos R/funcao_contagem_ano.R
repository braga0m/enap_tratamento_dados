###
if (!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse','readxl','data.table','writexl','openxlsx')

dados <- read_excel('LISTA_POLITICAS_20_04_2023.xlsx')

####
years <- c(1990:2022)
contagem_anos_aux <- tibble(Ano = years, n = rep(0,length(years)))

contagem_anos_por_area <- function(area){
  x <- dados%>%
    filter(Área == area)%>%
    group_by(Ano)%>%
    summarise(n = n())%>%
    bind_rows(contagem_anos_aux)%>%
    distinct(Ano,.keep_all = T)%>%
    arrange(Ano)%>%
    mutate(Área = rep(area,33), acumulado = cumsum(n))%>%
    select(Área, Ano, n, acumulado)
  
  return(x)
}

###
geral <- dados%>%
  group_by(Ano)%>%
  summarise(n = n())%>%
  mutate(acumulado = cumsum(n))

saude <- contagem_anos_por_area('Saúde')
educacao <- contagem_anos_por_area('Educação')
habitacao <- contagem_anos_por_area('Habitação e Urbanismo')
assistencia <- contagem_anos_por_area('Assistência Social')
ciencia <- contagem_anos_por_area('Ciência e Tecnologia')
meio_ambiente <- contagem_anos_por_area('Meio Ambiente')
trabalho <- contagem_anos_por_area('Trabalho e Emprego')
economia <- contagem_anos_por_area('Economia')
previdencia <- contagem_anos_por_area('Previdência')
justica <- contagem_anos_por_area('Justiça e Segurança Pública')
agro <- contagem_anos_por_area('Agropecuária e Agrária')
comunicacao <- contagem_anos_por_area('Comunicação')
cultura <- contagem_anos_por_area('Cultura')
defesa_nacional <- contagem_anos_por_area('Defesa Nacional')
adm_publica <- contagem_anos_por_area('Administração Pública')
direitos_humanos <- contagem_anos_por_area('Direitos Humanos')
desenvolvimento_regional <- contagem_anos_por_area('Desenvolvimento Regional')
industria_comercio <- contagem_anos_por_area('Indústria e Comércio')
infra <- contagem_anos_por_area("Infraestrutura")
seguranca_alimentar <- contagem_anos_por_area('Segurança Alimentar')
turismo <- contagem_anos_por_area('Turismo, Desporto e Lazer')
relacoes_exteriores <- contagem_anos_por_area('Relações Exteriores')

###
planilhas_nomes <- list('Geral' = geral,
                        'Saúde' = saude,
                        'Educação' = educacao,
                        'Habitação e Urbanismo' = habitacao,
                        'Assistência Social' = assistencia,
                        'Ciência e Tecnologia' = ciencia,
                        'Meio Ambiente' = meio_ambiente,
                        'Trabalho e Emprego' = trabalho,
                        'Economia' = economia,
                        'Previdência' = previdencia,
                        'Justiça e Segurança Pública' = justica,
                        'Agropecuária e Agrária' = agro,
                        'Comunicação' = comunicacao,
                        'Cultura' = cultura,
                        'Defesa Nacional' = defesa_nacional,
                        'Administração Pública' = adm_publica,
                        'Direitos Humanos' = direitos_humanos,
                        'Desenvolvimento Regional' = desenvolvimento_regional,
                        'Indústria e Comércio' = industria_comercio,
                        "Infraestrutura" = infra,
                        'Segurança Alimentar' = seguranca_alimentar,
                        'Turismo, Desporto e Lazer' = turismo,
                        'Relações Exteriores' = relacoes_exteriores 
                        )

write.xlsx(planilhas_nomes, file = 'contagem_de_politicas_por_ano.xlsx')
