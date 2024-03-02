###pacotes
if (!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse','readxl','data.table',
               'writexl','lubridate','survival',
               'vroom')
###dados
dados <- read_xlsx('C:\\Users\\Matias\\Documents\\Projetos_e_Estudos\\ENAP\\Dados\\LISTA_POLITICAS_20_04_2023.xlsx')

ipea <- vroom('C:\\Users\\Matias\\Documents\\Projetos_e_Estudos\\ENAP\\Dados\\Lista_Completa_Políticas_IPEA.csv')%>%
  select(politica,grande_area)%>%
  distinct(politica,grande_area)

dados <- dados%>%
  left_join(ipea, by = c('Nome' = 'politica'))%>%
  mutate(grande_area = replace_na(grande_area,'Social'))


###modificações
inicio_mandatos <- c(as.Date('1985-03-15'),as.Date('1990-03-15'),as.Date('1992-12-29'),
                     as.Date('1995-01-01'),as.Date('1999-01-01'),as.Date('2003-01-01'),
                     as.Date('2007-01-01'),as.Date('2011-01-01'),as.Date('2015-01-01'),
                     as.Date('2016-08-31'),as.Date('2019-01-01'))


dados <- dados%>% 
  mutate(Data = as.Date(Data,format = '%d/%m/%Y'))%>%
  mutate(Tempo_dias = if_else(Mandato == 'José Sarney',Data - inicio_mandatos[1],
                              if_else(Mandato == 'Fernando Collor',Data - inicio_mandatos[2],
                              if_else(Mandato == 'Itamar Franco',Data - inicio_mandatos[3],
                              if_else(Mandato == 'Fernando Henrique Cardoso I',Data - inicio_mandatos[4],
                              if_else(Mandato == 'Fernando Henrique Cardoso II',Data - inicio_mandatos[5],
                              if_else(Mandato == 'Luiz Inácio Lula da Silva I',Data - inicio_mandatos[6],
                              if_else(Mandato == 'Luiz Inácio Lula da Silva II',Data - inicio_mandatos[7],
                              if_else(Mandato == 'Dilma Rousseff I',Data - inicio_mandatos[8],
                              if_else(Mandato == 'Dilma Rousseff II',Data - inicio_mandatos[9],
                              if_else(Mandato == 'Michel Temer',Data - inicio_mandatos[10],
                              if_else(Mandato == 'Jair Bolsonaro',Data - inicio_mandatos[11],NA))))))))))))%>%
  mutate(Tempo_em_dias_1990 = Data - as.Date('1990-01-01'))%>%
  mutate(Tempo_dias = as.numeric(Tempo_dias), Tempo_em_dias_1990 = as.numeric(Tempo_em_dias_1990))%>%
  rename(Mandato_parcial = Mandato)%>%
  mutate(Mandato_parcial = factor(Mandato_parcial, levels = c('José Sarney','Fernando Collor','Itamar Franco',
                                            'Fernando Henrique Cardoso I','Fernando Henrique Cardoso II',
                                            'Luiz Inácio Lula da Silva I','Luiz Inácio Lula da Silva II',
                                            'Dilma Rousseff I','Dilma Rousseff II','Michel Temer',
                                            'Jair Bolsonaro')))%>%
  select(-c(Política))
  



#categotizando por governos completos
dados <- dados%>%
  mutate(Mandato_completo = case_when(
    (Mandato_parcial == 'Fernando Henrique Cardoso I'| Mandato_parcial == 'Fernando Henrique Cardoso II')~'FHC',
    (Mandato_parcial == 'Luiz Inácio Lula da Silva I'| Mandato_parcial == 'Luiz Inácio Lula da Silva II')~ 'Lula',
    (Mandato_parcial == 'Dilma Rousseff I'| Mandato_parcial == 'Dilma Rousseff II') ~ 'Dilma',
    (Mandato_parcial == 'Michel Temer') ~ 'Temer',
    (Mandato_parcial == 'Itamar Franco') ~ 'Itamar Franco',
    (Mandato_parcial == 'Jair Bolsonaro') ~ 'Bolsonaro',
    (Mandato_parcial == 'José Sarney') ~ 'Sarney',
    (Mandato_parcial == 'Fernando Collor') ~ 'Collor'))




#criando nova diferença dos tempos em dias
inicio_mandatos_completos <- c(as.Date('1985-03-15'),as.Date('1990-03-15'),as.Date('1992-12-29'),
                               as.Date('1995-01-01'),as.Date('2003-01-01'),as.Date('2011-01-01'),
                               as.Date('2016-08-31'),as.Date('2019-01-01'))
dados <- dados%>% 
  mutate(Tempo_dias_completo = if_else(Mandato_completo == 'Sarney',Data - inicio_mandatos_completos[1],
                              if_else(Mandato_completo == 'Collor',Data - inicio_mandatos_completos[2],
                              if_else(Mandato_completo == 'Itamar Franco',Data - inicio_mandatos_completos[3],
                              if_else(Mandato_completo == 'FHC',Data - inicio_mandatos_completos[4],
                              if_else(Mandato_completo == 'Lula',Data - inicio_mandatos_completos[5],
                              if_else(Mandato_completo == 'Dilma',Data - inicio_mandatos_completos[6],
                              if_else(Mandato_completo == 'Temer',Data - inicio_mandatos_completos[7],
                              if_else(Mandato_completo == 'Bolsonaro',Data - inicio_mandatos_completos[8],NA)))))))))%>%
  mutate(Tempo_dias_completo = as.numeric(Tempo_dias_completo))



#categorizando por períodos
dados <- dados %>%
  mutate(periodos = case_when(
    (Ano >= 1990 & Ano <= 1994) ~ '1990 - 1994',
    (Ano >= 1995 & Ano <= 1998) ~ '1995 - 1998',
    (Ano >= 1999 & Ano <= 2002) ~ '1999 - 2002',
    (Ano >= 2003 & Ano <= 2006) ~ '2003 - 2006',
    (Ano >= 2007 & Ano <= 2010) ~ '2007 - 2010',
    (Ano >= 2011 & Ano <= 2014) ~ '2011 - 2014',
    (Ano >= 2015 & Ano <= 2018) ~ '2015 - 2018',
    (Ano >= 2019 & Ano <= 2022) ~ '2019 - 2022'))

inicio_periodo <- c(as.Date('1990-01-01'),as.Date('1995-01-01'),as.Date('1999-01-01'),
                    as.Date('2003-01-01'),as.Date('2007-01-01'),as.Date('2011-01-01'),
                    as.Date('2015-01-01'),as.Date('2019-01-01'))

dados <- dados%>% 
  mutate(Tempo_dias_periodo = if_else(periodos == "1990 - 1994", Data - inicio_periodo[1],
                              if_else(periodos == "1995 - 1998", Data - inicio_periodo[2],
                              if_else(periodos == "1999 - 2002", Data - inicio_periodo[3],
                              if_else(periodos == "2003 - 2006", Data - inicio_periodo[4],
                              if_else(periodos == "2007 - 2010", Data - inicio_periodo[5],
                              if_else(periodos == "2011 - 2014", Data - inicio_periodo[6],
                              if_else(periodos == "2015 - 2018",Data - inicio_periodo[7],
                              if_else(periodos == "2019 - 2022",Data - inicio_periodo[8],NA)))))))))%>%
  mutate(Tempo_dias_periodo = as.numeric(Tempo_dias_periodo))

dados <- dados%>%
  mutate(periodos = factor(periodos, levels = c("1990 - 1994","1995 - 1998","1999 - 2002",
                                              "2003 - 2006","2007 - 2010","2011 - 2014",
                                              "2015 - 2018","2019 - 2022")))

dados <- dados%>%select(c(Nome,Área,grande_area,Data, Ano, Mandato_parcial, Mandato_completo,
                 periodos,Tempo_dias,Tempo_dias_completo,Tempo_em_dias_1990,Tempo_dias_periodo))


#write_xlsx(dados,'Dataset_para_analises.xlsx')



