###pacotes
if (!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse','readxl','data.table',
               'writexl','lubridate','survival')
###dados
dados <- read_xlsx('LISTA_POLITICAS_20_04_2023.xlsx')
list.files()

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
  mutate(Tempo_dias = as.numeric(Tempo_dias))%>%
  mutate(Censura = rep(1,531))%>%
  select(-c(Política,Ano,Data))%>%
  rename(Mandato_parcial = Mandato)

dados <- dados%>%mutate(Mandato_parcial = factor(Mandato_parcial, levels = c('José Sarney','Fernando Collor','Itamar Franco',
                                            'Fernando Henrique Cardoso I','Fernando Henrique Cardoso II',
                                            'Luiz Inácio Lula da Silva I','Luiz Inácio Lula da Silva II',
                                            'Dilma Rousseff I','Dilma Rousseff II','Michel Temer',
                                            'Jair Bolsonaro')))
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

#write_xlsx(dados,'Dataset_para_analises.xlsx')
#########################################################################################################################


###análises
dados <- read_excel('Dataset_para_analises.xlsx')
dias <- dados%>%pull(Tempo_dias)
censura <- dados%>%pull(Censura)


#área
area <- dados%>%pull(Área)
ekm <- survfit(Surv(dias,censura)~area, conf.int = F)

cores <- rainbow(22)
plot(ekm,conf.int = F,xlab="Tempo (em dias)", ylab="S(t)", main = 'Kaplan-Meier',lty = c(1,rep(1,11)),col = cores)


#mandato parcial
mandato2 <- dados%>%pull(Mandato_parcial)
ekm <- survfit(Surv(dias,censura)~mandato2, conf.int = F)

cores <- rainbow(11)
plot(ekm,conf.int = F,xlab="Tempo (em dias)", ylab="S(t)", main = 'Kaplan-Meier',lty = c(1,rep(1,11)),
    col = cores)
legend(1300,0.9,lty=c(1,1),c('José Sarney','Fernando Collor','Itamar Franco','Fernando Henrique Cardoso I',
                                                         'Fernando Henrique Cardoso II','Luiz Inácio Lula da Silva I',
                                                           'Luiz Inácio Lula da Silva II',
                                                           'Dilma Rousseff I',
                                                           'Dilma Rousseff II',
                                                           'Michel Temer',
                                                          'Jair Bolsonaro'),col = cores,cex = 0.55)






###
mod <- coxph(Surv(dados$Tempo_dias, dados$Censura) ~ (factor(dados$Mandato_parcial) + factor(dados$Área)))
mod2 <- coxph(Surv(dados$Tempo_dias, dados$Censura) ~ (dados$Mandato_parcial + dados$Área))

summary(mod)

cox.zph(mod)
cox.zph(mod2)
