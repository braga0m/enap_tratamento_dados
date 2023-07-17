#pacotes
if (!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse','readxl','data.table',
               'writexl','lubridate','survival')

########################################################################## LULA E FHC
fhc_inicio_mandato <- c(as.Date('1995-01-01'))
lula_inicio_mandato <- c(as.Date('2003-01-01'))


fhc_lula <- read_xlsx('LISTA_POLITICAS_20_04_2023.xlsx')%>%
  filter(Mandato %in% c("Luiz Inácio Lula da Silva I","Fernando Henrique Cardoso I"))%>%
  mutate(Data = as.Date(Data,format = '%d/%m/%Y'))%>%
  arrange(Data)%>%
  mutate(tempo = if_else(Mandato == "Fernando Henrique Cardoso",Data - fhc_inicio_mandato,Data - lula_inicio_mandato))%>%
  mutate(tempo = as.numeric(tempo))

#kaplan-meier
dias <- fhc_lula%>%pull(tempo)
censura <- rep(1,nrow(fhc_lula))
grupo <- c(rep(1,74),rep(2,155))
ekm <- survfit(Surv(dias,censura)~grupo, conf.int = F)

png(file="St_lula_fhc.png",width = 800, height = 500)
plot(ekm,conf.int=F, xlab="Tempo (em dias)", ylab="S(t)",lty = c(1,1),col = c('black','red'), main = 'Curvas de sobrevivência')
legend(200,0.25,lty=c(1,1),c("Fernando Henrique Cardoso","Luiz Inácio Lula da Silva"),col = c('black','red'))
dev.off()







################################################################### ÚLTIMO DILMA E BOLSONARO
dilma_seg_inicio_mandato <- c(as.Date('2015-01-01'))
bolson_inicio_mandato <- c(as.Date('2019-01-01'))

dilma2_bolso <- read_xlsx('/home/matheus/Documentos/ENAP/todas_políticas_mapeadas_COM_DATAS.xlsx')%>%
  filter(Mandato %in% c("Dilma Rousseff","Michel Temer","Jair Bolsonaro"))%>%
  mutate(Data = as.Date(Data,format = '%d/%m/%Y'))%>%
  filter(Data >= as.Date('2015-01-01'))%>%
  arrange(Data)%>%
  mutate(tempo = if_else(Mandato == "Jair Bolsonaro",Data - bolson_inicio_mandato,Data - dilma_seg_inicio_mandato))%>%
  mutate(tempo = as.numeric(tempo))

#kaplan-meier
dias <- dilma2_bolso%>%pull(tempo)
censura <- rep(1,nrow(dilma2_bolso))
grupo <- c(rep(1,63),rep(2,143))
ekm <- survfit(Surv(dias,censura)~grupo, conf.int = F)

#png(file="/home/matheus/Documentos/ENAP/Datasets/St_dilma2_bolsonaro.png",width = 800, height = 500)
plot(ekm,conf.int=F, xlab="Tempo (em dias)", ylab="S(t)",lty = c(1,1),col = c('dark orange','dark green'), main = 'Curvas de sobrevivência')
legend(0,0.25,lty=c(1,1),c("Dilma Rousseff (2ºMandato) e Michel Temer","Jair Bolsonaro"),col = c('dark orange','dark green'))
#dev.off()

dilma <- survfit(Surv(dias[1:63],censura[1:63])~1, conf.int = F)
bolsonaro <- survfit(Surv(dias[64:206],censura[64:206])~1, conf.int = F)

summary(dilma)
summary(bolsonaro)






############################################################### EDUCAÇÃO E SAÚDE
inicio <- c(as.Date('1990-01-01'))

educacao_saude <- read_xlsx('LISTA_POLITICAS_20_04_2023.xlsx')%>%
  filter(Área %in% c('Saúde','Educação'))%>%
  mutate(Data = as.Date(Data,format = '%d/%m/%Y'))%>%
  arrange(Área, Data)%>%
  mutate(tempo = Data - inicio)%>%
  mutate(tempo = as.numeric(tempo))

#kaplan-meier
dias <- educacao_saude%>%pull(tempo)
censura <- rep(1,nrow(educacao_saude))
grupo <- c(rep(1,45),rep(2,51))
ekm <- survfit(Surv(dias,censura)~grupo, conf.int = F)

png(file="St_educacao_saude.png",width = 800, height = 500)
plot(ekm,conf.int=F, xlab="Tempo (em dias)", ylab="S(t)",lty = c(1,1),
     col = c('#487D5B','#65B1AE'), main = 'Curvas de sobrevivência')
legend(0,0.25,lty=c(1,1),c("Educação","Saúde"),col = c('#487D5B','#65B1AE'))
dev.off()












################################################################# MANDATOS - LULA E FHC
fhc1_inicio <- as.Date('1995-01-01')
fhc2_inicio <- as.Date('1999-01-01')
lula1_inicio <- as.Date('2003-01-01')
lula2_inicio <- as.Date('2007-01-01')
  
fhc_lula <- read_xlsx('LISTA_POLITICAS_20_04_2023.xlsx')%>%
  filter(Mandato %in% c("Luiz Inácio Lula da Silva I","Fernando Henrique Cardoso I",
                        'Luiz Inácio Lula da Silva II','Fernando Henrique Cardoso II'))%>%
  mutate(Data = as.Date(Data,format = '%d/%m/%Y'))%>%
  arrange(Data)%>%
  mutate(tempo = case_when((Mandato == 'Fernando Henrique Cardoso I')~ Data - fhc1_inicio,
                           (Mandato == 'Fernando Henrique Cardoso II')~ Data - fhc2_inicio,
                           (Mandato == 'Luiz Inácio Lula da Silva I')~ Data - lula1_inicio,
                           (Mandato == 'Luiz Inácio Lula da Silva II')~ Data - lula2_inicio
                           ))%>%
  mutate(tempo = as.numeric(tempo))

dias <- fhc_lula%>%pull(tempo)
censura <- rep(1,nrow(fhc_lula))
grupo <- c(rep(1,26),rep(2,48),rep(3,71),rep(4,84))
ekm <- survfit(Surv(dias,censura)~grupo, conf.int = F)

png(file="lulala_fhc.png",width = 800, height = 500)
plot(ekm,conf.int=F, xlab="Tempo (em dias)", ylab="S(t)",lty = c(1,1,1,1),
     col = c('#2b313b','#ab243f','#2545b8','#22bd65'), main = 'Curvas de sobrevivência')
legend(0,0.3,lty=c(1,1,1,1),c('Fernando Henrique Cardoso I',"Fernando Henrique Cardoso II",
                              'Luiz Inácio Lula da Silva I','Luiz Inácio Lula da Silva II'),
                               col = c('#2b313b','#ab243f','#2545b8','#22bd65'))
dev.off()


################################################################# MANDATOS - DILMA 2, BOLSONARO E TEMER
dilma2_inicio <- as.Date('2015-01-01')
temer_inicio <- as.Date('2016-08-31')
bolsonaro_inicio <- as.Date('2019-01-01')

dilma2_temer_bolsonaro <- read_xlsx('LISTA_POLITICAS_20_04_2023.xlsx')%>%
  filter(Mandato %in% c('Dilma Rousseff II','Michel Temer','Jair Bolsonaro'))%>%
  mutate(Data = as.Date(Data,format = '%d/%m/%Y'))%>%
  arrange(Data)%>%
  mutate(tempo = case_when((Mandato == 'Dilma Rousseff II')~ Data - dilma2_inicio,
                           (Mandato == 'Michel Temer')~ Data - temer_inicio,
                           (Mandato == 'Jair Bolsonaro')~ Data - bolsonaro_inicio))%>%
  mutate(tempo = as.numeric(tempo))

dias <- dilma2_temer_bolsonaro%>%pull(tempo)
censura <- rep(1,nrow(dilma2_temer_bolsonaro))
grupo <- c(rep(1,24),rep(2,39),rep(3,143))
ekm <- survfit(Surv(dias,censura)~grupo, conf.int = F)

png(file="St_DILMa2_termer.png",width = 800, height = 500)
plot(ekm,conf.int=F, xlab="Tempo (em dias)", ylab="S(t)",lty = c(1,1,1),
     col = c('#a024bf','#71ab37','#163169'), main = 'Curvas de sobrevivência')
legend(0,0.3,lty=c(1,1,1),c('Dilma Rousseff II','Michel Temer','Jair Bolsonaro'),
       col = c('#a024bf','#71ab37','#163169'))
dev.off()