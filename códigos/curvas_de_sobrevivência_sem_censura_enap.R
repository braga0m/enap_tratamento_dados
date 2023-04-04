#pacotes
if (!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse','readxl','data.table',
               'writexl','lubridate','survival')

setwd('/home/matheus/Documentos/ENAP/Datasets')
getwd()

#################### LULA E FHC
fhc_inicio_mandato <- c(as.Date('1995-01-01'))
lula_inicio_mandato <- c(as.Date('2003-01-01'))


fhc_lula <- read_xlsx('/home/matheus/Documentos/ENAP/dados_tipo_DEFINITIVO_FINAL.xlsx')%>%
  filter(Mandato %in% c("Luiz Inácio Lula da Silva","Fernando Henrique Cardoso"))%>%
  mutate(Data = as.Date(Data,format = '%d/%m/%Y'))%>%
  arrange(Data)%>%
  mutate(tempo = if_else(Mandato == "Fernando Henrique Cardoso",Data - fhc_inicio_mandato,Data - lula_inicio_mandato))%>%
  mutate(tempo = as.numeric(tempo))

#kaplan-meier
dias <- fhc_lula%>%pull(tempo)
censura <- rep(1,nrow(fhc_lula))
grupo <- c(rep(1,74),rep(2,155))
ekm <- survfit(Surv(dias,censura)~grupo, conf.int = F)

png(file="/home/matheus/Documentos/ENAP/Datasets/St_lula_fhc.png",width = 800, height = 500)
plot(ekm,conf.int=F, xlab="Tempo (em dias)", ylab="S(t)",lty = c(1,1),col = c('black','red'), main = 'Curvas de sobrevivência')
legend(200,0.25,lty=c(1,1),c("Fernando Henrique Cardoso","Luiz Inácio Lula da Silva"),col = c('black','red'))
dev.off()

############### ÚLTIMO DILMA E BOLSONARO
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
