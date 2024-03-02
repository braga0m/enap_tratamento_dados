#pacotes
if (!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse','readxl','data.table',
               'writexl','lubridate','survival')

#
dados <- read_xlsx('C:\\Users\\Matias\\Documents\\Projetos_e_Estudos\\ENAP\\Dados\\lista_políticas_grupo_ENAP.xlsx')


#contagem de políticas de acordo com uma segunda variável
areas_tematicas <- dados%>%
  group_by(Área)%>%
  summarise(n = n())%>%
  arrange(desc(n))
  
mandatos_completos <- dados%>%
  group_by(Mandato_completo)%>%
  summarise(n = n())%>%
  arrange(desc(n))

mandatos_parcial <- dados%>%
  group_by(Mandato_parcial)%>%
  summarise(n = n())%>%
  arrange(desc(n))

grande_area <- dados%>%
  group_by(grande_area)%>%
  summarise(n = n())%>%
  arrange(grande_area)


####################################################curvas de sobrevivência - área
ST_area <- function(parametro,cor = 'black'){
  dias <- dados%>%
    filter(Área == parametro)%>%
    pull(Tempo_em_dias_1990)
    
    censura <- rep(1,length(dias))
    ekm <- survfit(Surv(dias,censura)~1, conf.int = F)
    
    curva <- plot(ekm,conf.int=F, xlab="Tempo (em dias)", ylab="S(t)", col = cor, main = parametro, xlim = c(0,12000))
   
    return(curva)
}

#Sáúde, Educação, Habitação,Assistência Social
par(mfrow = c(2,2))
agro <- ST_area('Agropecuária e Agrária','#af7ac5')
saude <- ST_area('Saúde','#65B1AE')
educacao <- ST_area('Educação','#487D5B')
assistencia_social <- ST_area('Assistência Social','#48c9b0')


#Infraestrutura, Adm Pública, Direitos Humanos e Meio Ambiente
par(mfrow = c(2,2))
infra <- ST_area('Infraestrutura','#873600')
adm_publica <- ST_area('Administração Pública','#2c3e50')
direitos_humanos <- ST_area('Direitos Humanos','#e74c3c')
meio_ambiente <- ST_area('Meio Ambiente','#58d68d')

#Cultura, Ciencia e Tec, Indústria e Com, Justiça e Seg
par(mfrow = c(2,2))
cultura <- ST_area('Cultura','#a11d21')
ciencia <- ST_area('Ciência e Tecnologia','#e59866')
industria <- ST_area('Indústria e Comércio','#CCCCFF')
justica <- ST_area('Justiça e Segurança Pública','#ed18cd')

#Desesa Nac, Desenvolv Reg, Economia Habitação e Urbanista
par(mfrow = c(2,2))
defesa_nacional <- ST_area('Defesa Nacional','#1f618d')
desenvolvimento_regional <- ST_area('Desenvolvimento Regional','#d868c9')
economia <- ST_area('Economia','#b7950b')
habitacao <- ST_area('Habitação e Urbanismo','#717d7e')

#Trabalho e Emprego, Turismo
par(mfrow = c(2,2))
trabalho <- ST_area('Trabalho e Emprego','#4d5656')
turismo <- ST_area('Turismo, Desporto e Lazer','#d35400')





######################################################### curvas sobrevivência - mandatos completos
ST_mandato_completo <- function(parametro,cor = 'black', presidente){
  dias <- dados%>%
    filter(Mandato_completo == parametro)%>%
    pull(Tempo_dias_completo)
  
  censura <- rep(1,length(dias))
  ekm <- survfit(Surv(dias,censura)~1, conf.int = F)
  
  curva <- plot(ekm,conf.int=F, xlab="Tempo (em dias)", ylab="S(t)", col = cor, main = presidente)
  
  return(curva)
}

#
par(mfrow = c(2,2))
collor <- ST_mandato_completo('Collor','#00ace7','Fernando Collor')
itamar <- ST_mandato_completo('Itamar Franco','#7b61d0','Itamar Franco')
fhc <- ST_mandato_completo('FHC','#ea8a00', 'Fernando Henrique Cardoso')

#
par(mfrow = c(2,2))
lula <- ST_mandato_completo('Lula','#fb2c00','Lula')
dilma <- ST_mandato_completo('Dilma','#161612','Dilma')
temer <- ST_mandato_completo('Temer','#33d616', 'Michel Temer')
bolsonaro <- ST_mandato_completo('Bolsonaro','#d68216', 'Jair Bolsonaro')






######################################################### curvas sobrevivência - mandatos parciais
ST_mandato_parcial <- function(parametro,cor = 'black', presidente){
  dias <- dados%>%
    filter(Mandato_parcial == parametro)%>%
    pull(Tempo_dias)
  
  censura <- rep(1,length(dias))
  ekm <- survfit(Surv(dias,censura)~1, conf.int = F)
  
  curva <- plot(ekm,conf.int=F, xlab="Tempo (em dias)", ylab="S(t)", col = cor, main = presidente)
  
  return(curva)
}

#
par(mfrow = c(2,2))
collor <- ST_mandato_parcial("Fernando Collor",'#00ace7','Fernando Collor')
itamar <- ST_mandato_parcial('Itamar Franco','#7b61d0','Itamar Franco')
fhc_1 <- ST_mandato_parcial("Fernando Henrique Cardoso I",'#ea8a00', 'Fernando Henrique Cardoso I')
fhc_2 <- ST_mandato_parcial("Fernando Henrique Cardoso II",'#b26a03', 'Fernando Henrique Cardoso II')


#
par(mfrow = c(2,2))
lula_1 <- ST_mandato_parcial('Luiz Inácio Lula da Silva I','#fb2c00','Lula I')
lula_2 <- ST_mandato_parcial('Luiz Inácio Lula da Silva II','#9c2309','Lula II')
dilma_1 <- ST_mandato_parcial("Dilma Rousseff I",'#161612','Dilma I')
dilma_2 <- ST_mandato_parcial('Dilma Rousseff II','#7e7e7d','Dilma II')

#
temer <- ST_mandato_parcial("Michel Temer",'#33d616', 'Michel Temer')
bolsonaro <- ST_mandato_parcial("Jair Bolsonaro",'#d68216', 'Jair Bolsonaro')




############################################################# curvas de sobrevivência por períodos
ST_periodo <- function(parametro,cor = 'black', periodo){
  dias <- dados%>%
    filter(periodos == parametro)%>%
    pull(Tempo_dias_periodo)
  
  censura <- rep(1,length(dias))
  ekm <- survfit(Surv(dias,censura)~1, conf.int = F)
  
  curva <- plot(ekm,conf.int=F, xlab="Tempo (em dias)", ylab="S(t)", col = cor, main = periodo,xlim = c(0,1500))
  
  return(curva)
}

#wraped
par(mfrow = c(2,2))
ST_periodo("1990 - 1994",'#000000',"1990 - 1994")
ST_periodo("1995 - 1998",'#cf0808',"1995 - 1998")
ST_periodo("1999 - 2002",'#25b808',"1999 - 2002")
ST_periodo("2003 - 2006",'#05adb2',"2003 - 2006")

par(mfrow = c(2,2))
ST_periodo("2007 - 2010",'#c8ce09',"2007 - 2010")
ST_periodo("2011 - 2014",'#9503b9',"2011 - 2014")
ST_periodo("2015 - 2018",'#0a18bf',"2015 - 2018")
ST_periodo("2019 - 2022",'#eba31d',"2019 - 2022")


#geral
periodos_ordenados <- dados%>%
  select(c(periodos,Tempo_dias_periodo))%>%
  arrange(periodos)

dados%>%
  select(c(periodos,Tempo_dias_periodo))%>%
  arrange(periodos)%>%
  group_by(periodos)%>%
  summarise(n = n())

tempo <- periodos_ordenados%>%pull(Tempo_dias_periodo)
censura <- rep(1,length(tempo))
grupos <- c(rep(1,31),rep(2,26),rep(3,48),rep(4,71),rep(5,84),rep(6,65),rep(7,63),
            rep(8,143))

ekm <- survfit(Surv(tempo,censura)~grupos, conf.int = F)

plot(ekm,conf.int=F, xlab="Tempo (em dias)", ylab="S(t)",lty = c(1,1,1,1,1,1,1,1),
     col = c('#000000','#cf0808','#25b808','#05adb2','#c8ce09','#9503b9','#0a18bf','#eba31d'),
     main = 'Curvas de sobrevivência por períodos')

legend(1300,0.99,lty=c(1,1,1,1,1,1,1,1),c(unique(periodos_ordenados$periodos)),
       col = c('#000000','#cf0808','#25b808','#05adb2','#c8ce09','#9503b9','#0a18bf','#eba31d'),cex = 0.9)





############################################################# curvas de sobrevivência por grandes areas
grande_area_ordenados <- dados%>%
  select(c(grande_area,Tempo_em_dias_1990))%>%
  arrange(grande_area)

tempo <- grande_area_ordenados%>%pull(Tempo_em_dias_1990)
censura <- rep(1,length(tempo))
grupos <- c(rep(1,32),rep(2,22),rep(3,125),rep(4,39),rep(5,37),rep(6,276))

ekm <- survfit(Surv(tempo,censura)~grupos, conf.int = F)

plot(ekm,conf.int=F, xlab="Tempo (em dias)", ylab="S(t)",lty = c(1,1,1,1,1,1),
     col = c('#000000','#d51443','#1420d5','#14d583','#d5c314','#a314d5'),
     main = 'Curvas de sobrevivência por macro área')

legend(400,0.5,lty=c(1,1,1,1,1,1),c(unique(grande_area_ordenados$grande_area)),
       col = c('#000000','#d51443','#1420d5','#14d583','#d5c314','#a314d5'),cex = 0.8)

################################################# curva de sobrevivencia geral
ST_geral <- function(cor = 'black', main){
  dias <- dados%>%
    mutate(Data = as.Date(Data,format = '%d/%m/%Y'))%>%
    mutate(Tempo_em_dias_1990 = Data - as.Date('1990-01-01'))%>%
    mutate(Tempo_em_dias_1990 = as.numeric(Tempo_em_dias_1990))%>%
    pull(Tempo_em_dias_1990)
  
  censura <- rep(1,length(dias))
  ekm <- survfit(Surv(dias,censura)~1, conf.int = F)
  
  curva <- plot(ekm,conf.int=F, xlab="Tempo (em dias)", ylab="S(t)", col = cor, main = main)
  
  return(curva)
}

ST_geral(main = 'Curva de Sobrevivência')
