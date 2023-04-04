#
library(readxl)
library(dplyr)
library(writexl)

#
setwd('/home/matheus/Documentos/ENAP')
getwd()
psf <- read_excel('Dataset_PSF.xlsx')
psf2 <- psf #dataframes não são 'espelhos'!


#
psf[,'AnoAdoção']#toda coluna AnoAdoção
psf[1,]#toda linha 1
psf[1,'AnoAdoção']#elemento específico
#psf_varind <- psf[,9:28]

#modificando tempo de adocao - PROVAVELMENTE NÃO É A MELHOR ALTERNATIVA!!
for(i in 1:nrow(psf2)){
  if(psf2$v1991[i] == 1){
    psf2$AnoAdoção[i] = '1991'
  }else if (psf2$v1992[i] == 1){
    psf2$AnoAdoção[i] = '1992'
  }else if (psf2$v1993[i] == 1){
    psf2$AnoAdoção[i] = '1993'
  }else if (psf2$v1994[i] == 1){
    psf2$AnoAdoção[i] = '1994'
  }
  else if (psf2$v1995[i] == 1){
    psf2$AnoAdoção[i] = '1995'
  }
  else if (psf2$v1996[i] == 1){
    psf2$AnoAdoção[i] = '1996'
  }
  else if (psf2$v1997[i] == 1){
    psf2$AnoAdoção[i] = '1997'
  }
  else if (psf2$v1998[i] == 1){
    psf2$AnoAdoção[i] = '1998'
  }
  else if (psf2$v1999[i] == 1){
    psf2$AnoAdoção[i] = '1999'
  }
  else if (psf2$v2000[i] == 1){
    psf2$AnoAdoção[i] = '2000'
  }
  else if (psf2$v2001[i] == 1){
    psf2$AnoAdoção[i] = '2001'
  }
  else if (psf2$v2002[i] == 1){
    psf2$AnoAdoção[i] = '2002'
  }
  else if (psf2$v2003[i] == 1){
    psf2$AnoAdoção[i] = '2003'
  }
  else if (psf2$v2004[i] == 1){
    psf2$AnoAdoção[i] = '2004'
  }
  else if (psf2$v2005[i] == 1){
    psf2$AnoAdoção[i] = '2005'
  }else if (psf2$v2006[i] == 1){
    psf2$AnoAdoção[i] = '2006'
  }else if (psf2$v2007[i] == 1){
    psf2$AnoAdoção[i] = '2007'
  }else if (psf2$v2008[i] == 1){
    psf2$AnoAdoção[i] = '2008'
  }else if (psf2$v2009[i] == 1){
    psf2$AnoAdoção[i] = '2009'
  }else if (psf2$v2010[i] == 1){
    psf2$AnoAdoção[i] = '2010'
  }
}

#SUBSTITUINDO VALORES DA BASE
table(psf2$AnoAdoção)
psf2$AnoAdoção <- gsub('#NULL!','2011',psf2$AnoAdoção)
psf2$AnoAdoção <- as.numeric(psf2$AnoAdoção)
str(psf2)

#exportando
write_xlsx(psf2,'/home/matheus/Documentos/ENAP\\Dataset_PSF_Modificado.xlsx')
