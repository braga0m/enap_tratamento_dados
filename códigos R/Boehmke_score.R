#
library(haven)
library(tidyverse)

setwd('/home/matheus/Downloads/dataverse_files')
getwd()

#
adoption <- read_dta('stinn1209adoptions.dta')#ano de adoção de cada política
policies <- read_dta('stinn1209policies.dta')#summary das políticas
dynamic <- read_dta('stinn1209dynamic.dta')#escore dinamico (bienio)
staticR <- read_dta('stinn1209staticR.dta')#escores estáticos reamostrados
static <- read_dta('stinn1209static.dta')#escores estatáticos
#linhas da tabela policies são as colunas da tabela adoption!
#checar lista de políticas no pdf, caso seja necessário.

#auxiliares
p <- adoption[7:143]
estados <- adoption$state_nam

################################################################################
################### WALKER SCORE ###############################################
################################################################################

ordem_policies <- policies$policy
p <- p[ordem_policies]

MIN <- policies$adopt_min
MAX <- policies$adopt_max

Wik <- list()
Wik <- NA*seq(137)
for(i in 1:50){
  for(j in 1:137){
    Wik[[i]][j] <- (MAX[j] - p[i,j])/(MAX[j] - MIN[j])
  }
}

Walker.Score <- c()
for(i in 1:50){
  Wik[[i]] <- as.numeric(Wik[[i]])
  #Walker.Score[i] <- mean(Wik[[i]],na.rm = T)
  Walker.Score[i] <- sum(Wik[[i]],na.rm = T)/137
}
################################################################################
################################### Todos os estados############################
################################################################################

#banco com estado e politicas
adoptionresumido<-adoption%>%
  select(-state, -state_num,-state_fips, -state_icpsr, -yrstatehd)

#banco transposto
adoptiontransp <- adoptionresumido%>%
  pivot_longer(!state_nam, names_to="politica", values_to ="ano")%>%
  na.omit()

estados<- unique(adoptiontransp$state_nam)
anos<-1913:2009

`%!in%` <- Negate(`%in%`)

states<-c()
years<-c()
valores<-c()


adoptionrace<-c()

for (e in estados){
  v1<-c()
  v2<-c()
  for (a in anos){
    
    teste1<-adoptiontransp%>%
      filter(state_nam == e & ano==a)
    
    valor1<- length(teste1$state_nam)
    
    politicas<- teste1$politica
    
    teste2<-adoptiontransp%>%
      filter(state_nam!=e & ano==a & politica %!in% politicas)
    
    valor2<-length(unique(teste2$politica))
    
    r<- valor1/valor2
    states<-c(states, e)
    years<-c(years, a)
    valores<-c(valores,r)
    
    v1<-c(v1, valor1)
    v2<-c(v2, valor2)
  }
  adoptionrace<-c(adoptionrace, sum(v1)/sum(v2))
} 

resultados<- data.frame(states, years, valores)resultados2<- data.frame(states, years, adoptionrace)

comparacao <- data.frame(estados, adoptionrace, static$ipct1209, dif=(adoptionrace-static$ipct1209))

hist(comparacao$dif)




#######
valor <- as.numeric(Walker.Score)
valor <- as.numeric(adoptionrace)
medida <- rep('Escore de Walker',50)
medida<- rep('Adoption Rate',50)
y <- tibble(adoption)

W <- data.frame(estados,valor)
A <- data.frame(estados,valor)


###############
W <- W%>%
  arrange(desc(valor))
W

A <- A%>%
  arrange(desc(valor))
A

###########
WALKER.ORDEM <- ggplot(W,aes(y = fct_rev(fct_inorder(estados)), x = valor))+
  geom_point(colour="#054F77", size=3)+
  labs(x = 'Escores de Walker', y = 'Estados')+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
WALKER.ORDEM

###########
ADOPTION.ORDEM <- ggplot(A,aes(y = fct_rev(fct_inorder(estados)), x = valor))+
  geom_point(colour="#CF0E0E", size=3)+
  labs(x = 'Adoption Rate', y = 'Estados')+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
ADOPTION.ORDEM


##########
comparacao_adop <- data.frame(estados, adoptionrace, static$ipct1209)
colnames(comparacao_adop) <- c('Estados','Escore_Calculado','Escore')

comparacao_walker <- data.frame(estados,walker, static$iwkr1209)
colnames(comparacao_walker) <- c('Estados','Escore_Calculado','Escore')

#########3
ggplot(comparacao_walker,aes(y = Escore_Calculado , x = Escore))+
  geom_point(colour="#054F77", size=3)+
  geom_smooth(se = FALSE, method = "lm",color = 1)+
  labs(y = 'Escore de Walker Calculado', x = 'Escore de Walker')+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

#########   
ggplot(comparacao_adop,aes(y = Escore_Calculado , x = Escore))+
  geom_point(colour="#CF0E0E", size=3)+
  geom_smooth(se = FALSE, method = "lm",color = 1)+
  labs(y = 'Adoption Rate Score Calculado', x = 'Adoption Rate Escore')+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))


ggsave("comparacao_walker.png", width = 158, height = 170, units = "mm")
