######pacotes e diretório
if (!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse','readxl','data.table','writexl')


################################################################################
###########
#leitura dos dados
#alguns datasets estão com a data em formato de caractere
#os ajustes dos datasets foram feitos no excel
#a coluna TIPO foi adicionada no excel
dados <- function(arquivo_xlsx,campo){
  area <- read_xlsx(str_c('../enap_tratamento_dados/datasets_enap_padrao/',arquivo_xlsx))%>%
    select(seq(8,ncol(.),3))%>%
    head(1)%>%
    select_if(~any(!is.na(.)))%>%
    gather(1:ncol(.),key = 'Política', value = 'Data')%>%
    #mutate(Data = as.Date.character(Data,format = '%d/%m/%Y'))%>%
    mutate(Ano = format(Data,format = '%Y'))%>%
    mutate(Área = rep(campo, nrow(.)))%>%
    drop_na(Ano, Data)
  
  return(area)
}

######dados
saude <- dados('Dataset_saúde.xlsx','Saúde')
educacao <- dados('Dataset_educação.xlsx','Educação')
habitacao <- dados('Dataset_habitação.xlsx','Habitação e Urbanismo')
assistencia <- dados('Dataset_assistência_social.xlsx','Assistência Social')
ciencia <- dados('Dataset_ciência_tecnologia.xlsx','Ciência e Tecnologia')
meio_ambiente <- dados('Dataset_meio_ambiente.xlsx','Meio Ambiente')
trabalho <- dados('Dataset_trabalho_emprego.xlsx','Trabalho e Emprego')
economia <- dados('Dataset_economia.xlsx','Economia')
previdencia <- dados('Dataset_previdencia.xlsx','Previdência')
justica <- dados('Dataset_justica_segurança_publica.xlsx','Justiça e Segurança Pública')
agro <- dados('Dataset_agropecuária_agrária.xlsx','Agropecuária e Agrária')
comunicacao <- dados('Dataset_cultura.xlsx','Comunicação')
cultura <- dados('Dataset_cultura.xlsx','Cultura')
defesa_nacional <- dados('Dataset_defesa_nacional.xlsx','Defesa Nacional')
adm_publica <- dados('Dataset_administração_pública.xlsx','Administração Pública')
desenvolvimento_regional <- dados('Dataset_desenvolvimento_regional.xlsx','Desenvolvimento Regional')
direitos_humanos <- dados('Dataset_direitos_humanos.xlsx','Direitos Humanos')
industria_comercio <- dados('Dataset_indústria_comércio.xlsx','Indústria e Comércio')
infra <- dados('Dataset_infraestrutura.xlsx',"Infraestrutura")
seguranca_alimentar <- dados('Dataset_segurança_alimentar.xlsx','Segurança Alimentar')
turismo <- dados('Dataset_turismo_lazer.xlsx','Turismo, Desporto e Lazer')
relacoes_exteriores <- dados('Dataset_relacoes_exteriores.xlsx','Relações Exteriores')


#####unificando as bases
dados <- bind_rows(saude,educacao,habitacao,assistencia,ciencia,meio_ambiente,
                   trabalho,economia,previdencia,justica,agro,comunicacao,
                   cultura,defesa_nacional,adm_publica,desenvolvimento_regional,
                   direitos_humanos,industria_comercio,infra,seguranca_alimentar,
                   turismo,relacoes_exteriores)

#####evitar realizar todo o processamento de novo
#write_xlsx(dados,'politicas_padrao.xlsx') #exportando a base
dados <- read_xlsx('politicas_padrao.xlsx',sheet = 1)

#####tratando os dados
#adicionando os mandatos dos presidentes a partir do José Sarney
dados <- dados%>%
  mutate(Data = as_date(dados$Data,format = '%d/%m/%Y'))%>%
  mutate(Mandato = case_when((Data >= '1985-03-15' & Data < '1990-03-15') ~ 'José Sarney',
                             (Data >= '1990-03-15' & Data < '1992-12-29') ~ 'Fernando Collor',
                             (Data >= '1992-12-29' & Data < '1995-01-01') ~ 'Itamar Franco',
                             (Data >= '1995-01-01' & Data < '2003-01-01') ~ 'Fernando Henrique Cardoso',
                             (Data >= '2003-01-01' & Data < '2011-01-01') ~ 'Luiz Inácio Lula da Silva',
                             (Data >= '2011-01-01' & Data < '2016-08-31') ~ 'Dilma Rousseff',
                             (Data >= '2016-08-31' & Data < '2019-01-01') ~ 'Michel Temer',
                             (Data >= '2019-01-01' & Data < '2023-01-01') ~ 'Jair Bolsonaro',
                             TRUE ~ 'outro'))%>%
  mutate(Data = format(Data, format = '%d/%m/%Y'))%>%
  mutate(Mandato = factor(Mandato, levels = c('José Sarney','Fernando Collor',
                                              'Itamar Franco','Fernando Henrique Cardoso',
                                              'Luiz Inácio Lula da Silva','Dilma Rousseff',
                                              'Michel Temer','Jair Bolsonaro')))

#colocando o nome das políticas
pasta_datasets <- list.files('../enap_tratamento_dados/datasets_enap_padrao/')
nome_politicas <- function(x,area){
  ministerio <- read_xlsx(str_c(getwd(),'/datasets_enap_padrao/',pasta_datasets[x]),sheet = 2)%>%
    filter(str_detect(variables,'atogf'))%>%
    mutate(description = str_sub(description,46))%>%
    mutate(type = rep(area,nrow(.)))
}

x1 <- nome_politicas(19,'Saúde')
x2 <- nome_politicas(11,'Educação')
x3 <- nome_politicas(3,"Assistência Social")
x4 <- nome_politicas(2,"Agropecuária e Agrária")
x5 <- nome_politicas(4,"Ciência e Tecnologia")
x6 <- nome_politicas(5,'Comunicação')
x7 <- nome_politicas(6,'Cultura')
x8 <- nome_politicas(7,'Defesa Nacional')
x9 <- nome_politicas(10,"Economia")
x10 <- nome_politicas(12,"Habitação e Urbanismo")
x11 <- nome_politicas(15,"Justiça e Segurança Pública")#
x12 <- nome_politicas(16,"Meio Ambiente")
x13 <- nome_politicas(17,"Previdência")
x14 <- nome_politicas(21,"Trabalho e Emprego")
x15 <- nome_politicas(1,"Administração Pública")
x16 <- nome_politicas(8,"Desenvolvimento Regional")
x17 <- nome_politicas(9,"Direitos Humanos")
x18 <- nome_politicas(14,"Infraestrutura")
x19 <- nome_politicas(18,"Relações Exteriores")
x20 <- nome_politicas(22,"Turismo, Desporto e Lazer")
x21 <- nome_politicas(20,"Segurança Alimentar")
x22 <- nome_politicas(13,"Indústria e Comércio")

nomes <- x1%>%
  bind_rows(x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,
            x19,x20,x21,x22)

dados <- dados%>%
  left_join(nomes,by = c('Política' = 'variables','Área' ='type'),keep = F)%>%
  rename(Nome = description)%>%
  mutate(Ano = as.numeric(Ano))%>%
  select(Política,Nome,Data, Ano, Área, Mandato)










################################################################################
#####gráfico de linhas geral
contagem_mandatos <- dados%>%
  group_by(Mandato)%>%
  summarise(n = n())

ggplot(contagem_mandatos, aes(x = Mandato, y = n)) + 
  geom_col(fill="#808080",color = 'black') +
  geom_text(aes(label = n,vjust = -0.2),fontface = 'bold',size = 3)+
  labs(x="Mandato", y = "Quantidade de políticas") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text.x = element_text(colour = "black", size=4.5),
        axis.text.y = element_text(colour = "black", size=8),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")

####gráfico de linhas por área - WRAP
contagem_anos_aux <- tibble(Ano = years, n = rep(0,length(years)))

contagem_anos_por_area <- function(area){
  x <- dados%>%
    filter(Área == area)%>%
    group_by(Ano)%>%
    summarise(n = n())%>%
    bind_rows(contagem_anos_aux)%>%
    distinct(Ano,.keep_all = T)%>%
    arrange(Ano)%>%
    mutate(Área = rep(area,33))%>%
    select(Área, Ano, n)
  
  return(x)
}

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

contagem_por_área <- bind_rows(saude,educacao,habitacao,assistencia,ciencia,meio_ambiente,
                               trabalho,economia,previdencia,justica,agro,comunicacao,
                               cultura,defesa_nacional,adm_publica,desenvolvimento_regional,
                               direitos_humanos,industria_comercio,infra,seguranca_alimentar,
                               turismo,relacoes_exteriores)



#PARTE 1
ordem<- seq(1990,2022)
ordem2<- seq(0,6)
contagem_por_área%>%
  filter(Área %in% c('Administração Pública','Agropecuária e Agrária','Assistência Social',
                     'Ciência e Tecnologia','Comunicação','Cultura'))%>%
  ggplot(aes(x=Ano,y=n,group=Área,colour=Área)) +
  geom_line(size=1.5) + geom_point(size=3.5) +
  scale_colour_manual(name="Área", values = c('Administração Pública' = '#2c3e50',
                                              'Agropecuária e Agrária'= '#af7ac5',
                                              'Assistência Social'= '#48c9b0',
                                              'Ciência e Tecnologia' = '#e59866',
                                              'Comunicação'= '#3498db',
                                              'Cultura' = '#a11d21'))+
  labs(x="Ano", y="Políticas") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=15),
        axis.text.x = element_text(colour = "black", size=7),
        axis.text.y = element_text(colour = "black", size=12),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 15),
        axis.line = element_line(colour = "black"),
        legend.position="top",
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15)) +
  scale_x_discrete(limits=ordem)+
  scale_y_discrete(limits=ordem2)+
  facet_wrap(vars(Área),dir = 'v',ncol = 2)
#ggsave("politicasporarea_wrap_PT1.png", width = 450, height = 200, units = "mm")

#PARTE 2 (6)
ordem<- seq(1990,2022)
ordem2<- seq(0,9)
contagem_por_área%>%
  filter(Área %in% c('Defesa Nacional','Desenvolvimento Regional','Direitos Humanos',
                     'Economia','Educação','Habitação e Urbanismo'))%>%
  ggplot(aes(x=Ano,y=n,group=Área,colour=Área)) +
  geom_line(size=1.5) + geom_point(size=3.5) +
  scale_colour_manual(name="Área", values = c('Defesa Nacional'= '#1f618d',
                                              'Desenvolvimento Regional' = '#d868c9',
                                              'Direitos Humanos' = '#e74c3c',
                                              'Economia'= '#b7950b',
                                              'Educação'='#487D5B',
                                              'Habitação e Urbanismo'= '#717d7e'))+
  labs(x="Ano", y="Políticas") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=15),
        axis.text.x = element_text(colour = "black", size=7),
        axis.text.y = element_text(colour = "black", size=12),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 15),
        axis.line = element_line(colour = "black"),
        legend.position="top",
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15)) +
  scale_x_discrete(limits=ordem)+
  scale_y_discrete(limits=ordem2)+
  facet_wrap(vars(Área),dir = 'v',ncol = 2)
#ggsave("politicasporarea_wrap_PT2.png", width = 450, height = 200, units = "mm")




#PARTE 3 (6)
ordem<- seq(1990,2022)
ordem2<- seq(0,8)
contagem_por_área%>%
  filter(Área %in% c('Indústria e Comércio','Infraestrutura','Justiça e Segurança Pública',
                     'Meio Ambiente','Previdência','Relações Exteriores'))%>%
  ggplot(aes(x=Ano,y=n,group=Área,colour=Área)) +
  geom_line(size=1.5) + geom_point(size=3.5) +
  scale_colour_manual(name="Área", values = c('Indústria e Comércio' = '#CCCCFF',
                                              'Infraestrutura' = '#873600',
                                              'Justiça e Segurança Pública' = '#ed18cd',
                                              'Meio Ambiente' = '#58d68d',
                                              'Previdência' = '#f4d03f',
                                              'Relações Exteriores' = '#8e44ad'))+
  labs(x="Ano", y="Políticas") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=15),
        axis.text.x = element_text(colour = "black", size=7),
        axis.text.y = element_text(colour = "black", size=12),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 15),
        axis.line = element_line(colour = "black"),
        legend.position="top",
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))+
  scale_x_discrete(limits=ordem)+
  scale_y_discrete(limits=ordem2)+
  facet_wrap(vars(Área),dir = 'v',ncol = 2)
#ggsave("politicasporarea_wrap_PT3.png", width = 450, height = 200, units = "mm")



#PARTE 4 (4)
ordem<- seq(1990,2022)
ordem2<- seq(0,6)
contagem_por_área%>%
  filter(Área %in% c('Saúde','Segurança Alimentar','Trabalho e Emprego',
                     'Turismo, Desporto e Lazer'))%>%
  ggplot(aes(x=Ano,y=n,group=Área,colour=Área)) +
  geom_line(size=1.5) + geom_point(size=3.5) +
  scale_colour_manual(name="Área", values = c('Saúde'='#65B1AE',
                                              'Segurança Alimentar' = '#3af710',
                                              'Trabalho e Emprego' = '#4d5656',
                                              'Turismo, Desporto e Lazer' = '#d35400'))+
  labs(x="Ano", y="Políticas") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=15),
        axis.text.x = element_text(colour = "black", size=7),
        axis.text.y = element_text(colour = "black", size=12),
        panel.border = element_blank(),
        strip.text.x = element_text(size = 15),
        axis.line = element_line(colour = "black"),
        legend.position="top",
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))+
  scale_x_discrete(limits=ordem)+
  scale_y_discrete(limits=ordem2)+
  facet_wrap(vars(Área),dir = 'v',ncol = 2)
#ggsave("politicasporarea_wrap_PT4.png", width = 450, height = 200, units = "mm")

####gráfico de barras - MANDATO
contagem_mandatos <- dados%>%
  group_by(Mandato)%>%
  summarise(n = n())

ggplot(contagem_mandatos, aes(x = Mandato, y = n)) + 
  geom_col(fill="#808080",color = 'black') +
  geom_text(aes(label = n,vjust = -0.2),fontface = 'bold',size = 3)+
  labs(x="Mandato", y = "Quantidade de políticas") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text.x = element_text(colour = "black", size=4.5),
        axis.text.y = element_text(colour = "black", size=8),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")
#ggsave("qnt_mandato_bl.png", width = 180, height = 120, units = "mm")

####MANDATO BARRAS POR ÁREA
contagem_mandatos_area <- dados%>%
  group_by(Mandato, Área)%>%
  summarise(n = n())

contagem_mandatos_area%>%
  filter(Mandato %in% c('José Sarney','Fernando Collor','Itamar Franco','Fernando Henrique Cardoso'))%>%
  ggplot(aes(x=Mandato, y=n, fill = Área)) + 
  geom_bar(stat="identity",position='dodge',color = 'black') +
  geom_text(aes(label = n),fontface = 'bold',size = 4.5,position = position_dodge(width = 0.9),vjust = -0.25)+
  scale_fill_manual(name="Área", values = c('Administração Pública' = '#2c3e50',
                                            'Agropecuária e Agrária'= '#af7ac5',
                                            'Assistência Social'= '#48c9b0',
                                            'Ciência e Tecnologia' = '#e59866',
                                            'Comunicação'= '#3498db',
                                            'Cultura' = '#a11d21',
                                            'Defesa Nacional'= '#1f618d',
                                            'Desenvolvimento Regional' = '#d868c9',
                                            'Direitos Humanos' = '#e74c3c',
                                            'Economia'= '#b7950b',
                                            'Educação'='#487D5B',
                                            'Habitação e Urbanismo'= '#717d7e',
                                            'Indústria e Comércio' = '#CCCCFF',
                                            'Infraestrutura' = '#873600',
                                            'Justiça e Segurança Pública' = '#ed18cd',
                                            'Meio Ambiente' = '#58d68d',
                                            'Previdência' = '#f4d03f',
                                            'Relações Exteriores' = '#8e44ad',
                                            'Saúde'='#65B1AE',
                                            'Segurança Alimentar' = '#3af710',
                                            'Trabalho e Emprego' = '#4d5656',
                                            'Turismo, Desporto e Lazer' = '#d35400')) +
  labs(x="", y="Quantidade de políticas") +
  theme_bw() +
  ylim(0,25)+
  theme(axis.title.y =element_text(colour="black", size=15),
        axis.title.x = element_text(colour="black", size=15),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour = "black", size=15),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text.x = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.position="top")+
  facet_wrap(vars(Mandato),dir = 'v',ncol = 2, scales = 'free_x',shrink = T,
             strip.position = 'top')
#ggsave("politicas_por_mandato_p1.png", width = 450, height = 200, units = "mm")



contagem_mandatos_area%>%
  filter(Mandato %in% c('Luiz Inácio Lula da Silva','Dilma Rousseff','Michel Temer','Jair Bolsonaro'))%>%
  ggplot(aes(x=Mandato, y=n, fill = Área)) + 
  geom_bar(stat="identity",position='dodge',color = 'black') +
  geom_text(aes(label = n),fontface = 'bold',size = 4.5,position = position_dodge(width = 0.9),vjust = -0.25)+
  scale_fill_manual(name="Área", values = c('Administração Pública' = '#2c3e50',
                                            'Agropecuária e Agrária'= '#af7ac5',
                                            'Assistência Social'= '#48c9b0',
                                            'Ciência e Tecnologia' = '#e59866',
                                            'Comunicação'= '#3498db',
                                            'Cultura' = '#a11d21',
                                            'Defesa Nacional'= '#1f618d',
                                            'Desenvolvimento Regional' = '#d868c9',
                                            'Direitos Humanos' = '#e74c3c',
                                            'Economia'= '#b7950b',
                                            'Educação'='#487D5B',
                                            'Habitação e Urbanismo'= '#717d7e',
                                            'Indústria e Comércio' = '#CCCCFF',
                                            'Infraestrutura' = '#873600',
                                            'Justiça e Segurança Pública' = '#ed18cd',
                                            'Meio Ambiente' = '#58d68d',
                                            'Previdência' = '#f4d03f',
                                            'Relações Exteriores' = '#8e44ad',
                                            'Saúde'='#65B1AE',
                                            'Segurança Alimentar' = '#3af710',
                                            'Trabalho e Emprego' = '#4d5656',
                                            'Turismo, Desporto e Lazer' = '#d35400')) +
  labs(x="", y="Quantidade de políticas") +
  theme_bw() +
  ylim(0,25)+
  theme(axis.title.y =element_text(colour="black", size=15),
        axis.title.x = element_text(colour="black", size=15),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour = "black", size=15),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text.x = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.position="none")+
  facet_wrap(vars(Mandato),dir = 'v',ncol = 2, scales = 'free_x',shrink = T,
             strip.position = 'top')
#ggsave("politicas_por_mandato_p2.png", width = 450, height = 200, units = "mm")
