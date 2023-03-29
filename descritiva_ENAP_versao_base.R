#pacotes
if (!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse','readxl','data.table','writexl')

setwd('/home/matheus/Documentos/ENAP/Datasets')
getwd()

######DADOS
#educação modificada
edu_mod <- read_xlsx('Dataset_educação.xlsx')%>%
  select(seq(8,ncol(.),3))%>%
  head(1)%>%
  select_if(~any(!is.na(.)))%>%
  gather(1:ncol(.),key = 'Política', value = 'Data')%>%
  #mutate(Data = as.Date.character(Data,format = '%d/%m/%Y'))%>%
  mutate(Ano = format(Data,format = '%Y'))%>%
  mutate(Área = rep("Educação", nrow(.)))%>%
  drop_na(Ano, Data)

#saúde modificada
sau_mod <- read_xlsx('Dataset_saúde.xlsx')%>%
  select(seq(8,ncol(.),3))%>%
  head(1)%>%
  select_if(~any(!is.na(.)))%>%
  gather(1:ncol(.),key = 'Política', value = 'Data')%>%
  #mutate(Data = as.Date.character(Data,format = '%d/%m/%Y'))%>%
  mutate(Ano = format(Data,format = '%Y'))%>%
  mutate(Área = rep("Saúde", nrow(.)))%>%
  drop_na(Ano, Data)

#habitação modificada
hab_mod <- read_xlsx('Dataset_habitação.xlsx')%>%
  select(seq(8,ncol(.),3))%>%
  head(1)%>%
  select_if(~any(!is.na(.)))%>%
  gather(1:ncol(.),key = 'Política', value = 'Data')%>%
  #mutate(Data = as.Date.character(Data,format = '%d/%m/%Y'))%>%
  mutate(Ano = format(Data,format = '%Y'))%>%
  mutate(Área = rep("Habitação e Urbanismo", nrow(.)))%>%
  drop_na(Ano, Data)

#assistencia modificada
assist_mod <- read_xlsx('Dataset_assistência_social.xlsx')%>%
  select(seq(8,ncol(.),3))%>%
  head(1)%>%
  select_if(~any(!is.na(.)))%>%
  gather(1:ncol(.),key = 'Política', value = 'Data')%>%
  #mutate(Data = as.Date.character(Data,format = '%d/%m/%Y'))%>%
  mutate(Ano = format(Data,format = '%Y'))%>%
  mutate(Área = rep("Assistência Social", nrow(.)))%>%
  drop_na(Ano, Data)

#ciencia modificado
tec_mod <- read_xlsx('Dataset_ciência_tecnologia.xlsx')%>%
  select(seq(8,ncol(.),3))%>%
  head(1)%>%
  select_if(~any(!is.na(.)))%>%
  gather(1:ncol(.),key = 'Política', value = 'Data')%>%
  #mutate(Data = as.Date.character(Data,format = '%d/%m/%Y'))%>%
  mutate(Ano = format(Data,format = '%Y'))%>%
  mutate(Área = rep("Ciência e Tecnologia", nrow(.)))%>%
  drop_na(Ano, Data)


#meio ambiente modificado
meio_amb_mod <- read_xlsx('Dataset_meio_ambiente.xlsx')%>%
  select(seq(8,ncol(.),3))%>%
  head(1)%>%
  select_if(~any(!is.na(.)))%>%
  gather(1:ncol(.),key = 'Política', value = 'Data')%>%
  #mutate(Data = as.Date.character(Data,format = '%d/%m/%Y'))%>%
  mutate(Ano = format(Data,format = '%Y'))%>%
  mutate(Área = rep("Meio Ambiente", nrow(.)))%>%
  drop_na(Ano, Data)

#trabalho modificada
trab_mod <- read_xlsx('Dataset_trabalho_emprego.xlsx')%>%
  select(seq(8,ncol(.),3))%>%
  head(1)%>%
  select_if(~any(!is.na(.)))%>%
  gather(1:ncol(.),key = 'Política', value = 'Data')%>%
  #mutate(Data = as.Date.character(Data,format = '%d/%m/%Y'))%>%
  mutate(Ano = format(Data,format = '%Y'))%>%
  mutate(Área = rep("Trabalho e Emprego", nrow(.)))%>%
  drop_na(Ano, Data)

#economia modificada
econ_mod <- read_xlsx('Dataset_economia.xlsx')%>%
  select(seq(8,ncol(.),3))%>%
  head(1)%>%
  select_if(~any(!is.na(.)))%>%
  gather(1:ncol(.),key = 'Política', value = 'Data')%>%
  mutate(Data = as.Date.character(Data,format = '%d/%m/%Y'))%>%
  mutate(Ano = format(Data,format = '%Y'))%>%
  mutate(Área = rep("Economia", nrow(.)))%>%
  drop_na(Ano, Data)


#previdência modificada
previd_mod <- read_xlsx('Dataset_previdencia.xlsx')%>%
  select(seq(8,ncol(.),3))%>%
  head(1)%>%
  select_if(~any(!is.na(.)))%>%
  gather(1:ncol(.),key = 'Política', value = 'Data')%>%
  mutate(Data = as.Date.character(Data,format = '%d/%m/%Y'))%>%
  mutate(Ano = format(Data,format = '%Y'))%>%
  mutate(Área = rep("Previdência", nrow(.)))%>%
  drop_na(Ano, Data)

#justiça modificada
just_mod <- read_xlsx('Dataset_justica_segurança_publica.xlsx')%>%
  select(seq(8,ncol(.),3))%>%
  head(1)%>%
  select_if(~any(!is.na(.)))%>%
  gather(1:ncol(.),key = 'Política', value = 'Data')%>%
  mutate(Data = as.Date.character(Data,format = '%d/%m/%Y'))%>%
  mutate(Ano = format(Data,format = '%Y'))%>%
  mutate(Área = rep("Justiça e Segurança Pública", nrow(.)))%>%
  drop_na(Ano, Data)

#agro modificada
agro_mod <- read_xlsx('Dataset_agropecuária_agrária.xlsx')%>%
  select(seq(8,ncol(.),3))%>%
  head(1)%>%
  select_if(~any(!is.na(.)))%>%
  gather(1:ncol(.),key = 'Política', value = 'Data')%>%
  #mutate(Data = as.Date.character(Data,format = '%d/%m/%Y'))%>%
  mutate(Ano = format(Data,format = '%Y'))%>%
  mutate(Área = rep("Agropecuária e Agrária", nrow(.)))%>%
  drop_na(Ano, Data)

#comunicação modificada
comunicacao_mod <- read_xlsx('Dataset_comunicação.xlsx')%>%
  select(seq(8,ncol(.),3))%>%
  head(1)%>%
  select_if(~any(!is.na(.)))%>%
  gather(1:ncol(.),key = 'Política', value = 'Data')%>%
  mutate(Data = as.Date.character(Data,format = '%d/%m/%Y'))%>%
  mutate(Ano = format(Data,format = '%Y'))%>%
  mutate(Área = rep("Comunicação", nrow(.)))%>%
  drop_na(Ano, Data)

#cultura modificda
cultura_mod <- read_xlsx('Dataset_cultura.xlsx')%>%
  select(seq(8,ncol(.),3))%>%
  head(1)%>%
  select_if(~any(!is.na(.)))%>%
  gather(1:ncol(.),key = 'Política', value = 'Data')%>%
  mutate(Data = as.Date.character(Data,format = '%d/%m/%Y'))%>%
  mutate(Ano = format(Data,format = '%Y'))%>%
  mutate(Área = rep("Cultura", nrow(.)))%>%
  drop_na(Ano, Data)
  
#defesa nacional modificada
def_nacional_mod <- read_xlsx('Dataset_defesa_nacional.xlsx')%>%
  select(seq(8,ncol(.),3))%>%
  head(1)%>%
  select_if(~any(!is.na(.)))%>%
  gather(1:ncol(.),key = 'Política', value = 'Data')%>%
  #mutate(Data = as.Date.character(Data,format = '%d/%m/%Y'))%>%
  mutate(Ano = format(Data,format = '%Y'))%>%
  mutate(Área = rep("Defesa Nacional", nrow(.)))%>%
  drop_na(Ano, Data)

#administração pub modificada
adm_pub_mod <-read_xlsx("Dataset_administração_pública.xlsx")%>%
  select(seq(8,ncol(.),3))%>%
  head(1)%>%
  select_if(~any(!is.na(.)))%>%
  gather(1:ncol(.),key = 'Política', value = 'Data')%>%
  #mutate(Data = as.Date.character(Data,format = '%d/%m/%Y'))%>%
  mutate(Ano = format(Data,format = '%Y'))%>%
  mutate(Área = rep("Administração Pública", nrow(.)))%>%
  drop_na(Ano, Data)

  
#desenvolvimento regional mod
desen_regional_mod <- read_xlsx("Dataset_desenvolvimento_regional.xlsx")%>%
  select(seq(8,ncol(.),3))%>%
  head(1)%>%
  select_if(~any(!is.na(.)))%>%
  gather(1:ncol(.),key = 'Política', value = 'Data')%>%
  #mutate(Data = as.Date.character(Data,format = '%d/%m/%Y'))%>%
  mutate(Ano = format(Data,format = '%Y'))%>%
  mutate(Área = rep("Desenvolvimento Regional", nrow(.)))%>%
  drop_na(Ano, Data)
  

#direitos_humanos modificada
dir_humanos_mod <- read_xlsx("Dataset_direitos_humanos.xlsx")%>%
  select(seq(8,ncol(.),3))%>%
  head(1)%>%
  select_if(~any(!is.na(.)))%>%
  gather(1:ncol(.),key = 'Política', value = 'Data')%>%
  mutate(Data = as.Date.character(Data,format = '%d/%m/%Y'))%>%
  mutate(Ano = format(Data,format = '%Y'))%>%
  mutate(Área = rep("Direitos Humanos", nrow(.)))%>%
  drop_na(Ano, Data)
  
#industria e comercio modificada
industria_comercio_mod <- read_xlsx("Dataset_indústria_comércio.xlsx")%>%
  select(seq(8,ncol(.),3))%>%
  head(1)%>%
  select_if(~any(!is.na(.)))%>%
  gather(1:ncol(.),key = 'Política', value = 'Data')%>%
  #mutate(Data = as.Date.character(Data,format = '%d/%m/%Y'))%>%
  mutate(Ano = format(Data,format = '%Y'))%>%
  mutate(Área = rep("Indústria e Comércio", nrow(.)))%>%
  drop_na(Ano, Data)


#infra modificada
infra_mod <- read_xlsx("Dataset_infraestrutura.xlsx")%>%
  select(seq(8,ncol(.),3))%>%
  head(1)%>%
  select_if(~any(!is.na(.)))%>%
  gather(1:ncol(.),key = 'Política', value = 'Data')%>%
  mutate(Data = as.Date.character(Data,format = '%d/%m/%Y'))%>%
  mutate(Ano = format(Data,format = '%Y'))%>%
  mutate(Área = rep("Infraestrutura", nrow(.)))%>%
  drop_na(Ano, Data)


#seg_alimentar mod
seg_alimentar_mod <- read_xlsx("Dataset_segurança_alimentar.xlsx")%>%
  select(seq(8,ncol(.),3))%>%
  head(1)%>%
  select_if(~any(!is.na(.)))%>%
  gather(1:ncol(.),key = 'Política', value = 'Data')%>%
  mutate(Data = as.Date.character(Data,format = '%d/%m/%Y'))%>%
  mutate(Ano = format(Data,format = '%Y'))%>%
  mutate(Área = rep("Segurança Alimentar", nrow(.)))%>%
  drop_na(Ano, Data)


#turismo modificada
turismo_mod <- read_xlsx("Dataset_turismo_lazer.xlsx")%>%
  select(seq(8,ncol(.),3))%>%
  head(1)%>%
  select_if(~any(!is.na(.)))%>%
  gather(1:ncol(.),key = 'Política', value = 'Data')%>%
  #mutate(Data = as.Date.character(Data,format = '%d/%m/%Y'))%>%
  mutate(Ano = format(Data,format = '%Y'))%>%
  mutate(Área = rep("Turismo, Desporto e Lazer", nrow(.)))


#relacões exteriores mod
relex_mod <- read_xlsx("Dataset_relacoes_exteriores.xlsx" )%>%
  select(seq(8,ncol(.),3))%>%
  head(1)%>%
  select_if(~any(!is.na(.)))%>%
  gather(1:ncol(.),key = 'Política', value = 'Data')%>%
  mutate(Data = as.Date.character(Data,format = '%d/%m/%Y'))%>%
  mutate(Ano = format(Data,format = '%Y'))%>%
  mutate(Área = rep("Relações Exteriores", nrow(.)))














#####################merge dos dados
#531 com datas
##?? no total
dados <- rbind(sau_mod,edu_mod,hab_mod,assist_mod,just_mod,previd_mod,meio_amb_mod,
               econ_mod,trab_mod,tec_mod,agro_mod,comunicacao_mod,def_nacional_mod,
               cultura_mod,adm_pub_mod,desen_regional_mod,dir_humanos_mod,
               industria_comercio_mod,infra_mod,seg_alimentar_mod,turismo_mod,relex_mod)



dados <- read_xlsx('/home/matheus/Documentos/ENAP/dados_tipo_DEFINITIVO_FINAL.xlsx')

dados%>%
  group_by(Área)%>%
  summarise(n = n())%>%
  arrange(desc(n))





















############################gráfico de linhas geral
years <- c(1990:2022)
distintos <- setdiff(years,unique(dados$Ano))

contagem <- dados%>%
  mutate(Ano = as.numeric(Ano))%>%
  group_by(Ano)%>%
  summarise(n = n())
  #bind_rows(tibble(Ano = distintos, n = rep(0,4)))%>%
  #arrange(Ano)


ordem<- seq(1990,2022)
ordem2<- seq(5,60,5)
ggplot(contagem, aes(x=Ano, y=n, group=1)) +
  geom_line(size=1.5,colour="#A9A9A9") + geom_point(colour="#808080",size=3) +
  labs(x="Ano", y="Quantidade de políticas") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text.x = element_text(colour = "black", size=4.8),
        axis.text.y = element_text(colour = "black", size=7),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))+
  scale_x_discrete(limits=ordem)+
  scale_y_discrete(limits = ordem2)
ggsave("politicasporano.png", width = 180, height = 120, units = "mm")





##############################gráfico de linhas por área - AJUSTE
distintos_saude <- setdiff(years,dados%>%filter(Área == 'Saúde')%>%select(Ano)%>%unique()%>%pull())
distintos_edu <- setdiff(years,dados%>%filter(Área == 'Educação')%>%select(Ano)%>%unique()%>%pull())
distintos_hab <- setdiff(years,dados%>%filter(Área == 'Habitação e Urbanismo')%>%select(Ano)%>%unique()%>%pull())
distintos_assist <- setdiff(years,dados%>%filter(Área == 'Assistência Social')%>%select(Ano)%>%unique()%>%pull())
distintos_econ <- setdiff(years,dados%>%filter(Área == "Economia")%>%select(Ano)%>%unique()%>%pull())
distintos_tec <- setdiff(years,dados%>%filter(Área == "Ciência e Tecnologia")%>%select(Ano)%>%unique()%>%pull())
distintos_trab <- setdiff(years,dados%>%filter(Área == "Trabalho e Emprego")%>%select(Ano)%>%unique()%>%pull())
distintos_prev <- setdiff(years,dados%>%filter(Área == "Previdência")%>%select(Ano)%>%unique()%>%pull())
distintos_meio_amb <- setdiff(years,dados%>%filter(Área == "Meio Ambiente")%>%select(Ano)%>%unique()%>%pull())
distintos_just <- setdiff(years,dados%>%filter(Área == "Justiça e Segurança Pública")%>%select(Ano)%>%unique()%>%pull())
distintos_cultura <- setdiff(years,dados%>%filter(Área == "Cultura")%>%select(Ano)%>%unique()%>%pull())
distintos_defesa_nacional <- setdiff(years,dados%>%filter(Área == 'Defesa Nacional')%>%select(Ano)%>%unique()%>%pull())
distintos_comunicacao <- setdiff(years,dados%>%filter(Área == "Comunicação")%>%select(Ano)%>%unique()%>%pull())
distintos_agro <- setdiff(years,dados%>%filter(Área == "Agropecuária e Agrária")%>%select(Ano)%>%unique()%>%pull())
distintos_adm_pub <- setdiff(years,dados%>%filter(Área == "Administração Pública")%>%select(Ano)%>%unique()%>%pull())
distintos_des_reg <- setdiff(years,dados%>%filter(Área == "Desenvolvimento Regional")%>%select(Ano)%>%unique()%>%pull())
distintos_dir_hum <- setdiff(years,dados%>%filter(Área == "Direitos Humanos")%>%select(Ano)%>%unique()%>%pull())
distintos_indust <- setdiff(years,dados%>%filter(Área == "Indústria e Comércio")%>%select(Ano)%>%unique()%>%pull())
distintos_infra <- setdiff(years,dados%>%filter(Área == "Infraestrutura")%>%select(Ano)%>%unique()%>%pull())
distintos_seg_alimentar <- setdiff(years,dados%>%filter(Área == "Segurança Alimentar")%>%select(Ano)%>%unique()%>%pull())
distintos_turismo <- setdiff(years,dados%>%filter(Área == "Turismo, Desporto e Lazer")%>%select(Ano)%>%unique()%>%pull())
distintos_relex <- setdiff(years,dados%>%filter(Área == "Relações Exteriores")%>%select(Ano)%>%unique()%>%pull())


contagem_por_área <- dados%>%
  mutate(Ano = as.numeric(Ano))%>%
  group_by(Ano, Área)%>%
  summarise(n = n())%>%
  bind_rows(tibble(Ano = distintos_saude,
                   Área = rep('Saúde',length(distintos_saude)),
                   n = rep(0,length(distintos_saude))))%>%
  bind_rows(tibble(Ano = distintos_edu,
                   Área = rep('Educação',length(distintos_edu)),
                   n = rep(0,length(distintos_edu))))%>%
  bind_rows(tibble(Ano = distintos_hab,
                   Área = rep('Habitação e Urbanismo',length(distintos_hab)),
                   n = rep(0,length(distintos_hab))))%>%
  bind_rows(tibble(Ano = distintos_assist,
                   Área = rep('Assistência Social',length(distintos_assist)),
                   n = rep(0,length(distintos_assist))))%>%
  bind_rows(tibble(Ano = distintos_econ,
                   Área = rep("Economia",length(distintos_econ)),
                   n = rep(0,length(distintos_econ))))%>%
  bind_rows(tibble(Ano = distintos_tec,
                   Área = rep("Ciência e Tecnologia",length(distintos_tec)),
                   n = rep(0,length(distintos_tec))))%>%
  bind_rows(tibble(Ano = distintos_meio_amb,
                   Área = rep('Meio Ambiente',length(distintos_meio_amb)),
                   n = rep(0,length(distintos_meio_amb))))%>%
  bind_rows(tibble(Ano = distintos_just,
                   Área = rep('Justiça e Segurança Pública',length(distintos_just)),
                   n = rep(0,length(distintos_just))))%>%
  bind_rows(tibble(Ano = distintos_prev,
                   Área = rep('Previdência',length(distintos_prev)),
                   n = rep(0,length(distintos_prev))))%>%
  bind_rows(tibble(Ano = distintos_trab,
                   Área = rep("Trabalho e Emprego",length(distintos_trab)),
                   n = rep(0,length(distintos_trab))))%>%
  bind_rows(tibble(Ano = distintos_comunicacao,
                   Área = rep("Comunicação",length(distintos_comunicacao)),
                   n = rep(0,length(distintos_comunicacao))))%>%
  bind_rows(tibble(Ano = distintos_cultura,
                   Área = rep("Cultura",length(distintos_cultura)),
                   n = rep(0,length(distintos_cultura))))%>%
  bind_rows(tibble(Ano = distintos_agro,
                   Área = rep("Agropecuária e Agrária",length(distintos_agro)),
                   n = rep(0,length(distintos_agro))))%>%
  bind_rows(tibble(Ano = distintos_defesa_nacional,
                   Área = rep("Defesa Nacional",length(distintos_defesa_nacional)),
                   n = rep(0,length(distintos_defesa_nacional))))%>%
  bind_rows(tibble(Ano = distintos_des_reg,
                   Área = rep("Desenvolvimento Regional",length(distintos_des_reg)),
                   n = rep(0,length(distintos_des_reg))))%>%
  bind_rows(tibble(Ano = distintos_adm_pub,
                   Área = rep("Administração Pública",length(distintos_adm_pub)),
                   n = rep(0,length(distintos_adm_pub))))%>%
  bind_rows(tibble(Ano = distintos_dir_hum,
                   Área = rep("Direitos Humanos",length(distintos_dir_hum)),
                   n = rep(0,length(distintos_dir_hum))))%>%
  bind_rows(tibble(Ano = distintos_indust,
                   Área = rep("Indústria e Comércio",length(distintos_indust)),
                   n = rep(0,length(distintos_indust))))%>%
  bind_rows(tibble(Ano = distintos_infra,
                   Área = rep("Infraestrutura",length(distintos_infra)),
                   n = rep(0,length(distintos_infra))))%>%
  bind_rows(tibble(Ano = distintos_seg_alimentar,
                   Área = rep("Segurança Alimentar",length(distintos_seg_alimentar)),
                   n = rep(0,length(distintos_seg_alimentar))))%>%
  bind_rows(tibble(Ano = distintos_turismo,
                   Área = rep("Turismo, Desporto e Lazer",length(distintos_turismo)),
                   n = rep(0,length(distintos_turismo))))%>%
  bind_rows(tibble(Ano = distintos_relex,
                   Área = rep("Relações Exteriores",length(distintos_relex)),
                   n = rep(0,length(distintos_relex))))%>%
  arrange(Ano)

















############################LINHAS SE SOBREPONDO - NÃO ESTÁ SENDO USADO!
ordem<- seq(1990,2022)
ordem2<- seq(0,9)
contagem_por_área%>%
  filter(Área %in%c("Educação", "Saúde",'Habitação e Urbanismo','Assistência Social'))%>%
ggplot(aes(x=Ano,y=n,group=Área,colour=Área)) +
  geom_line(size=1.5) + geom_point(size=4) +
  scale_colour_manual(name="Área", values = c("#487D5B","#65B1AE",'#FF7676','#FDA172'), 
                      labels = c("Educação", "Saúde",'Habitação e Urbanismo','Assistência Social'))+
  labs(x="Ano", y="Políticas") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")+
  scale_x_discrete(limits=ordem)+
  scale_y_discrete(limits=ordem2)








######################################## WRAP
#PARTE 1 (6)
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
ggsave("politicasporarea_wrap_PT1.png", width = 450, height = 200, units = "mm")




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
ggsave("politicasporarea_wrap_PT2.png", width = 450, height = 200, units = "mm")




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
ggsave("politicasporarea_wrap_PT3.png", width = 450, height = 200, units = "mm")



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
ggsave("politicasporarea_wrap_PT4.png", width = 450, height = 200, units = "mm")

















#############################################criando variável mandato
dados <- dados%>%
  mutate(Mandato = case_when((Data >= '1985-03-15' & Data < '1990-03-15') ~ 'José Sarney',
                             (Data >= '1990-03-15' & Data < '1992-12-29') ~ 'Fernando Collor',
                             (Data >= '1992-12-29' & Data < '1995-01-01') ~ 'Itamar Franco',
                             (Data >= '1995-01-01' & Data < '2003-01-01') ~ 'Fernando Henrique Cardoso',
                             (Data >= '2003-01-01' & Data < '2011-01-01') ~ 'Luiz Inácio Lula da Silva',
                             (Data >= '2011-01-01' & Data < '2016-08-31') ~ 'Dilma Rousseff',
                             (Data >= '2016-08-31' & Data < '2019-01-01') ~ 'Michel Temer',
                             (Data >= '2019-01-01' & Data < '2023-01-01') ~ 'Jair Bolsonaro',
                             TRUE ~ 'outro'))%>%
  mutate(Data = format(Data, format = '%d/%m/%Y'))
  
dados <- dados%>%
  mutate(Mandato = factor(Mandato, levels = c('José Sarney','Fernando Collor','Itamar Franco',
                                              'Fernando Henrique Cardoso','Luiz Inácio Lula da Silva',
                                              'Dilma Rousseff','Michel Temer',
                                              'Jair Bolsonaro')))







##############################################gráficos do mandato - geral - BARRAS
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
ggsave("qnt_mandato_bl.png", width = 180, height = 120, units = "mm")











############################gráficos do mandato - por área - BARRAS
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
ggsave("politicas_por_mandato_p1.png", width = 450, height = 200, units = "mm")



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
ggsave("politicas_por_mandato_p2.png", width = 450, height = 200, units = "mm")















#######################################LEGISLAÇÃO
dados_legislacao <- dados%>%
  drop_na(TIPO)

############## gráficos legislação - geral
contagem_legislacao <- dados_legislacao%>%
  group_by(TIPO)%>%
  summarise(n = n())

ggplot(contagem_legislacao, aes(x = TIPO, y = n)) + 
  geom_col(fill="#808080",color = 'black') +
  geom_text(aes(label = n,vjust = -0.2),fontface = 'bold',size = 3)+
  labs(x="", y = "Quantidade de políticas") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text.x = element_text(colour = "black", size=6),
        axis.text.y = element_text(colour = "black", size=8),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")
ggsave("qnt_legislacao_bl.png", width = 180, height = 120, units = "mm")

########################## gráficos da legislação por area - por área
contagem_leg_area <- dados_legislacao%>%
  group_by(TIPO, Área)%>%
  summarise(n = n())

contagem_leg_area%>%
ggplot(aes(x=TIPO, y=n, fill=Área)) + 
  geom_bar(stat="identity",position='dodge',color = 'black') +
  geom_text(aes(label = n),fontface = 'bold',size = 3.8,position = position_dodge(width = 0.9),vjust = -0.25)+
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
  theme(axis.title.y =element_text(colour="black", size=15),
        axis.title.x = element_text(colour="black", size=15),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour = "black", size=15),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text.x = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.position="top") +
  facet_wrap(vars(TIPO),dir = 'v',ncol = 2, scales = 'free_x',shrink = T,
             strip.position = 'top')
ggsave("politicas_por_legislacao_wrap.png", width = 450, height = 200, units = "mm")



