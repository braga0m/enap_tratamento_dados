#pacotes
if (!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse','readxl','data.table',
               'writexl','lubridate')

#dados
dados <- read_xlsx('C:\\Users\\Matias\\Documents\\Projetos_e_Estudos\\ENAP\\Dados\\LISTA_POLITICAS_20_04_2023.xlsx')
glimpse(dados)


#gráfico de barras por décadas
data_decadas <- dados%>%
  mutate(Data = as.Date(Data,format = '%d/%m/%Y'))%>%
  mutate(Décadas = floor(year(Data)/10)*10)%>%
  group_by(Décadas)%>%
  summarise('Número de Adoções' = n())

data_decadas%>%
  ggplot(aes(x = Décadas, y = `Número de Adoções`))+
  geom_col(fill="#808080",color = 'black')+
  geom_text(aes(label = `Número de Adoções` ,vjust = -0.2),fontface = 'bold',size = 3)+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text.x = element_text(colour = "black", size = 12),
        axis.text.y = element_text(colour = "black", size = 8),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")
ggsave("figura_1_artigo.png", width = 300, height = 120, units = "mm")


#gráfico de barras por ano
n_anos <- read_xlsx('C:\\Users\\Matias\\Documents\\Projetos_e_Estudos\\
                    ENAP\\enap_tratamento_dados\\contagem_de_politicas_por_ano.xlsx')

n_anos%>%
  ggplot(aes(x = Ano, y = n))+
  geom_col(fill="#808080",color = 'black')+
  theme_bw() +
  labs(y = 'Número de Políticas Instituídas', x = 'Décadas')+
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text.x = element_text(colour = "black", size = 12),
        axis.text.y = element_text(colour = "black", size = 8),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")
ggsave("figura_1_1_artigo.png", width = 300, height = 120, units = "mm")