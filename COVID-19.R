# Casos de covid 19
# fonte de dados: https://covid.saude.gov.br/
# Configurando o diretório de trabalho

setwd("D:/ArquivosPessoais/OneDrive/Drive/FCD/R/GraficoCovid")
getwd()

#Lendo arquivo de dados formato.csv 
dados<- read.csv("HIST_PAINEL_COVIDBR_06jul2020.CSV",sep = ';')

#vendo os tipos de dados
str(dados)
#View(dados)

library(dplyr)
#Novo dataset das cidades do estudo,somente para fins didáticos
blumenau<- filter(dados, municipio %in% c("Blumenau", "Joinville", "Santa Maria", "Santa Cruz do Sul") )
str(blumenau)
#View(blumenau)


library(stringr)
library(lubridate)
#install.packages("plotly")
library(plotly)

#data set com as cidades interessadas dos ultimos 60 dias
novo<- dados %>%
  select(municipio, estado, data, obitosAcumulado, casosAcumulado, casosNovos) %>%
  #filter(municipio %in% c("Blumenau", "Joinville", "Itajaí", "Indaial", "Timbo", "Pomerode", "Santa Maria"),
  filter(municipio %in% c("Blumenau", "Joinville", "Itajaí"),
         estado %in% c("RS", "SC"), 
         dmy(dados$data) > Sys.Date()-60 )  %>%
  arrange(as.Date(data), municipio) 


#View(novo)
library(scales)

#Grafico CASOS CONFIRMADOS
g1<-ggplot(data = novo, aes(x=as.Date(data, format = "%d/%m/%y"), y=casosAcumulado, color=municipio, shape=municipio,  group=municipio))  + 
  geom_line()+
  geom_point(size=1) +
  labs(x="Data", y="Casos") +
  ggtitle("Confirmados") + 
  scale_x_date(breaks = date_breaks("1 weeks"), labels = date_format("%d/%m/%y"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "top")

#gGrafico de ÓBITOS
g2<-ggplot(data = novo, aes(x=as.Date(data, format = "%d/%m/%y"), y=obitosAcumulado, color=municipio, shape=municipio,  group=municipio)) + 
  geom_line()+
  geom_point(size=1) +
  labs(x="Data", y="") +
  ggtitle("Óbitos") +
  scale_x_date(breaks = date_breaks("1 weeks"), labels = date_format("%d/%m/%y"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "top" )

#casos novos
g3<-ggplot(data = novo, aes(x=as.Date(data, format = "%d/%m/%y"), y=casosNovos, color=municipio, shape=municipio,  group=municipio)) + 
  geom_line()+
  geom_point(size=1) +
  labs(x="Data", y="") +
  ggtitle("Novos casos") +
  scale_x_date(breaks = date_breaks("1 weeks"), labels = date_format("%d/%m/%y"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "top" )


require(gridExtra)
library(grid)

#Cabeçalho do grafico
tg <- textGrob("Covid-19 Blumenau e cidades próximas nos últimos 60 dias", gp=gpar(fontsize=16))
sg <- textGrob(paste("Fonte: https://covid.saude.gov.br/ acesso em:",format(today(), "%d/%m/%y")), gp=gpar(fontsize=15, fontface=3L))

margin <- unit(0.0, "line")

#Criando pagina dos graficos
grid.newpage()
grid.arrange(tg, sg, rectGrob(), 
             heights = unit.c(grobHeight(tg) + margin, 
                              grobHeight(sg) + margin, 
                              unit(1,"null")))

grid.arrange(g1, arrangeGrob(g2,g3, ncol=2), heights=c(2.5/4, 1.5/4), ncol=1, top=tg, bottom=sg)

