# Carregar pacotes/dependências
library(magrittr)       # CRAN v2.0.2
library(readr)          # CRAN v2.1.2
library(lubridate)      # CRAN v1.8.0
library(dplyr)          # CRAN v1.0.8
library(purrr)          # CRAN v0.3.4
library(GetBCBData)     # CRAN v0.6
library(ipeadatar)      # CRAN v0.1.6
library(sidrar)         # CRAN v0.2.7
library(gtrendsR)       # CRAN v1.5.0
library(rbcb)           # CRAN v0.1.8
library(tidyr)          # CRAN v1.2.0
library(tsibble)        # CRAN v1.1.1
library(stringr)        # CRAN v1.4.0
library(forecast)       # CRAN v8.16
library(timetk)         # CRAN v2.7.0
library(caret)          # CRAN v6.0-91
library(glmnet)         # CRAN v4.1-4
library(plotly)         # CRAN v4.10.0
library(ggplot2)        # CRAN v3.3.5
library(feasts)         # CRAN v0.2.2
library(fabletools)     # CRAN v0.3.2
library(scales)         # CRAN v1.1.1
library(ggtext)         # CRAN v0.1.1
# Testado nas versões R: 3.6.3 / 4.0.5 / 4.1.2 / 4.1.3 e 4.2.0
library(HDeconometrics) # [github::gabrielrvsc/HDeconometrics] v0.1.0


##


##eSSE COMANDO RETIRA A NOTACAO CIENTIFICA
options(scipen = 999)
library(tsibble)
library(tsibbledata)
vic_elec <-gafa_stock
pelt <- PBS

#Use a fun��o de ajuda para explorar o que as s�ries gafa_stock, PBSe representam.vic_elec pelt
#Use autoplot()para plotar algumas das s�ries nesses conjuntos de dados.
vic_elec %>%
  autoplot()+
  labs(title = 'Ativos',y='Pre�o',x='Data')

pelt %>%
  ggplot(aes(x=Month,y=Cost))+
  geom_line(stat = 'identity')


  
    

##Use filter()para descobrir quais dias corresponderam ao pre�o m�ximo de fechamento de cada uma das quatro a��es em gafa_stock.

# Cruzamento de dados
vic_elec$Adj_Close <-as.numeric(vic_elec$Adj_Close)

preco_acao <- vic_elec%>% tidyr::pivot_wider(
  id_cols     = "Date",
  names_from  = 'Symbol',
  values_from = "Adj_Close"
)

gafa_stock %>%
  as_tibble %>%
  group_by(Symbol) %>%
  summarise(maximum = max(Close), .groups = 'drop')

gafa_stock %>%
  group_by(Symbol) %>% 
  filter(Close == max(Close))


#3.Baixe o arquivo tute1.csvdo site do livro , abra-o no Excel (ou algum outro aplicativo de planilha) e revise seu conte�do. Voc� deve encontrar quatro colunas de informa��es. 
#As colunas B a D cont�m, cada uma, uma s�rie trimestral, rotulada como Sales, AdBudget e GDP. Sales cont�m as vendas trimestrais de uma pequena empresa no per�odo de 1981 a 2005. 
#AdBudget � o or�amento de publicidade e PIB � o produto interno bruto. Todas as s�ries foram corrigidas pela infla��o.

#Voc� pode ler os dados em R com o seguinte script:
tute1 <- readr::read_csv("Livros/Previs�o Princ�pios e Pr�tica (3� ed)/tute1.csv")
View(tute1)

#Converter os dados em s�ries temporais
mytimeseries <- tute1 %>%
  dplyr::mutate(Quarter = yearquarter(Quarter)) %>%
  as_tsibble(index = Quarter)

#Construir gr�ficos de s�ries temporais de cada uma das tr�s s�ries
mytimeseries %>%
  pivot_longer(-Quarter) %>%
  ggplot(aes(x = Quarter, y = value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y")

##O USgaspacote cont�m dados sobre a demanda de g�s natural nos EUA.

#Instale o USgas pacote.

install.packages('Usgas')

library(USgas)

energia <-USgas::us_total
USgas::us_monthly
USgas::us_residential

##Crie um tsibble de us_totalcom ano como �ndice e estado como chave.

energia$year <-format(energia$year,format="%Y") 

energia <-energia %>%
  as_tsibble(index=year,key = state) 

#Trace o consumo anual de g�s natural por estado para a �rea da Nova Inglaterra (compreendendo os estados de Maine, Vermont, New Hampshire, Massachusetts, Connecticut e Rhode Island).

total_ano <-energia %>% 
  filter(state == c("Maine","Vermont","New Hampshire","Massachusetts","Connecticut","Rhode Island"))%>%
  select(year,y)%>%
  group_by(year)%>%
  summarise(total = sum(y))

##Voc� precisa incluir stat='identity', que basicamente informa ao ggplot2 que voc� fornecer� os valores y para o barplot, em vez de contar o n�mero agregado de linhas para cada valor x,
#que � o padr�ostat=count

ggplot(total_ano,aes(x=year,y=total))+
geom_bar(stat = 'identity')

#5 Fa�a o download no site tourism.xlsxdo livro e leia-o em R usando

turismo<-readxl::read_excel("Livros/Previs�o Princ�pios e Pr�tica (3� ed)/tourism.xlsx")

#Crie um tsibble id�ntico ao tourism tsibble do tsibble pacote

turismo_tsi <-tsibble::tourism


#Descubra qual combina��o de Regione Purpose teve o n�mero m�ximo de viagens noturnas em m�dia.

turismo%>%
  group_by(Region,Purpose)%>%
  summarise(media = mean(Trips))%>%
  arrange(desc(media))%>%
  head(20)%>%
  ##Reorder ordena as colunas pelas maiores
  ##Fill usa a coluna pupose como paleta de cores
  ggplot(aes(x=reorder(Region,-media),y=media,fill=Purpose))+
  geom_bar(stat = 'identity') +
  ggplot2::labs(
    title = 'Combinacao de regiao e purpose com a media de maiores viagens',
    x= 'Regiao',
    y='Media',
    caption = 'Dados do site do turismo'
  )

#Crie um novo tsibble que combine as Finalidades e Regi�es, e tenha apenas o total de viagens por Estado.

turismo %>%
  select(Purpose,Region,Trips)%>%
  group_by(Region)%>%
  summarise(Total = sum(Trips))%>%
  arrange(desc(Total))%>%
  head(10)%>%
  ggplot(aes(x=reorder(Region,-Total),y=Total))+
  geom_bar(stat='identity', fill="steelblue")+
  theme_gray()
  
  
#Crie um novo tsibble que combine as Finalidades e Regi�es, e tenha apenas o total de viagens por Estado.


#6Crie gr�ficos de tempo das quatro s�ries temporais a seguir: Bricksde aus_production, Lynxde pelt, Closede gafa_stock, Demandde vic_elec.

aus_production%>%
  ggplot(aes(x=Quarter,y=Bricks))+
  geom_line(na.rm = TRUE,size=2,color='blue')


gafa_stock%>%
  ggplot(aes(x=Date,y=Close,color=Symbol))+
  geom_line(size=1)

#7O aus_arrivalsconjunto de dados inclui chegadas internacionais trimestrais � Austr�lia do Jap�o, Nova Zel�ndia, Reino Unido e Estados Unidos.

#Use autoplot(), gg_season()e gg_subseries()para comparar as diferen�as entre as chegadas desses quatro pa�ses.

library(fpp3)

aus_arrivals%>%
  autoplot(size=1)

aus_arrivals%>%
  gg_season()
##
options(scipen = 999)
aus_arrivals%>%
  gg_subseries()

#8 Os dados mensais do varejo australiano s�o fornecidos em formato aus_retail. Selecione uma das s�ries temporais da seguinte forma (mas escolha seu pr�prio valor inicial):

set.seed(12345678)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))
  
myseries%>%
  autoplot(size=1,color='green')

myseries%>%
  gg_season()

myseries%>%
  gg_subseries()

myseries%>%
  ACF(Turnover) %>% autoplot()


dgoog <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2018) %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE) %>%
  mutate(diff = difference(Close))


