#pacotes/dependÃªncias
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
# Testado nas versÃµes R: 3.6.3 / 4.0.5 / 4.1.2 / 4.1.3 e 4.2.0
library(HDeconometrics) # [github::gabrielrvsc/HDeconometrics] v0.1.0
library(purrr)
library(readr)
library(latex2exp)
library(ggplot2)
##


##eSSE COMANDO RETIRA A NOTACAO CIENTIFICA
options(scipen = 999)
library(tsibble)
library(tsibbledata)


##Considere as informações do PIB em global_economy. Faça um gráfico do PIB per capita para cada país ao longo do tempo. 
#Qual país tem o maior PIB per capita? Como isso mudou ao longo do tempo?

unique(global_economy$Country)

brazil_pib <- global_economy %>%
  filter(Country == c('Brazil','Monaco','Argentina','Australia','Euro area','OECD members'))%>%
  mutate(Pib_capition = GDP/Population)

ggplot(brazil_pib,aes(x=Year,y=Pib_capition,fill=Country,color=Country))+
  geom_line(size=2,na.rm = TRUE)

#3Para cada uma das seguintes séries, faça um gráfico dos dados. Se a transformação parecer apropriada, faça-a e descreva o efeito.
#PIB dos Estados Unidos de global_economy.

global_economy %>%
  filter(Country == 'United States')%>%
  ggplot(aes(x=Year,y=GDP))+
  geom_line(size=2,color='blue')+
  ggplot2::labs(
    title = 'Pib dos Estados Unidos',
    x= 'PIB',
    y='ANO',
    caption = 'Conjunto de dados Global Economy'
  )


### ajsutar pela inflcao

global_economy%>%
  dplyr::filter(Country == 'United States')%>%
  mutate(valores_ajustado = GDP/CPI*100)%>%
  ggplot()+
  geom_line(aes(x=Year,y=GDP,color='green'))+
  geom_line(aes(x=Year,y=valores_ajustado,color='red'))+
  guides()

  
  
  

#Abate de "Touros, novilhos e novilhos" vitorianos em aus_livestock.

aus_livestock%>%
  ggplot(aes(x=Month,y=Count))+
  geom_line(size=2,color='red')

#Demanda de eletricidade vitoriana de vic_elec.

vic_elec%>%
  ggplot(aes(x=Time,y=Demand))+
  geom_line()

aus_production%>%
  ggplot(aes(x=Quarter,y=Electricity))+
  geom_line()

## gerando uma media de 5 dias para a eletricidade
aus_exports <- aus_production %>%
  mutate(
    '5-MA' = slider::slide_dbl(Electricity, mean,
                               .before = 2, .after = 2, .complete = TRUE)
  )

aus_exports %>%
  autoplot(Electricity,size=1) +
  geom_line(aes(y = `5-MA`),na.rm = TRUE, colour = "#D55E00",size=1) +
  labs(y = "Eletrecidade",
       title = "Consumo de energia e media movel de 5 periodos") +
  guides(colour = guide_legend(title = "series"))


##Produção de gás de aus_production.

aus_production %>%
  ggplot(aes(x=Quarter,y=Gas))+
  geom_line()


#Por que uma transformação Box-Cox é inútil para os canadian_gasdados

fpp3::canadian_gas
library(latex2exp)
library(ggplot2)


lambda <- fpp3::canadian_gas %>%
  features(Volume, features = guerrero) %>%
  pull(lambda_guerrero)
fpp3::canadian_gas %>%
  autoplot(box_cox(Volume, lambda)) +
  labs(y = "",
       title = latex2exp::TeX(paste0(
         "Transformed gas production with $\\lambda$ = ",
         round(lambda,2))))
#Qual transformação Box-Cox você selecionaria para seus dados de varejo (do Exercício 8 na Seção 2.10 )?

myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))

lambda <- myseries %>%
  features(Turnover, features = guerrero) %>%
  pull(lambda_guerrero)
myseries %>%
  autoplot(box_cox(Turnover, lambda)) +
  labs(y = "",
       title = latex2exp::TeX(paste0(
         "Transformed gas production with $\\lambda$ = ",
         round(lambda,2))))

#10 Este exercício usa os canadian_gasdados (produção mensal de gás canadense em bilhões de metros cúbicos, janeiro de 1960 a fevereiro de 2005).

#a)Plote os dados usando autoplot(), gg_subseries()e gg_season()observe o efeito da mudança sazonal ao longo do tempo

fpp3::canadian_gas%>%
  autoplot()
fpp3::canadian_gas%>%
  gg_subseries()
fpp3::canadian_gas%>%
  gg_season()

#b)Faça uma decomposição STL dos dados. Você precisará escolher uma janela sazonal para permitir a alteração da forma do componente sazonal.
fpp3::canadian_gas %>%
  model(
    STL(Volume ~ trend(window = 10) +
          season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()

#c)Como a forma sazonal muda ao longo do tempo? [Dica: tente plotar o componente sazonal usando gg_season().]

fpp3::canadian_gas %>%
  model(
    STL(Volume ~ trend(window = 10) +
          season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  select(season_year)%>%
  gg_season()

#d)Compare os resultados com os obtidos usando SEATS e X-11. Como eles são diferentes?

x11_dcmp <- fpp3::canadian_gas %>%
  model(x11 = X_13ARIMA_SEATS(Volume ~ x11())) %>%
  components()
autoplot(x11_dcmp) +
  labs(title =
         "Decomposition do total de consumo de gas using X-11.")

x11_dcmp %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Volume, colour = "Data")) +
  geom_line(aes(y = season_adjust,
                colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail") +
  scale_colour_manual(
    values = c("gray", "#0072B2", "#D55E00"),
    breaks = c("Data", "Seasonally Adjusted", "Trend")
  )

fpp3::canadian_gas %>%
  model(x11 = X_13ARIMA_SEATS(Volume ~ x11())) %>%
  components()%>%
  select(seasonal)%>%
  gg_season()


#sets

seats_dcmp <- fpp3::canadian_gas %>%
  model(seats = X_13ARIMA_SEATS(Volume ~ seats())) %>%
  components()
autoplot(seats_dcmp) +
  labs(title =
         "Decomposition of total US retail employment using SEATS")

seats_dcmp %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Volume, colour = "Data")) +
  geom_line(aes(y = season_adjust,
                colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail") +
  scale_colour_manual(
    values = c("gray", "#0072B2", "#D55E00"),
    breaks = c("Data", "Seasonally Adjusted", "Trend")
  )



fpp3::canadian_gas %>%
  model(x11 = X_13ARIMA_SEATS(Volume ~ seats())) %>%
  components()%>%
  select(seasonal)%>%
  gg_season()






