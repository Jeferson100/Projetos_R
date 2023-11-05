
#install.packages("GGally")
library(GGally)
#install.packages("corrplot")
library(corrplot)
#install.packages("arm")
library(arm)

library(readxl)
dados <- read_excel("~/POS GRADUAÇÃO, CIENCIA DE DADOS/Análise Multivariada de Dados/Exerc_A_fatorial.xlsx")
View(dados)

#NUMERO DE LINHAS
nrow(dados)

# verificar tipo de dados
str(dados)

# dados faltante

sum(is.na(dados))

summary(dados)

# porcentagem de dados faltantes por coluna
col_fal <-round(colSums(is.na(dados))*100/nrow(dados),2)
col_fal
# FILTRANDO AS COLUNAS QUE TEM DADOS FALTANTES
col_fal[col_fal>0]


# tratando dados faltantess
dados$A1 = ifelse(is.na(dados$A1), mean(dados$A1, na.rm = TRUE), dados$A1)
dados$A2 = ifelse(is.na(dados$A2), mean(dados$A2, na.rm = TRUE), dados$A2)
dados$A3 = ifelse(is.na(dados$A3), mean(dados$A3, na.rm = TRUE), dados$A3)
dados$A4 = ifelse(is.na(dados$A4), mean(dados$A4, na.rm = TRUE), dados$A4)
dados$A5 = ifelse(is.na(dados$A5), mean(dados$A5, na.rm = TRUE), dados$A5)
dados$C1 = ifelse(is.na(dados$C1), mean(dados$C1, na.rm = TRUE), dados$C1)
dados$C2 = ifelse(is.na(dados$C2), mean(dados$C2, na.rm = TRUE), dados$C2)
dados$C3 = ifelse(is.na(dados$C3), mean(dados$C3, na.rm = TRUE), dados$C3)
dados$C4 = ifelse(is.na(dados$C4), mean(dados$C4, na.rm = TRUE), dados$C4)
dados$C5 = ifelse(is.na(dados$C5), mean(dados$C5, na.rm = TRUE), dados$C5)
dados$E1 = ifelse(is.na(dados$E1), mean(dados$E1, na.rm = TRUE), dados$E1)
dados$E2 = ifelse(is.na(dados$E2), mean(dados$E2, na.rm = TRUE), dados$E2)
dados$E3 = ifelse(is.na(dados$E3), mean(dados$E3, na.rm = TRUE), dados$E3)
dados$E4 = ifelse(is.na(dados$E4), mean(dados$E4, na.rm = TRUE), dados$E4)
dados$E5 = ifelse(is.na(dados$E5), mean(dados$E5, na.rm = TRUE), dados$E5)
dados$N1 = ifelse(is.na(dados$N1), mean(dados$N1, na.rm = TRUE), dados$N1)
dados$N2 = ifelse(is.na(dados$N2), mean(dados$N2, na.rm = TRUE), dados$N2)
dados$N3 = ifelse(is.na(dados$N3), mean(dados$N3, na.rm = TRUE), dados$N3)
dados$N4 = ifelse(is.na(dados$N4), mean(dados$N4, na.rm = TRUE), dados$N4)
dados$N5 = ifelse(is.na(dados$N5), mean(dados$N5, na.rm = TRUE), dados$N5)
dados$O1 = ifelse(is.na(dados$O1), mean(dados$O1, na.rm = TRUE), dados$O1)
dados$O3 = ifelse(is.na(dados$O3), mean(dados$O3, na.rm = TRUE), dados$O3)
dados$O4 = ifelse(is.na(dados$O4), mean(dados$O4, na.rm = TRUE), dados$O4)
dados$O5 = ifelse(is.na(dados$O5), mean(dados$O5, na.rm = TRUE), dados$O5)
dados$education = ifelse(is.na(dados$education), mean(dados$education, na.rm = TRUE), dados$education)

sum(is.na(dados))

col_fal <-round(colSums(is.na(dados))*100/nrow(dados),2)
col_fal

## ANALISE FATORIAL:

#É possível que as variações de três ou quatro variáveis observadas possam ser explicadas por
#somente um fator , o que evidencia a utilidade da análise fatorial para descrever um conjunto de dados
#utilizando para isso apenas alguns fatores.



## VERIFICANDO A CORRELACAO: O PRIMEIRO PASSO DA ANALISE FATORIAL E VERIFICAR SE EXISTE 
# CORRELÇÂÕ ENTRE AS VARIAVEIS.

corr<- cor(dados)
round(cor(dados),2)
corrplot(corr ,method = "number",is.corr=FALSE)

## TESTE DE BARTÇETT:
## O teste de esfericidade de Bartlett serve para testar a correlação entre as variaveis.
#A hipótese nula é de que as variáveis não sejam correlacionadas na população.

install.packages("psych")
require(psych)

cortest.bartlett(dados)
# Com o p-valor menor que 0,05 a hipotese nula é rejeitada, indicando que ha correlacao entre as
# variaveis.


##Medida de adequacidade da amostra de Kaiser Meyer Olkin (KMO)

#é o índice usado para avaliar a adequacidade da análise fatorial. Valores altos (entre 0,5 e 1,0) indicam que a análise
#fatorial é apropriada Valores abaixo de 0,5 indicam que a análise fatorial pode ser inadequada.

KMO(dados)

## COMO O KMO FOI MAIOR QUE 0,5 PODEMOS CONTINUAR COM A ANALISE FATORIAL.

##ANÁLISE DE COMPONENTES PRINCIPAIS
#Na análise de componentes principais, o objetivo da extração de fatores é 
#encontrar um conjunto de fatores que formem uma combinação linear das variáveis 
#originais ou da matriz de correlações. Assim, se as variáveis X1 , X2 , X3 , . , Xn 
#são altamente correlacionadas entre si, elas serão combinadas para formar um fator, 
#e assim, sucessivamente, com todas as demais variáveis da matriz de correlação.

fit<- princomp(dados,cor=TRUE)

summary(fit)

## O componente 1 é o que mais tem influencia na variaçia total, representando 18%.

#Determinacao do numero de fatores

## Para a seleção dos numeros de fatores que serão usado, deve-se verificar no grafico
# onde acontece uma suavização dos dados.

screeplot(fit)
plot(fit,type="lines")


# A um grande gap entre o primeiro componente e o segundo, entre o segundo e o terceiro componente tambem a um gap.
#Ocorre uma suavizacao entre o terceiro e o quinto, outro gap entre o quinto e o sexto componente e 
# uma suavização entre o sexto componente e o decimo componente.



pca_dados <- principal(dados,nfactors = 6,n.obs =  nrow(dados),rotate = "none",scores = TRUE )
pca_dados


#Matriz Rotada do Fator

pca_varimax <-principal(dados,nfactors = 6,n.obs =  nrow(dados),rotate = "varimax",scores = TRUE )
pca_varimax


biplot(pca_varimax)
fa.diagram(pca_varimax)
# So com seis fatores todas as 28 variaveis recebem a classificão de um fator. Entre 
# 2 a 5 fatores algumas variaveis ficam isoladas sem receber nenhuma classificação de fator.


# FONTE:https://smolski.github.io/livroavancado/analisf.html
