library(tidyverse)
library(titanic)

train <- titanic_train
test <- titanic_test
test <- merge(test, titanic_gender_class_model, by="PassengerId")

test

## Explorando os dados

summary(test)
summary(train)
str(test)

## verificando os dados faltantes
colSums(is.na(train))
colSums(is.na(test))

## tratando dados faltatantes
mean(train$Age, na.rm = TRUE)
median(train$Age, na.rm = TRUE)

train$Age = ifelse(is.na(train$Age), median(train$Age, na.rm=T), train$Age)
test$Age = ifelse(is.na(test$Age), median(train$Age, na.rm=T), test$Age)

col_fal <-round(colSums(is.na(train))*100/nrow(train),2)
col_fal

round(colSums(is.na(test))*100/nrow(test),2)

# Removendo as variáveis Cabin, passengerId, Ticket e Name por não serem importantes na modelagem 
train <- subset(train, select = -c(Cabin, PassengerId, Ticket, Name))
test <- subset(test, select = -c(Cabin, PassengerId, Ticket, Name))


# Converter "Survived","Pclass","Sex","Embarked" para fatores
for (i in c("Survived","Pclass","Sex","Embarked")){
  train[,i] <- as.factor(train[,i])
}
for (j in c("Survived","Pclass","Sex","Embarked")){
  test[,j] <- as.factor(test[,j])
}

# Removendo linhas com dados incompletos (caso ainda tenha)
train <- train[complete.cases(train),]

## Criando o modelo

## modelo 1
mod1 <- glm(Survived ~ ., data = train, family = binomial(link = "logit"))
mod1
summary(mod1)

## Apos criarmos o modelo com todas as variaveis independentes, criamos um segundo modelo 
# so com as variaveis de p-valor significantes.Variaveis retiradas do modelo Parch, Fare,
#EmbarkedC,EmbarkedQ,EmbarkedS.

# Modelo 2
mod2 <- glm(Survived ~ Pclass + Sex + Age + SibSp,
            data = train, family = binomial(link = "logit"))
summary(mod2)

# Razão de Chances
library(questionr)
odds.ratio(mod2)

exp(mod2$coefficients)

# Interpretando os resulatados vemos que a classe 1 (1.00) tem uma chance maior que a classe 2 (0.307)
# sobrevivencia de 3,23 (1/0.307), ja a classe 1 tem uma chance em relacao a classe 2 de 10.52632 (1/0.095)maior sebrovida.

## O sexo a chance de uma mulher sobreviver em relacao ao homem e de 15,625 (1/0.0645) maior.

##A medida que vai aumentando a idade a chance de sobreviver vai reduzindo em 0.0388 (1-0.9612).

## Sibsp =  A medida que o numero de conjugues ou irmao aumenta a chance de sobrevide 
# reduz em 0.299.(1-0.701)

### Predicao ###
pred <- predict(mod2, test, type = "response") 
result <- as.factor(ifelse(pred > 0.5,1,0))

### Desempenho do modelo ###

# Matriz de confusão e medidas
library(caret)
confusionMatrix(result, test$Survived, positive = "1")

# Curva ROC e AUC
library(pROC)
auc <- roc(test$Survived, pred)
plot.roc(auc, print.thres = T) # descobrimos o ponto de corte que fornece melhor soma de S e E

# Usando o novo ponto de corte
result2 <- as.factor(ifelse(pred > 0.551,1,0))
confusionMatrix(result2, test$Survived, positive = "1")

