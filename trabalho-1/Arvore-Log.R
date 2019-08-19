

install.packages("caret") #alguns plots
install.packages("xtable") #Matrix de Confusão
install.packages("C50") #Matrix de Confusão
install.packages("rpart") #Matrix de Confusão
install.packages("e1071") #Matrix de Confusão

library(rpart)#Função RPart (CART)
library(C50) # C5.0
library(e1071)#Função RPart (CART)



library(caret) #alguns plots
library(xtable) #Matrix de Confusão

iris_tree <-rpart(Species ~ ., data=iris, method='class')
printcp(iris_tree)
plotcp(iris_tree)
summary(iris_tree)


plot(iris_tree, uniform=TRUE, main="Classificação das Iris")
text(iris_tree,use.n=TRUE, all= TRUE, cex=.75)
table(predict(iris_tree, iris, type ="class"), iris[ , "Species"])

set.seed(17)
myiris <- iris

random <- sample(1:nrow(iris), 0.8 * nrow(iris))
iris_train <- myiris[random, ]
iris_test <- myiris[-random, ]

iris_tree <-rpart(Species ~ ., data=iris_train, method='class')
printcp(iris_tree)
plotcp(iris_tree)
summary(iris_tree)

irisprev_tree_test<-predict(iris_tree, iris_test)

plot(iris_tree, uniform=TRUE, main="Classificação das Iris")
text(iris_tree,use.n=TRUE, all= TRUE, cex=.75)



iris_tree2 <-C5.0(Species ~ ., data=iris_train)
summary(iris_tree2)
plot(iris_tree2,main="Método C 5.0")
irisprev_tree2<-predict(iris_tree2, iris_test)
confusion <- confusionMatrix(iris_test$Species,irisprev_tree2)

irisprev_tree2_train<-predict(iris_tree2, iris_train)
confusion_train <- confusionMatrix(iris_train$Species,irisprev_tree2_train)

table(predict(iris_tree, iris_train, type ="class"), iris_train[ , "Species"])

table(predict(iris_tree, iris_test, type ="class"), iris_test[ , "Species"])

iris_tree2 <-C5.0(Species ~ ., data=iris_train)
summary(iris_tree2)
plot(iris_tree2,main="Método C 5.0")
prev_tree<-predict(iris_tree2, iris_test)
confusion <- confusionMatrix(iris_test$Species,prev_tree)
 

prev_tree2<-predict(iris_tree2, iris_train)
confusion2 <- confusionMatrix(iris_train$Species,prev_tree2)

library(OneR)
data(breastcancer)
data <- breastcancer

set.seed(12) # Pode ser outra semente - 12 é para dar o mesmo resultado
random <- sample(1:nrow(data), 0.8 * nrow(data))
data_train <- data[random, ]

#conjunto de teste
data_test <- data[-random, ]

#Treinar e obter modelo usando o conjunto de Treino
cancer_tree<-rpart(Class ~ ., data=data_train, method='class')
printcp(cancer_tree)
plotcp(cancer_tree)
summary(cancer_tree)
plot(cancer_tree, uniform=TRUE, main="Classificação do Cancer de Mama")
text(cancer_tree,use.n=TRUE, all= TRUE, cex=.75)

#Previsões
tree_part_train<-predict(cancer_tree, data_train, type ="class")
confusion_part_train<-confusionMatrix(data_train$Class, tree_part_train)

tree_part_test<-predict(cancer_tree, data_test, type ="class")
confusion_part_test<-confusionMatrix(data_test$Class, tree_part_test)



cancer_tree2 <-C5.0(Class ~ ., data=data_train)
summary(cancer_tree2)
plot(cancer_tree2,main="Método C 5.0")
cancerprev_tree2<-predict(cancer_tree2, data_test)
confusion <- confusionMatrix(data_test$Class,cancerprev_tree2)

cancerprev_tree_train<-predict(cancer_tree2, data_train)
confusion_train <- confusionMatrix(data_train$Class,cancerprev_tree_train)

confusion
confusion_train
prev_tree2<-predict(iris_tree2, iris_train)
confusion2 <- confusionMatrix(iris_train$Species,prev_tree2)




#Titanic
data("Titanic")

Titanic_df=as.data.frame(Titanic)

repeating_sequence=rep.int(seq_len(nrow(Titanic_df)), Titanic_df$Freq) #Repete cada combinação em acordo com a frequência dos dados originais
 
Titanic_dataset=Titanic_df[repeating_sequence,]
#Não precisamos da frequencia
Titanic_dataset$Freq=NULL

Titanic_tree<-rpart(Survived ~ ., data=Titanic_dataset, method='class')

printcp(Titanic_tree)
plotcp(Titanic_tree)
summary(Titanic_tree)
plot(Titanic_tree, uniform=TRUE, main="Classificação - Sobreviventes do Titanic")
text(Titanic_tree,use.n=TRUE, all= TRUE, cex=.75)

table(predict(Titanic_tree, Titanic_dataset, type ="class"), Titanic_dataset[ , "Survived"])


Titanic_tree2 <-C5.0(Survived ~ ., data=Titanic_dataset)
summary(Titanic_tree2)
plot(Titanic_tree2,main="Método C 5.0")
Titanicprev_tree2<-predict(Titanic_tree2, Titanic_dataset)
confusion <- confusionMatrix(Titanic_dataset$Survived,Titanicprev_tree2)


#################Regressão Logit

Titanic_logit<-glm(Survived ~ ., data=Titanic_dataset, family='binomial')
summary(Titanic_logit)
Titanic_pred<-predict(Titanic_logit, Titanic_dataset, type="response")

table(Titanic_dataset$Survived, Titanic_pred > 0.5 )

Titanic_step<-step(Titanic_logit)
summary(Titanic_step)
Titanic_s_pred<-predict(Titanic_step, Titanic_dataset, type="response")
table(Titanic_dataset$Survived, Titanic_s_pred > 0.5 )

cancer_log<-glm(Class ~ ., data=data_train, family= 'binomial')
summary(cancer_log)
cancer_pred<-predict(cancer_log, data_test, type="response")

table(data_test$Class, cancer_pred <= 0.5 )

cancer_s_log<-step(cancer_log)
summary(cancer_s_log)
cancer_s_pred<-predict(cancer_s_log, data_test, type="response")

table(data_test$Class, cancer_s_pred <= 0.5 )

