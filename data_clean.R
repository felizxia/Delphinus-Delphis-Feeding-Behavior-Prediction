library(dplyr)
library(lubridate)
library(kernlab)
library(gam)
library(e1071)
library(randomForest)
require(caret)
setwd("/Users/rita/Documents/data")
dolphin<- read.csv("Behaviour-Table 1.csv",na.string=c("/","","UND","DD/TT","NA"))
#data cleaning
dolphin$T<- as.numeric(dolphin$T)
dolphin$S<- as.numeric(dolphin$S)
dolphin$Se<- as.numeric(dolphin$Se)
dolphin$Fe[dolphin$Fe=="1b"]<-"1"
dolphin$Fe<- as.numeric(dolphin$Fe)
dolphin$J[dolphin$J>=1]<-1
dolphin$J[dolphin$J==0]<-0
dolphin$J<- as.factor(dolphin$J)
Reaction<- dolphin$Reaction
dolphin$Species<- factor(dolphin$Species, labels=c("Delphinus Delphis","Tursiops Truncatus"))
dolphin$Behavior[dolphin$Behavior=="Ss"]<-"S"
dolphin$Behavior[dolphin$Behavior=="Tr"]<-"TR"
# what is 4m in S/Se... means?
dolphin=dolphin %>%  
  select(Date,Time,Species,S,Se,HSB,FB,Fe,T,J,Our.boat.dist,Movement.formation,Reaction,Category) %>%
  na.omit %>%
  filter(!S==0,!Se==0,!T==0,!Movement.formation=="TI")%>%
  mutate(J_A=J,Behavior=Category)%>%
  select(Date,Species,S,Se,HSB,FB,Fe,T,J_A,Our.boat.dist,Reaction,Behavior,Movement.formation)
dolphin
levels(dolphin$Movement.formation)<- c("Alone", "Echelon","Front","Line","Spread","Tight")
levels(dolphin$Behavior)<- c("Feeding","Not Feeding","Not Feeding","Not Feeding","Not Feeding","Not Feeding","Not Feeding","Not Feeding")
#  2 ways of separate train data and test data 
set.seed(1122)
sample<-sample(nrow(dolphin), 256)
train_data<- dolphin[sample, ]
test_data <- dolphin[-sample,]
prop.table(table(train_data$Behavior))
prop.table(table(test_data$Behavior))
set.seed(12345)
sample_test<- dolphin[order(runif(320)),]
train<- sample_test[1:256,]
test<- sample_test[257:320,]
prop.table(table(train$Behavior))
prop.table(table(test$Behavior))
##GLM Model
# Behavior~Reaction(***)
model<- glm(Behavior ~ Species+J_A+Our.boat.dist+Reaction,dolphin,family = quasibinomial)
summary(model)
model_gam<- gam(Behavior ~ Species+J_A+Our.boat.dist+Reaction,dolphin,family = quasibinomial)
summary(model_gam)
# Reaction~ J_A~ Behavior(*)
model1<- glm(Reaction ~ J_A+Our.boat.dist+Species+Behavior,dolphin,family = quasibinomial)
summary(model1)
model1_gam<- gam(Reaction ~ J_A+Our.boat.dist+Species+Behavior,dolphin,family = quasibinomial)
summary(model1_gam)

# SVM model ~ decide which kernel to use
library(kernlab)
require(reshape2)
#accuracy~ 60.94%
svm_test <-ksvm(Behavior ~ J_A+Our.boat.dist+Reaction,data = train,kernel = "rbfdot")
#accuracy~ 83.65%
svm2 <-ksvm(Reaction ~ J_A+Our.boat.dist+Behavior+Movement.formation,data = train_data,kernel = "rbfdot")
svm_predict<- predict(svm2,test_data)
table(svm_predict,test_data$Reaction)
agreement <- svm_predict==test_data$Reaction
prop.table(table(agreement))

# decision tree
library(C50)
library(gmodels)
decision<- C5.0(train_data,train_data$Reaction)
d_test<-predict(decision,test_data,type="class")
CrossTable(test_data$Reaction,d_test,prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,dnn=c('Actual Reaction','Predicted Reaction'))
boost<- C5.0(train,train$Reaction,trials=20)
boost_test<- predict(boost,test)
CrossTable(test$Reaction,boost_test,prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,dnn=c('Actual Reaction','Predicted Reaction'))


# Random Forest
random_model<- randomForest(Reaction ~ J_A+Our.boat.dist+Behavior+Movement.formation,data = train_data)
predict_random<- predict(random_model,test_data,type = "class")
rf_table<-table(test_data$Reaction ,predict_random)
random_forest_table = confusionMatrix(rf_table)
random_forest_table





