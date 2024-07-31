#Import dataset
Medicalpremium = read.csv("D:/UOC/Z. Academic/3rd year/semester 2/ST 3082/a. Projects/Project 2/Advanced Analysis/Medicalpremium.csv")

attach(Medicalpremium)

#libraries
library(glmnet)
library(mdatools)

#Cleaning data
sum(is.na(Medicalpremium))
sum(duplicated(Medicalpremium))

#Cleaning data
sum(is.na(Medicalpremium))
sum(duplicated(Medicalpremium))

#Recode Variables

Medicalpremium$BMI<-Medicalpremium$Weight/(Medicalpremium$Height/100)^2
Medicalpremium$bmi_category <- cut(Medicalpremium$BMI,
                                   breaks=c(0, 18.5, 25, 30, 40,100),
                                   labels=c('Underweight', 'Normal BMI', 'Overweight', 'Obese','Severely obese'))
Medicalpremium$NumberOfMajorSurgeries1<-ifelse(Medicalpremium$NumberOfMajorSurgeries=="0","0",
                                               ifelse(Medicalpremium$NumberOfMajorSurgeries=="1","1",
                                                      ifelse(Medicalpremium$NumberOfMajorSurgeries=="2","2-3",
                                                             ifelse(Medicalpremium$NumberOfMajorSurgeries=="3","2-3",0))))

#Spliting data for trainin and testing
set.seed(1)
train = sample(1:nrow(Medicalpremium),(nrow(Medicalpremium)*8)/10)
test = (-train)
MedicalpremiumTrain = Medicalpremium[train,]
MedicalpremiumTest = Medicalpremium[test,]

#Advanced Analysis
##For training data
yTrain = MedicalpremiumTrain$PremiumPrice

xTrain = data.frame(MedicalpremiumTrain) #Includes y
dim(xTrain)
names(xTrain)
sapply(xTrain,class)
xTrain = transform(xTrain,Diabetes=as.factor(Diabetes),BloodPressureProblems=as.factor(BloodPressureProblems),
                   AnyTransplants=as.factor(AnyTransplants),AnyChronicDiseases=as.factor(AnyChronicDiseases),
                   KnownAllergies=as.factor(KnownAllergies),HistoryOfCancerInFamily=as.factor(HistoryOfCancerInFamily)
                   ,NumberOfMajorSurgeries1=as.factor(NumberOfMajorSurgeries1),Height = as.numeric(Height), 
                   Weight=as.numeric(Weight)
                   )

##For testing data
###names(MedicalpremiumTest)
#test1 = data.frame(MedicalpremiumTest)
#testx = test1[,-c(6,7,13,14)] #Includes y response
#dim(testx)
#names(testx)
xTest = data.frame(MedicalpremiumTest)
dim(xTest)
names(xTest)
sapply(xTest, class)
xTest = transform(xTest,Diabetes=as.factor(Diabetes),BloodPressureProblems=as.factor(BloodPressureProblems),
                   AnyTransplants=as.factor(AnyTransplants),AnyChronicDiseases=as.factor(AnyChronicDiseases),
                   KnownAllergies=as.factor(KnownAllergies),HistoryOfCancerInFamily=as.factor(HistoryOfCancerInFamily)
                   ,NumberOfMajorSurgeries1=as.factor(NumberOfMajorSurgeries1),Height = as.numeric(Height), 
                  Weight=as.numeric(Weight)
                  )


##Random Forest-------------------------------------------------------------------------------------------------
set.seed(1)
library(randomForest)
require(caTools)
rf.Medicalpremium = randomForest(PremiumPrice~.,data = xTrain, importance = TRUE)

#predicted values
yhat.rf = predict(rf.Medicalpremium,newdata = xTest) 

mean((yhat.rf-(xTest$PremiumPrice))^2) #7816627
importance(rf.Medicalpremium)
varImpPlot(rf.Medicalpremium)

#variable importance plot
library(caret)
library(ggplot2)
vi = varImp(rf.Medicalpremium,scale = FALSE)
ggplot(vi, aes(x = reorder(names(vi), -Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Variable Importance Plot") +
  xlab("Variable") +
  ylab("Importance")

##model accuracy----------------------------------------------------------------------------------------------------------------
#rmse 
library(Metrics)
rmse_rf = rmse(xTest$PremiumPrice ,yhat.rf)
rmse_rf

#mape
mape_rf = mape(xTest$PremiumPrice,yhat.rf)
mape_rf



##Gradient Boosting Machines (including XGBoost)
library(gbm)
set.seed(1)
boost.MedicalPremium = gbm(PremiumPrice~.,data=xTrain, distribution = "gaussian")
summary(boost.MedicalPremium)
yhat.boost =  predict(boost.MedicalPremium,newdata = xTest) 
  
library(Metrics)
rmse_boost = rmse(xTest$PremiumPrice ,yhat.boost)
rmse_boost

#mape
mape_boost = mape(xTest$PremiumPrice,yhat.boost)
mape_boost


###Partial Dependence Plot___________________________________-##########################wrong
dev.new()
par(mfrow = c(2,2))
plot(boost.MedicalPremium, i.var = 1)
plot(boost.MedicalPremium, i.var = 3)
plot(boost.MedicalPremium, i.var = 5)
plot(boost.MedicalPremium, i.var = 4)





plot(boost.MedicalPremium,i="Age")
plot(boost.MedicalPremium,i="AnyTransplants")

