#Import dataset
Medicalpremium <- read.csv("D:/UOC/Z. Academic/3rd year/semester 2/ST 3082/a. Projects/Project 2/Advanced Analysis/Medicalpremium.csv")

attach(Medicalpremium)

#libraries
library(glmnet)
library(mdatools)

#Summary of the data set
summary(Medicalpremium)

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
y = yTrain
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

x = data.matrix(xTrain[-11])

##For testing data

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

##Ridge Regression--------------------------------------------------------------------------------
set.seed(1)
ridge_model = glmnet(x,y, alpha = 0)
cv_model1 = cv.glmnet(x,y,alpha =0)
plot(cv_model1)
best_lambda1 = cv_model1$lambda.min
best_model1 = glmnet(x,y,alpha = 0,lambda = best_lambda1)
coef(best_model1)
ridge_predict = predict(ridge_model,s=best_lambda1,newx = data.matrix(xTest[-11]))

#accuracy

MSE_Ridge = sum((ridge_predict-MedicalpremiumTest$PremiumPrice)^2)
MSE_Ridge #2825107538

#rmse (scale dependent)
library(Metrics)
rmse_ridge = rmse(MedicalpremiumTest$PremiumPrice,ridge_predict)
rmse_ridge

#mape (scale independent)
mape_ridge = mape(MedicalpremiumTest$PremiumPrice,ridge_predict)
mape_ridge




##Lasso Regression------------------------------------------------------------------------
set.seed(1)
lasso_model = glmnet(x,y, alpha = 1)
cv_model2 = cv.glmnet(x,y,alpha =1)
plot(cv_model2)
best_lambda2 = cv_model2$lambda.min
best_model2 = glmnet(x,y,alpha = 1,lambda = best_lambda2)
coef(best_model2)
lasso_predict = predict(lasso_model,s=best_lambda2,newx = data.matrix(xTest[-11]))

#accuracy

MSE_lasso = sum((lasso_predict-MedicalpremiumTest$PremiumPrice)^2)
MSE_lasso #2759293901

#rmse (scale dependent)
library(Metrics)
rmse_lasso = rmse(MedicalpremiumTest$PremiumPrice,lasso_predict)
rmse_lasso

#mape (scale independent)
mape_lasso = mape(MedicalpremiumTest$PremiumPrice,lasso_predict)
mape_lasso




##Elastic Net---------------------------------------------------------------------------
set.seed(1)
elastic_model = glmnet(x,y, alpha = 0.5)

cv_model3 = cv.glmnet(x,y,alpha =0.5)
plot(cv_model3)
best_lambda3 = cv_model3$lambda.min
best_model3 = glmnet(x,y,alpha = 0.5,lambda = best_lambda3)
coef(best_model3)
elastic_predict = predict(elastic_model,s=best_lambda3,newx = data.matrix(xTest[-11]))

#accuracy

MSE_elastic = sum((elastic_predict-MedicalpremiumTest$PremiumPrice)^2)
MSE_elastic #2759707882

#rmse (scale dependent)
library(Metrics)
rmse_elastic = rmse(MedicalpremiumTest$PremiumPrice,elastic_predict)
rmse_elastic

#mape (scale independent)
mape_elastic = mape(MedicalpremiumTest$PremiumPrice,elastic_predict)
mape_elastic













##NOT SURE ABOVE BELOW ANALYSIS##


##Cluster Analysis
library(cluster)
###Hierachial Clustering
xscale = scale(x)
d = dist(xscale,method="euclidean")
clust = hclust(d,method = "ward.D")
plot(clust,cex=0.7)

###kmeans clustering
kmeans(xscale,2)

