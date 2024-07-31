library(mdatools)
library(ggplot2)
Medicalpremium <- read.csv("D:\\UOC1\\3\\2nd sem\\ST 3082\\project\\archive\\Medicalpremium.csv", header=TRUE, stringsAsFactors=FALSE)
attach(Medicalpremium)
Medicalpremium$BMI<-Medicalpremium$Weight/(Medicalpremium$Height/100)^2
set.seed(1)
train <- sample(1:nrow(Medicalpremium),(nrow(Medicalpremium)*8)/10)
test <- (-train)
Medicalpremium<-Medicalpremium[,-c(6,7)]
Medicalpremium_train<-Medicalpremium[train,]
Medicalpremium_test<-Medicalpremium[test,]

XC<-Medicalpremium_train[,-9]
YC<-Medicalpremium_train[,9,drop=FALSE]
XT<-Medicalpremium_test[,-9]
YT<-Medicalpremium_test[,9,drop=FALSE]


#PCA
#CALIBRATION
m = pca(XC, scale = TRUE, info = "People PCA model")
m = selectCompNum(m, 5)

print(m)
#VALIDATION
m = pca(XC, 7, scale = TRUE, x.test = XT, info = "PCA model")
m = selectCompNum(m, 5)
summary(m$res$cal)

par(mfrow = c(1, 2))
mdaplot(m$res$cal$scores, type = "p", show.labels = TRUE, show.lines = c(0, 0))
mdaplot(m$loadings, type = "p", show.labels = TRUE, show.lines = c(0, 0))


x <- seq(0.5, 1.5, 0.25)

#Fitting PLR model
model1<-pls(XC,YC,scale=TRUE,cv=1,info = 'Premium price prediction model')
#model summary
summary(model1)
#bias - bias for prediction vs measures
#Residual Prediction Deviation (RPD)
plot(model1)
plotXScores(model1,show.labels=FALSE,col="#107082",legend=FALSE)
plotXYLoadings(model1,show.labels=TRUE,legend=FALSE)
#model calibration result
summary(model1$res$cal)
install.packages("rmarkdown")

