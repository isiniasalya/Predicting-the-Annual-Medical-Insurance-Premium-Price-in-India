Medicalpremium <- read.csv("D:/UOC/Z. Academic/3rd year/semester 2/ST 3082/a. Projects/Project 2/Advanced Analysis/Medicalpremium.csv")

#recording variables and adding to the data frame-------------------
#ref : https://www.datanovia.com/en/lessons/compute-and-add-new-variables-to-a-data-frame-in-r/
#ref : https://www.listendata.com/2015/06/r-keep-drop-columns-from-data-frame.html
library(tidyverse)
BMI = Medicalpremium$Weight/(Medicalpremium$Height/100)^2
Medicalpremium$BMI<-BMI
NumberOfMajorSurgeries2=ifelse(Medicalpremium$NumberOfMajorSurgeries=="0","0",
                               ifelse(Medicalpremium$NumberOfMajorSurgeries=="1","1",
                                      ifelse(Medicalpremium$NumberOfMajorSurgeries=="2","2",
                                             ifelse(Medicalpremium$NumberOfMajorSurgeries=="3","2",0))))
Medicalpremium$NumberOfMajorSurgeries2 = NumberOfMajorSurgeries2
Medicalpremium = transform(Medicalpremium,
                         Diabetes=as.factor(Diabetes),
                         BloodPressureProblems=as.factor(BloodPressureProblems),
                        AnyTransplants=as.factor(AnyTransplants),
                       AnyChronicDiseases=as.factor(AnyChronicDiseases),
                      KnownAllergies=as.factor(KnownAllergies),
                      HistoryOfCancerInFamily=as.factor(HistoryOfCancerInFamily),
                    NumberOfMajorSurgeries2=as.factor(NumberOfMajorSurgeries2),
                   Height = as.numeric(Height), 
                  Weight=as.numeric(Weight))

#Checking for skewedness
#because k-means clustering is not well-suited for variables with highly skewed distributions or outliers. 
library(e1071)
skewness(Medicalpremium$PremiumPrice) #approximately normal

#standardizing the continuous variables
scaled_age = scale(data$Age)
data$Age = scaled_age
scaled_bmi = scale(data$BMI)
data$BMI = scaled_bmi

#converting the categorical variable into dummies using one-hot encoding
#ref: https://datatricks.co.uk/one-hot-encoding-in-r-three-simple-methods
library(caret)
dummy <- dummyVars(" ~ NumberOfMajorSurgeries2", data=data)
newNumberOfMajorSurgeries2 <- data.frame(predict(dummy, newdata = data))
data$NumberOfMajorSurgeries2 = newNumberOfMajorSurgeries2


data = subset(Medicalpremium, select = -c(Height,Weight,NumberOfMajorSurgeries) )
data %>%
  is.na() %>%
  sum()


#training and testing data sets----------------------------------------------------------------------------------------------------------
set.seed(1)
train <- sample(1:nrow(data),(nrow(data)*8)/10)
test <- (-train)
Medicalpremium.train<-data[train,]
Medicalpremium.test<-data[test,]


#kproto-------------------------------------------------------------------------------------------------------------------------------------
#install.packages("clustMixType")
sapply(Medicalpremium.train,class)
library(clustMixType)

#calculate optimal number of cluster, index values and clusterpartition with Silhouette-index
val = validation_kproto(method = "silhouette", data = Medicalpremium.train, k = 2:5, nstart = 5)
val$k_opt #2clusters

# apply k-prototypes
kpres = kproto(Medicalpremium.train,val$k_opt)
clprofiles(kpres, Medicalpremium.train)
