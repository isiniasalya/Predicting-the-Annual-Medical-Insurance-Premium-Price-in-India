Medicalpremium <- read.csv("D:/UOC/Z. Academic/3rd year/semester 2/ST 3082/a. Projects/Project 2/Advanced Analysis/Medicalpremium.csv")

#recording variables and adding to the dataframe-------------------
#ref : https://www.datanovia.com/en/lessons/compute-and-add-new-variables-to-a-data-frame-in-r/
#ref : https://www.listendata.com/2015/06/r-keep-drop-columns-from-data-frame.html
#library(tidyverse)
BMI = Medicalpremium$Weight/(Medicalpremium$Height/100)^2
Medicalpremium$BMI<-BMI
NumberOfMajorSurgeries2=ifelse(Medicalpremium$NumberOfMajorSurgeries=="0","0",
                               ifelse(Medicalpremium$NumberOfMajorSurgeries=="1","1",
                                      ifelse(Medicalpremium$NumberOfMajorSurgeries=="2","2",
                                             ifelse(Medicalpremium$NumberOfMajorSurgeries=="3","2",0))))
Medicalpremium$NumberOfMajorSurgeries2 = as.numeric(NumberOfMajorSurgeries2)
sapply(Medicalpremium, class)                

data = subset(Medicalpremium, select = -c(Height,Weight,NumberOfMajorSurgeries) )
data %>%
  is.na() %>%
  sum()

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


#training and testing data sets----------------------------------------------------------------------------------------------------------
set.seed(1)
train <- sample(1:nrow(data),(nrow(data)*8)/10)
test <- (-train)
Medicalpremium.train<-data[train,]
Medicalpremium.test<-data[test,]


#perform k-means--------------------------------------------------------------------------------------------------------

#find no of clusters with the maximum mean silhouette value
library(factoextra)
library(NbClust)
# Silhouette method
fviz_nbclust(Medicalpremium.train, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method") #optimal no of clusters = 7

#proceed with 7 clusters
sil <- silhouette(kmeans(Medicalpremium.train, 7)$cluster, dist(Medicalpremium.train))
summary(sil)
# Perform K-means clustering with 3 clusters
set.seed(123)
kmeans.fit <- kmeans(Medicalpremium.train, 7)
# Get the cluster assignments
cluster.labels <- kmeans.fit$cluster
# Add the cluster assignments to the original data
Medicalpremium.train$cluster <- cluster.labels
# View the cluster assignments
table(Medicalpremium.train$cluster)

#view the clusters
library(ggplot2)
#Create a scatterplot of the iris dataset with cluster assignments
ggplot(Medicalpremium.train, aes(x = BMI , y = PremiumPrice, color = factor(cluster))) +
  geom_point(size = 4) +
  labs(title = "K-means Clustering (k = 7)", x = "Age", y = "Premium Price")

