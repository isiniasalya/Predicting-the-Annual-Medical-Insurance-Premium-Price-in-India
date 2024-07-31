Medicalpremium <- read.csv("D:/UOC/Z. Academic/3rd year/semester 2/ST 3082/a. Projects/Project 2/Advanced Analysis/Medicalpremium.csv")

#recording variables and adding to the dataframe-------------------
#ref : https://www.datanovia.com/en/lessons/compute-and-add-new-variables-to-a-data-frame-in-r/
#ref : https://www.listendata.com/2015/06/r-keep-drop-columns-from-data-frame.html

Medicalpremium$BMI<-Medicalpremium$Weight/(Medicalpremium$Height/100)^2
Medicalpremium %>%
  transmute(
    BMI = Weight/(Height/100)^2
  )
NumberOfMajorSurgeries2=ifelse(Medicalpremium$NumberOfMajorSurgeries=="0","0",
                               ifelse(Medicalpremium$NumberOfMajorSurgeries=="1","1",
                                      ifelse(Medicalpremium$NumberOfMajorSurgeries=="2","2",
                                             ifelse(Medicalpremium$NumberOfMajorSurgeries=="3","2",0))))
Medicalpremium$NumberOfMajorSurgeries2 = NumberOfMajorSurgeries2
#named 2-3 group 2


data = subset(Medicalpremium, select = -c(Height,Weight,NumberOfMajorSurgeries))
#missing values
library(tidyverse)
data %>%
  is.na() %>%
  sum()

#column number of response variable
col_num = which(names(data) == 'PremiumPrice')
col_num

#making all columns numeric
sapply(data,class)
data$NumberOfMajorSurgeries2 = as.numeric(data$NumberOfMajorSurgeries2)
sapply(data,class)

#training and testing data sets
set.seed(1)

#indexes
train <- sample(1:nrow(data),(nrow(data)*8)/10)
test <- (-train)

#for XGBoosting convert them into a matrix
Medicalpremium_mat = as.matrix(data)
dim(Medicalpremium_mat)

#Remove the column names and row names
dimnames(Medicalpremium_mat) <- list(NULL, NULL)

Medicalpremium.train<-Medicalpremium_mat[train,]
dim(Medicalpremium.train)
Medicalpremium.test<-Medicalpremium_mat[test,]
dim(Medicalpremium.test)



#perform XGBoosting--------------------------------------------------------------------------------------
#ref: https://www.projectpro.io/recipes/apply-xgboost-r-for-regression
library(xgboost) # for fitting the xgboost model
library(caret) # for general data preparation and model fitting

Medicalpremium.train_y <- Medicalpremium.train[,col_num]
Medicalpremium.train_x <- Medicalpremium.train[,-col_num]

Medicalpremium.test_y <- Medicalpremium.test[,col_num]
Medicalpremium.test_x <- Medicalpremium.test[,-col_num]

length(Medicalpremium.train_y)
dim(Medicalpremium.train_x)

#define final training and testing sets
xgb_train = xgb.DMatrix(data = Medicalpremium.train_x, label = Medicalpremium.train_y)
xgb_test = xgb.DMatrix(data = Medicalpremium.test_x, label = Medicalpremium.test_y)

#defining a watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each itertion
model_xgb = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 100)
#output min: [33]	train-rmse:2300.984575	test-rmse:3395.480718
#From the above model which is run of 100 epochs, a minim=um RMSE score is achieved atiteration 33. 
#After that, the RMSE starts to increase slightly, which indicates over fitting of data, 
#hence we will consider our model to be built on 33 epochs only.

#define final model
model_xgboost = xgboost(data = xgb_train, max.depth = 3, nrounds = 33, verbose = 0)
summary(model_xgboost)

#accuracy-------------------------------------------------------------------------------------------
predicted_xgb = predict(model_xgboost,newdata = Medicalpremium.test_x)

#rmse (scale dependent)
library(Metrics)
rmse_xgb = rmse(Medicalpremium.test_y,predicted_xgb)
rmse_xgb

#mape (scale independent)
mape_xgb = mape(Medicalpremium.test_y,predicted_xgb)
mape_xgb








