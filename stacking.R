Medicalpremium <- read.csv("D:/UOC/Z. Academic/3rd year/semester 2/ST 3082/a. Projects/Project 2/Advanced Analysis/Medicalpremium.csv")

#recording variables and adding to the dataframe-------------------
#ref : https://www.datanovia.com/en/lessons/compute-and-add-new-variables-to-a-data-frame-in-r/
#ref : https://www.listendata.com/2015/06/r-keep-drop-columns-from-data-frame.html

Medicalpremium$BMI<-Medicalpremium$Weight/(Medicalpremium$Height/100)^2
BMI = Medicalpremium$Weight/(Medicalpremium$Height/100)^2
Medicalpremium$BMI<-BMI
Medicalpremium = transform(Medicalpremium,
                           Diabetes=as.factor(Diabetes),
                           BloodPressureProblems=as.factor(BloodPressureProblems),
                           AnyTransplants=as.factor(AnyTransplants),
                           AnyChronicDiseases=as.factor(AnyChronicDiseases),
                           KnownAllergies=as.factor(KnownAllergies),
                           HistoryOfCancerInFamily=as.factor(HistoryOfCancerInFamily),
                           NumberOfMajorSurgeries=as.factor(NumberOfMajorSurgeries),
                           Height = as.numeric(Height), 
                           Weight=as.numeric(Weight))

#onehot encoding for NumberOfMajorSurgeries
library(caret)
dummy <- dummyVars(" ~  NumberOfMajorSurgeries", data=Medicalpremium)
dummy_df <- data.frame(predict(dummy, newdata = Medicalpremium)) 
sapply(dummy_df,class)
Medicalpremium <- cbind(Medicalpremium, dummy_df)
sapply(Medicalpremium,class)

Medicalpremium = transform(Medicalpremium,
                           NumberOfMajorSurgeries.0=as.factor(NumberOfMajorSurgeries.0),
                           NumberOfMajorSurgeries.1=as.factor(NumberOfMajorSurgeries.1),
                           NumberOfMajorSurgeries.2=as.factor(NumberOfMajorSurgeries.2),
                           NumberOfMajorSurgeries.3=as.factor(NumberOfMajorSurgeries.3))

data = subset(Medicalpremium, select = -c(Height,Weight,NumberOfMajorSurgeries) )
sapply(data,class)


sapply(data,class)
library(tidyverse)
data %>%
  is.na() %>%
  sum()

#training and testing datasets----------------------------------------------------------------------------------------------------------
set.seed(1)
train <- sample(1:nrow(data),(nrow(data)*8)/10)
test <- (-train)
Medicalpremium.train<-data[train,]
Medicalpremium.test<-data[test,]


#perform ensemble learning--------------------------------------------------------------------------------------------------------------
#ref: https://www.r-bloggers.com/2020/05/how-to-build-stacked-ensemble-models-in-r/

library(h2o)
# initialize the h2o
h2o.init()

#create the train and test h2o data frames
train_df_h2o<-as.h2o(Medicalpremium.train)
test_df_h2o<-as.h2o(Medicalpremium.test)

# Identify predictors and response
y <- "PremiumPrice"
x <- setdiff(names(train_df_h2o), y)

#sub train and test datasets
sub_train <- sample(1:nrow(Medicalpremium.train),(nrow(Medicalpremium.train)*8)/10)
sub_test <- (-sub_train)
Medicalpremium.sub_train<-data[sub_train,]
Medicalpremium.sub_test<-data[sub_test,]
sub_train_df_h2o<-as.h2o(Medicalpremium.sub_train)
sub_test_df_h2o<-as.h2o(Medicalpremium.sub_test)

# Number of CV folds (to generate level-one data for stacking)
nfolds <- 5

##step1: Generate a 3-model ensemble (GBM + RF + Logistic)
# Train & Cross-validate a GBM(gradient boosting machine) (including XGBoost)
st_gbm <- h2o.gbm(x = x,
                  y = y,
                  training_frame = sub_train_df_h2o,
                  nfolds = nfolds,
                  keep_cross_validation_predictions = TRUE,
                  seed = 5)

# Train & Cross-validate a RF
st_rf <- h2o.randomForest(x = x,
                          y = y,
                          training_frame = sub_train_df_h2o,
                          nfolds = nfolds,
                          keep_cross_validation_predictions = TRUE,
                          seed = 5)

# Train & Cross-validate a GLM(Generalized Linear model)
st_glm = h2o.glm(x = x,
                 y = y,
                 training_frame = sub_train_df_h2o,
                 nfolds = nfolds,
                 keep_cross_validation_predictions = TRUE,
                 seed = 5)


# Evaluate base learner performance on a test set
perf_gbm_test <- h2o.performance(st_gbm, newdata = sub_test_df_h2o)
perf_rf_test <- h2o.performance(st_rf, newdata = sub_test_df_h2o)
perf_glm_test <- h2o.performance(st_glm, newdata = sub_test_df_h2o)


# Train a stacked random forest ensemble using the GBM, RF and LR above
#error
ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                metalearner_algorithm="drf",
                                training_frame = sub_test_df_h2o,
                                base_models = list(st_gbm, st_rf, st_glm))
# Evaluate ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = test_df_h2o)
perf 



# Compare to base learner performance on the test set----------------###########################################wrong
#auc value: 1- when predictions all are correct 0- when all predictions are wrong
baselearner_best_auc_test <- max(h2o.auc(perf_gbm_test), h2o.auc(perf_rf_test), h2o.auc(perf_glm_test))############why NULL?????????????????
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("Best Base-learner Test AUC:  %s", baselearner_best_auc_test))
print(sprintf("Ensemble Test AUC:  %s", ensemble_auc_test))

