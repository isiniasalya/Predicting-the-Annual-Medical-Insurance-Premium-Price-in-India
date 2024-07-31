Medicalpremium <- read.csv("D:/UOC/Z. Academic/3rd year/semester 2/ST 3082/a. Projects/Project 2/Advanced Analysis/Medicalpremium.csv")

#recording variables and adding to the dataframe-------------------
#ref : https://www.datanovia.com/en/lessons/compute-and-add-new-variables-to-a-data-frame-in-r/
#ref : https://www.listendata.com/2015/06/r-keep-drop-columns-from-data-frame.html
BMI = Medicalpremium$Weight/(Medicalpremium$Height/100)^2
Medicalpremium$BMI<-BMI

Medicalpremium = transform(Medicalpremium,
                           Diabetes=as.factor(Diabetes),
                           BloodPressureProblems=as.factor(BloodPressureProblems),
                           AnyTransplants=as.factor(AnyTransplants),
                           AnyChronicDiseases=as.factor(AnyChronicDiseases),
                           KnownAllergies=as.factor(KnownAllergies),
                           HistoryOfCancerInFamily=as.factor(HistoryOfCancerInFamily),
                           NumberOfMajorSurgeries=as.numeric(NumberOfMajorSurgeries),
                           Height = as.numeric(Height), 
                           Weight=as.numeric(Weight))

data = subset(Medicalpremium, select = -c(Height,Weight) )
sapply(data,class)
library(tidyverse)
data %>%
  is.na() %>%
  sum()


#training and testing data sets----------------------------------------------------------------------------------------------------------
set.seed(1)
train <- sample(1:nrow(data),(nrow(data)*8)/10)
test <- (-train)
Medicalpremium.train<-data[train,]
Medicalpremium.test<-data[test,]
 
lin_mod = lm(PremiumPrice ~. ,data = Medicalpremium.train)

###best subset selection----------------------------------------------------------------------------------------------------------------- 
#ref : http://www.science.smith.edu/~jcrouser/SDS293/labs/lab8-r.html
library(leaps)
model.regfit_full = regsubsets(PremiumPrice~., data = Medicalpremium.train, nvmax = 10)
reg_summary = summary(model.regfit_full)
reg_summary

# k fold cross validation to select the models with the best no of predictors
#ref: ref book
k <- 10
n <- nrow (Medicalpremium.train)
set.seed (1)
folds <- sample ( rep (1:k, length = n))
cv.errors <- matrix (NA, k, 10,dimnames = list (NULL , paste (1:10)))

predict.regsubsets <- function (object , newdata , id, ...) {
  form <- as.formula (object$call[[2]])
  mat <- model.matrix(form , newdata)
  coefi <- coef (object , id = id)
  xvars <- names (coefi)
  mat[, xvars] %*% coefi
}
for (j in 1:k) {
  best.fit <- regsubsets (PremiumPrice~.,data = Medicalpremium.train[folds != j, ],nvmax = 10)
  for (i in 1:10) {
    pred <- predict (best.fit ,  Medicalpremium.train[folds == j, ], id = i)
    cv.errors[j, i] <-mean (( Medicalpremium.train$PremiumPrice[folds == j] - pred)^2)
  }
}

mean.cv.errors <- apply (cv.errors , 2, mean)
which.min(mean.cv.errors)
plot (mean.cv.errors , type = "b")
#model with 7 predictors is selected


reg.best <- regsubsets (PremiumPrice ~ ., data = Medicalpremium.train ,nvmax = 7)
summary(reg.best)
plot(reg.best, scale = "r2")
coeffi = coef (reg.best , 7 )


#model accuracy----------------------------------------------------------------------------------------------------------------
selected_vars <- names(coef (reg.best , 7))[-1]
formu = paste0("PremiumPrice ~ ", paste(selected_vars, collapse = " + "))

formu
model_subset = lm(PremiumPrice ~ Age + AnyTransplants1 + AnyChronicDiseases1 + HistoryOfCancerInFamily1 + NumberOfMajorSurgeries2 + NumberOfMajorSurgeries3 + BMI, 
           data = Medicalpremium.train)
#car::vif(model_subset)



################################################################################################

predicted_sub = predict(model_subset ,newdata = Medicalpremium.test)

#rmse (scale dependent)
library(Metrics)
rmse_sub = rmse(Medicalpremium.test$PremiumPrice,predicted_sub)
rmse_sub

#mape (scale independent)
mape_sub = mape(Medicalpremium.test$PremiumPrice,predicted_sub)
mape_sub
#____________________________________________________________________





#r2 and cp etc to select the number of variables needed------------------------------------------------------------
reg_summary$adjr2
adj_r2_max = which.max(reg_summary$adjr2) #9
par(mfrow = c(2,2))
plot(reg_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
points(adj_r2_max, reg_summary$adjr2[adj_r2_max], col ="red", cex = 2, pch = 20)

cp_min = which.min(reg_summary$cp) #7
plot(reg_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
points(cp_min, reg_summary$cp[cp_min], col = "red", cex = 2, pch = 20)

bic_min = which.min(reg_summary$bic) #6
plot(reg_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(bic_min, reg_summary$bic[bic_min], col = "red", cex = 2, pch = 20)

plot(model.regfit_full, scale = "adjr2")
coef(model.regfit_full, 9)

plot(model.regfit_full, scale = "Cp")
coef(model.regfit_full, 7)

plot(model.regfit_full, scale = "bic")
coef(model.regfit_full, 6)


