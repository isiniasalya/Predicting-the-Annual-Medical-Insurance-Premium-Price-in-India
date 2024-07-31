set.seed(1)
train <- sample(1:nrow(Medicalpremium),(nrow(Medicalpremium)*8)/10)
test <- (-train)
Medicalpremium<-Medicalpremium[train,]

#Recording Variable-------------------------------------------------------------------
Medicalpremium$BMI<-Medicalpremium$Weight/(Medicalpremium$Height/100)^2
Medicalpremium$bmi_category <- cut(Medicalpremium$BMI,
                                   breaks=c(0, 18.5, 25, 30, 40,100),
                                   labels=c('Underweight', 'Normal BMI', 'Overweight', 'Obese','Severely obese'))
Medicalpremium$NumberOfMajorSurgeries1<-ifelse(Medicalpremium$NumberOfMajorSurgeries=="0","0",
                                               ifelse(Medicalpremium$NumberOfMajorSurgeries=="1","1",
                                                      ifelse(Medicalpremium$NumberOfMajorSurgeries=="2","2-3",
                                                             ifelse(Medicalpremium$NumberOfMajorSurgeries=="3","2-3",0))))


#VIF---------------------------------------------------------------------------------------
lm1<-lm(PremiumPrice~Age+Diabetes+BloodPressureProblems+AnyTransplants +AnyChronicDiseases+BMI+
          KnownAllergies+HistoryOfCancerInFamily+NumberOfMajorSurgeries,data=Medicalpremium)
car::vif(lm1)