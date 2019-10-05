#read dataset
library(readxl)
Data <- read_excel("C:/Users/Suresh/Desktop/Data.xlsx")
Data$Eye_Sight = factor(Data$Eye_Sight,levels = c('no','yes'),labels = c(0,1))
View(Data)

#data cleaning
library(dplyr)
Data1 <- subset(Data,power_left_eye<13)
Data2 <- subset(Data1,power_left_eye>-13)
Data3 <- subset(Data2,power_right_eye<13)
Data4 <- subset(Data3,power_right_eye>-13)
View(Data4)

#
set1 = Data4[, 2:4]
set1

#scaling of data
set1[, 1:2] = scale(set1[, 1:2])

#fitting logistical regression to train_set
#creating model1 using all independents
model1 = glm(formula = Eye_Sight ~ ., family = binomial , data = set1)
#creating model2 using independent power right eye
model2 = glm(formula = Eye_Sight ~ power_right_eye, family = binomial , data = set1)
#creating model3 using independent power left eye
model3 = glm(formula = Eye_Sight ~ power_left_eye, family = binomial , data = set1)



#predicting the test_set result
#for model1
prob_pred1 = predict(model1 , type = 'response' , newdata = set1[-3])
prob_pred1
y_pred1 = ifelse(prob_pred1 > 0.5,1,0)
y_pred1
#for model2
prob_pred2 = predict(model2 , type = 'response' , newdata = set1[-3])
y_pred2 = ifelse(prob_pred2 > 0.5,1,0)
#for model3
prob_pred3 = predict(model3 , type = 'response' , newdata = set1[-3])
y_pred3 = ifelse(prob_pred3 > 0.5,1,0)

#creating confusion matrix
#for model1
#cm = table(test_set[, 3], y_pred)
cm1 = table(unlist(set1[, 3]), y_pred1)
cm1
#for model2
cm2 = table(unlist(set1[, 3]), y_pred2)
#for model3
cm3 = table(unlist(set1[, 3]), y_pred3)


#histogram representation
hist(prob_pred1)

#evaluating performance of model
library(ROCR)
#pred <- predict(set1, Data4, type = 'prob')
#roc <- performance(prob_pred1, "tpr", "fpr")
#plot(roc)
pred <- prediction(prob_pred1, set1$Eye_Sight)
eval <- performance(pred, "acc")

max <- which.max(slot(eval,"y.values")[[1]])
acc <- slot(eval,"y.values")[[1]][max]
cut <- slot(eval,"x.values")[[1]][max]
roc <- performance(pred, "tpr", "fpr")
roc1 <- performance(pred, "tnr", "fnr")
plot(roc1)
plot(roc)
abline(a=0, b=1)
#Akaike Information Criteria(AIC)
AIC(model1)
AIC(model2)
AIC(model3)
#smaller AIC VALUE,that accurate model fits
