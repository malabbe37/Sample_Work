# Name: Michael Labbe
# Date: 03/08/23
# Goal of Program: DataManipulation, MachineLearning, ROC Curves
# Data Files Used: NHANES


library(tidyverse)
library(neuralnet)
library(NHANES)

#1.

#Create one-hot encoded variable "HardDrugs1” 
NHANES2 = NHANES %>% mutate(HardDrugs1 = ifelse(HardDrugs == "Yes", 1, 0))
summary(NHANES2$HardDrugs1)

#Create one-hot encoded variable "RM1” 
NHANES2 = NHANES2 %>% mutate(RM1 = ifelse(RegularMarij == "Yes", 1, 0))
summary(NHANES2$RM1)

#Create one-hot encoded variable "Sex1” 
NHANES2 = NHANES2 %>% mutate(Sex1 = ifelse(Gender == "male", 1, 0))
summary(NHANES2$Sex1)

attach(NHANES2)
table(HardDrugs1,RM1,Sex1)


#2.

#Select rows to be used
NHANES2 <- NHANES2%>%dplyr::select(HardDrugs1,RM1,Age,BMI,Sex1)
glimpse(NHANES2)


#3.

#Omit any NA values in data set
NHANES2 <- na.omit(NHANES2)

#Find number of rows in our data
nrow(NHANES2)


#4.

set.seed(1842)

#Divide data into training and test set
sample_size <- floor(0.8*nrow(NHANES2))
picked <- sample(seq_len(nrow(NHANES2)),size=sample_size)
NHANES_train <- NHANES2[picked,]
NHANES_test <- NHANES2[-picked,]

glimpse(NHANES_train)
glimpse(NHANES_test)


#5.

#Set up formula
harddrugs_form <- as.formula(HardDrugs1~RM1+Age+BMI+Sex1)

#Fit the Neural Network
harddrugs_nn1 = neuralnet(harddrugs_form, NHANES_train,
                               hidden=1, stepmax=1e+08,rep=10,lifesign = "minimal",linear.output = F)

#6.

plot(harddrugs_nn1,rep="best")


#7.

library(caret)

drugs_pred_nn1 <- predict(harddrugs_nn1,NHANES_test,rep=9)
preds <- ifelse(drugs_pred_nn1>0.5,1,0)
confusionMatrix(factor(preds),factor(NHANES_test$HardDrugs1),positive = "1")


#8.

#In our output, we get a sensitivity of 0.43523 which indicated to us that our model is better
#at identifying non-hard drug users than identifying hard drug users.


#9.

#Set up formula
harddrugs_form <- as.formula(HardDrugs1~RM1+Age+BMI+Sex1)

#Fit the Neural Network
harddrugs_nn2 = neuralnet(harddrugs_form, NHANES_train,
                          hidden=2, stepmax=1e+08,rep=10,lifesign = "minimal",linear.output = F)


#10.

plot(harddrugs_nn2,rep="best")


#11.

drugs_pred_nn2 <- predict(harddrugs_nn2,NHANES_test,rep=5)
preds2 <- ifelse(drugs_pred_nn2>0.5,1,0)
confusionMatrix(factor(preds2),factor(NHANES_test$HardDrugs1),positive = "1")


#12.

library(ROCR)

roc_preds1 <- prediction(preds,NHANES_test$HardDrugs1)
roc_perf1 <- performance(roc_preds1,"tpr","fpr")
plot(roc_perf1,col="blue")

roc_preds2 <- prediction(preds2,NHANES_test$HardDrugs1)
roc_perf2 <- performance(roc_preds2,"tpr","fpr")
#Add our tree to the plot
plot(roc_perf2,add=T,col="red")

#13.

auc_perf <- performance(roc_preds1,measure="auc")
auc_perf@y.values
auc_perf2 <- performance(roc_preds2,measure="auc")
auc_perf2@y.values

#The AUC shows us how effective a model is; the greater the area under the curve, the 
#better the model is. Here, we observe visually and by calculating that the AUC curve 
#is larger for our first model (using just 1 node). Therefore, we can conclude that the 
#model with two (2) nodes provides no advantage over the model with a single node.