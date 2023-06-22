# Name: Michael Labbe
# Date: 02/09/23
# Goal of Program: Machine Learning, Party Kit, Confusion Matrices
# Data Files Used: NHANES


library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
install.packages("rlang")
library(rlang)
library(partykit)

install.packages("NHANES")
library(NHANES)

data(NHANES)
glimpse(NHANES)

hard_drugs <- NHANES %>% drop_na(HardDrugs)
glimpse(hard_drugs)

#1.

set.seed(1842)

#Divide data into training and test set
sample_size <- floor(0.8*nrow(hard_drugs))
picked <- sample(seq_len(nrow(hard_drugs)),size=sample_size)
hd_train <- hard_drugs[picked,]
hd_test <- hard_drugs[-picked,]

glimpse(hd_train)
glimpse(hd_test)


#2.

#Run tree model to predict HardDrugs from RegularMarij
library(rpart)
harddrugs_tree <- rpart(HardDrugs~RegularMarij, data=hard_drugs)
harddrugs_tree


#3. 

#Produce party plot of tree
library(partykit)
plot(as.party(harddrugs_tree))


#4.
#Create predictions from our model
predictions <- predict(harddrugs_tree, hd_test, type="class")

#Create confusion matrix
confusion <- table(hd_test$HardDrugs,predictions,dnn=c("actual","predicted"))
confusion


#5.

#Run tree model to predict HardDrugs from RegularMarij, Age, AlcoholYear and BMI
harddrugs_tree2 <- rpart(HardDrugs~RegularMarij+Age+AlcoholYear+BMI, data=hard_drugs)
harddrugs_tree2


#6.

#Produce party plot for our second tree
plot(as.party(harddrugs_tree2))


#7.

#Create predictions from our second model
predictions2 <- predict(harddrugs_tree2, hd_test, type="class")

#Create confusion matrix
confusion <- table(hd_test$HardDrugs,predictions2,dnn=c("actual","predicted"))
confusion


#8.

#Using a thirty-two year old regular marijiuana user with a BMI of 40: We start at
#the root node (RegularMarij), we move on to the child node of yes as this particular
#individual is a regular marijiuana user. From Age, we move on to the child node of
#BMI becuase the age is >=31.5. Lastly, because the BMI >= 39.69, we arrive at our 
#last stop. In Node 6, we see that the probability leans to a no. Therefore, using
#our decision tree, we can predict that this hypotherical person would not be a hard
#drug user. 


#9.

#Miclassification rate (error rate)
printcp(harddrugs_tree2)


#Part B

#1.

#Create 500 random, uniform values between 0 and 10 for x1 and x2
x1 <- runif(500,0,10)
x2 <- runif(500,0,10)

set.seed(1842)

#Create 500 observations of simulated response "yes" or "no"
y <- sample(c("yes","no"),500,replace=TRUE)

#Bind the data together
data <- cbind.data.frame(x1,x2,y)

glimpse(data)


#2.

#Create scatterplot of x1 vs x2 with colors indicating response variable y
ggplot(data,aes(x1,x2,color=y))+geom_jitter()


#3.

set.seed(1842)

#Divide data into training and test set
sample_size2 <- floor(0.8*nrow(data))
picked2 <- sample(seq_len(nrow(data)),size=sample_size2)
data_train <- data[picked2,]
data_test <- data[-picked2,]

glimpse(data_train)
glimpse(data_test)


#4.

#Run tree model to predict y from x1 and x2
data_tree <- rpart(y~x1+x2, data=data)
data_tree

#Produce party plot of tree
plot(as.party(data_tree))


#5.

#Create predictions from our model
predictions2 <- predict(data_tree, data_test, type="class")

#Create confusion matrix
confusion <- table(data_test$y,predictions2,dnn=c("actual","predicted"))
confusion

#Calculate the percentage that are correctly predicted
correct_percentage <- sum(diag(confusion))/sum(confusion)
correct_percentage


#6.

#Because we know that there is no predictable relationship between the randomly
#created y ("response") variable and the randomly created x1 and x2 ("predictor") 
#variables, the 59% correct values for our predictions of our test set makes sense.
#We are basically training our model using random chance rather than working off
#of pre-existing relationships. We might as well be tossing a coin in the air 
#several times. We would expect something around 50% just like we see here. 


#7.

#Create 500 random, uniform values between 0 and 10 for x1 and x2
x1 <- runif(500,0,10)
x2 <- runif(500,0,10)

set.seed(1842)

#Create 500 observations of simulated response "yes" or "no"
y <- ifelse((x1>5 & x2>8),"yes","no")

#Bind the data together
data2 <- cbind.data.frame(x1,x2,y)

glimpse(data2)


#8.

#Create scatterplot of x1 vs x2 with colors indicating response variable y
ggplot(data2,aes(x1,x2,color=y))+geom_jitter()


#9.

set.seed(1842)

#Divide data into training and test set
sample_size3 <- floor(0.8*nrow(data2))
picked3 <- sample(seq_len(nrow(data2)),size=sample_size3)
data2_train <- data2[picked3,]
data2_test <- data2[-picked3,]

#Run tree model to predict y from x1 and x2
data2_tree <- rpart(y~x1+x2, data=data2)
data2_tree

#Produce party plot of tree
plot(as.party(data2_tree))


#10.

#Create predictions from our model
predictions3 <- predict(data2_tree, data2_test, type="class")

#Create confusion matrix
confusion <- table(data2_test$y,predictions3,dnn=c("actual","predicted"))
confusion

#Calculate the percentage that are correctly predicted
correct_percentage2 <- sum(diag(confusion))/sum(confusion)
correct_percentage2


#11.

#The algorithm selected the shortest path for sorting of values. If the algorithm
#would have selected x1 as the root node, the algorithm would have had many more
#values to sort through seeing as there are many more values >= 5 than there are 
#values >= 8. 


#12.

#The algorithm associated with decision trees is especially effective at finding 
#the binary splits in the data.  That does not mean that the algorithm found the 
#exact split points. Instead, the algorithm found the optimal split points in 
#order to minimize the classification error.



