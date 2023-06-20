library(readr)
library(tidyverse)
library(stringr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(class)
library(hardhat) 
library(ipred)
library(caret)
library(rpart)
library(partykit)
library(randomForest)

#Read in the Hotels data
Hotels <- read_csv("hotels.csv")

#One-hot encode the reservation status variable into a new variable
Hotels$Canceled <- ifelse(Hotels$reservation_status == "Check-Out" | Hotels$reservation_status == "No-Show", 0, 1)

#Create a new feature that is the week number for the arrival date
Hotels$week_of_arrival <- week(Hotels$arrival_date)

#Create a new feature “Lead Time” (# of days between booking and arrival date)
Hotels <- Hotels%>%mutate(`Lead Time`=interval(booking_date,arrival_date)/dyears(1))

#Create one-hot encoded variable "Repeat Customer” 
Hotels = Hotels %>% 
  mutate(`Repeat Customer` = ifelse(previous_bookings_not_canceled >= 1, 1, 0)) 

#Create a one-hot encoded variable called “Portugal”
Hotels = Hotels %>% 
  mutate(Portugal = ifelse(country == "Portugal", 1, 0))

#Part A:

#1.
#Groups one-hot encoded variable
Hotels = Hotels %>% mutate(Groups = ifelse(market_segment == "Groups", 1, 0))
table(Hotels$Groups)

#2.
#Resort Hotel one-hot encoded variable
Hotels = Hotels %>% mutate(Resort = ifelse(hotel == "Resort Hotel", 1, 0))
table(Hotels$Resort)

#3.
Hotels2 <- dplyr::select(Hotels,Canceled,rate,required_car_parking_spaces,adults,children,
                  total_of_special_requests,Portugal,`Lead Time`,`Repeat Customer`,
                  Groups,Resort)
glimpse(Hotels2)

#4.
Hotels2 <- na.omit(Hotels2)
nrow(Hotels2)

#5.
rescale_x <- function(x){(x-min(x))/(max(x)-min(x))}

Hotels2$rate <- rescale_x(Hotels2$rate)
Hotels2$required_car_parking_spaces <- rescale_x(Hotels2$required_car_parking_spaces)
Hotels2$adults <- rescale_x(Hotels2$adults)
Hotels2$children <- rescale_x(Hotels2$children)
Hotels2$total_of_special_requests <- rescale_x(Hotels2$total_of_special_requests)
Hotels2$'Lead Time' <- rescale_x(Hotels2$'Lead Time')

glimpse(Hotels2)

#6.
set.seed(1842)

#Divide data into training and test set
sample_size <- floor(0.8*nrow(Hotels2))
picked <- sample(seq_len(nrow(Hotels2)),size=sample_size)
hotels_train <- Hotels2[picked,]
hotels_test <- Hotels2[-picked,]

glimpse(hotels_train)
glimpse(hotels_test)

#7.
#Run the classifier (k = sqrt(sample_size))
canceled_knn5 <- knn(hotels_train[-1],test=hotels_test[-1],cl=hotels_train$Canceled,k=5)

#Create a confusion matrix
confusion <- table(canceled_knn5,hotels_test$Canceled)
confusion

#find the percentage misclassified
1-sum(diag(confusion))/sum(confusion)

#8.
#Run the classifier (k = sqrt(sample_size))
canceled_knn25 <- knn(hotels_train[-1],test=hotels_test[-1],cl=hotels_train$Canceled,k=25)

#Create a confusion matrix
confusion2 <- table(canceled_knn25,hotels_test$Canceled)
confusion2

#find the percentage misclassified
1-sum(diag(confusion2))/sum(confusion2)

#9.
#Run the classifier (k = sqrt(sample_size))
canceled_knn75 <- knn(hotels_train[-1],test=hotels_test[-1],cl=hotels_train$Canceled,k=75)

#Create a confusion matrix
confusion3 <- table(canceled_knn75,hotels_test$Canceled)
confusion3

#Find the percentage misclassified
1-sum(diag(confusion3))/sum(confusion3)

#10.
#The k-value that provides the best prediction is k=5. This is evidenced by the 
#mis-classification rate (0.18 vs. 0.19/0.20).

#11.
#Create confusion matrix for knn
confusionMatrix(canceled_knn5,as.factor(hotels_test$Canceled))

#12.
#Fit a tree
canceled_tree <- rpart(as.factor(Canceled)~.,data=hotels_train)

plot(as.party(canceled_tree))

#13.
set.seed(1842)
#Predict the test data
tree_pred <- predict(canceled_tree,newdata = hotels_test,"class")

#Create confusion matrix for tree
confusionMatrix(tree_pred,as.factor(hotels_test$Canceled))

#14.
set.seed(1842)
names(hotels_train) <- make.names(names(hotels_train))
canceled_rf <- randomForest(as.factor(Canceled)~.,data=hotels_train,mtry=3,ntree=500)
canceled_rf

#15.
set.seed(1842)
names(hotels_test) <- make.names(names(hotels_test))
#predict the test data
tree_pred2 <- predict(canceled_rf,newdata = hotels_test,"class")

#Create confusion matrix
confusionMatrix(tree_pred2,as.factor(hotels_test$Canceled))

#16.
set.seed(1842)
canceled_rf2 <- randomForest(as.factor(Canceled)~.,data=hotels_train,mtry=5,ntree=500)
canceled_rf2

#17.
set.seed(1842)
#predict the test data
tree_pred3 <- predict(canceled_rf2,newdata = hotels_test,"class")

#Create confusion matrix
confusionMatrix(tree_pred3,as.factor(hotels_test$Canceled))

#18.
#Sensitivity is the proportion of actual positives detected by our model. Specificity, on the other 
#hand, is the proportion of actual negatives detected by our model. In other words, using our example,
#the specificity of a test describes the model's ability to correctly identify an individual who will
#not cancel. Comparing the confusion matrix from our Random Forest model with the confusion
#matrix from our KNN model, we see that our Random Forest model's sensitivity and specificity 
#both take on larger values indicating that the RF model will produce both less false negatives and 
#less false positives. 

#19.
#Code provided via PDF


#Part B:

#1. 
set.seed(1842)

#Create 500 random, uniform values between 0 and 10 for x1 and x2
x1 <- runif(500,0,10)
x2 <- runif(500,0,10)

#create a simulated class variable
y <- sample(c(1,0),500,replace=TRUE)

#Bind the data together
data <- cbind.data.frame(x1,x2,y)

glimpse(data)

#2.
set.seed(1842)

#Divide data into training and test set
sample_size <- floor(0.8*nrow(data))
picked <- sample(seq_len(nrow(data)),size=sample_size)
data_train <- data[picked,]
data_test <- data[-picked,]

glimpse(data_train)

#3.
#Create the grid layout
data_grid <- expand.grid(x1 = seq(0,10,0.1), x2 = seq(0,10,0.1))

glimpse(data_grid)

#4.
#Run the classifier (k = sqrt(sample_size))
data_grid$pred <- knn(data_train[-3],test=data_grid[1:2],cl=data_train$y,k=25)
#Plot the predictions
ggplot(data_grid,aes(x1,x2,color=pred))+geom_point()

#5.
#Run the classifier (k = sqrt(sample_size))
data_grid$pred <- knn(data_train[-3],test=data_grid[1:2],cl=data_train$y,k=5)
#Plot the predictions
ggplot(data_grid,aes(x1,x2,color=pred))+geom_point()

#6.
#Run the classifier (k = sqrt(sample_size))
data_grid$pred <- knn(data_train[-3],test=data_grid[1:2],cl=data_train$y,k=1)
#Plot the predictions
ggplot(data_grid,aes(x1,x2,color=pred))+geom_point()

#7.
#Run the classifier (k = sqrt(sample_size))
data_grid$pred <- knn(data_train[-3],test=data_grid[1:2],cl=data_train$y,k=500)
#Plot the predictions
ggplot(data_grid,aes(x1,x2,color=pred))+geom_point()

#Using a value of K that is equal to the number of values in the training data set
#is always uninformative because too large of a k value means that only a low number, 
#or in this case, 1 sample combination is possible thus limiting the number of iterations
#that are different. 

#8.
#Code provided via PDF


#Part C:

#1.
#make a data set with two groups
#make group 1
x1 <- rnorm(250,6,1)
x2 <- rnorm(250,6,1)
y <- rep(1,250)
g1 <- cbind.data.frame(x1,x2,y)
glimpse(g1)
#make group2
x1 <- rnorm(250,5,1)
x2 <- rnorm(250,5,1)
y <- rep(0,250)
g2 <- cbind.data.frame(x1,x2,y)
glimpse(g2)
#combine the dataframes
sim_dat3 <- rbind(g1,g2)
glimpse(sim_dat3)

#Scatterplot of data with classification color coded
ggplot(sim_dat3,aes(x1,x2,color=y))+geom_point()

#2.
set.seed(1842)

#Divide data into training and test set
sample_size <- floor(0.8*nrow(sim_dat3))
picked <- sample(seq_len(nrow(sim_dat3)),size=sample_size)
simdat3_train <- data[picked,]
simdat3_test <- data[-picked,]

glimpse(simdat3_train)

#3.
#Run the classifier (k = sqrt(sample_size))
data_grid$pred1 <- knn(simdat3_train[-3],test=data_grid[1:2],cl=simdat3_train$y,k=25)
#Plot the predictions
ggplot(data_grid,aes(x1,x2,color=pred1))+geom_point()

#4.
#Run the classifier (k = sqrt(sample_size))
data_grid$pred1 <- knn(simdat3_train[-3],test=data_grid[1:2],cl=simdat3_train$y,k=5)
#Plot the predictions
ggplot(data_grid,aes(x1,x2,color=pred1))+geom_point()

#5.
#Run the classifier (k = sqrt(sample_size))
data_grid$pred1 <- knn(simdat3_train[-3],test=data_grid[1:2],cl=simdat3_train$y,k=1)
#Plot the predictions
ggplot(data_grid,aes(x1,x2,color=pred1))+geom_point()

#6.
#As the value of K goes up, we see a dampening of our variability, but the bias goes up. 
#As the value of K goes down, the bias goes down, but we see an increase in variability. 

#7. 
#I feel that the K value of 25 did the best job at matching the true nature of the data.
#Because the square root of n in this case is sqrt(500) = 22.36, the value of K which lies  
#closest to the recommended starting point in this case is 25. I also believe, looking at 
#the scatter plot the bias-variability tradeoff is most in balance with K=25. 

#8.
#Code provided via PDF