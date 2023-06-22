library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
install.packages("rlang")
library(rlang)
library(lubridate)
library(dplyr)
library(tidygeocoder)

#Read in the Hotels data
Hotels <- read_csv("hotels.csv")

#One-hot encode the reservation status variable into a new variable
Hotels$Cancelled <- ifelse(Hotels$reservation_status == "Check-Out" | Hotels$reservation_status == "No-Show", 0, 1)

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

#Read in the Countries data set
Countries <- read_csv("countries.csv")

#Join the Countries dataset with the Hotels data
Hotels <- left_join(Hotels, Countries, by = c("country" = "Country"))
glimpse(Hotels)

#Europe one-hot encoded variable
Hotels = Hotels %>% 
  mutate(Europe = ifelse(Continent == "Europe", 1, 0))

Hotels <- as.data.frame(unclass(Hotels),                     # Convert all columns to factor
                       stringsAsFactors = TRUE)

#Part A:

#1.
set.seed(1842)

#Divide data into training and test set
sample_size <- floor(0.8*nrow(Hotels))
picked <- sample(seq_len(nrow(Hotels)),size=sample_size)
hotels_train <- Hotels[picked,]
hotels_test <- Hotels[-picked,]

glimpse(hotels_train)

#2.
#Create our form we want for our model
Form <- as.formula(as.factor(Cancelled)~hotel+rate+reserved_room_type+meal+market_segment+
                     required_car_parking_spaces+adults+children+total_of_special_requests+
                     Portugal+Europe+Lead.Time+week_of_arrival+Repeat.Customer)
hotels_forest <- randomForest(Form,data=hotels_train,mtry=14,ntree=100,na.action=na.roughfix)
hotels_forest

#3.
hotels_forest2 <- randomForest(Form,data=hotels_train,mtry=14,ntree=500,na.action=na.roughfix)
hotels_forest2

#4.
hotels_forest3 <- randomForest(Form,data=hotels_train,mtry=14,ntree=1000,na.action=na.roughfix)
hotels_forest3

#5.
#See word document

#6.
#Using the importance function,
importance(hotels_forest3)
#we see Height, mheight and shoe are most important (largest values)

#7.
#See word document

#8.
#Now let's test our Bagging model with our test set:
hotels_test$forest_pred <- predict(hotels_forest3, hotels_test, type="class")
table(hotels_test$Cancelled, hotels_test$forest_pred)

#9.
#Create Random Forest from training data set (mtry = 5)
hotels_forest4 <- randomForest(Form,data=hotels_train,mtry=5,ntree=500,na.action=na.roughfix)
hotels_forest4

#10.
#Create Random Forest from training data set (mtry = 3)
hotels_forest5 <- randomForest(Form,data=hotels_train,mtry=3,ntree=500,na.action=na.roughfix)
hotels_forest5

#11.
#Create Random Forest from training data set (mtry = 1)
hotels_forest6 <- randomForest(Form,data=hotels_train,mtry=1,ntree=500,na.action=na.roughfix)
hotels_forest6

#12.
#See word document

#13.
#Now let's test our Random Forest with our test set:
hotels_test$forest_pred <- predict(hotels_forest4, hotels_test, type="class")
table(hotels_test$Cancelled, hotels_test$forest_pred)



#Part B:

#1.
set.seed(1842)

#Create 500 random, uniform values between 0 and 10 for x1 and x2
x1 <- runif(500,0,10)
x2 <- runif(500,0,10)

#Create 500 observations of simulated response "yes" or "no"
y <- sample(c("yes","no"),500,replace=TRUE)

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
#Create our form we want for our model
Form <- as.formula(as.factor(y)~x1+x2)
data_forest <- randomForest(Form,data=data_train,mtry=2,ntree=100,na.action=na.roughfix)
data_forest

#4. 
#See word document

#5.
#Now let's test our model with our test set:
data_test$forest_pred <- predict(data_forest, data_test, type="class")
table(data_test$y, data_test$forest_pred)

#6.
#Create the grid layout
data_grid <- expand.grid(x1 = seq(0,10,0.1), x2 = seq(0,10,0.1))
#Predict the grid values
data_grid$pred <- predict(data_forest, data_grid, type="class")
#Plot the predictions
ggplot(data_grid,aes(x1,x2,color=pred))+geom_point()

#Part C:

#make a data set with two groups
#make group 1
x1 <- rnorm(250,6,1)
x2 <- rnorm(250,6,1)
y <- rep("yes",250)
g1 <- cbind.data.frame(x1,x2,y)
glimpse(g1)
#make group2
x1 <- rnorm(250,5,1)
x2 <- rnorm(250,5,1)
y <- rep("no",250)
g2 <- cbind.data.frame(x1,x2,y)
glimpse(g2)
#combine the dataframes
sim_dat3 <- rbind(g1,g2)
glimpse(sim_dat3)

#1.
ggplot(sim_dat3,aes(x1,x2,color=y))+geom_point()

#2.
set.seed(1842)

#Divide data into training and test set
sample_size <- floor(0.8*nrow(sim_dat3))
picked <- sample(seq_len(nrow(sim_dat3)),size=sample_size)
sim_dat3_train <- sim_dat3[picked,]
sim_dat3_test <- sim_dat3[-picked,]

glimpse(sim_dat3_train)

#3.
#Create our form we want for our model
Form <- as.formula(as.factor(y)~x1+x2)
sim_dat3_forest <- randomForest(Form,data=sim_dat3_train,mtry=2,ntree=100,na.action=na.roughfix)
sim_dat3_forest

#4.
#Now let's test our model with our test set:
sim_dat3_test$forest_pred <- predict(sim_dat3_forest, sim_dat3_test, type="class")
table(sim_dat3_test$y, sim_dat3_test$forest_pred)

#5.
#Create the grid layout
sim_dat3_grid <- expand.grid(x1 = seq(0,10,0.1), x2 = seq(0,10,0.1))
#Predict the grid values
sim_dat3_grid$pred <- predict(sim_dat3_forest, sim_dat3_grid, type="class")
#Plot the predictions
ggplot(sim_dat3_grid,aes(x1,x2,color=pred))+geom_point()

#6.
#See word document







