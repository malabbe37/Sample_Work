library(tidyverse)
library(dplyr)
library(rlang)
library(partykit)
library(readr)
library(rpart)
library(class)
library(hardhat) 
library(ipred)
library(NHANES)

data(NHANES)

#Part A:

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


#2.

#Select rows to be used
NHANES2 <- NHANES2%>%dplyr::select(HardDrugs1,RM1,Age,AlcoholYear,HHIncomeMid,Height,TotChol,Sex1)
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

#Fit a generalized linear model
glm_fit <- glm(HardDrugs1~RM1+Age+AlcoholYear+HHIncomeMid+Height+TotChol,data=NHANES_train,family=binomial)

summary(glm_fit)


#6.

#Predict Hard Drugs values for our test set using our GLM
glm_probs <- predict(glm_fit,new=NHANES_test,type="response")

table(round(glm_probs),NHANES_test$HardDrugs1)


#7.

#Create 2nd GLM
glm_fit2 <- glm(HardDrugs1~RM1+Age+Sex1,data=NHANES_train,family=binomial)

summary(glm_fit2)


#8.

#Predict Hard Drugs values for our test set using our 2nd GLM
glm_probs2 <- predict(glm_fit2,new=NHANES_test,type="response")

table(round(glm_probs2),NHANES_test$HardDrugs1)


#9.

#Create 3rd GLM
glm_fit3 <- glm(HardDrugs1~RM1,data=NHANES_train,family=binomial)

summary(glm_fit3)


#10.

#Predict Hard Drugs values for our test set using our 3rd GLM
glm_probs3 <- predict(glm_fit3,new=NHANES_test,type="response")

table(round(glm_probs3),NHANES_test$HardDrugs1)


#11.

#Between the 3 models, I would go with the first model we fitted utilizing all 
#6 predictors. Even though our penalty score is higher in our AIC calculation
#with the larger amount of predictors, each individual predictor variables is
#statistically significant and thus we end up with a lower AIC than any other model.


#12.

#See pdf for Part A code.



#Part B:

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

#Groups one-hot encoded variable
Hotels = Hotels %>% mutate(Groups = ifelse(market_segment == "Groups", 1, 0))

#Resort Hotel one-hot encoded variable
Hotels = Hotels %>% mutate(Resort = ifelse(hotel == "Resort Hotel", 1, 0))


#1.

#Select variables in our data set to be used
Hotels2 <- dplyr::select(Hotels,Canceled,rate,required_car_parking_spaces,adults,children,
                         total_of_special_requests,Portugal,`Lead Time`,`Repeat Customer`,
                         Groups,Resort)
glimpse(Hotels2)


#2.

set.seed(1842)

#Divide data into training and test set
sample_size <- floor(0.8*nrow(Hotels2))
picked <- sample(seq_len(nrow(Hotels2)),size=sample_size)
hotels_train <- Hotels2[picked,]
hotels_test <- Hotels2[-picked,]

glimpse(hotels_train)


#3.

#Create Generalized Linear Model for 'Canceled' response variable
glm_fit4 <- glm(Canceled~rate+required_car_parking_spaces+adults+children+
                total_of_special_requests+Portugal+`Lead Time`+`Repeat Customer`+
                Groups+Resort,data=hotels_train,family=binomial)

summary(glm_fit4)


#4.

#The hypothesis test for the 'required_car_parking_spaces' predictor variable comes
#out to have a p-value of 0.71 which is far higher than our alpha value of 0.05. 
#Therefore, we do not have enough evidence to reject our null hypothesis and we say 
#that the predictor variable 'required_car_parking_spaces' is non-significant in 
#the prediction of our response variable 'Canceled'. 


#5.

#Predict Canceled values for our test set using our GLM
glm_probs4 <- predict(glm_fit4,new=hotels_test,type="response")

table(round(glm_probs4),hotels_test$Canceled)


#6.

#Looking at the coefficient for rate in our summary of our generalized linear
#model, we see that we have an estimated value of 0.007612. This means that 
#for every 1 unit increase in our response variable 'Canceled', we can expect 
#a 0.007612 change in rate. This tells us that holding all other variables 
#constant, the chance of a customer canceling goes up as the rate of the room 
#goes up!


#7.

#Looking at the coefficient for Portugal in our summary of our generalized linear
#model, we see that we have an estimated value of 1.616. This means that 
#for every 1 unit increase in our response variable 'Canceled', we can expect 
#a 1.616 change in Portugal. This means that customers in Portugal are 1.616
#times more likely to cancel! 


#8.

#See pdf for Part B code.