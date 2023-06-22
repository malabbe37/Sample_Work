# Name: Michael Labbe
# Date: 01/25/23
# Goal of Program: Sampling Distributions, Confidence Intervals, Quantiles
# Data Files Used: Berkley Arrests

#Part A

A <- rnorm(70000,40,6)
B <- rnorm(30000,90,10)

#Create a tibble (population) consisting of a single variable x 
time <- tibble(x=c(A,B))
ggplot(time,aes(x=x))+geom_histogram()


set.seed(1994)
#Create a sample from this distribution 
time_samples <- replicate(1000,sample_n(time,100,replace=F))

#Calculate the means
time_means <- map_dbl(time_samples,mean)

#Create histogram of sampling distribution of the 1000 means
ggplot(tibble(time_means), aes(x=time_means))+geom_histogram(bins = 15)


#We can estimate the standard error of our statistic simply by taking the standard deviation of our means
sd(time_means)


set.seed(1994)
#Create a sample from this distribution 
case_samples <- replicate(1000,sample_n(time,5,replace=T))

#Calculate the means
case_sums <- map_dbl(case_samples,sum)

#Create histogram of sampling distribution of the 1000 means
ggplot(tibble(case_sums),aes(x=case_sums))+geom_histogram(bins = 15)

prob <- sum(case_sums>480)/length(case_sums)
prob


A2 <- rnorm(50000,40,6)
B2 <- rnorm(50000,90,10)

#Create a tibble (population) consisting of a single variable x 
time2 <- tibble(x=c(A2,B2))

set.seed(1994)
#Create a sample from this distribution 
case_samples2 <- replicate(1000,sample_n(time2,5,replace=T))

#Calculate the means
case_sums2 <- map_dbl(case_samples2,sum)

#Create histogram of sampling distribution of the 1000 means
ggplot(tibble(case_sums2),aes(x=case_sums2))+geom_histogram(bins = 15)

prob2 <- sum(case_sums2>480)/length(case_sums2)
prob2



#Part B:
  
  
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)

arrests <- read_csv("Berkeley_PD_Log_-_Arrests.csv")
head(arrests)

weight <- tibble(x=arrests$Weight)
ggplot(weight,aes(x=x))+geom_histogram()

set.seed(1994)
#Create a sample from this distribution 
weight_samples <- replicate(1000,sample_n(weight,100,replace=T))

#Calculate the means
weight_IQR <- map_dbl(weight_samples,IQR,na.rm=T)

#Create histogram of sampling distribution of the 1000 means
ggplot(tibble(weight_IQR), aes(x=weight_IQR))+geom_histogram(bins = 15)

#Confidence interval = statistic +/- 1.96*standard error
max <- mean(weight_IQR) + 1.96*sd(weight_IQR)
min <- mean(weight_IQR) - 1.96*sd(weight_IQR)
print(paste("It is reasonable to assume with 95% confidence that the true IQR for the subjects' body weight would be between", round(min,2), "and", round(max,2),"pounds."))

#Using the quantile function:
max2 <- quantile(weight_IQR,prob=c(0.025))
min2 <- quantile(weight_IQR,prob=c(0.975))
print(paste("We are 95% confident that the true IQR for the subjects' body weight would be between", round(max2,3), "and", round(min2,3)))

