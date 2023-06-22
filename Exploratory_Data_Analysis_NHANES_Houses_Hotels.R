# Name: Michael Labbe
# Date: 01/16/23
# Goal of Program: Exploratory Data Analysis
# Data Files Used: NHANES, Houses, Hotels

# Part A:

install.packages("NHANES")
library(NHANES)
library(tidyverse)
library(dplyr)

?NHANES

glimpse(NHANESraw)

ggplot(data = NHANESraw, aes(x = Age, y = Height))+geom_point()

# This scatterplot shows a strong, positive, 
# linear association between age and height between 
# the age of 1 and ~18 years old. There don't appear 
# to be any outliers in the data. There also seems to
# be a weak, negative, linear relationship between 
# the age of ~18 to 80. 

NHANESraw2 = filter(NHANESraw, Age >= 18)

ggplot(data = NHANESraw2, aes(x = Age, y = Height))+geom_point()

# Here, we can see more clearly that there exists 
# a weak, negative, linear relationship between the 
# age of ~18 to 80.

# histogram plot of height by gender
ggplot(data = NHANESraw, aes(x = Height, color = Gender))+geom_histogram()
# get male heights only and find standard deviation
NHANESmale = filter(NHANESraw, Gender == "male")
sd(na.omit(NHANESmale$Height))
# get female heights only and find standard deviation
NHANESfemale = filter(NHANESraw, Gender == "female")
sd(na.omit(NHANESfemale$Height))

# The shape of the data seems to follow a left skewed,
# bell-shaped pattern for both male and female heights.
# However, the center of the male heights hovers
# around 175 cm whereas the center of the female 
# heights hovers around 162 cm. The spread of male 
# heights spans from ~78 to ~202 cm with a standard 
# deviation of 25.56 cm. The spread of female heights
# spans from ~80 to ~185 cm with a standard deviation
# of 20.49 cm. 

# filter the data to only show ages 18-69
NHANESraw3 = filter(NHANESraw, Age >= 18 & Age <= 69)
# select only hard drugs and gender columns
justharddrugsandgender <- select(NHANESraw3, HardDrugs, Gender)
# omit na values and store data in new object
justharddrugsandgender2 = na.omit(justharddrugsandgender)
# create bar plot with gender as the color aesthetic
ggplot(data = justharddrugsandgender2, aes(x = HardDrugs, color = Gender))+geom_bar()
# create bar plot with HardDrugs as the color aesthetic
ggplot(data = justharddrugsandgender2, aes(x = Gender, color = HardDrugs))+geom_bar()

# The second bar chart better represents the relationship 
# between "Gender" and "HardDrugs because you can more 
# clearly see the difference between males and females 
# with respect to deciding whether or not to do hard drugs.
# Here, we see that males were nearly twice more likely to 
# use hard drugs than females. 

# select only hard drugs and gender columns
justregularmarijandgender <- select(NHANESraw3, RegularMarij, HardDrugs)
# omit na values and store data in new object
justregularmarijandgender2 = na.omit(justregularmarijandgender)
# create bar plot with gender as the color aesthetic
ggplot(data = justregularmarijandgender2, aes(x = RegularMarij, color = HardDrugs))+geom_bar()

# Here, "RegularMarij" is more predictive of hard drug use than
# "Gender". This is because there were over double the amount
# hard drug users present in regular marijuana users than in non-
# regular marijuana users. 

# Part B:

Houses <- read_csv("Houses.csv")

glimpse(Houses)

ggplot(data = Houses, aes(x = Total))+geom_histogram()
median(Houses$Total)

# The shape of the data is heavily right skewed with a center
# (median) falling at $166,493.5 and a variability spanning 
# from 0 to nearly 5 million dollars. There appear to be two
# outliers in the data, both falling over $1,000,000 in total
# value.

# Filter the outliers (two houses > 1 million dollars)
ExpensiveHouses = filter(Houses, Total >= 1000000)
# Select total cost and address for outliers
ExpensiveHouses <- select(ExpensiveHouses, Total, Address)
ExpensiveHouses
# filter out houses under 1 million dollars
NonExpensiveHouses = filter(Houses, Total < 1000000)
# Get the mean acres and squarefootage to use as comparison statistic
mean(NonExpensiveHouses$Acres)
mean(NonExpensiveHouses$SQFT)

# 2029 Alston Avenue is a house that sits on a nearly 40-acre
# property which is 39 more acres than the average (~0.4) 
# which explains why the price tag is so high. This disparity
# is further displayed in the below visualization:

ggplot(data = Houses, aes(x = Acres))+geom_histogram()

# As for 2211 Byrd Street, this property contains a house 
# with over 4,500 sqft of living space which is ~2,800 sqft 
# more than the average house living space which, again, 
# explains the outlier. 

# Part C:
  
Hotels <- read_csv("hotels.csv")

glimpse(Hotels)

# Bar Charts (Categorical Variables):

# Hotel vs Arrival Date
ggplot(data = Hotels, aes(x = arrival_date, color = hotel))+geom_bar()

# Hotel vs Assigned Room Type
ggplot(data = Hotels, aes(x = assigned_room_type, color = hotel))+geom_bar()

# Hotel vs Booking Date
ggplot(data = Hotels, aes(x = booking_date, color = hotel))+geom_bar()

# Hotel vs Customer Type
ggplot(data = Hotels, aes(x = customer_type, color = hotel))+geom_bar()

# Hotel vs Market Segment
ggplot(data = Hotels, aes(x = market_segment, color = hotel))+geom_bar()

# Hotel vs Meal
ggplot(data = Hotels, aes(x = meal, color = hotel))+geom_bar()

# Hotel vs Reserved Room Type
ggplot(data = Hotels, aes(x = reserved_room_type, color = hotel))+geom_bar()

# Hotel vs Reservation Status
ggplot(data = Hotels, aes(x = reservation_status, color = hotel))+geom_bar()

# Boxplots/Histograms (Quantitative Variables)

# Hotel vs Adults
ggplot(data = Hotels, aes(x = adults, color = hotel))+geom_boxplot()

# Hotel vs Children
ggplot(data = Hotels, aes(x = children, color = hotel))+geom_boxplot()

# Hotel vs Previous Bookings Not Canceled
ggplot(data = Hotels, aes(x = previous_bookings_not_canceled, color = hotel))+geom_boxplot()

# Hotel vs Previous Cancellations
ggplot(data = Hotels, aes(x = previous_cancellations, color = hotel))+geom_boxplot()

# Hotel vs Rate
ggplot(data = Hotels, aes(x = rate, color = hotel))+geom_boxplot()

# Hotel vs Required Car Parking Spaces
ggplot(data = Hotels, aes(x = required_car_parking_spaces, color = hotel))+geom_histogram()

# Hotel vs Stays in Week Nights
ggplot(data = Hotels, aes(x = stays_in_week_nights, color = hotel))+geom_boxplot()

# Hotel vs Stays in Weekend Nights
ggplot(data = Hotels, aes(x = stays_in_weekend_nights, color = hotel))+geom_boxplot()

# Hotel vs Total of Special Requests
ggplot(data = Hotels, aes(x = total_of_special_requests, color = hotel))+geom_boxplot()

# The city hotel appears to have a much larger 
# proportion of its customers with room type A 
# than that of the resort hotel. The city hotel 
# also appears to have a much larger proportion of 
# self-catering vs that of the resort hotel. 
# The resort hotel is the only hotel that offers 
# full board meals (breakfast, lunch and dinner).
# The city hotel is the only hotel with room type
# P while the resort hotel is the only hotel 
# with room types H and L. 
# The city hotel had a larger proportion of 
# cancellations than the resort hotel.
# The number of adults at the resort hotel showed
# a greater number of outliers with the majority
# of adults for both the city and the resort hotel 
# averaging around 2. 
# The rate of the city hotel had both a higher mean
# and a smaller variability than the resort hotel 
# indicating that the resort hotel depended more on
# the type of room/upgrades for the corresponding rate.
# The resort hotel had more stays on average during 
# week nights.

Hotels %>%
  group_by(country) %>%
  summarize(count=n()) %>% arrange(desc(count))

Hotels2 <- filter(Hotels, country %in% c('Portugal', 'United Kingdom', 'France', 'Spain', 'Germany', 'Italy', 'Ireland', 'Belgium', 'China', 'Brazil'))

ggplot(data = Hotels2, aes(x = country, color = hotel))+geom_bar()

# Ireland and United Kingdom customers were shown to favor
# the resort hotel over the city hotel. 
# The rest of the countries (with the exception of Spain)
# were shown to favor the city hotel over the resort hotel.