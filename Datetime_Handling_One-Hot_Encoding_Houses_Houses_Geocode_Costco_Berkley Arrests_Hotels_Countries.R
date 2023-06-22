# Name: Michael Labbe
# Date: 01/25/23
# Goal of Program: Datetime Handling, One-Hot Encoding, etc.
# Data Files Used: Houses, Houses Geocode, Costco, Berkley Arrests, Hotels, Countries

# Part A:

library(readr)
library(tidyverse)
library(stringr)
library(ggplot2)
library(ggmap)
library(lubridate)
library(dplyr)
library(tidygeocoder)

#Read in the Houses data
Houses <- read_csv("Houses.csv")

#Read in the Houses Geocode data
Houses_Geocode <- read_csv("houses_geocode.csv")

#Join Houses and Houses Geocode data sets together
Houses <- left_join(Houses,Houses_Geocode)

#Look at the data
glimpse(Houses)

#Identify a bounding box
my_left=min(Houses$lon)-.01
my_top=max(Houses$lat)+.01
my_right=max(Houses$lon)+.01
my_bot=min(Houses$lat)-.01

#Pull a map from stamen maps
sb <- get_stamenmap(bbox=c(left=my_left,bottom=my_bot,
                           right=my_right,top=my_top),zoom=12,"toner")

my_map <- ggmap(sb)
#Map of houses
houses_map <- my_map+geom_point(data=Houses,aes(y=lat,x=lon),size=3)
houses_map

#Add the houses to our map
story_map <- my_map+geom_point(data=Houses,aes(y=lat,x=lon,col=factor(Story)),size=3)
story_map

#Mutate Houses data frame to create new feature "Year_Built", segmenting "Year" variable into categories
Houses <- mutate(Houses, 
                    Year_Built = cut(
                      Year,
                      breaks = c(0, 1950, 1970, 1990, Inf),
                      labels = c("<1950", "1950-1970", "1970-1990", ">1990"),
                      right = FALSE
                    )
)

#Map of "Year" variable color-coded
year_map <- my_map+geom_point(data=Houses,aes(y=lat,x=lon,col=factor(Year_Built)),size=3)
year_map

#Map of "Year" variable with facet_wrap year categories
Year_map <- my_map+geom_point(data=Houses,aes(y=lat,x=lon,col=factor(Year_Built)),size=3) + facet_wrap(~Year_Built)
Year_map

#The trend that can be seen in the facet wrap mapping of the year categories is that
#older homes tend to have been built more in the center of the Wake County area and 
#newer homes tend to have been built further out from the center of the Wake County 
#area. This would seem to indicate that as time went on and the demand for homes 
#went up as more people moved in, the homes were built further and further out.

#Read in the Costco data
Costco <- read_csv("Costco.csv")

library(tidygeocoder)

#Send the addresses to the geocoder
my_geos <- geo(street=Costco$Address,
               city=Costco$City, state=Costco$State, method="osm")

glimpse(my_geos)

#rename the variables
my_geos <- my_geos%>% select(street,lat,lon=long)

#Map of Year variable (without facet_wrap) with Costco Warehouses
Costco_map <- my_map+geom_point(data=Houses,aes(y=lat,x=lon,col=factor(Year_Built)),size=3)+geom_point(data=my_geos,aes(y=lat,x=lon),shape=18,color=15,size=7)
Costco_map

#Recreate previous map using Terrain version of Stamen Map
sb <- get_stamenmap(bbox=c(left=my_left,bottom=my_bot,
                           right=my_right,top=my_top),zoom=12,"terrain")
my_map <- ggmap(sb)
Costco_map <- my_map+geom_point(data=Houses,aes(y=lat,x=lon,col=factor(Year_Built)),size=3)+geom_point(data=my_geos,aes(y=lat,x=lon),shape=18,color=15,size=7)
Costco_map

#Part B

#Read in the Berkley Arrests data
Berkley_Arrests <- read_csv("Berkeley_PD_Log_-_Arrests.csv")
Berkley_Arrests

#Convert Date of Birth and Data and Time (of Arrest) variables to date-times
#Data and Time (of Arrest)
Berkley_Arrests$`Date and Time` <- mdy_hms(Berkley_Arrests$`Date and Time`,tz="America/Los_Angeles")
class(Berkley_Arrests$`Date and Time`)
#Date of Birth
Berkley_Arrests$`Date of Birth` <- mdy(Berkley_Arrests$`Date of Birth`,tz="America/Los_Angeles")
class(Berkley_Arrests$`Date of Birth`)

#See the data
glimpse(Berkley_Arrests)

#Which day of the week did most arrests occur?
ggplot(Berkley_Arrests,aes(x=wday(Berkley_Arrests$`Date and Time`, label=T)))+geom_bar()
#Wednesday!

#Age of individuals at the time of arrest?
Berkley_Arrests <- Berkley_Arrests%>%mutate(`Arrest Age`=floor(interval(`Date of Birth`,`Date and Time`)/dyears(1)))

summary(Berkley_Arrests$`Arrest Age`)

#Age differs by >1 year from age recorded by Officer
Berkley_Arrests%>%filter(abs(`Arrest Age`-Age)>1)%>%select(Subject)

#Part C

#Read in the Hotels data
Hotels <- read_csv("hotels.csv")

#One-hot encode the reservation status variable into a new variable
Hotels$Cancelled <- 1
Hotels$Cancelled <- ifelse(Hotels$reservation_status == "Check-Out" | Hotels$reservation_status == "No-Show", 0, Hotels$Cancelled)

#Graph of proportion of canceled bookings at the two hotels (city and resort)
ggplot(
  data = Hotels,
  aes(x = Cancelled, fill = hotel)
) + geom_bar()

#Create a new feature that is the week number for the arrival date
Hotels$week_of_arrival <- week(Hotels$arrival_date)

#Number of bookings at each hotel for each of the 52 weeks
ggplot(
  data = Hotels,
  aes(x = week, fill = hotel)
) + geom_bar()


#Calculate average of variable "Rate" for each of 52 weeks (Resort and City)
Average <- Hotels %>% group_by(week,hotel) %>% summarize(av_rate = mean(rate))
Average
#Plot that shows relationship between rate average and week (Resort and City)
ggplot(
  data = Average,
  aes(x = week, y = av_rate, color = hotel)
) + geom_point()

#The rates for city hotels are greater than the rates for the resort hotels
#between week ranges 1-25 and 35-52. Between the weeks of 26-34, the rates for
#resort hotels greatly exceed that of the city hotels (summer months). There 
#exists a possible outlier in the data at week ~50 where we see the resort hotel 
#rate is much higher than the city hotel rate. It could be the case that there 
#is a spike around the week of Christmas that causes this as well. 

#Calculate average of variable "Canceled" for each of 52 weeks (Resort and City)
Average2 <- Hotels %>% group_by(week,hotel) %>% summarize(av_can = mean(Cancelled))
Average2
#Plot that shows relationship between cancelled average and week (Resort and City)
ggplot(
  data = Average2,
  aes(x = week, y = av_can, color = hotel)
) + geom_point()

#There seems to be an overall lower number of booking cancellations on average
#throughout the entire year among resort hotels in comparison to that of city hotels. 
#The average number of cancellations among city hotels tends to stay relatively stable 
#throughout the year while the resort hotels tend to have less cancellations on 
#average between weeks 1-20 and 40-52. 

#Create a new feature “Lead Time” (# of days between booking and arrival date)
Hotels <- Hotels%>%mutate(`Lead Time`=interval(booking_date,arrival_date)/dyears(1))

#Create boxplots of your new variable that allow comparison of the two hotels
ggplot(
  data = Hotels,
  aes(x = `Lead Time`, y = hotel)
) + geom_boxplot() 

#Create one-hot encoded variable "Repeat Customer” 
Hotels = Hotels %>% 
  mutate(`Repeat Customer` = ifelse(previous_bookings_not_canceled >= 1, 1, 0))  

#Create plot that contrasts the two hotels on Repeat Customer
ggplot(
  data = Hotels,
  aes(x = `Repeat Customer`)
) + geom_bar() + facet_wrap(~hotel)

#Create a one-hot encoded variable called “Portugal”
Hotels = Hotels %>% 
  mutate(Portugal = ifelse(country == "Portugal", 1, 0))

#Create plot that contrasts the two hotels on Repeat Customer
ggplot(
  data = Hotels,
  aes(x = Portugal)
) + geom_bar() + facet_wrap(~hotel)

#Read in the Countries data set
Countries <- read_csv("countries.csv")

#Join the Countries dataset with the Hotels data
Hotels <- left_join(Hotels, Countries, by = c("country" = "Country"))
glimpse(Hotels)

#Europe one-hot encoded variable
Hotels = Hotels %>% 
  mutate(Europe = ifelse(Continent == "Europe", 1, 0))

#Create plot that contrasts the two hotels on Europe
ggplot(
  data = Hotels,
  aes(x = Europe)
) + geom_bar() + facet_wrap(~hotel)

