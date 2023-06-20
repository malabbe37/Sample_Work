library(tidyverse)

#1.
#Read in data
skulls <- read_csv("skulls.csv")
glimpse(skulls)

#2.
#Omit NAs
skulls <- na.omit(skulls)
nrow(skulls)

#3.
#Rescale the data using z-scores
skulls <- as.data.frame(lapply(skulls,scale))
glimpse(skulls)

#Construct PCA of data
skulls_pca <- prcomp(skulls,scale=T)

#Look at the principal components rotation matrix
skulls_pca$rotation

#4.
#Produce a screenplot of the variance proportion vs PC 
plot(skulls_pca,type="l")

#5.
#Looking at the screeplot, 3 components looks like the appropriate number of principal components
#to use in our data. This is because a larger proportion of variance can be explained by the first 
#three principal components than the rest. As we can see, the "elbow" of the screeplot falls right 
#at that third principal component on the x axis. If we'd like to reduce our dimensions 
#from 12 to 3 by using the first three principal components in place of using all of the 
#predictor variables, we would still retain 74.21% of the variability in the data.

#6.
#Summary of principal components object
summary(skulls_pca)

#7.
#If the researcher would like to use the principal components that hold a total of 95%
#of the variance in the original data, she will need to use 8 principal components. 
#The way we can see this is simply by looking at the summary output data in the 
#"Cumulative Proportion" row and continue to go to the next successive PC column 
#until at least 0.95 is reached. Whatever column you end up at reveals the minimum 
#principal components needed to achieve the desired variance retention. 

#8.
#The second principal component is contrasting the measurement of the Nasion-prosthion Height,
#Biasterionic Breadth, Biauricular Breadth and the Maximum Cranial Breadth with everything 
#else. I know this based on the negative signs in the output of the list of variables under 
#each principal component. These negative signs indicate that there is a negative correlation 
#between the Nasion-prosthion Height, Biasterionic Breadth, Biauricular Breadth and the 
#Maximum Cranial Breadth measurements in comparison to everything else. 

