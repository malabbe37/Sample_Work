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
skulls2 <- as.data.frame(lapply(skulls,scale))
glimpse(skulls2)

#4.
#Set the seed
set.seed(1842)

#Try multiple values of K 
k_ss <- rep(0,9)
for(i in 1:9){
  skull_clust <- kmeans(skulls2,centers=i,nstart=30)
  k_ss[i] <- skull_clust$tot.withinss
}

#Plot to see the change within total sum of squares
plot(k_ss)

#Utilizing the "elbow" of the graph, the ideal value 
#of K I will use is 4. As we can see from the graph,
#this is the point at which the value starts to taper
#off (from 4 to 5 only sees minimal reduction in k_ss).

#5.
#Use 4 clusters!
skull_clust <-  kmeans(skulls2,centers=4,nstart=30)
sclust <- as.factor(skull_clust$cluster)
skull_plot1 <- ggplot(skulls,aes(x=GOL))+geom_boxplot()
skull_plot1 <- skull_plot1+aes(color=sclust)
skull_plot1

#6.
#Show the total within sum of squares 
skull_clust$tot.withinss

#7.
#Create complete linkage hierarchical clustering 
skull_dist <- dist(skulls2)
skull_hclust <- hclust(skull_dist,method="complete")
plot(skull_hclust)

#8.
#Cut the tree to K=4
skull_cluster <- cutree(skull_hclust,k=4)

#Create side-by-side boxplots of the GOL variable 
skull_plot2 <- ggplot(skulls,aes(x=GOL))+geom_boxplot()
skull_plot2 <- skull_plot2+aes(color=as.factor(skull_cluster))
skull_plot2

#9.
#Set the seed
set.seed(1842)

#Create average linkage hierarchical clustering
skull_hclust2 <- hclust(skull_dist,method="average")
plot(skull_hclust2)

#10.
#Cut the tree to K=4
skull_cluster2 <- cutree(skull_hclust2,k=4)

#Create side-by-side boxplots of the GOL variable 
skull_plot3 <- ggplot(skulls,aes(x=GOL))+geom_boxplot()
skull_plot3 <- skull_plot3+aes(color=as.factor(skull_cluster2))
skull_plot3

