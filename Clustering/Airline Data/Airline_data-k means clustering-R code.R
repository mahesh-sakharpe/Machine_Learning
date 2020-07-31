#install.packages("plyr")
library(plyr)
airline_data <- read.csv(file.choose())
View(airline_data)
plot(airline_data)
km <- kmeans(airline_data,8) ## kmeans clustering - 4 clusters;k ~ sqrt(n/2)
km

install.packages("animation")
library(animation)
km <- kmeans.ani(airline_data,8)
km$cluster
km$withinss
km$centers
km_8 <- kmeans(airline_data,10)
wss<-(nrow(airline_data)-1)*sum(apply(airline_data,2,var))
for(i in 2:10) wss[i]<-sum(kmeans(airline_data,centers = i)$withinss)        
plot(1:10,wss,type = "b",xlab = "No of clusters",ylab = "Avg distance")
