## Load package 'readxl' to load data from xlsx file
library(readxl)
airline_data <- read.csv(file.choose())
View(airline_data)
normalized_data <- scale(airline_data[,2:12])
d <- dist(normalized_data,method = "euclidean") ## Distance matrix
fit <- hclust(d,method = "complete")

plot(fit)          #### display Dendrogram
plot(fit,hang = -1)
groups <- cutree(fit,k=100)

rect.hclust(fit,k=100,border="red")

membership <- as.matrix(groups)
final <- data.frame(airline_data,membership)
View(final)
final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
View(final1)
