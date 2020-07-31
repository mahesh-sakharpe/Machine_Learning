## Load package 'readxl' to load data from xlsx file
library(readxl)
crime_data <- read.csv(file.choose())
View(crime_data)
normalized_data <- scale(crime_data[,2:5])
d <- dist(normalized_data,method = "euclidean") ## Distance matrix
fit <- hclust(d,method = "complete")

plot(fit)          #### display Dendrogram
plot(fit,hang = -1)
groups <- cutree(fit,k=8)

rect.hclust(fit,k=8,border="red")

membership <- as.matrix(groups)
final <- data.frame(crime_data,membership)
View(final)
final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
View(final1)
