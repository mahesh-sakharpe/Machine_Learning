install.packages("recommenderlab", dependencies = TRUE)
install.packages("Matrix")
library(Matrix)
library("recommenderlab")
library(caTools)

# movie rating data
joke_rate_data <- read.csv(file.choose())
View(joke_rate_data)

## metdata about the variable
str(joke_rate_data)
joke_rate_data <- apply(joke_rate_data,MARGIN = 2,
                        function(x)
                          {ifelse(x==99,NA,x)})
View(joke_rate_data)
str(joke_rate_data)


## rating distribution
hist(joke_rate_data)
rug(joke_rate_data,side = 1)

## the data type should be realRatingMatrix in order to build recommendation engine
joke_rate_data_matrix <- as(joke_rate_data, 'realRatingMatrix')

## Popularity based
joke_recomm_model1 <- Recommender(joke_rate_data_matrix, method="POPULAR")
joke_recomm_model1

## Predictions for two users
recommended_items1 <- predict(joke_recomm_model1, joke_rate_data_matrix[], n=5)
as(recommended_items1, "list")

## popularity model recommends the same movies for all users, we need to improve our model
## user based collaborative filtering
joke_recomm_model2 <- Recommender(joke_rate_data_matrix, method="UBCF")
joke_recomm_model2

## Predictions for two users
recommended_items2 <- predict(joke_recomm_model2, joke_rate_data_matrix[], n=3)
as(recommended_items2, "list")
