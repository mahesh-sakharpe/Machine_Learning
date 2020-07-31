# Load calories_consumed.csv dataset
install.packages("readr")
library(readr)
cal_consumed <- read.csv(file.choose())
View(cal_consumed)
cal_consumed

# Exploratory data analysis #
summary(cal_consumed)

# Scatter plot #
plot(cal_consumed$Weight.gained..grams., cal_consumed$Calories.Consumed)    # plot(X,Y)
attach(cal_consumed)

# Correlation Coefficient (r) #
cor(Weight.gained..grams., Calories.Consumed)

# Simple Linear Regression Model #
reg <- lm(Calories.Consumed ~ Weight.gained..grams.)                    #lm(Y ~ X)
summary(reg)
pred <- predict(reg)
pred
reg$residuals
sum(reg$residuals)
mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(cal_consumed))  #RMSE
sqrt(mean(reg$residuals^2))
confint(reg,level = 0.95)
predict(reg,interval="predict")
predict


## ggplot for adding regresion line for data ##
library(ggplot2)
ggplot(data = cal_consumed, aes(x = Weight.gained..grams., y = Calories.Consumed)) +
  geom_point(color='blue') +
  geom_line(color='red',data = cal_consumed, aes(x=Weight.gained..grams., y=pred))

## Logrithamic Model/ Transformation ##
# x = log(Weight.gained..grams.); y = Calories.Consumed
plot(log(Weight.gained..grams.), Calories.Consumed)
cor(log(Weight.gained..grams.), Calories.Consumed)
reg_log <- lm(Calories.Consumed ~ log(Weight.gained..grams.))         ## lm(Y ~ X)
summary(reg_log)
predict(reg_log)
reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(cal_consumed))    ###RMSE
confint(reg_log, level = 0.95)
predict(reg_log,interval = "confidence")

## Exponential Transformation ##
## x =Weight.gained..grams. and y = log(Calories_Consumed)
plot(Weight.gained..grams., log(Calories.Consumed))
cor(Weight.gained..grams., log(Calories.Consumed))
reg_exp <- lm(log(Calories.Consumed) ~ Weight.gained..grams.)     ## lm(log(Y) ~ X)
summary(reg_exp)
reg_exp$residuals
sqrt(mean(reg_exp$residuals^2))
logat <- predict(reg_exp)
logat
at <- exp(logat)
error = cal_consumed$Calories.Consumed   -Calories.Consumed 
error
sqrt(sum(error^2)/nrow(cal_consumed))
confint(reg_exp,level = 0.95)
predict(reg_exp,interval = "confidence")
