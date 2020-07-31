# Load delivery_time.csv dataset

library(readr)
del_time <- read.csv(file.choose())
View(del_time)
del_time

# Exploratory data analysis #
summary(del_time)

# Scatter plot #
plot(del_time$Delivery.Time, del_time$Sorting.Time)    # plot(X,Y)
attach(del_time)

# Correlation Coefficient (r) #
cor(Delivery.Time, Sorting.Time)

# Simple Linear Regression Model #
reg <- lm(Delivery.Time ~ Sorting.Time)                    #lm(Y ~ X)
summary(reg)
pred <- predict(reg)
pred
reg$residuals
sum(reg$residuals)
mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(del_time))  #RMSE
sqrt(mean(reg$residuals^2))
confint(reg,level = 0.95)
predict(reg,interval="predict")
predict


## ggplot for adding regresion line for data ##
library(ggplot2)
ggplot(data = del_time, aes(x = Delivery.Time, y = Sorting.Time)) +
  geom_point(color='blue') +
  geom_line(color='red',data = del_time, aes(x=Delivery.Time, y=pred))

## Logrithamic Model/ Transformation ##
# x = log(Delivery.Time); y = Sorting.Time
plot(log(Delivery.Time), Sorting.Time)
cor(log(Delivery.Time), Sorting.Time)
reg_log <- lm(Sorting.Time ~ log(Delivery.Time))         ## lm(Y ~ X)
summary(reg_log)
predict(reg_log)
reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(del_time))    ###RMSE
confint(reg_log, level = 0.95)
predict(reg_log,interval = "confidence")

## Exponential Transformation ##
## x =Delivery.Time and y = log(Sorting.Time)
plot(Delivery.Time, log(Sorting.Time))
cor(Delivery.Time, log(Sorting.Time))
reg_exp <- lm(log(Sorting.Time) ~ Delivery.Time)     ## lm(log(Y) ~ X)
summary(reg_exp)
reg_exp$residuals
sqrt(mean(reg_exp$residuals^2))
logat <- predict(reg_exp)
logat
at <- exp(logat)
error = del_time$Sorting.Time   -Sorting.Time 
error
sqrt(sum(error^2)/nrow(del_time))
confint(reg_exp,level = 0.95)
predict(reg_exp,interval = "confidence")

############################
## Polynomial model with 2 degree ( Quadratic model)

plot(Delivery.Time, Sorting.Time)
plot(Delivery.Time*Delivery.Time, Sorting.Time)
cor(Delivery.Time*Delivery.Time, Sorting.Time)
plot(Delivery.Time*Delivery.Time, log(Sorting.Time))
cor(Delivery.Time, log(Sorting.Time))
cor(Delivery.Time*Delivery.Time, log(Sorting.Time))
# lm(Y ~ X + I(X*X) +....+ I(X*X*X))
reg2degree <- lm(log(Sorting.Time) ~ Delivery.Time + I(Delivery.Time*Delivery.Time))
summary(reg2degree)
logpol <- predict(reg2degree)
expy <- exp(logpol)
expy
err = del_time$Sorting.Time  -expy
err
sqrt(sum(err^2)/nrow(del_time))      #RMSE
confint(reg2degree, level = 0.95)
predict(reg2degree,interval = "confidence")

## Visualization

ggplot(data = del_time, aes(x = Delivery.Time + I(Delivery.Time^2), y = log(Sorting.Time))) +
  geom_point(color='blue') + 
  geom_line(color='red',data = del_time, aes(x=Delivery.Time+I(Delivery.Time^2), y=logpol))

### Polynomial model with 3 degree ###

reg3degree <- lm(log(Sorting.Time)~Delivery.Time + I(Delivery.Time*Delivery.Time) + I(Delivery.Time*Delivery.Time*Delivery.Time))
summary(reg3degree)
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)

## Visualization  
ggplot(data = del_time, aes(x = Delivery.Time + I(Delivery.Time^2) + I(Delivery.Time^3), y= Sorting.Time))+
  geom_point(color='blue') +
  geom_line(color='red', data = del_time, aes(x=Delivery.Time+I(Delivery.Time^2)+I(Delivery.Time^3), y=expy3))
