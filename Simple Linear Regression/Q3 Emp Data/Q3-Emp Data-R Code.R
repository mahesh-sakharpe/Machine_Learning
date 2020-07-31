# Load Emp_data.csv dataset

library(readr)
emp_data <- read.csv(file.choose())
View(emp_data)
emp_data

# Exploratory data analysis #
summary(emp_data)

# Scatter plot #
plot(emp_data$Salary_hike, emp_data$Churn_out_rate)    # plot(X,Y)
attach(emp_data)

# Correlation Coefficient (r) #
cor(Salary_hike, Churn_out_rate)

# Simple Linear Regression Model #
reg <- lm(Salary_hike ~ Churn_out_rate)                    #lm(Y ~ X)
summary(reg)
pred <- predict(reg)
pred
reg$residuals
sum(reg$residuals)
mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(emp_data))  #RMSE
sqrt(mean(reg$residuals^2))
confint(reg,level = 0.95)
predict(reg,interval="predict")
predict


## ggplot for adding regresion line for data ##
library(ggplot2)
ggplot(data = emp_data, aes(x = Salary_hike, y = Churn_out_rate)) +
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=Salary_hike, y=pred))

## Logrithamic Model/ Transformation ##
# x = log(Salary_hike); y = Churn_out_rate
plot(log(Salary_hike), Churn_out_rate)
cor(log(Salary_hike), Churn_out_rate)
reg_log <- lm(Churn_out_rate ~ log(Salary_hike))         ## lm(Y ~ X)
summary(reg_log)
predict(reg_log)
reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(emp_data))    ###RMSE
confint(reg_log, level = 0.95)
predict(reg_log,interval = "confidence")

## Exponential Transformation ##
## x =Salary_hike and y = log(Churn_out_rate)
plot(Salary_hike, log(Churn_out_rate))
cor(Salary_hike, log(Churn_out_rate))
reg_exp <- lm(log(Churn_out_rate) ~ Salary_hike)     ## lm(log(Y) ~ X)
summary(reg_exp)
reg_exp$residuals
sqrt(mean(reg_exp$residuals^2))
logat <- predict(reg_exp)
logat
at <- exp(logat)
error = emp_data$Churn_out_rate   -Churn_out_rate 
error
sqrt(sum(error^2)/nrow(emp_data))
confint(reg_exp,level = 0.95)
predict(reg_exp,interval = "confidence")

############################
## Polynomial model with 2 degree ( Quadratic model)

plot(Salary_hike, Churn_out_rate)
plot(Salary_hike*Salary_hike, Churn_out_rate)
cor(Salary_hike*Salary_hike, Churn_out_rate)
plot(Salary_hike*Salary_hike, log(Churn_out_rate))
cor(Salary_hike, log(Churn_out_rate))
cor(Salary_hike*Salary_hike, log(Churn_out_rate))
# lm(Y ~ X + I(X*X) +....+ I(X*X*X))
reg2degree <- lm(log(Churn_out_rate) ~ Salary_hike + I(Salary_hike*Salary_hike))
summary(reg2degree)
logpol <- predict(reg2degree)
expy <- exp(logpol)
expy
err = emp_data$Churn_out_rate  -expy
err
sqrt(sum(err^2)/nrow(emp_data))      #RMSE
confint(reg2degree, level = 0.95)
predict(reg2degree,interval = "confidence")

## Visualization

ggplot(data = emp_data, aes(x = Salary_hike + I(Salary_hike^2), y = log(Churn_out_rate))) +
  geom_point(color='blue') + 
  geom_line(color='red',data = emp_data, aes(x=Salary_hike+I(Salary_hike^2), y=logpol))

