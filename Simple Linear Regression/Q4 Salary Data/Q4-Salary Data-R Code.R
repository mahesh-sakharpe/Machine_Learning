# Load Salary_Data.csv dataset

library(readr)
salary_data <- read.csv(file.choose())
View(salary_data)
salary_data

# Exploratory data analysis #
summary(salary_data)

# Scatter plot #
plot(salary_data$YearsExperience, salary_data$Salary)    # plot(X,Y)
attach(salary_data)

# Correlation Coefficient (r) #
cor(YearsExperience, Salary)

# Simple Linear Regression Model #
reg <- lm(Salary ~ YearsExperience)                    #lm(Y ~ X)
summary(reg)
pred <- predict(reg)
pred
reg$residuals
sum(reg$residuals)
mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(salary_data))  #RMSE
sqrt(mean(reg$residuals^2))
confint(reg,level = 0.95)
predict(reg,interval="predict")
predict


## ggplot for adding regresion line for data ##
library(ggplot2)
ggplot(data = salary_data, aes(x = YearsExperience, y = Salary)) +
  geom_point(color='blue') +
  geom_line(color='red',data = salary_data, aes(x=YearsExperience, y=pred))

## Logrithamic Model/ Transformation ##
# x = log(YearsExperience); y = Salary
plot(log(YearsExperience), Salary)
cor(log(YearsExperience), Salary)
reg_log <- lm(Salary ~ log(YearsExperience))         ## lm(Y ~ X)
summary(reg_log)
predict(reg_log)
reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(salary_data))    ###RMSE
confint(reg_log, level = 0.95)
predict(reg_log,interval = "confidence")

