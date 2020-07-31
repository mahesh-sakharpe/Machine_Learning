#### Forcasting Exponential Smoothing Prediction - Plastic Sales #####

install.packages("forecast")
install.packages("fpp")
install.packages("smooth")
library(forecast)
library(fpp)
library(smooth)

library(readxl)

df_plastic <- read.csv(file.choose())
View(df_plastic)
library(tseries)

# Converting data into time series object
plastics <- ts(df_plastic$Sales,frequency = 4,start = c(49))
View(plastics)

# Dividing entire data into training and testing data
train <- amts[1:40]
test <- amts[41:60] 

# Seasonal data
# converting time series object
train <- ts(train,frequency = 4)
test <- ts(test,frequency = 4)

# Plotting time series data
plot(plastics) # Visualization shows that it has level,trend, seasonality => Additive Seasonality

# Using HoltWinters Function #
# Optimum Values
# with alpha = 0.2 which is default value
# Assuming time series data has only level parameter
# Alpha = level smoothing, Beta = Trend smoothing, Gama = Seasonality Smoothing
hw_a <- HoltWinters(train,alpha = 0.2,beta = F,gamma = F)
hw_a
hwa_pred <- data.frame(predict(hw_a,n.ahead = 4))
hwa_pred

# By looking at plot the forecasted values are not showing any characters of train data
plot(forecast(hw_a,h=4))
hwa_mape <- MAPE(hwa_pred$fit,test)*100
hwa_mape

# with alpha =0.2, beta=0.1
# Assuming time series data has level and trend parameter
hw_ab <- HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = F)
hw_ab
hwab_pred <- data.frame(predict(hw_ab,n.ahead = 4))
hwab_pred

# by looking at the plot the forecasted values are still missing some characters exhibited by train data
plot(forecast(hw_ab,h=4))
hwab_mape <- MAPE(hwab_pred$fit,test)*100
hwab_mape

# with alpha =0.2, beta =0.1, gamma=0.1
# Assuming time series data has level,trend and sesonality
hw_abg <- HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = 0.1)
hw_abg
hwabg_pred <- data.frame(predict(hw_abg,n.ahead = 4))
hwab_pred

# by looking at the plot the characters of forecasted values are closely following historical data
plot(forecast(hw_abg,h=4))
hwabg_mape <- MAPE(hwab_pred$fit,test)*100
hwabg_mape

# without optimum values
hw_na <- HoltWinters(train,beta = F,gamma = F)
hw_na
hwna_pred <- data.frame(predict(hw_na,n.ahead = 4))
hwna_pred
plot(forecast(hw_na,h=4))
hwna_mape <- MAPE(hwna_pred$fit,test)*100
hwna_mape

hw_nab <- HoltWinters(train,gamma = F)
hw_nab
hwnab_pred <- data.frame(predict(hw_nab,n.ahead = 4))
hwnab_pred
plot(forecast(hw_nab,h=4))
hwnab_mape <- MAPE(hwnab_pred$fit,test)*100
hwnab_mape

hw_nabg <- HoltWinters(train)
hw_nabg
hwnabg_pred <- data.frame(predict(hw_nabg,n.ahead = 4))
hwnabg_pred
plot(forecast(hw_nabg,h=4))
hwnabg_mape <- MAPE(hwnabg_pred$fit,test)*100
hwnabg_mape
