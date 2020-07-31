###### Multi-Linear Regression #####

#Question No 3 ----

library(ggplot2)
library(car)

Corol <- read.csv(file.choose()) 
str(Corol)
Corolla<-Corol[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]

summary(Corolla)
write.csv(summary(Corolla),"summary_toyota.csv")
boxplot(Corolla,col = 1:9)
boxplot(scale(Corolla),ylim=c(-5,5))
# Checking out the correlations and the Pair plot
cor(Corolla)
write.csv(cor(Corolla),'cor_coro.csv')
# no pair of variables except Age and Price is highly correlated
pairs(Corolla)

#Target Price

#Building my first model

model_T_1 <- lm(Price~.,data = Corolla)
summary(model_T_1) # R2 = 0.8638 ........... cc and doors are seems to be insignificant in our model
pred_T_1 <- predict(model_T_1,Corolla)
cor(pred_T_1,Corolla$Price) # We are getting correlation  as 0.9293884
rmse_T_1 <- mean(model_T_1$residuals^2)^.5 ;rmse_T_1# and RMSE value as 1338.258
avPlots(model_T_1)
# Check for the Influence plot
influenceIndexPlot(model_T_1,grid = T,id = list(col="red",cex=1.5))
influencePlot(model_T_1,id=list(col="red"))
influence_index <- as.integer(rownames(influencePlot(model_T_1))) # Ok, here we are getting 3 influence index in our model
vif(model_T_1) # ok, in our model we can say there is no colinearity problem exists as all the vif values are less than 10
influenceIndexPlot(model_T_1,grid = T,id = list(col="red",cex=2))


#Building my Second model
df_Corola <- Corolla[-c(influence_index),]

model_T_2 <- lm(Price~.,data = df_Corola)
summary(model_T_2) # R2 = 0.8852... cc and doors are now significant
pred_T_2 <- predict(model_T_2,df_Corola)
cor(pred_T_2,df_Corola$Price) # We are getting correlation  as 0.9408425
rmse_T_2 <- mean(model_T_2$residuals^2)^.5;rmse_T_2 # and RMSE value as 1227.474

avPlots(model_T_2)
