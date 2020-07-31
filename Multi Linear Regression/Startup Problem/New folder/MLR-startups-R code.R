####### Multi-Linear Regression ######
#Q1----
"Prepare a prediction model for profit of 50_startups data.
 Do transformations for getting better predictions of profit and
 make a table containing R^2 value for each prepared model."

library(ggplot2)
library(car)

Startups <- read.csv(file.choose())         
head(Startups)
summary(Startups)
boxplot(Startups[,-4],col = c("lightblue","pink","lightgreen",'Orange'))
write.csv(summary(Startups),"Startupsummary.csv")
attach(Startups)
pairs(Startups[,-4],pch=20,cex=1.5,col=State);par(xpd = TRUE)
# Red = Florida, Black = California, Green = New York
legend(x = "bottomr",legend = unique(State),fill = State,cex=0.63,bg="transparent",bty = "n");par(xpd = F)
cor(Startups[,-4])
write.csv(cor(Startups[,-4]),"correlation.csv")
attach(Startups)

#First model ----

#Lets build a simple model
model.S <- lm(Profit~R.D.Spend+Administration+Marketing.Spend) 
summary(model.S) # R² = 0.9507
rmse_1 <- mean(model.S$residuals^2)^.5 # RMSE value is 8855.344
pred_S1 <- predict(model.S,newdata = Startups)
cor(pred_S1,Startups$Profit) # correlation is 0.975062

vif(model.S) # In our model no colinearity problem exists between the variables.
avPlots(model.S)
influencePlot(model.S,col='lightblue',pch=16)
influence_index = as.integer(rownames(influencePlot(model.S,id = list(n=2,col="red",cex=2),pch=20,col="green")[]));influence_index

# Checking Significance level with individual variables
summary(lm(Profit~Administration)) # Only 4 % of variation in the "Profit" is explained by the "Administration"
summary(lm(Profit~R.D.Spend)) # 94 % of variation in the "Profit" is explained by the "R.D.Spend"
summary(lm(Profit~Marketing.Spend)) # Only 55 % of variation in the "Profit" is explained by the "Marketing.Spend"
summary(lm(Profit~State)) # Only 2 % of variation in the "Profit" is explained by the "State"

# Examining Variable "State" taking only "R.D.Spend" as independent variable.
ggplot(data = Startups,aes(x=R.D.Spend,y=Profit,color=State))+geom_smooth(method = "lm")+geom_point(cex=3)
ggplot(data = Startups,aes(x=R.D.Spend,y=Profit))+geom_smooth(method = "lm")+geom_point(cex=3,aes(color=State))

# So here only 2 variable is Relevent and other are insignificant 
# Removing the variable "State" will have no impact over our model.

#Second Model ----
#Removing the Influencing observations
df_Startups <- Startups[-c(influence_index),]
model.S.2 <- lm(Profit~R.D.Spend+Administration+Marketing.Spend,data = df_Startups)
summary(model.S.2) # R² = 0.9626
vif(model.S.2) # In our model no colinearity problem exists between the variables.
avPlots(model.S.2,id=list(n=3,col="red"),lwd=2,col='forestgreen')
rmse_2 <- mean(model.S.2$residuals^2)^.5 # RMSE value is 6774.245
pred_S2 <- predict(model.S.2,Startups)
cor(pred_S2,Startups$Profit) # correlation is 0.9748282

#Final model ----
model.S.3 <- lm(Profit~R.D.Spend+Marketing.Spend,data = df_Startups)
summary(model.S.3) # In our model R² = 0.9612 (0.9594 Adjusted) and other variable except R.D.Spend are insignificant.
avPlots(model.S.3,id=list(n=3,col="red"),lwd=2,col='forestgreen')
pred_S3 <- predict(model.S.3,Startups) # Predicted Values
cor(pred_S3,Startups$Profit) # 0.9748121
rmse_3 <- mean(model.S.3$residuals^2)^.5 # RMSE value is 6899.99





