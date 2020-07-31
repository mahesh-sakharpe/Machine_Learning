#### Multi-Linear Regression #####

#Question No 2 ----


library(ggplot2)
library(car)

Comp <- read.csv(file.choose())
colnames(Comp)

df_comp <- Comp[,-1]
head(Comp) 
ls_comp = list("speed" = table(Comp$speed),"ram"=table(Comp$ram),"screen"=table(Comp$screen),
               "cd"=table(Comp$cd),"multi"=table(Comp$multi),table(Comp$premium),"ads"=table(Comp$ads),"trend"=table(Comp$trend))

barplot(ls_comp$speed,main="speed",xlab = "speed",ylab = "No of computers")
str(Comp)

summary(Comp) #
write.csv(summary(Comp),'CompSummary.csv')

boxplot(Comp[,-c(1,7,8,9)])$out
boxplot(scale(Comp[,-c(1,7,8,9)]))$out
boxplot(scale(log(Comp[,-c(1,7,8,9)])))$out

install.packages("rgl")
library(rgl)

plot(Comp[,-c(1,7:9)])
cor(Comp[,-c(1,7:9)])
write.csv(cor(Comp[,-c(1,7:9)]),'Comp_COR.csv')

# Model 1  ----
model_Comp_1 <-lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend,data = df_comp)
summary(model_Comp_1) # R2 = 0.7756

model_Comp_1.1 <- lm(price~.,data=df_comp)
summary(model_Comp_1.1)

vif(model_Comp_1)
# No colinearity problem in our model.
avPlots(model_Comp_1)
pred_C_1 <- predict(model_Comp_1,newdata = df_comp)
cor(pred_C_1,df_comp$price) # Correlation is 0.8806631
rmse_C_1 <- mean(model_Comp_1$residuals^2)^.5 # rmse = 275.1298
influence_1 <- as.integer(rownames(influencePlot(model_Comp_1,id = list(n=5,col="blue",cex=1.2))))
influenceIndexPlot(model_Comp_1,id=list(n=10,col='red'))

# model 2 ----
# Log transformation
df_comp2 <- data.frame(scale(log(Comp[,-c(1,2,7,8,9)])),"price" = df_comp$price,"cd" = df_comp$cd,"premium" = df_comp$premium,"multi" = df_comp$multi)
model_Comp_2 <- lm(price~.,data=df_comp2)
summary(model_Comp_2) # 0.7426
influenceIndexPlot(model_Comp_2,id = list(n=20,col='red'))

# model 3 ------

influ_comp <- as.integer(rownames(influencePlot(model_Comp_2,id = list(n=20,col="blue"))))

length(influ_comp)
df_comp3 <- df_comp2[-c(influ_comp),]#head(df_comp2)
model_Comp_3 <- lm(price~.,data=df_comp3)
summary(model_Comp_3) # R2 = 0.7508

influenceIndexPlot(model_Comp_3,id = list(n=20,col='red'))

avPlots(model_Comp_3)
pred_C_3 <- predict(model_Comp_3,newdata = df_comp3)
cor(pred_C_3,df_comp3$price) # Correlation is 0.8664749
rmse_C_3 <- mean(model_Comp_3$residuals^2)^.5 # rmse = 281.3819


# model 4 ----

influence(model_Comp_1)
influenceIndexPlot(model_Comp_1)
influencing_obs <- length(which(rowSums(influence.measures(model_Comp_1)$is.inf) > 0));influencing_obs # These are the influencing observations
influence_obs <- as.integer(rownames(influencePlot(model_Comp_1,id=list(n=90,col="red"))))

length(influence_obs)

1- 186/nrow(df_comp)
df_Comp_scale <- data.frame(df_comp[,-c(6,7,8)],"premium"=df_comp$premium,"cd"=df_comp$cd,"multi"=df_comp$multi)#,"cd"=df_comp3$cd,"multi"=df_comp3$multi
df_Comp_scale <- df_Comp_scale[-c(influence_obs),]
head(df_Comp_scale)

df_comp[influence_obs,]

model_Comp_4 <- lm(price~.,data=df_Comp_scale) 
summary(model_Comp_4) # R2 = 0.804
avPlots(model_Comp_4)
pred_C_4 <- predict(model_Comp_4,newdata = df_comp)
cor(pred_C_4,df_comp$price) # Correlation is 0.879
rmse_C_4 <- mean(model_Comp_4$residuals^2)^.5 # rmse = 238.0004



