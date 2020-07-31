############ Logistic Regression ########

# Question 1 ----
"Whether the client has subscribed a term deposit or not "

# Importing Packages ----
library(car)
library(ROCR)
#install.packages("MLmetrics")
#install.packages("caret")
library(caret)
library(MLmetrics)


Bank <- read.csv(file.choose(),sep = ";")
table(Bank$y)
head(Bank)
colnames(Bank)
str(Bank)
summary(Bank);
write.csv(summary(Bank),"BankSummary.csv")
boxplot(Bank)
boxplot(scale(Bank[,c(1,6,10,12:15)]),ylim = c(-10,20))

#write.csv(summary(Bank),"BankSummary.csv")
# Train and Test Split and Model Evaluation ----
set.seed(101)
Test_Spl <- as.integer(sample(x = rownames(Bank),size = round(nrow(Bank)*(30/100)),replace = F))
Train_B <- Bank[-c(Test_Spl),]
Test_B <- Bank[c(Test_Spl),]
nrow(Train_B);nrow(Test_B)


# Model Building 1 ----
model_B1 <- glm(y~.,data = Train_B,family = binomial(link = "logit"))
summary(model_B1) # AIC = 15017 and Insignificant variables are age pdays balance default age
#Test Efficiency
Y_B1 <- predict(model_B1,Test_B)
prob_B1 <-predict(model_B1,Test_B,type = "response")

plot(prob_B1,Test_B$y,
     col=ifelse((prob_B1<0.5 & Test_B$y =="no")|(prob_B1>0.5 & Test_B$y =="yes"),"green","red")
     # ,pch = ifelse((prob_B1<0.5 & Test_B$y =="no")|(prob_B1>0.5 & Test_B$y =="yes"),1,4)
)
# In this plot the red circle represents wrong prediction and the green circle represents correct prediction
plot(Y_B1,col=ifelse((prob_B1<0.5 & Test_B$y =="no")|(prob_B1>0.5 & Test_B$y =="yes"),"green","red"))
confu_B1 <- table(prob_B1>0.5,Test_B$y) ;confu_B1
effi_B1 <- sum(diag(confu_B1))/sum(confu_B1);effi_B1 # Efficiency of my model is 0.900317
# F_1 Score
pB1 <- ifelse(prob_B1>0.5,1,0)
ytrueB1 <- ifelse(Test_B$y=="yes",1,0)
F1_Score(y_true = ytrueB1,y_pred = pB1) # 0.945201
#Train Efficiency
Y_B1.1 <- predict(model_B1,Train_B)
prob_B1.1 <-predict(model_B1,Train_B,type = "response")
plot(prob_B1.1,Train_B$y,
     col=ifelse((prob_B1.1<0.5 & Train_B$y =="no") | (prob_B1.1>0.5 & Train_B$y =="yes"),"green","red")
     # ,pch = ifelse((prob_B1.1<0.5 & Train_B$y =="no")|(prob_B1.1>0.5 & Train_B$y =="yes"),1,4)
)
# In this plot the red circle represents wrong prediction and the green circle represents correct prediction
plot(Y_B1.1,col=ifelse((prob_B1.1<0.5 & Train_B$y =="no")|(prob_B1.1>0.5 & Train_B$y =="yes"),"green","red"))
confu_B1.1 <- table(prob_B1.1>0.5,Train_B$y) ;confu_B1.1
effi_B1.1 <- sum(diag(confu_B1.1))/sum(confu_B1.1);effi_B1.1 # Efficiency of my model is 0.9010996

influenceIndexPlot(model_B1,id=list(col="red"))    
influence_B1 <- as.integer(rownames(influencePlot(model_B1,id=list(n=5,col="red"))))
length(influence_B1)

# ROC Curve
rocrpred1<-prediction(prob_B1,Test_B$y)
rocrperf1<-performance(rocrpred1,'tpr','fpr')
str(rocrperf1)
rocrperf1@x.values
plot(rocrperf1,colorize=T)

rocr_cutoff <- data.frame(cut_off = rocrperf1@alpha.values[[1]],fpr=rocrperf1@x.values,tpr=rocrperf1@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)

rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)



# Model Building 2 ----
head(Train_B)
model_B2 <- glm(y~.,data = Train_B[-influence_B1,-c(1,14,5)],family = "binomial")
summary(model_B2) # AIC = 15010 Insignificant variables are age pdays balance default
#Test Efficiency
Y_B2 <- predict(model_B2,Test_B)
prob_B2 <-predict(model_B2,Test_B,type = "response")
plot(prob_B2,Test_B$y,
     col=ifelse((prob_B2<0.5 & Test_B$y =="no")|(prob_B2>0.5 & Test_B$y =="yes"),"green","red")
     # ,pch = ifelse((prob_B1<0.5 & Test_B$y =="no")|(prob_B1>0.5 & Test_B$y =="yes"),1,4)
)
# In this plot the red circle represents wrong prediction and the green circle represents correct prediction
plot(Y_B2,col=ifelse((prob_B2<0.5 & Test_B$y =="no")|(prob_B2>0.5 & Test_B$y =="yes"),"green","red"))
confu_B2 <- table(prob_B2>0.5,Test_B$y) ;confu_B2
effi_B2 <- sum(diag(confu_B2))/sum(confu_B2);effi_B2 # Efficiency of my model is 0.9000958

# F_1 Score
pB2 <- ifelse(prob_B2>0.5,1,0)
ytrueB2 <- ifelse(Test_B$y=="yes",1,0)
F1_Score(y_true = ytrueB2,y_pred = pB2) # 0.9450817

#Train Efficiency
Y_B2.1 <- predict(model_B2,Train_B[-influence_B1,-c(1,14,5)])
prob_B2.1 <-predict(model_B2,Train_B[-influence_B1,],type = "response")
plot(prob_B2.1,Train_B[-influence_B1,]$y,
     col=ifelse((prob_B2.1<0.5 & Train_B[-influence_B1,]$y =="no") | (prob_B2.1>0.5 & Train_B[-influence_B1,]$y =="yes"),"green","red")
     # ,pch = ifelse((prob_B1.1<0.5 & Train_B$y =="no")|(prob_B1.1>0.5 & Train_B$y =="yes"),1,4)
)
# In this plot the red circle represents wrong prediction and the green circle represents correct prediction
plot(Y_B2.1,col=ifelse((prob_B2.1<0.5 & Train_B[-influence_B1,]$y =="no")|(prob_B2.1>0.5 & Train_B[-influence_B1,]$y =="yes"),"green","red"))
confu_B2.1 <- table(prob_B2.1>0.5,Train_B[-influence_B1,]$y) ;confu_B2.1
effi_B2.1 <- sum(diag(confu_B2.1))/sum(confu_B2.1);effi_B2.1 # Efficiency of my model is 0.9012073

# ROC Curve
rocrpred2<-prediction(prob_B2,Test_B$y)
rocrperf2<-performance(rocrpred2,'tpr','fpr')
str(rocrperf2)
rocrperf2@x.values
par(mfrow=c(1,2));plot(rocrperf2,colorize=T,main = "ROC for model 2")
plot(rocrperf1,colorize=T,main = "ROC for model 1");par(mfrow=c(1,1)) # Must run this code

#cutoff value
rocr_cutoff2 <- data.frame(cut_off = rocrperf2@alpha.values[[1]],fpr=rocrperf2@x.values,tpr=rocrperf3@y.values)
colnames(rocr_cutoff2) <- c("cut_off","FPR","TPR")
View(rocr_cutoff2)

library(dplyr)

rocr_cutoff2$cut_off <- round(rocr_cutoff2$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff2 <- arrange(rocr_cutoff2,desc(TPR))
View(rocr_cutoff2)


