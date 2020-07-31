######## Logistic Regresion #######

# Question 2 ----

# Importing Packages ----
library(car)
library(ROCR)
#install.packages("MLmetrics")
#install.packages("caret")
library(caret)
library(MLmetrics)

df <- read.csv(file.choose())
EMA <- ifelse(test = df$affairs > 0 ,yes = 'yes',no = 'no')
Affair <- data.frame(df[,-c(2)],EMA)

head(Affair)
colnames(Affair)
str(Affair)
write.csv(summary(Affair),"AffairSummary.csv")
boxplot(Affair)
boxplot(data.frame(scale(Affair[,-c(2,5,10)]),Affair[,c(2,5,10)]))
boxplot(Affair$age)$out
boxplot(Affair$X)$out
Affair <- Affair[Affair$X<9000,]
str(Affair)
table(Affair$EMA)


# Train and Test Split ----
set.seed(101)
Split_A <- sample(as.integer(rownames(Affair)),size = round(nrow(Affair)*0.3),replace = F )
Train_A <- Affair[-Split_A,]
Test_A <- Affair[Split_A,]
nrow(Train_A);nrow(Test_A)
# Model 1 ----
model_A1 <- glm(EMA~.,data=Train_A,family = 'binomial')
summary(model_A1) # AIC = 429.34 indignificant variables are X,children,education,occupation
pred_A1 <- predict(model_A1,Test_A,type = "response")
confusion_A1 <- table(Test_A$EMA,pred_A1>0.5);confusion_A1
effi_A1 <- sum(diag(confusion_A1))/sum(confusion_A1);effi_A1 # 0.740113
plot(pred_A1,Test_A$EMA,
     col=ifelse((pred_A1<0.5 & Test_A$EMA =="no")|(pred_A1>0.5 & Test_A$EMA =="yes"),"green","red")
     # ,pch = ifelse((prob_B1<0.5 & Test_B$y =="no")|(prob_B1>0.5 & Test_B$y =="yes"),1,4)
)
# In this plot the red circle represents wrong prediction and the green circle represents correct prediction
plot(pred_A1,col=ifelse((pred_A1<0.5 & Test_A$EMA =="no")|(pred_A1>0.5 & Test_A$EMA =="yes"),"green","red"))

rocrpredA1<-prediction(pred_A1,Test_A$EMA)
rocrperfA1<-performance(rocrpredA1,'tpr','fpr')
str(rocrperfA1)
rocrperfA1@x.values
plot(rocrperfA1,colorize=T)

influenceIndexPlot(model_A1,id = list(n = 5,col = "red"))
in_1 <-as.integer(rownames(influencePlot(model_A1,id = list(col = "red"))))

rocr_cutoffA1 <- data.frame(cut_off = rocrperfA1@alpha.values[[1]],fpr=rocrperfA1@x.values,tpr=rocrperfA1@y.values)
colnames(rocr_cutoffA1) <- c("cut_off","FPR","TPR")
View(rocr_cutoffA1)

# F_1 Score
pA1 <- ifelse(pred_A1>0.5,1,0)
ytrueA1 <- ifelse(Test_A$EMA=="yes",1,0)
F1_Score(y_true = ytrueA1,y_pred = pA1) # 0.8413793


library(dplyr)

rocr_cutoffA1$cut_off <- round(rocr_cutoffA1$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoffA1 <- arrange(rocr_cutoffA1,desc(TPR))
View(rocr_cutoffA1)


# Model 2 ----
model_A2 <- glm(EMA~.,data=Train_A[-in_1,-c(8,7,1,5)],family = 'binomial')
summary(model_A2) # AIC = 421.8 indignificant variables are X,children,education,occupation
pred_A2 <- predict(model_A2,Test_A,type = "response")
confusion_A2 <- table(Test_A$EMA,pred_A2>0.5);confusion_A2
effi_A2 <- sum(diag(confusion_A2))/sum(confusion_A2);effi_A2 # 0.7570621
plot(pred_A2,Test_A$EMA,
     col=ifelse((pred_A2<0.5 & Test_A$EMA =="no")|(pred_A2>0.5 & Test_A$EMA =="yes"),"green","red")
     # ,pch = ifelse((prob_B1<0.5 & Test_B$y =="no")|(prob_B1>0.5 & Test_B$y =="yes"),1,4)
)
# In this plot the red circle represents wrong prediction and the green circle represents correct prediction
plot(pred_A2,col=ifelse((pred_A2<0.5 & Test_A$EMA =="no")|(pred_A2>0.5 & Test_A$EMA =="yes"),"green","red"))

rocrpredA2<-prediction(pred_A2,Test_A$EMA)
rocrperfA2<-performance(rocrpredA2,'tpr','fpr')
str(rocrperfA2)
rocrperfA2@x.values
plot(rocrperfA2,colorize=T)

influenceIndexPlot(model_A2,id = list(n = 5,col = "red"))
in_1 <-as.integer(rownames(influencePlot(model_A2,id = list(col = "red"))))

rocr_cutoffA2 <- data.frame(cut_off = rocrperfA2@alpha.values[[1]],fpr=rocrperfA2@x.values,tpr=rocrperfA2@y.values)
colnames(rocr_cutoffA2) <- c("cut_off","FPR","TPR")
View(rocr_cutoffA2)

# F_1 Score
pA2 <- ifelse(pred_A2>0.5,1,0)
ytrueA2 <- ifelse(Test_A$EMA=="yes",1,0)
F1_Score(y_true = ytrueA2,y_pred = pA2) # 0.8512111


library(dplyr)

rocr_cutoffA2$cut_off <- round(rocr_cutoffA2$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoffA2 <- arrange(rocr_cutoffA2,desc(TPR))
View(rocr_cutoffA2)

par(mfrow=c(1,2));plot(rocrperfA2,colorize=T,main="model2");plot(rocrperfA1,colorize=T,main="model1");par(mfrow=c(1,1))
