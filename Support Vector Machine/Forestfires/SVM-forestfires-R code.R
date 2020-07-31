#### Support Vector Machine (SVM) Forestfire ####

library(kernlab)
library(caret)

forestfires <- read.csv(file.choose())
str(forestfires)
head(forestfires)
dim(forestfires)

normalize_dummy <- function(x){
  col <- ncol(x)
  row <- nrow(x)
  y <- 1:nrow(x)
  for (i in 1:col) {
    if(class(x[,i])=="numeric" | class(x[,i])=="integer")
    {
      minx <- min(x[,i])
      maxx <- max(x[,i])
      for (j in 1:row) {
        x[j,i] <- ifelse((x[j,i] - minx) != 0,yes = ((x[j,i]- minx) / (maxx - minx)),no=0)
        
      }
    }
      }
  f <- c()
  for (i in 1:ncol(x)) {
    if(class(x[,i])=="factor"){
      dummies <- data.frame(dummies::dummy(x[,i]))
      y <- data.frame(y,dummies)
      f <- c(f,i)
    }
    else{
      next
    }
  }
if(is.null(f)){
  output <- x
}
else{output <- data.frame(x[,-f],y[,-1])}
  return(output)
  }

install.packages("dummies")
library(dummies)
norm_ff <- normalize_dummy(forestfires[,-31])
dim(norm_ff)
table(forestfires$size_category)

## Splitting data into train and test ##
set.seed(101)
split <- sample(nrow(norm_ff),nrow(norm_ff)*0.7,replace = TRUE)
train_ff <- norm_ff[split,]
test_ff <- norm_ff[-split,]

## Different types of kernels
## "rbfdot","polydot","tanhdot","vanilladot","laplacedot","basseldot","anovadot","splinedot","matrix"

modelk1 <- ksvm(forestfires$size_category[split]~temp+rain+wind+RH,
                data=train_ff,kernel="vanilladot")
pred1 <- predict(modelk1,test_ff)
table(Predicted=pred1,Actual=forestfires$size_category[-split])
methods <- c("rbfdot","polydot","tanhdot","vanilladot","laplacedot","basseldot",
             "anovadot","splinedot","matrix")
SVMinfo = list();SVMtable = list();SVMAccu = list()
for (i in methods) {
  model <- ksvm(forestfires$size_category[split]~temp+rain+wind+RH,
                data=train_ff,kernel=i)
  pred <- predict(model,test_ff)
  SVMinfo[[i]] <- pred
  SVMtable[[i]] <- table(Predicted=pred,Actual=forestfires$size_category[-split])
  SVMAccu[[i]] <- mean(pred==forestfires$size_category[-split])
  
}
SVMinfo
SVMtable
SVMAccu

install.packages("MLmetrics")
library(MLmetrics)
MLmetrics::F1_Score(y_pred = SVMinfo$vanilladot,y_true =forestfires$size_category[-split] )
MLmetrics::F1_Score(y_pred = SVMinfo$rbfdot,y_true =forestfires$size_category[-split] )
MLmetrics::F1_Score(y_pred = SVMinfo$tanhdot,y_true =forestfires$size_category[-split] )

