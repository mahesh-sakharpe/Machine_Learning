claimants <- read.csv(file.choose())
View(claimants)
#claimants <- na.omit(claimants)
#View(claimants)
str(claimants)
summary(claimants)
attach(claimants)
#dim(claimants)
#colnames(claimants)
#claimants <- claimants[,-1]

mod_lm <- lm(ATTORNEY~factor(CLMSEX) + factor(CLMINSUR) + factor(SEATBELT) + 
               CLMAGE + LOSS, family = "binomial",data = claimants)
summary(mod_lm)
pred1 <- predict(mod_lm,claimants)
pred1
plot(claimants$ATTORNEY,pred1)
plot(pred1)

model <- glm(ATTORNEY~.,data = claimants,family = "binomial")

prob <- predict(model,claimants,type = "response")
summary(model)

confusion <- table(prob>0.5,claimants$ATTORNEY)
confusion

Accuracy <- sum(diag(confusion)/sum(confusion))
Accuracy
