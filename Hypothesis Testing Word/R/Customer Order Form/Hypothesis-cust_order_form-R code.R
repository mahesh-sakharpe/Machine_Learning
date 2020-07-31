#### Hypothesis Testing-Customer Order Form Dataset ####

library(BSDA)
library(nortest)

custord_form <-read.csv(file.choose()) 
attach(custord_form)
View(custord_form)
custord_form1 <- as.data.frame(cbind(c("Phillippines","Indonesia","Malta","India"),
                                              rep(c("Phillippines","Indonesia","Malta","India"),
                                                  c(300,300,300,300))))
View(custord_form1)
colnames(custord_form1) <- c("Type","Location")
head(custord_form1)
custord_form_table <- table(custord_form1$Type,custord_form1$Location)


chisq.test(custord_form_table)

