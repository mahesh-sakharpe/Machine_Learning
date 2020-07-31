#### Hypothesis Testing Cutlets dataset ####

library(BSDA)
library(nortest)

cutlets <- read.csv(file.choose())
attach(cutlets)
colnames(cutlets) <- c("Unit.A","Unit.B")

View(cutlets)
attach(cutlets)

## Normality Test ##
ad.test(Unit.A)
ad.test(Unit.B)

shapiro.test(Unit.A)
shapiro.test(Unit.B)

## Variance Test ##
var.test(Unit.A,Unit.B)

## 2 Sample T-test ##
t.test(Unit.A,Unit.B,alternative = "two.sided",
       conf.level = 0.95,correct = TRUE)

t.test(Unit.A,Unit.B,alternative = "two.sided",var.equal = T)

## 2 Sample T-test ##
t.test(Unit.A,Unit.B,alternative = "greater",
       conf.level = 0.95,correct = TRUE)

t.test(Unit.A,Unit.B,alternative = "greater",var.equal = T)

## 2 Sample T-test ##
t.test(Unit.A,Unit.B,alternative = "less",
       conf.level = 0.95,correct = TRUE)

t.test(Unit.A,Unit.B,alternative = "less",var.equal = T)
