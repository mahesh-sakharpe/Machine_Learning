#### Hypothesis Testing labTAT ####

library(BSDA)
library(nortest)

labTat <- read.csv(file.choose())
attach(labTat)
colnames(labTat) <- c("Laboratory.1","Laboratory.2","Laboratory.3","Laboratory.4")

View(labTat)
attach(labTat)

## Normality Test ##
ad.test(Laboratory.1)
ad.test(Laboratory.2)
ad.test(Laboratory.3)
ad.test(Laboratory.4)

Stacked_Data <- stack(labTat)
View(Stacked_Data)
attach(Stacked_Data)

## Variance Test ##

leveneTest(Stacked_Data$values,Stacked_Data$ind,data = Stacked_Data)

#### One-way ANOVA ####
Anova_results <- aov(values~ind,data = Stacked_Data)
summary(Anova_results)
