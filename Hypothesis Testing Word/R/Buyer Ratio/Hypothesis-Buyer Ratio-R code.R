#### Hypothesis Testing-Byer Ratio Dataset ####

library(BSDA)
library(nortest)

buyer_ratio <- read.csv(file.choose())
View(buyer_ratio)
colnames(buyer_ratio) <- c("Observed.Values", "East","West","North","South")
View(buyer_ratio)
attach(buyer_ratio)

stacked_data <- stack(buyer_ratio)
View(stacked_data)
table(values,ind)
chisq.test(table(values,ind))

