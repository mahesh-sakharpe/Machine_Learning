#### Hypothesis Testing-Fantaloons Dataset ####

library(BSDA)
library(nortest)

fantaloons <- read.csv(file.choose())
View(fantaloons)
head(fantaloons)
nrow(fantaloons)
attach(fantaloons)


##### Chi-seq Test ####

df_falt = as.data.frame(cbind(c(Weekdays,Weekend),rep(c("Weekdays","Weekend"),c(400,400)))) # 1 for Female; and 2 for Male
colnames(df_falt) = c("Gender","Day")
head(df_falt)
df_tab2 = table(df_falt);df_tab2

chisq.test(df_tab2)

##### Two Proportional T Test ####

prop.test(x=c(66,120),n=c(280,340),conf.level = 0.95,
          correct = FALSE,alternative = "two.sided")
prop.test(x=c(167,47),n=c(280,340),conf.level = 0.95,
          correct = FALSE,alternative = "two.sided")
