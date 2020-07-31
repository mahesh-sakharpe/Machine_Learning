##### Assocoation Rules - Books #####

library(arules)
library(arulesViz)

book <- read.csv(file.choose())

# Book First Apriori algorithm ----
# Apriori algorithm with support 0.02 and confidence 0.5 and minimum length as 3
Brules <- apriori(as.matrix(book),parameter=list(support=0.05, confidence = 0.5,minlen=3)) # Set of 190 rules
inspect(head(sort(Brules, by = "lift")))
inspect(head(sort(Brules, by = "confidence")))
inspect(head(sort(Brules, by = "support")))
inspect(head(sort(Brules, by = c("count","lift")))) # Maximum count for 299
# here we are getting maximum support values as 0.1495, maximum confidence value as 1 and maximum lift as 2.6009

head(quality(Brules))
colfunc <- colorRampPalette(c("black","red", "yellow"))
plot(Brules,method = "scatterplot",jitter=0,col=colfunc(30))
# plot methods are'matrix', 'mosaic', 'doubledecker', 'graph',
#'paracoord', 'scatterplot', 'grouped matrix', 'two-key plot', 'matrix3D', 'iplots'
set.seed(103);plot(Brules,method = "graph")
plot(Brules,method = "paracoord")
plot(Brules,method = "grouped matrix",col=colfunc(300))
plot(Brules,method = "two-key plot",jitter=0)
plot(Brules,method = "matrix",engine = "3d")# iplots
install.packages("iplots")
install.packages("rJava")
library(iplots)
library(rJava)
plot(Brules,method = "iplots")


Youthrules <- subset(Brules, items %in% "YouthBks")
inspect(Youthrules[1:5])


