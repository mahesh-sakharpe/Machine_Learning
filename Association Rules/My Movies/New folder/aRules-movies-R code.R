##### Assocoation Rules - Movies #####

library(arules)
library(arulesViz)

movies <- read.csv(file.choose())
head(movies)


# Book First Apriori algorithm ----
# Apriori algorithm with support 0.005 and confidence 0.05 and minimum length as 3
Mrules <- apriori(as.matrix(movies[,6:15]),parameter=list(support=0.005, confidence = 0.05,minlen=3)) # Set of 186 rules
inspect(head(sort(Mrules, by = "lift")))
inspect(head(sort(Mrules, by = "confidence")))
inspect(head(sort(Mrules, by = "support")))
inspect(head(sort(Mrules, by = c("count","lift")))) # Maximum count for 4
# here we are getting maximum support values as 0.4, maximum confidence value as 1 and maximum lift as 10

head(quality(Mrules))
colfunc <- colorRampPalette(c("black","red", "yellow"))
plot(Mrules,method = "scatterplot",jitter=0,col=colfunc(30),cex=2)
# plot methods are'matrix', 'mosaic', 'doubledecker', 'graph',
#'paracoord', 'scatterplot', 'grouped matrix', 'two-key plot', 'matrix3D', 'iplots'
set.seed(103);plot(Mrules,method = "graph")
plot(Mrules,method = "paracoord")
plot(Mrules,method = "grouped matrix",col=colfunc(300))
plot(Mrules,method = "two-key plot",jitter=0,pch=20,cex=2)
plot(Mrules,method = "matrix",engine = "3d")# iplots

inspect(Mrules)

Harrypotter <- subset(Mrules, items %in% "Harry.Potter1")
inspect(Harrypotter[1:5])
# The people who buy HarryPotter most likely to buy Lord of the ring.