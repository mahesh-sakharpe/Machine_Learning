##### Assocoation Rules - groceries #####

library(arules)
library(arulesViz)

groceries <- read.transactions(file.choose(),sep = ",")
itemFrequency(groceries[, 1:5])
inspect(groceries[1:5])

itemFrequencyPlot(x = groceries,topN=10)

# Book First Apriori algorithm ----
# Apriori algorithm with support 0.005 and confidence 0.05 and minimum length as 3
Grules <- apriori(groceries,parameter=list(support=0.005, confidence = 0.05,minlen=3)) # Set of 186 rules
inspect(head(sort(Grules, by = "lift")))
inspect(head(sort(Grules, by = "confidence")))
inspect(head(sort(Grules, by = "support")))
inspect(head(sort(Grules, by = c("count","lift")))) # Maximum count for 228
# here we are getting maximum support values as 0.0231, maximum confidence value as 0.7 and maximum lift as 4.085

head(quality(Grules))
colfunc <- colorRampPalette(c("black","red", "yellow"))
plot(Grules,method = "scatterplot",jitter=0,col=colfunc(30))
# plot methods are'matrix', 'mosaic', 'doubledecker', 'graph',
#'paracoord', 'scatterplot', 'grouped matrix', 'two-key plot', 'matrix3D', 'iplots'
set.seed(103);plot(Grules,method = "graph")
plot(Grules,method = "paracoord")
plot(Grules,method = "grouped matrix",col=colfunc(300))
plot(Grules,method = "two-key plot",jitter=0)
plot(Grules,method = "matrix",engine = "3d")


yogurtrules <- subset(Grules, items %in% "yogurt")
inspect(yogurtrules[1:5])
