## Association Rule Transactions Retail Example ##

retail_data <- read.csv(file.choose())

View(retail_data)
str(retail_data)
# converting everything into character format 
retail_data[] <- lapply(retail_data,as.character)
View(retail_data)
retail_data1 <- na.omit(retail_data)
View(retail_data1)
# Creating a custom fucntion to collapse all the items in a transaction into 
# a single sentence 
paste_fun <- function(i){
  return (paste(as.character(i),collapse=" "))
}

# Applying the custom function
retail_data1["new_col"] <- apply(retail_data1,1,paste_fun)
View(retail_data1)

install.packages("tm")
# tm package is used to do text manipulation and forming DTM and TDM matrices
library(tm)
x <- Corpus(VectorSource(retail_data1$new_col)) # Selecting the new column which
# contains all items of a transaction in a single sentence
x <- tm_map(x,stripWhitespace)
# Creating a TDM matrix
dtm0 <- t(TermDocumentMatrix(x))
# Converting TDM matrix to data frame
dtm0_df <- data.frame(as.matrix(dtm0))
View(dtm0_df)

# Association Rules 

library(arules)
library(arulesViz)
# Item Frequecy plot
windows()
# count of each item from all the transactions 
barplot(sapply(dtm0_df,sum),col=1:10)
# Applying apriori algorithm to get relevant rules
rules <- apriori(as.matrix(dtm0_df),parameter = list(support=0.005,confidence=0.07,minlen=3))
inspect(rules)
plot(rules,jitter=0)

# Sorting rules by confidence 
rules_conf <- sort(rules,by="confidence")
inspect(rules_conf)
# Sorint rules by lift ratio
rules_lift <- sort(rules,by="lift")
inspect(rules_lift)



plot(rules,method = "graph")
plot(rules,method = "grouped")
