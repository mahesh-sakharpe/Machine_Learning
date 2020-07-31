install.packages("tm")   ## for text mining
install.packages("SnowballC") ## for text stemming
install.packages("wordcloud") ## word-cloud generator
install.packages("RColorBrewer") ## color palettes
#Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
text <- readLines(file.choose())
## text <- read.csv()
View(text)
docs <- Corpus(VectorSource(text))
inspect(docs)
toSpace <- function (x, pattern ) gsub(pattern, " ", x)
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
## Convert the text to lower case
docs <- tm_map(docs, tolower)
## Remove Numbers
docs <- tm_map(docs, removeNumbers)
## Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
## Remove your own stop word
## Specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2"))
## Remove Punctuations
docs <- tm_map(docs, removePunctuation)
## Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
## Text stemming
## docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
a <- as.matrix(dtm)
b <- sort(rowSums(a),decreasing = TRUE)
d <- data.frame(word = names(b),freq=b)
wordcloud(words = d$word, freq = d$freq, min.freq = 0,max.words = 1000,
          random.order = FALSE, rot.per = 0.35, colors =brewer.pal(1,"Dark2"))
findFreqTerms(dtm, lowfreq = 8)
findAssocs(dtm, terms = "freedom", corlimit = 0.3)
head(d, 10)
barplot(d[1:10,]$freq, las= 2, names.arg = d[1:10,]$word,
        col = "lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

