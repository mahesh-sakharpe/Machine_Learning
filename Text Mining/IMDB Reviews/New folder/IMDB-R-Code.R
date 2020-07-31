# ############# IMDB reviews Extraction ################
setwd("D:\\ExcelR\\Data Science\\Assignments\\Text Mining\\IMDB Reviews")
library(rvest)
library(XML)
library(magrittr)

#a<-10
the_godfather<-NULL
url1<-"https://www.imdb.com/title/tt0068646/reviews?sort=helpfulnessScore&dir=desc&ratingFilter="
for(i in 1:10){
  url<-read_html(as.character(paste(url1,i,sep="")))
  the<-url %>%
    html_nodes(".show-more__control") %>%
    html_text() 
  the_godfather<-c(the_godfather,the)
}
unique(the_godfather)
write.table(unique(the_godfather),file="the_godfather.txt")

# Cleaning unwanted "http://" ----
df_tgf <- gsub(pattern = "http.*",replacement = "",x = the_godfather)
# again filter "https"
df_tgf <- gsub("https.*","",df_tgf)
df_tgf <- gsub('\""','',df_tgf)

install.packages("textcat")
library(textcat)
# Consider only English Words 
table(textcat(df_tgf))
df_tgf[which(textcat(df_tgf)=="norwegian")]
consider <- c(which(textcat(df_tgf)!="norwegian"))
df_tgf <- df_tgf[consider]

# latent Dirichlet allocation ----

stops <- readLines(file.choose()) # stopwards
tgf_Cor <- Corpus(VectorSource(df_tgf))
tgf_Cor <- tm_map(tgf_Cor, removePunctuation) 
stop <- unique(c(stopwords('english'),stops,"the","due", "are", "not", "for","the", "this", "and",  "that", "there", "new", "near", "beyond", "time", "from", "been", "both", "than",  "has","now", "until", "all", "use", "two", "ave", "blvd", "east", "between", "end", "have", "avenue", "before",    "just", "mac", "being",  "when","levels","remaining","based", "still", "off", "over", "only", "north", "past", "twin", "while","then"))
tgf_Cor <- tm_map(tgf_Cor, removeWords, stop)
tgf_Cor <- tm_map(tgf_Cor, removeNumbers) 
tgf_Cor <- tm_map(tgf_Cor, stripWhitespace)
tgf_tdm <- TermDocumentMatrix(tgf_Cor)

# Convert tdm to dtm
tgf_dtm <- t(tgf_tdm)
rowTotals <- apply(tgf_dtm, 1, sum)
tgf_dtm2 <- tgf_dtm[rowTotals > 3,]
tgf_dtm2$dimnames$Terms

# LDA 
library(topicmodels)
tgf_LDA <- LDA(x = tgf_dtm2, 10) # 10 Topics
tgf_LDA_terms <- terms(tgf_LDA, 5) # first 10 terms of every topic
tgf_LDA_terms



# NLP -----
install.packages("syuzhet")
library("syuzhet")
tgf_ <- get_sentences(df_tgf)
class(tgf_)
str(tgf_)
head(tgf_)

# there are 6 methods for sentiment analysis
sentiments <- c("syuzhet", "afinn", "bing", "nrc","stanford", "custom")
A <- NULL;Sent_List<-NULL
for(i in sentiments[1:4]){
  Sent_List[[i]] <- get_sentiment(tgf_, method = i)
  A[[i]] =  table(get_sentiment(tgf_, method = i))
  
}
A
Sent_List

# Lets analyse the data

# plot For nrc ----
plot(Sent_List$nrc, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h = 0, col = "red",lwd=2)
abline(h = 2, col = "lightgreen",lwd=1)
abline(h = 4, col = "forestgreen",lwd=1)
abline(h = 6, col = "blue",lwd=2)

# To extract the sentence with the most negative emotional valence
negative <- Sent_List$nrc[which.min(Sent_List$nrc)]
tgf_[which(Sent_List$nrc==negative)]
# and to extract the most positive sentence
positive <- Sent_List$nrc[which.max(Sent_List$nrc)]
tgf_[which(Sent_List$nrc==positive)]

# plot For bing ----
plot(Sent_List$bing, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h = 0, col = "red",lwd=2)
abline(h = 2, col = "lightgreen",lwd=1)
abline(h = 4, col = "forestgreen",lwd=1)
abline(h = 6, col = "blue",lwd=2)
# To extract the sentence with the most negative emotional valence
negative <- Sent_List$bing[which.min(Sent_List$bing)]
tgf_[which(Sent_List$bing==negative)]
# and to extract the most positive sentence
positive <- Sent_List$bing[which.max(Sent_List$bing)]
tgf_[which(Sent_List$bing==positive)]




# percentage based figures ----
percent_vals <- get_percentage_values(Sent_List$bing)
plot(
  percent_vals, 
  type="l", 
  main="Throw the ring in the volcano Using Percentage-Based Means", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence", 
  col="red"
)
ft_values <- get_transformed_values(
  Sent_List$bing, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  scale_vals = TRUE,
  scale_range = FALSE
)

plot(
  ft_values, 
  type ="h", 
  main ="xTransformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = ifelse(ft_values >0 ,"green","orange")
)


# categorize each sentence by eight emotions ----
nrc_data <- get_nrc_sentiment(tgf_)

# subset

sad_items <- which(nrc_data$sadness > 0)
head(tgf_[sad_items])

# To view the emotions as a barplot
barplot(sort(colSums(prop.table(nrc_data[, 1:8]))), horiz = T, cex.names = 0.7,
        las = 1, main = "Emotions", xlab = "Percentage",
        col = 1:8)

# Word Cloud ----
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

tgfdm <- as.matrix(tgf_tdm)
sorttgftdm <- sort(rowSums(tgfdm),decreasing=TRUE)
df <- data.frame(word = names(sorttgftdm),freq=sorttgftdm)
windows()

wordcloud(words = df$word[-1], freq = df$freq, min.freq = 3,
          max.words=150, random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(20, "Dark2"))

findFreqTerms(tgf_dtm2, lowfreq = 8)
findAssocs(tgf_dtm2, terms = "creature", corlimit = 0.3)
head(df, 10)

barplot(df[2:20,]$freq, las = 2, names.arg = df[2:20,]$word,
        col =brewer.pal(20, "Dark2"), main ="Most frequent words",
        ylab = "Word frequencies")

df_tgf[which(grepl(pattern = "buy",x = df_tgf))]
