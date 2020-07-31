library(rvest)
library(XML)
library(magrittr)

# Snapdeal reviews #############################
surl_1 <- "https://www.snapdeal.com/product/activa-sd75led3i6-80-cm-315/618953333749/reviews?page=2"
surl_2 <- "&sortBy=HELPFUL"
snapdeal_reviews <- NULL
for (i in 1:30){
  surl <- read_html(as.character(paste(surl_1,surl_2,sep=as.character(i))))
  srev <- surl %>%
    html_nodes("#defaultReviewsCard p") %>%
    html_text()
  snapdeal_reviews <- c(snapdeal_reviews,srev)
}

write.table(snapdeal_reviews,"fullHDTV.txt",row.names = FALSE)
getwd()

# Cleaning unwanted "http://" ----
df_activa <- gsub(pattern = "http.*",replacement = "",x = snapdeal_reviews)
# again filter "https"
df_activa <- df_activa[-which(grepl("<U+",df_activa))]
df_activa <- gsub("https.*","",df_activa)
df_activa <- gsub("#.*","",df_activa)
df_activa <- gsub("@.*","",df_activa)

library(textcat)
# Consider only English Words 
table(textcat(df_activa))
df_activa[which(textcat(df_activa)=="german")]
consider <- c(which(textcat(df_activa)!="norwegian"))
df_activa <- df_activa[consider]

# latent Dirichlet allocation ----

stops <- readLines(file.choose()) # stopwards
activa_Cor <- Corpus(VectorSource(df_activa))
activa_Cor <- tm_map(activa_Cor, removePunctuation) 
stop <- unique(c(stopwords('english'),stops,"the","due", "are", "not", "for","the", "this", "and",  "that", "there", "new", "near", "beyond", "time", "from", "been", "both", "than",  "has","now", "until", "all", "use", "two", "ave", "blvd", "east", "between", "end", "have", "avenue", "before",    "just", "mac", "being",  "when","levels","remaining","based", "still", "off", "over", "only", "north", "past", "twin", "while","then"))
activa_Cor <- tm_map(activa_Cor, removeWords, stop)
activa_Cor <- tm_map(activa_Cor, removeNumbers) 
activa_Cor <- tm_map(activa_Cor, stripWhitespace)
activa_tdm <- TermDocumentMatrix(activa_Cor)

# Convert tdm to dtm
activa_dtm <- t(activa_tdm)
rowTotals <- apply(activa_dtm, 1, sum)
activa_dtm2 <- activa_dtm[rowTotals > 3,]
activa_dtm2$dimnames$Terms

# LDA 
library(topicmodels)
activa_LDA <- LDA(x = activa_dtm2, 10) # 10 Topics
activa_LDA_terms <- terms(activa_LDA, 5) # first 10 terms of every topic
activa_LDA_terms

# NLP -----
library("syuzhet")
activa_hd <- get_sentences(df_activa)
class(activa_hd)
str(activa_hd)
head(activa_hd)