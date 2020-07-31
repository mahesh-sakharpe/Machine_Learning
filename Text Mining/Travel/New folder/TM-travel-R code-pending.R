########## Extracting reviews from a travel website ###################
setwd("D:\\ExcelR\\Data Science\\Assignments\\Text Mining\\Travel\\New folder")
library(rvest)
library(XML)
library(magrittr)

#a<-10
rev<-NULL
url1<-"https://www.tripadvisor.in/Hotel_Review-g662606-d291406-Reviews-LABRANDA_Isla_Bonita-Costa_Adeje_Adeje_Tenerife_Canary_Islands.html"

for(i in 1:10){
  url<-read_html(as.character(paste(url1,i,sep="")))
  ping<-url %>%
    html_nodes(".common-text-ReadMore__content--2X4LR") %>%
    html_text() 
  rev<-c(rev,ping)
}

write.table(rev,"travel.txt")

travel <- readLines(file.choose())
# Cleaning unwanted "http://" ----
df_tripadv <- gsub(pattern = "http.*",replacement = "",x = travel)
# again filter "https"
df_tripadv <- df_tripadv[-which(grepl("<U+",df_tripadv))]
df_tripadv <- gsub("https.*","",df_tripadv)
df_tripadv <- gsub("#.*","",df_tripadv)
df_tripadv <- gsub("@.*","",df_tripadv)

library(textcat)
# Consider only English Words 
table(textcat(df_tripadv))
df_tripadv[which(textcat(df_tripadv)=="german")]
consider <- c(which(textcat(df_tripadv)!="norwegian"))
df_tripadv <- df_tripadv[consider]

# latent Dirichlet allocation ----

stops <- readLines(file.choose()) # stopwards
tripadv_Cor <- Corpus(VectorSource(df_tripadv))
tripadv_Cor <- tm_map(tripadv_Cor, removePunctuation) 
stop <- unique(c(stopwords('english'),stops,"the","due", "are", "not", "for","the", "this", "and",  "that", "there", "new", "near", "beyond", "time", "from", "been", "both", "than",  "has","now", "until", "all", "use", "two", "ave", "blvd", "east", "between", "end", "have", "avenue", "before",    "just", "mac", "being",  "when","levels","remaining","based", "still", "off", "over", "only", "north", "past", "twin", "while","then"))
tripadv_Cor <- tm_map(tripadv_Cor, removeWords, stop)
tripadv_Cor <- tm_map(tripadv_Cor, removeNumbers) 
tripadv_Cor <- tm_map(tripadv_Cor, stripWhitespace)
tripadv_tdm <- TermDocumentMatrix(tripadv_Cor)

# Convert tdm to dtm
tripadv_dtm <- t(tripadv_tdm)
rowTotals <- apply(tripadv_dtm, 1, sum)
tripadv_dtm2 <- tripadv_dtm[rowTotals > 3,]
tripadv_dtm2$dimnames$Terms

# LDA 
library(topicmodels)
tripadv_LDA <- LDA(x = tripadv_dtm2, 10) # 10 Topics
tripadv_LDA_terms <- terms(tripadv_LDA, 5) # first 10 terms of every topic
tripadv_LDA_terms

# NLP -----
library("syuzhet")
tripadv_bonita <- get_sentences(df_tripadv)
class(alexa_dot)
str(alexa_dot)
head(alexa_dot)