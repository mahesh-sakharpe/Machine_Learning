setwd("D:\\ExcelR\Data Science\Assignments\Text Mining")
install.packages("rvest")
library(rvest)
install.packages("XML")
library(XML)
library(magrittr)

## Amazon Reviews #####
aurl <- "https://www.amazon.in/All-new-Echo-Dot-3rd-Gen/dp/B0792KTHKK/ref=sr_1_1?keywords=alexa&qid=1568202813&s=gateway&sr=8-1#customerReviews"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
} 
write.table(amazon_reviews,"alexa123",row.names = F)
