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
 
