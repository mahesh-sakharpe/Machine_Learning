########## Extracting reviews from a travel website ###################
setwd("D:\\ExcelR\\Data Science\\Assignments\\Text Mining\\Travel")
library(rvest)
library(XML)
library(magrittr)


#a<-10
rev<-NULL
url1<-"https://www.tripadvisor.in/Hotel_Review-g147399-d2354539-Reviews-or"
url2<-"-The_Venetian_on_Grace_Bay-Providenciales_Turks_and_Caicos.html#REVIEWS"
for(i in 1:10){
  url<-read_html(as.character(paste(url1,i,url2,sep="")))
  ping<-url %>%
    html_nodes(".hotels-review-list-parts-ExpandableReview__reviewText--3oMkH") %>%
    html_text() 
  rev<-c(rev,ping)
}
write.table(rev,"travel.txt")


a<-10
rev<-NULL
url1<-"https://www.tripadvisor.in/Hotel_Review-g662606-d291406-Reviews-LABRANDA_Isla_Bonita-Costa_Adeje_Adeje_Tenerife_Canary_Islands.html"

for(i in 0:8){
  url<-read_html(as.character(paste(url1,i*a,sep="")))
  ping<-url %>%
    html_nodes(".partial_entry") %>%
    html_text() 
  rev<-c(rev,ping)
}
write.table(rev,"travel.txt")
