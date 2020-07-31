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
