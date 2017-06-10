library(XML)
library(RCurl)
library(stringr)
library(httr) 
library(rvest)
library(dplyr)
test <- read.csv(file.choose())
tested_final <- data.frame()
# test <- read.csv("jalaj.csv")
colnames(test) <- "product"
test$product <- as.character(test$product)
getGoogleURL <- function(search.term, domain = '.co.in', quotes=TRUE) 
{
  search.term <- gsub(' ', '%20', search.term)
  if(quotes) search.term <- paste('%22', search.term, '%22', sep='') 
  getGoogleURL <- paste('http://www.google', domain, '/search?q=',
                        search.term, sep='')
}

getGoogleLinks <- function(google.url) {
  doc <- getURL(google.url, httpheader = c("User-Agent" = "R
                                           (2.10.0)"))
  html <- htmlTreeParse(doc, useInternalNodes = TRUE, error=function
                        (...){})
  nodes <- getNodeSet(html, "//h3[@class='r']//a")
  page <- read_html(google.url)
  page1 <- page %>% 
    html_nodes("h3") %>% # Get all notes of type cite. You can change this to grab other node types.
    html_text()
 
  return(as.data.frame(cbind(sapply(nodes, function(x) x <- xmlAttrs(x)[["href"]]),page1)))
}

geturlgoogle <- function(len_id){
  search.term <- paste(len_id,'sprint phone')
  quotes <- "FALSE"
  search.url <- getGoogleURL(search.term=search.term, quotes=quotes)
  
  links <- getGoogleLinks(search.url)
  
  
  links1 <- links[grep("shop2.sprint.com/NASApp",links$V1),]  
  # links1$V1 <- gsub("*%3F","?",links1$V1)
  # links1$V1 <- gsub("*%3D","=",links1$V1)
  # links1 <- substring(links1, 8)
  # final <- str_split_fixed(links1, "%", 2)[,1]
  url <- tryCatch(links[1,2], error=function(e) "NULL")
  print(len_id)
  Sys.sleep(runif(1, 5.0, 30.0))
  return(paste(len_id,url))
  
}
product <- unique(test$product)
lookup_final_google <- lapply(as.character(product[171:length(product)]),geturlgoogle)
tested1 <- rbind(do.call(rbind, lapply(lookup_final_google, data.frame, stringsAsFactors=FALSE)))
tested <- rbind(do.call(rbind, lapply(lookup_final_google, data.frame, stringsAsFactors=FALSE)))
i= nrow(tested)
tested <- rbind(tested,tested1)
tested <- cbind(tested,as.data.frame(product))
colnames(tested) <- c("id","sku")
library(reshape2)
 final <- cbind(colsplit(tested$id," ",c("id_sprint","name")),as.data.frame(product))
write.csv(final,"tested1.csv")
