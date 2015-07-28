## Description: Combined cloud for big data and statistics
## Author: Gonzalo Rivero
## Date: 27-Jul-2015 17:30

library(httr)
library(wordcloud)
library(jsonlite)
library(plyr)
library(tm)

## Parameters
APIKEY <- "API-KEY-HERE"
BASIC_URL <- "http://api.nytimes.com/svc/search/v2/articlesearch.json"
PAGES <- 1000

get_page <- function(query, page) {
    api_url <- paste(BASIC_URL,
                     "?q=", query,
                     "&begin_date=20130101",
                     "&page=", page,
                     "&api-key=", APIKEY, sep="")
    return(content(GET(api_url)))
}


extract_text <- function(jsonlist) {
    textdata <- jsonlist[["response"]][[2]]
    return(llply(textdata, function(x) x$lead_paragraph))
}


main <- function(pages=PAGES) {
    statistics <- vector("list", pages)
    bigdata <- vector("list", pages)
    
    for (i in 1:pages){
        Sys.sleep(0.5)
        cat("Capturing page", i, "\n")
        statistics[[i]] <- extract_text(get_page("statistics", i))
        bigdata[[i]] <- extract_text(get_page("%22big+data%22", i))
    }
    return(list("statistics"=statistics,
                "bigdata"=bigdata))
}

results <- main(PAGES)
results <- llply(results, unlist)

saveRDS(results, file="./dta/raw_data.RData")

results <- readRDS("./dta/raw_data.RData")

## Clean up data 
tokens <- sapply(results, function(x) lapply(x, MC_tokenizer))
corpus <- lapply(lapply(tokens, unlist), function(m) m[m != ""])
corp <- Corpus(VectorSource(corpus))
corp <- tm_map(corp, content_transformer(tolower))
corp <- tm_map(corp, function(x) removeWords(x, stopwords()))

## Format for wordcloud
term_matrix <- TermDocumentMatrix(corp)
term_matrix <- as.matrix(term_matrix)
colnames(term_matrix) <- c("statistics", "bigdata")

## Wordclouds

png("./img/comparison_cloud.png", width=12, height=8, units='in', res=500)
comparison.cloud(term_matrix, max.words=500, random.order=FALSE)
dev.off()

png("./img/commonality_cloud.png", width=12, height=8, units='in', res=500)
commonality.cloud(term_matrix, max.words=500, random.order=FALSE)
dev.off()
