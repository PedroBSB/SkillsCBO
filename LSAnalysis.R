rm(list=ls())
#Call the libraries
library(tm)
library(ggplot2)
library(reshape2)
library(wordcloud)
library(RWeka)
library(dplyr)
library(tidytext)
library(tidyr)
library(igraph)
library(ggraph)
library(visNetwork)
library(lexRankr)

#Read data
data8483<-read.table("Data\\EmpregosScraping8483.txt",sep="@", header = TRUE, stringsAsFactors=FALSE)
clean.corpus <- function(string) {
  corpus <- Corpus(VectorSource(string))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tolower))
  #corpus <- tm_map(corpus, removePunctuation)
  #corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("portuguese")[-which(stopwords("portuguese")=="são")]) 
  return(corpus)
}

################################################################################################
################################### CONFEITEIRO ################################################

#Latent Semantic Analysis
page_text<-data8483[which(data8483$Category=="CF"),"Information"]
top_15 <- lexRankr::lexRank(page_text,
                            #only 1 article; repeat same docid for all of input vector
                            docId = rep(1, length(page_text)),
                            #return 15 sentences to mimick /u/autotldr's output
                            n = 15,
                            continuous = TRUE)

#reorder the top 15 sentences to be in order of appearance in article
order_of_appearance <- order(as.integer(gsub("_","",top_15$sentenceId)))
#extract sentences in order of appearance
ordered_top_15 <- top_15[order_of_appearance, "sentence"]
ordered_top_15

#Write results
fileConn<-file("top_15CF.txt")
writeLines(ordered_top_15, fileConn)
close(fileConn)


################################################################################################
################################### MASSEIRO    ################################################


#Latent Semantic Analysis
page_text<-data8483[which(data8483$Category=="MA"),"Information"]
top_15 <- lexRankr::lexRank(page_text,
                            #only 1 article; repeat same docid for all of input vector
                            docId = rep(1, length(page_text)),
                            #return 15 sentences to mimick /u/autotldr's output
                            n = 15,
                            continuous = TRUE)

#reorder the top 15 sentences to be in order of appearance in article
order_of_appearance <- order(as.integer(gsub("_","",top_15$sentenceId)))
#extract sentences in order of appearance
ordered_top_15 <- top_15[order_of_appearance, "sentence"]
ordered_top_15

#Write results
fileConn<-file("top_15MA.txt")
writeLines(ordered_top_15, fileConn)
close(fileConn)

################################################################################################
################################### PADEIRO     ################################################

#Latent Semantic Analysis
page_text<-data8483[which(data8483$Category=="PD"),"Information"]
top_15 <- lexRankr::lexRank(page_text,
                            #only 1 article; repeat same docid for all of input vector
                            docId = rep(1, length(page_text)),
                            #return 15 sentences to mimick /u/autotldr's output
                            n = 15,
                            continuous = TRUE)

#reorder the top 15 sentences to be in order of appearance in article
order_of_appearance <- order(as.integer(gsub("_","",top_15$sentenceId)))
#extract sentences in order of appearance
ordered_top_15 <- top_15[order_of_appearance, "sentence"]
ordered_top_15

#Write results
fileConn<-file("top_15PD.txt")
writeLines(ordered_top_15, fileConn)
close(fileConn)



################################################################################################
################################### TRABALHADOR DE FABRICAÇÃO DE SORVETE  ######################

#Latent Semantic Analysis
page_text<-data8483[which(data8483$Category=="TS"),"Information"]
top_15 <- lexRankr::lexRank(page_text,
                            #only 1 article; repeat same docid for all of input vector
                            docId = rep(1, length(page_text)),
                            #return 15 sentences to mimick /u/autotldr's output
                            n = 15,
                            continuous = TRUE)

#reorder the top 15 sentences to be in order of appearance in article
order_of_appearance <- order(as.integer(gsub("_","",top_15$sentenceId)))
#extract sentences in order of appearance
ordered_top_15 <- top_15[order_of_appearance, "sentence"]
ordered_top_15

#Write results
fileConn<-file("top_15TS.txt")
writeLines(ordered_top_15, fileConn)
close(fileConn)

