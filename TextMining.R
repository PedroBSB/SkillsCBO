rm(list=ls())
#Call the libraries
library(tm)
library(ggplot2)
library(reshape2)
library(wordcloud)
library(RWeka)
library(dplyr)
library(tidytext)
library(igraph)
library(ggraph)
library(visNetwork)
#### Produto http://shiny.rstudio.com/gallery/word-cloud.html
#### https://kateto.net/network-visualization
#Read data
data8483<-read.table("Data\\EmpregosScraping8483.txt",sep="@", header = TRUE, stringsAsFactors=FALSE)
clean.corpus <- function(string) {
  corpus <- Corpus(VectorSource(string))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("portuguese")[-which(stopwords("portuguese")=="são")]) 
  return(corpus)
}

################################################################################################
################################### CONFEITEIRO ################################################

textCF <- paste(data8483[which(data8483$Category=="CF"),"Information"],collapse = " ")
textCF<-gsub("formação formação","formação",textCF)
textCF<-gsub("formação formação","formação",textCF)
textCF<-gsub("descrição descrição","descrição",textCF)
textCF<-gsub("local trabalho local","local trabalho",textCF)
textCF<-gsub("trabalho local trabalho","trabalho local",textCF)
corpus.CF <- clean.corpus(textCF)
text_df <- tibble(line = 1, text = corpus.CF[[1]]$content)
textCF.ngram <- text_df %>% 
  #head(5000) %>%    # otherwise this will get very large
  unnest_tokens(words, text, token = "ngrams", n = 3, n_min = 1)
countCF<-textCF.ngram %>%  count(words, sort = TRUE)
countCF <- countCF[order(-countCF$n),] 

#Wordcloud CF
pal <- brewer.pal(8,"Dark2")
png("wordcloudCF.png", width=1280,height=800)
wordcloud(countCF$words, countCF$n, scale=c(8,.2),min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal)
dev.off()

#Network
paired_words <- text_df %>%
  unnest_tokens(words, text, token = "ngrams", n = 2)
separated_words <- paired_words %>%
  separate(words, c("word1", "word2"), sep = " ")
words_counts <- separated_words %>%
  count(word1, word2, sort = TRUE)

#Plot Network
words_counts %>%
  graph_from_data_frame() %>%
  ggraph(layout = "drl") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Confeiteiro",
       subtitle = "Text mining",
       x = "", y = "")

#Dynamic Network
graph<-graph_from_data_frame(words_counts, directed = FALSE, vertices = NULL)
E(graph)$width <- words_counts$n/10

#Community
wc <- cluster_louvain(graph)
V(graph)$color <- wc$membership + 1
weights <- ifelse(crossing(wc, graph), 1, 100)
layout <- layout_with_kk(graph, weights=weights)

net <- visIgraph(graph, layout) %>% 
  visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE)
visSave(net, file = "networkCF.html")

#Vis network
#data <- toVisNetworkData(graph)
#Nodes
#nodes<-data$nodes
#nodes$title <- paste0("<p>", wc$membership + 1,"<br>Tooltip !</p>")
#nodes$group <- sample(LETTERS[1:3], nb, replace = TRUE)

#Edge
#edges<-data$edges
#edges$title <- paste0("<p>", 1:nb,"<br>Edge Tooltip !</p>")
#net <- visNetwork(nodes = nodes, edges = edges)
#visSave(net, file = "network.html")



################################################################################################
################################### MASSEIRO    ################################################

textMA <- paste(data8483[which(data8483$Category=="MA"),"Information"],collapse = " ")
textMA<-gsub("formação formação","formação",textMA)
textMA<-gsub("formação formação","formação",textMA)
textMA<-gsub("descrição descrição","descrição",textMA)
textMA<-gsub("local trabalho local","local trabalho",textMA)
textMA<-gsub("trabalho local trabalho","trabalho local",textMA)
corpus.MA <- clean.corpus(textMA)
text_df <- tibble(line = 1, text = corpus.MA[[1]]$content)
textMA.ngram <- text_df %>% 
  #head(5000) %>%    # otherwise this will get very large
  unnest_tokens(words, text, token = "ngrams", n = 3, n_min = 1)
countMA<-textMA.ngram %>%  count(words, sort = TRUE)
countMA <- countMA[order(-countMA$n),] 

#Wordcloud MA
pal <- brewer.pal(8,"Dark2")
png("wordcloudMA.png", width=1280,height=800)
wordcloud(countMA$words, countMA$n, scale=c(8,.2),min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal)
dev.off()


#Network
paired_words <- text_df %>%
  unnest_tokens(words, text, token = "ngrams", n = 2)
separated_words <- paired_words %>%
  separate(words, c("word1", "word2"), sep = " ")
words_counts <- separated_words %>%
  count(word1, word2, sort = TRUE)

#Plot Network
words_counts %>%
  graph_from_data_frame() %>%
  ggraph(layout = "drl") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Masseiro",
       subtitle = "Text mining",
       x = "", y = "")

#Dynamic Network
graph<-graph_from_data_frame(words_counts, directed = FALSE, vertices = NULL)
E(graph)$width <- words_counts$n/10

#Community
wc <- cluster_louvain(graph)
V(graph)$color <- wc$membership + 1
weights <- ifelse(crossing(wc, graph), 1, 100)
layout <- layout_with_kk(graph, weights=weights)

net <- visIgraph(graph, layout) %>% 
  visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE)
visSave(net, file = "networkMA.html")


################################################################################################
################################### PADEIRO     ################################################

textPD <- paste(data8483[which(data8483$Category=="PD"),"Information"],collapse = " ")
textPD<-gsub("formação formação","formação",textPD)
textPD<-gsub("formação forPDção","formação",textPD)
textPD<-gsub("descrição descrição","descrição",textPD)
textPD<-gsub("local trabalho local","local trabalho",textPD)
textPD<-gsub("trabalho local trabalho","trabalho local",textPD)
corpus.PD <- clean.corpus(textPD)
text_df <- tibble(line = 1, text = corpus.PD[[1]]$content)
textPD.ngram <- text_df %>% 
  #head(5000) %>%    # otherwise this will get very large
  unnest_tokens(words, text, token = "ngrams", n = 3, n_min = 1)
countPD<-textPD.ngram %>%  count(words, sort = TRUE)
countPD <- countPD[order(-countPD$n),] 

#Wordcloud PD
pal <- brewer.pal(8,"Dark2")
png("wordcloudPD.png", width=1280,height=800)
wordcloud(countPD$words, countPD$n, scale=c(8,.2),min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal)
dev.off()


#Network
paired_words <- text_df %>%
  unnest_tokens(words, text, token = "ngrams", n = 2)
separated_words <- paired_words %>%
  separate(words, c("word1", "word2"), sep = " ")
words_counts <- separated_words %>%
  count(word1, word2, sort = TRUE)

#Plot Network
words_counts %>%
  graph_from_data_frame() %>%
  ggraph(layout = "drl") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Padeiro",
       subtitle = "Text mining",
       x = "", y = "")

#Dynamic Network
graph<-graph_from_data_frame(words_counts, directed = FALSE, vertices = NULL)
E(graph)$width <- words_counts$n/10

#Community
wc <- cluster_louvain(graph)
V(graph)$color <- wc$membership + 1
weights <- ifelse(crossing(wc, graph), 1, 100)
layout <- layout_with_kk(graph, weights=weights)

net <- visIgraph(graph, layout) %>% 
  visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE)
visSave(net, file = "networkPD.html")

################################################################################################
################################### TRABALHADOR DE FABRICAÇÃO DE SORVETE  ######################

textTS <- paste(data8483[which(data8483$Category=="TS"),"Information"],collapse = " ")
textTS<-gsub("formação formação","formação",textTS)
textTS<-gsub("formação forTSção","formação",textTS)
textTS<-gsub("descrição descrição","descrição",textTS)
textTS<-gsub("local trabalho local","local trabalho",textTS)
textTS<-gsub("trabalho local trabalho","trabalho local",textTS)
corpus.TS <- clean.corpus(textTS)
text_df <- tibble(line = 1, text = corpus.TS[[1]]$content)
textTS.ngram <- text_df %>% 
  #head(5000) %>%    # otherwise this will get very large
  unnest_tokens(words, text, token = "ngrams", n = 3, n_min = 1)
countTS<-textTS.ngram %>%  count(words, sort = TRUE)
countTS <- countTS[order(-countTS$n),] 

#Wordcloud TS
pal <- brewer.pal(8,"Dark2")
png("wordcloudTS.png", width=1280,height=800)
wordcloud(countTS$words, countTS$n, scale=c(8,.2),min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal)
dev.off()



#Plot Network
words_counts %>%
  graph_from_data_frame() %>%
  ggraph(layout = "drl") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Fabricação de sorvete",
       subtitle = "Text mining",
       x = "", y = "")

#Dynamic Network
graph<-graph_from_data_frame(words_counts, directed = FALSE, vertices = NULL)
E(graph)$width <- words_counts$n/10

#Community
wc <- cluster_louvain(graph)
V(graph)$color <- wc$membership + 1
weights <- ifelse(crossing(wc, graph), 1, 100)
layout <- layout_with_kk(graph, weights=weights)

net <- visIgraph(graph, layout) %>% 
  visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE)
visSave(net, file = "networkTS.html")
