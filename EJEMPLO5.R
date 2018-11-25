library(tm) 
library(ggplot2) 
library(ggthemes) 

setwd("D:/FJRA/NLP/TED_01/DATOS/TED")

text.df<-read.csv('oct_delta.csv', encoding = "WINDOWS-1252") 
help("read.csv")
tweets<-data.frame(ID=seq(1:nrow(text.df)), text=text.df$text) 

tryTolower <- function(x){  
  y = NA  
  try_error = tryCatch(tolower(x), error = function(e) e)  
  if (!inherits(try_error, 'error'))    
    y = tolower(x)  
  return(y) 
}

custom.stopwords <- c(stopwords('english'), 'lol', 'smh', 'delta', 'amp') 
clean.corpus<-function(corpus){
  
  corpus <- tm_map(corpus, content_transformer(tryTolower)) 
  corpus <- tm_map(corpus, removeWords, custom.stopwords) 
  corpus <- tm_map(corpus, removePunctuation) 
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers) 
  return(corpus) 
  } 

names(tweets) <-c("doc_id", "text")
corpus <- VCorpus(DataframeSource(tweets))
corpus<-clean.corpus(corpus) 
tdm<-TermDocumentMatrix(corpus,control=list(weighting =weightTf)) 
tdm.tweets.m<-as.matrix(tdm) 
term.freq<-rowSums(tdm.tweets.m) 

#Se crea el objeto freq.df para contener los datos de las frecuencias de tirminos
freq.df<-data.frame(word= names(term.freq),frequency=term.freq) 
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]




#GRAFOS DE TÉRMINOS relacionados con refund

library(igraph) 
library(qdap) 

word_network_plot(refund$text[1:3]) 
title(main='@DeltaAssist Refund Word Network')

word_associate(tweets$text,match.string=c('refund'),
               stopwords=Top200Words,network.plot = T,
               cloud.colors=c('gray85','darkred')) 

title(main='@DeltaAssist Refund Word Network')

