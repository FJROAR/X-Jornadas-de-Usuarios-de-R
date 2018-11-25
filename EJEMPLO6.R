library(tm) 
library(ggplot2) 
library(ggthemes) 

setwd("D:/FJRA/NLP/TED_01/DATOS/TED")

text.df<-read.csv('oct_delta.csv') 

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



#CONSTRUCCIÓN DE DENDOGRAMAS

names(tweets) <-c("doc_id", "text")
corpus <- VCorpus(DataframeSource(tweets))
corpus<-clean.corpus(corpus) 
tdm<-TermDocumentMatrix(corpus,control=list(weighting =weightTf)) 
tdm.tweets.m<-as.matrix(tdm)

tdm2 <- removeSparseTerms(tdm, sparse=0.975)
tdm2.tweets.m<-as.matrix(tdm2)

hc <- hclust(dist(tdm2, method="euclidean"), method="complete") 
plot(hc,yaxt='n', main='@DeltaAssist Dendrogram')


library(dendextend) 
library(circlize) 

hcd <- as.dendrogram(hc) 
clusMember <- cutree(hc,4) 
labelColors <- c('darkgrey', 'darkred', 'black', '#bada55') 

hcd<-color_labels(hcd,4, col = c('#bada55','darkgrey', "black", 'darkred'))

hcd<-color_branches(hcd,4, col = c('#bada55','darkgrey', "black", 'darkred')) 
circlize_dendrogram(hcd, labels_track_height = 0.5, dend_track_height = 0.4) 
