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


names(tweets) <-c("doc_id", "text")
corpus <- VCorpus(DataframeSource(tweets))
corpus<-clean.corpus(corpus) 

#Word - Clouds
library(wordcloud)

tdm<-TermDocumentMatrix(corpus,control=list(weighting =weightTf)) 
tdm.tweets.m<-as.matrix(tdm)

term.freq<-rowSums(tdm.tweets.m) 
freq.df<-data.frame(word=names(term.freq), frequency=term.freq) 
freq.df<-freq.df[order(freq.df[,2], decreasing=T),] 

wordcloud(freq.df$word,freq.df$frequency, max.words = 100, 
          colors=c('black','darkred')) 

help("wordcloud")
#Relaciones y Comparaciones entre wordclouds

tryTolower <- function(x){  
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)  
  if (!inherits(try_error, 'error'))    
    y = tolower(x)  
  return(y) 
}

custom.stopwords <- c(stopwords('english'), 'sorry', 'amp', 'delta', 'amazon') 

clean.vec<-function(text.vec){  
  text.vec <- tryTolower(text.vec)  
  text.vec <- removeWords(text.vec, custom.stopwords)  
  text.vec <- removePunctuation(text.vec)  
  text.vec <- stripWhitespace(text.vec)  
  text.vec <- removeNumbers(text.vec)  
  return(text.vec) 
}


amzn<-read.csv('amzn_cs.csv') 
delta<-read.csv('oct_delta.csv')

amzn.vec<-clean.vec(amzn$text) 
delta.vec<-clean.vec(delta$text)

amzn.vec <- paste(amzn.vec, collapse=" ") 
delta.vec <- paste(delta.vec, collapse=" ") 
all <- c(amzn.vec, delta.vec) 
corpus <- VCorpus(VectorSource(all))

tdm <- TermDocumentMatrix(corpus) 
tdm.m <- as.matrix(tdm) 
colnames(tdm.m) = c("Amazon", "delta") 

pal <- brewer.pal(8, "Purples") 
pal <- pal[-(1:4)]

commonality.cloud(tdm.m, max.words=200,  random.order=FALSE,colors=pal)
comparison.cloud(tdm.m, max.words=200,random.order=FALSE,title.size=1.0,
                 colors=brewer.pal(ncol(tdm.m),"Dark2")) 


#Pirámides de relaciones para tener en cuenta la frecuencia conjunta de aparición

library(plotrix) 
common.words <- subset(tdm.m, tdm.m[, 1] > 0 & tdm.m[, 2] > 0)

difference <- abs(common.words[, 1] - common.words[, 2])

common.words <- cbind(common.words, difference) 
common.words <- common.words[order(common.words[, 3], decreasing = TRUE), ] 

top25.df <- data.frame(x = common.words[1:25, 1],
                       y = common.words[1:25, 2],             
                       labels = rownames(common.words[1:25, ]))

pyramid.plot(top25.df$x, 
             top25.df$y,  
             labels = top25.df$labels,             
             gap = 14, 
             top.labels = c("Amazon", "Words", "delta"),             
             main = "Words in Common", 
             laxlab = NULL, 
             raxlab = NULL, 
             unit = NULL) 
                       
            