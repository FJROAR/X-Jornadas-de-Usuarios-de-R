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
tdm<-TermDocumentMatrix(corpus,control=list(weighting =weightTf)) 
tdm.tweets.m<-as.matrix(tdm) 
term.freq<-rowSums(tdm.tweets.m) 

#Se crea el objeto freq.df para contener los datos de las frecuencias de términos
freq.df<-data.frame(word= names(term.freq),frequency=term.freq) 
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]

#Gráfico de barras
freq.df$word<-factor(freq.df$word, levels=unique(as.character(freq.df$word))) 
ggplot(freq.df[1:20,], aes(x=word,  y=frequency)) +
  geom_bar(stat="identity", fill='darkred') +
  coord_flip()+
  theme_gdocs() +  
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0) 





