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




#ASOCIACIÓN DEL TIRMINO "apologies" CON OTROS TÈRMINOS

associations<-findAssocs(tdm, 'apologies', 0.11)  #Función para encontrar 
                                                  #asociaciones
associations<-as.data.frame(associations) 
associations$terms<-row.names(associations) 
associations$terms<-factor(associations$terms, levels=associations$terms)

ggplot(associations, aes(y=terms)) +  
  geom_point(aes(x=apologies), data=associations, size=5) +  
  theme_gdocs() + 
  geom_text(aes(x=apologies, label=apologies),  colour="darkred",hjust=-.25,size=8) +  
  theme(text=element_text(size=20), axis.title.y=element_blank()) 
