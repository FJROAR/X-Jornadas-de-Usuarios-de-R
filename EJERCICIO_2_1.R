options(stringsAsFactors = FALSE) 
Sys.setlocale('LC_ALL','C') 
library(tm) 
library(stringi)

setwd("D:/FJRA/NLP/TED_01/DATOS/TED")

text.df<-read.csv('oct_delta.csv')

tweets<- data.frame(ID=seq(1:nrow(text.df)),text=text.df$text)

tryTolower <- function(x)
{  
  # return NA when there is an error  
  y = NA  
  # tryCatch error  
  try_error = tryCatch(tolower(x), error = function(e) e)  
  # if not an error  
  if (!inherits(try_error, 'error'))    
    
    y = tolower(x)  
  
  return(y) 
  
}

custom.stopwords <- c(stopwords('english'), 'lol', 'smh', 'delta')


#LIMPIEZA DE TEXTOS
clean.corpus<-function(corpus)
{  
  corpus <- tm_map(corpus, content_transformer(tryTolower))  
  corpus <- tm_map(corpus, removeWords,  custom.stopwords)  
  corpus <- tm_map(corpus, removePunctuation)  
  corpus <- tm_map(corpus, stripWhitespace)  
  corpus <- tm_map(corpus, removeNumbers)  
  
  return(corpus) 
  
} 

#Corpus de datos con metainformaci�n de IDs
names(tweets) <-c("doc_id", "text")
corpus <- VCorpus(DataframeSource(tweets))


#Limpieza del corpus
corpus<-clean.corpus(corpus) 

as.list(corpus)[1]


#AN�LISIS DE FRECUENCIAS (y construcci�n de una matriz t�rmino-documento)
tdm<-TermDocumentMatrix(corpus,control=list(weighting =weightTf)) 
tdm.tweets.m<-as.matrix(tdm)

#Construcci�n de un data frame con 2 columnas, una para los t�rminos y otra para la
#frecuencia de �stos
term.freq<-rowSums(tdm.tweets.m) 
freq.df<-data.frame(word=names(term.freq), frequency=term.freq) 



#Ejercicio: Cu�les s�n los 10 t�rminos m�s y menos frecuentes en los tweets

freq.df<-freq.df[order(freq.df[,2], decreasing=T),] 
head(freq.df, 10)
tail(freq.df, 10)
