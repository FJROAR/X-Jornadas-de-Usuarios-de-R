#install.packages(openNLP)
#install.packages("openNLPmodels.en",repos = "http://datacube.wu.ac.at/",type = "source")

options(stringsAsFactors = FALSE) 
Sys.setlocale('LC_ALL','C') 
library(gridExtra) 
library(ggmap) 
library(ggthemes) 
library(NLP) 
library(openNLP) 
library(openNLPmodels.en)
library(pbapply) 
library(stringr) 
library(rvest) 
library(doBy) 
library(tm) 
library(cshapes)

setwd("D:/FJRA/NLP/TED_01/DATOS/TED/HILLARY/C8_final_txts/")

#Lectura de Datos

temp <- list.files(pattern='*.txt') 

for (i in 1:length(temp)) 
  assign(temp[i], readLines(temp[i])) 

all.emails<-pblapply(temp, get)

#Se elimina la frase final de los mails que no intervendrá en los análisis y algunos elementos
#vía expresiones regulares
txt.clean<- function(x){ 
  x<-x[-1] 
  x<-paste(x,collapse= " ") 
  x<-str_replace_all(x, "[a-zA-Z0-9_.+-]+@[a-zAZ0-9-]+\\.[a-zA-Z0-9-.]+", "")  
  x<-str_replace_all(x,  "Doc No.","")  
  x<-str_replace_all(x,  "UNCLASSIFIED U.S. Department of State Case No.","") 
  x<-removeNumbers(x) 
  x<-as.String(x) 
  
  return(x) 
  
}

all.emails<-pblapply(all.emails,txt.clean) 

names(all.emails)<-temp 
rm(list=temp)

#Se analizan los mail mediante la librería openNLP

persons <- Maxent_Entity_Annotator(kind = 'person') 
locations <- Maxent_Entity_Annotator(kind = 'location') 
organizations <- Maxent_Entity_Annotator(kind = 'organization') 
sent.token.annotator <-   Maxent_Sent_Token_Annotator(language = "en") 
word.token.annotator <-   Maxent_Word_Token_Annotator(language = "en") 
pos.tag.annotator <-   Maxent_POS_Tag_Annotator(language = "en") 

#Aplicación del análisis a un sólo mail
annotations <- annotate(all.emails[[3]], 
                        list(sent.token.annotator,
                             word.token.annotator, 
                             pos.tag.annotator, 
                             persons,
                             locations,
                             organizations))

#Esta salida sólo tiene los inicios y finales de caracteres
ann.df<-as.data.frame(annotations)[,2:5] 
ann.df$features<-unlist(as.character(ann.df$features)) 
ann.df[244:250,]

#Se saca por cada una de las líneas las palabras o/y frases
anno.chars<-NULL 
for (i in 1:nrow(ann.df)) anno.chars[i]<- 
((substr(all.emails[[3]],ann.df[i,2],ann.df[i,3]))) 
ann.df$words<-anno.chars

#Se extraen las entidades
subset(ann.df$words,grepl("*person", ann.df$features)==T)
subset(ann.df$words,grepl("*location", ann.df$features)==T) 
subset(ann.df$words,grepl("*organization", ann.df$features)==T)

#Visto lo anterior puede ser interesante investigar "National Security Council"

third.email<-readLines(temp[[3]]) 
entity.pos<-grep("National Security Council", third.email) 
third.email[entity.pos]


#ANEXO: MÚLTIPLES DOCUMENTOS
#Ahora se trabajará con los 551 mails de Hillary clinton

annotate.entities <- function(doc,
                              annotation.pipeline) { 
  annotations <- annotate(doc, annotation.pipeline) 
  AnnotatedPlainTextDocument(doc, annotations) 
  } 

ner.pipeline <- list(  Maxent_Sent_Token_Annotator(),  
                       Maxent_Word_Token_Annotator(),
                       Maxent_POS_Tag_Annotator(),  
                       Maxent_Entity_Annotator(kind = "person"),  
                       Maxent_Entity_Annotator(kind = "location"),  
                       Maxent_Entity_Annotator(kind = "organization") 
                       )

all.ner<-pblapply(all.emails,annotate.entities,ner.pipeline) 

#Cada elemento de la lista es un data frame correspondiente a un deter
#minado mail
all.ner<-pluck(all.ner,"annotations") 
all.ner<-pblapply(all.ner, as.data.frame)

#Acceso a uno de los mails
all.ner[[3]][244:250,]

#Se rehace la lista añadiendo nuevos vectores por Tokens
all.ner<-Map(function(tex,fea,id) cbind(fea,     
    entity=substring(tex, fea$start,fea$end),file=id),     
    all.emails,all.ner,temp) 
all.ner[[3]][244:250,]

#Se construye un dataframe unificado donde cada fila es una anotación
#para todo el corpus
all.ner<-do.call(rbind,all.ner) 
all.ner$features<-unlist(as.character(   
  all.ner$features))

#Selección de algunas entidades
all.per<-all.ner[grep("person", all.ner$features),] 
all.loc<-all.ner[grep("location", all.ner$features),] 
all.org<-all.ner[grep("organization",                   
                      all.ner$features),]


#Análisis de entidades

#Por frecuencia de aparición
orgs<-as.matrix(table(as.factor(all.org$entity)))
orgs<-orgs[order(orgs[,1], decreasing=T),] 
side.margins <- par(mar = c(11,2,1,1) + 0.3) 
barplot(orgs[1:20], las = 2)

#Por localización: Construcción de mapas
library(OpenStreetMap)
library(rgdal)
library(jsonlite)

uni.loc<-unique(all.loc$entity)[1:50]


nominatim_osm <- function(address = NULL)
{
  if(suppressWarnings(is.null(address)))
    return(data.frame())
  tryCatch(
    d <- jsonlite::fromJSON( 
      gsub('\\@addr\\@', gsub('\\s+', '\\%20', address), 
           'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
    ), error = function(c) return(data.frame())
  )
  if(length(d) == 0) return(data.frame())
  return(data.frame(lon = as.numeric(d$lon), lat = as.numeric(d$lat)))
}

aux <- unlist(lapply(uni.loc, FUN = nominatim_osm))

index1 <- c(1:5) * 2 - 1
index2 <- c(1:5) * 2

uni.loc.geo$lon = aux[index1]
uni.loc.geo$lat = aux[index2]

uni.loc<-cbind(uni.loc,uni.loc.geo) 
world.data <- fortify(cshp(date=                    
                    as.Date("2015-06-30"))) 
base.map <- ggplot(world.data, 
  aes(long,lat,group=group)) +  
  geom_polygon(fill="white", 
               color="grey80", 
               size=0.25) 


email.locs<-base.map + geom_point(data = uni.loc, aes(x = lon, y=lat, group=NA), 
                                  size=2.0, alpha=.5, colour="red") +
  theme_few()+ggtitle("NER Locations") 

email.locs

eu.map <-get_googlemap('Luxembourg', zoom=4, maptype='roadmap') 

