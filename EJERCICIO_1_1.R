#Text Mining in R 
#Ted Kwartler #Twitter: @tkwartler 
#Chapter 2 Text Mining Basics

#Set Options 
options(stringsAsFactors=F) 
Sys.setlocale('LC_ALL','C') 

library(rJava)
library(stringi) 
library(stringr) 
library(qdap) 

setwd("D:/FJRA/NLP/TED_01/DATOS/TED")

#PRIMEROS PASOS EN TEXT MINING

text.df<-read.csv('oct_delta.csv')

#Ejercicio: En text.df crear un campo weekdayn que identifique un dma de la semana con un nzmero del
#1 al 7, siendo el 1 el Lunes. Hacer uso de la funcisn grpel
patterns<-c('Mon','Tue','Wed','Thu','Fri','Sat','Sun') 
replacements<-seq(1:7) 
text.df$weekdayn<-mgsub(patterns,replacements,text.df$weekday) 
