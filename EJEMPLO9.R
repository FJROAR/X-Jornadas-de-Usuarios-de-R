rm(list=ls())

library(dplyr)
library(tidytext)
library(tidyr)
library(gutenbergr)



#Text in Esperanto: "La Aventuroj de Alicio en Mirlando"
Esperanto_Book1 <- gutenberg_download(c(17482))

nwords_EsperantoBook1 <- Esperanto_Book1 %>%
  unnest_tokens(word, text)


#Tokens del libro, aislando puntuaciones y 
#demás tipos de separadores

nwords_EsperantoBook1$word