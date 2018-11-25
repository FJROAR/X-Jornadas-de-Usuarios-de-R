rm(list=ls())

#Formato texto basico
text <- c("En un lugar de la mancha",
          "de cuyo nombre",
          "no quiero acordarme")


#Transformación en dataframe basico
text_bs <- data.frame(line = 1:3, text = text)
text_bs
class(text_bs$text)


#Transformación en un objeto tibble

library(dplyr)
text_df <- data_frame(line = 1:3, text = text)
text_df
class(text_df$text)


#Tokenización de objeto tibble

library(tidytext)
text_df %>% unnest_tokens(word, text)