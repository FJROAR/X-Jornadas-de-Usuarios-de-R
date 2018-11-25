library(ggplot2)
library(dplyr)
library(stringr)

library(tidytext)
library(tidyr)
library(gutenbergr)


#Hay que corregir esta función para que quite las palabras por idioma

Cuenta_Palabras <- function(LibCode){
  
  #Stop_Words
  
  Stop_Words_Esp <- unique(read.csv("D:/FJRA/NLP/TALLER_R/PRACTICAS/StopWords.csv", sep = ",", stringsAsFactors = FALSE)[2])
  Stop_Words_Engl <- unique(read.csv("D:/FJRA/NLP/TALLER_R/PRACTICAS/StopWords.csv", sep = ",", stringsAsFactors = FALSE)[1])
  
  names(Stop_Words_Esp) <- "word"
  Stop_Words_Esp$lexicon <- "FJRA"
  
  names(Stop_Words_Engl) <- "word"
  Stop_Words_Engl$lexicon <- "FJRA"
  
  Stop_Words_Total <- rbind(Stop_Words_Esp, Stop_Words_Engl)
  
  Book <- gutenberg_download(LibCode)
  
  nwords_Book <- Book %>%
    unnest_tokens(word, text) %>%
    anti_join(Stop_Words_Total) %>%
    count(word, sort = TRUE) %>%
    ungroup()
  
  return(sum(nwords_Book$n))
}


AnalisisZipfPlot <- function(LibCode){
  

  Book <- gutenberg_download(LibCode)

  nwords_Book <- Book %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) %>%
    ungroup()

  total_words <- as.numeric(nwords_Book %>% summarize(total = sum(n)))
  nwords_Book <- nwords_Book %>% mutate(total = total_words )

  freq_by_rank <- nwords_Book %>%
    mutate(rank = row_number(),
           `term frequency` = n/total)
  
  freq_by_rank %>%
    ggplot(aes(rank, `term frequency`)) +
    geom_line(size = 1.1, alpha = 0.8, color = 'blue', show.legend = FALSE) +
    scale_x_log10() +
    scale_y_log10()

}

AnalisisZipfModel <- function(LibCode){
  
  
  Book <- gutenberg_download(LibCode)
  
  nwords_Book <- Book %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) %>%
    ungroup()
  
  total_words <- as.numeric(nwords_Book %>% summarize(total = sum(n)))
  nwords_Book <- nwords_Book %>% mutate(total = total_words )
  
  freq_by_rank <- nwords_Book %>%
    mutate(rank = row_number(),
           `term frequency` = n/total)
  
  rank_subset <- freq_by_rank %>%
    filter(rank < 500,
           rank > 10)
  Model <- lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)
  summary(Model)
  
  return(Model$coefficients)
  
}


Comparador <- function(LibCode1, LibCode2){
  
  LibCode = LibCode1
  Model_Coeficient <-AnalisisZipfModel(LibCode)
  
  cons_Esperanto <- Model_Coeficient[1]
  pend_Esperanto <- Model_Coeficient[2]
  n_Esperanto <- Cuenta_Palabras(LibCode)
  
  #English: Macberth
  
  LibCode = LibCode2
  Model_Coeficient <-AnalisisZipfModel(LibCode)
  
  cons_English <- Model_Coeficient[1]
  pend_English <- Model_Coeficient[2]
  n_English <- Cuenta_Palabras(LibCode)
  
  return(c(cons_Esperanto, cons_English, pend_Esperanto, pend_English, n_Esperanto, n_English))
  
}


Tabla_Comparacion_stop <- data.frame(colnames(c("cons_Esperanto", "cons_English",
                                           "pend_Esperanto", "pend_English", "n_Esperanto", "n_English")))


#La Falo de Usxero-Domo vs The Fall of the House of Usher Autor 2 Normalizado
Tabla_Comparacion_stop <- rbind(Tabla_Comparacion_stop, c(3, Comparador(17425, 932)))
names(Tabla_Comparacion_stop) <- c("cod_libro", "cons_Esperanto", "cons_English","pend_Esperanto", "pend_English", "n_Esperanto", "n_English")


#Robinsono Kruso vs Robinson Crusoe Autor 3 Normalizado Es un tanto extraño, parece que no hay equivalencia de libro
#Tabla_Comparacion <- rbind(Tabla_Comparacion, Comparador(11511, 6936))
#names(Tabla_Comparacion) <- c("cons_Esperanto", "cons_English","pend_Esperanto", "pend_English", "n_Esperanto", "n_English")


#La Legendo de Dorm-Valeto vs The Legend of Sleepy Hollow Autor 4 Normalizado
Tabla_Comparacion_stop <- rbind(Tabla_Comparacion_stop, c(5, Comparador(19293, 41)))
names(Tabla_Comparacion_stop) <- c("cod_libro", "cons_Esperanto", "cons_English","pend_Esperanto", "pend_English", "n_Esperanto", "n_English")

Tabla_Comparacion_stop$cod_libro <- c(17425, 19293)
Tabla_Comparacion_stop$Title <- c("The Fall of the House of Usher", "The Legend of Sleepy Hollow")