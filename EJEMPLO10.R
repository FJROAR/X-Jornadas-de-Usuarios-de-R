library(ggplot2)
library(dplyr)
library(stringr)

library(tidytext)
library(tidyr)
library(gutenbergr)

#LibCode = 17482

Cuenta_Palabras <- function(LibCode){

  Book <- gutenberg_download(LibCode)
  head(Book)  
  nwords_Book <- Book %>%
    unnest_tokens(word, text) %>%
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
    filter(rank < 500, #Original 10 a 500
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
  
  LibCode = LibCode2
  Model_Coeficient <-AnalisisZipfModel(LibCode)
  
  cons_English <- Model_Coeficient[1]
  pend_English <- Model_Coeficient[2]
  n_English <- Cuenta_Palabras(LibCode)
  
  return(c(cons_Esperanto, cons_English, pend_Esperanto, pend_English, n_Esperanto, n_English))
  
}


Tabla_Comparacion <- data.frame(colnames(c("cod_libro","cons_Esperanto", "cons_English",
                                           "pend_Esperanto", "pend_English", "n_Esperanto", "n_English")))

#Makbeto vs Mackbert Autor 1 No normalizado
Tabla_Comparacion <- rbind(Tabla_Comparacion, c(1, Comparador(47913, 2264)))
names(Tabla_Comparacion) <- c("cod_libro", "cons_Esperanto", "cons_English","pend_Esperanto", "pend_English", "n_Esperanto", "n_English")

#Hamleto vs Hamlet, Prince of Denmark Autor 1 Normalizable
Tabla_Comparacion <- rbind(Tabla_Comparacion, c(2, Comparador(37279, 1524)))
names(Tabla_Comparacion) <- c("cod_libro", "cons_Esperanto", "cons_English","pend_Esperanto", "pend_English", "n_Esperanto", "n_English")


#La Falo de Usxero-Domo vs The Fall of the House of Usher Autor 2 Normalizado
Tabla_Comparacion <- rbind(Tabla_Comparacion, c(3, Comparador(17425, 932)))
names(Tabla_Comparacion) <- c("cod_libro", "cons_Esperanto", "cons_English","pend_Esperanto", "pend_English", "n_Esperanto", "n_English")


#Robinsono Kruso vs Robinson Crusoe Autor 3 Normalizado Ojo, parece no ser el mismo libro, parece ser una parte en todo caso
Tabla_Comparacion <- rbind(Tabla_Comparacion, c(4, Comparador(11511, 12623)))
names(Tabla_Comparacion) <- c("cod_libro", "cons_Esperanto", "cons_English","pend_Esperanto", "pend_English", "n_Esperanto", "n_English")


#La Legendo de Dorm-Valeto vs The Legend of Sleepy Hollow Autor 4 Normalizado
Tabla_Comparacion <- rbind(Tabla_Comparacion, c(5, Comparador(19293, 41)))
names(Tabla_Comparacion) <- c("cod_libro", "cons_Esperanto", "cons_English","pend_Esperanto", "pend_English", "n_Esperanto", "n_English")

#La lasta Usonano vs The Last American Autor 5 No Normalizado
Tabla_Comparacion <- rbind(Tabla_Comparacion, c(6, Comparador(25386, 27307)))
names(Tabla_Comparacion) <- c("cod_libro", "cons_Esperanto", "cons_English","pend_Esperanto", "pend_English", "n_Esperanto", "n_English")

