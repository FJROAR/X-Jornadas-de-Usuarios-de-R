library(dplyr) 
library(janeaustenr) 
library(tidytext)

#Contar tokens por palabra y libro 
book_words <- austen_books() %>%  
  unnest_tokens(word, text) %>%  
  count(book, word, sort = TRUE) %>%  
  ungroup()


#Aplicar tf - idf
book_words <- book_words %>%  
  bind_tf_idf(word, book, n) 
book_words

#En palabras muy comunes como "the" y "to" en inglés, tf-idf es prácticamente 0

library(ggplot2)

book_words %>%  
  arrange(desc(tf_idf)) %>%  
  mutate(word = factor(word, levels = rev(unique(word)))) %>%  
  group_by(book) %>%  
  top_n(15) %>%  
  ungroup %>%  
  ggplot(aes(word, tf_idf, fill = book)) +  
  geom_col(show.legend = FALSE) +  
  labs(x = NULL, y = "tf-idf") +  
  facet_wrap(~book, ncol = 2, scales = "free") +  
  coord_flip()

#tf - idf muestra en este ejemplo que los nombres propios son quizás los más "distintores" entre
#los distintos libros. Además muestra un comportamiento muy similar a lo largo de las 6 obras
#seleccionadas al ser estas diferenciadas prácticamente por los nombres de sus personajes y lugares
