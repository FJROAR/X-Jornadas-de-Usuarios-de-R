library(dplyr)
library(tidyr)

titles <- c("Twenty Thousand Leagues under the Sea", 
            "The War of the Worlds", 
            "Pride and Prejudice", 
            "Great Expectations")

library(gutenbergr)

books <- gutenberg_works(title %in% titles) %>%  
  gutenberg_download(meta_fields = "title") 

library(stringr)

# divide into documents, each representing one chapter 
reg <- regex("^chapter ", ignore_case = TRUE) 
by_chapter <- books %>%  
  group_by(title) %>%  
  mutate(chapter = cumsum(str_detect(text, reg))) %>%  
  ungroup() %>%  filter(chapter > 0) %>%  
  unite(document, title, chapter)

# split into words 
by_chapter_word <- by_chapter %>%  
  unnest_tokens(word, text)

# find document-word counts 
word_counts <- by_chapter_word %>%  
  anti_join(stop_words) %>%  
  count(document, word, sort = TRUE) %>%  
  ungroup()

#Se pasan los datos a una matriz dispersa de tipo documento término. En este caso los documentos son los
#capítulos de los libros
chapters_dtm <- word_counts %>%  
  cast_dtm(document, word, n)

chapters_dtm

#Se aplica un topic modeling con 4 topics, dado que los datos vienen de 4 libros distintos
library(topicmodels)

chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234)) 
chapters_lda

#Análisis de topic por probabilidades asociadas a las palabras
chapter_topics <- tidy(chapters_lda, matrix = "beta") 
chapter_topics

#5 Términos más probables que definen cada topic
top_terms <- chapter_topics %>%  
  group_by(topic) %>%  
  top_n(5, beta) %>%  
  ungroup() %>%  
  arrange(topic, -beta)
top_terms

#Visualización
library(ggplot2)
top_terms %>%  
  mutate(term = reorder(term, beta)) %>%  
  ggplot(aes(term, beta, fill = factor(topic))) +  
  geom_col(show.legend = FALSE) +  
  facet_wrap(~ topic, scales = "free") +  
  coord_flip()

#¿Qué topics definen a cada uno de los documentos? (Capítulos de éstos)

chapters_gamma <- tidy(chapters_lda, matrix = "gamma") 
chapters_gamma

chapters_gamma <- chapters_gamma %>%  
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)

chapters_gamma %>%  
  mutate(title = reorder(title, gamma * topic)) %>%  
  ggplot(aes(factor(topic), gamma)) +  
  geom_boxplot() +  
  facet_wrap(~ title)

#Se observa que en los 3 primeros libros, cada capítulo está prácticamente asociado a su topic, excepto
#en el cuarto libro donde hay capítulos que podrían estar cercanos en téminos a Pride and Prejudice