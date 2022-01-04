rm(list=ls())
par(mfrow = c(1,1))

library(gutenbergr)
library(dplyr)
library(tidytext)
library(tidyr)
library(magrittr)
library(ggplot2)

g <- gutenberg_works(languages=c('pl'))
books_id<-c(8119, 27081)
books <- g[g$gutenberg_id %in% books_id,c("gutenberg_id","title")]
v <- gutenberg_download(books_id, mirror = "http://aleph.gutenberg.org")
v %<>% left_join(books) %>%
  mutate(gutenberg_id = NULL)
bigrams <- v %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams <- bigrams %>% drop_na()

bigram_counts <- bigrams %>%
  count(title, bigram)  %>%
  arrange(desc(n))



create_plots<-function(bigrams_cnt)
{
  theme_set(theme_bw())
  
  bigrams_cnt %>%
    group_by(title) %>%
    slice_head(n=20) %>%
    ungroup() %>%
    mutate(bigram = reorder(bigram, n)) %>%
    ggplot() +
    geom_col(aes(bigram, n, fill = title)) +
    coord_flip() +
    facet_wrap(~title, nrow = 2, scales = "free") +
    theme(legend.position = "bottom")
}

create_plots(bigram_counts)


library(igraph)


plot.graph <- function(g) {
  
  plot(g, vertex.size = 2, edge.arrow.size = 0.5, edge.color = "black")
}

bigrams_sep <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigram_counts <- bigrams_sep %>% 
  count(word1, word2, sort = TRUE)

bigram_graph <- bigram_counts %>%
  filter(n > 3) %>%
  graph_from_data_frame()

plot.graph(bigram_graph)

bigram_comp<- components(bigram_graph)
bigram_comp

bigram_comp1 <- bigram_graph - V(bigram_graph)[bigram_comp$membership != 1]
bigram_comp2 <- bigram_graph - V(bigram_graph)[bigram_comp$membership != 2]

par(mfrow = c(1,2))
plot.graph(bigram_comp1)
plot.graph(bigram_comp2)

par(mfrow = c(1,1))

library(stopwords)
#https://cran.r-project.org/web/packages/stopwords/readme/README.html

bigrams_flt <- bigrams_sep %>%
  filter(!word1 %in% c(stopwords("pl", source = "stopwords-iso"),'się','w')) %>%
  filter(!word2 %in% c(stopwords("pl", source = "stopwords-iso"),'się','w'))

bigrams_uni <- bigrams_flt %>%
  unite(bigram, word1, word2, sep = " ")

bigram_counts_uni <- bigrams_uni %>%
  count(title, bigram)  %>%
  arrange(desc(n))

create_plots(bigram_counts_uni)

bigram_counts_uni <- bigrams_flt %>% 
  count(word1, word2, sort = TRUE)

bigram_graph <- bigram_counts_uni %>%
  filter(n > 2) %>%
  graph_from_data_frame()


plot.graph(bigram_graph)

bigram_comp <- components(bigram_graph)
bigram_comp

bigram_comp1 <- bigram_graph - V(bigram_graph)[bigram_comp$membership != 5]
bigram_comp2 <- bigram_graph - V(bigram_graph)[bigram_comp$membership != 7]

par(mfrow = c(1,2))
plot.graph(bigram_comp1)
plot.graph(bigram_comp2)
