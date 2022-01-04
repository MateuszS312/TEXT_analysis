rm(list=ls())

library(gutenbergr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(gridExtra)


gutenberg_works() %>%
  filter(author=="Verne, Jules")
#Twenty Thousend Leagues under the sea id 164
Twenty_k_leagues<- gutenberg_download(gutenberg_id=164, mirror = "http://aleph.gutenberg.org")  

plt_twenty_k_leagues_sig<-Twenty_k_leagues%>%
unnest_tokens(word,text) %>% 
anti_join(stop_words, by = c("word" = "word")) %>%
count(word,sort=T)%>%filter(n>200) %>%
mutate(word1 = reorder(word, n)) %>%
ggplot() +
geom_col(aes(word1, n)) +
coord_flip()+
ggtitle('# Most significant words Twenty_k_leagues')+ 
ylab('#word')+ 
xlab('significant Word')

plt_twenty_k_leagues_all<-Twenty_k_leagues%>%
  unnest_tokens(word,text) %>% 
  count(word,sort=T)%>%filter(n>2000) %>%
  mutate(word1 = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word1, n)) +
  coord_flip()+
  ggtitle('# All words Twenty_k_leagues')+ 
  ylab('#word')+ 
  xlab('significant Word')


gutenberg_works() %>%
  filter(author=="Joyce, James")
#Ulysses id 4300
ulysses <- gutenberg_download(gutenberg_id=4300, mirror = "http://aleph.gutenberg.org")  


plt_ulysses_sig<-ulysses%>%
unnest_tokens(word,text) %>% 
anti_join(stop_words, by = c("word" = "word")) %>%
count(word,sort=T)%>%filter(n>200) %>%
mutate(word1 = reorder(word, n)) %>%
ggplot() +
geom_col(aes(word1, n)) +
coord_flip()+
ggtitle('# Most significant words Ulysses')+ 
ylab('#word')+ 
xlab('significant Word')

plt_ulysses_all<-ulysses%>%
  unnest_tokens(word,text) %>% 
  count(word,sort=T)%>%filter(n>2000) %>%
  mutate(word1 = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word1, n)) +
  coord_flip()+
  ggtitle('# All words Ulysses')+ 
  ylab('#word')+ 
  xlab('significant Word')


grid.arrange(plt_twenty_k_leagues_all,plt_twenty_k_leagues_sig,plt_ulysses_all,plt_ulysses_sig, ncol=2)
