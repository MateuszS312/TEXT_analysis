rm(list=ls())

library(gutenbergr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(gridExtra)




data<-gutenberg_works(languages = c("en", "fr")) %>%
  filter(author=="Verne, Jules")
levels(as.factor(data$language))
data%>% count(language, sort = TRUE)

books<-data.frame(en_id=c(103,3748,164,16457,3526,44278,11556,1268,3091,3808),fr_id=c(800,4791,5097,46111,4548,38674,16826,14287,14806,5126 ))
books$title<-data$title[data$gutenberg_id %in% books$en_id]
books$title_fr<-data$title[data$gutenberg_id %in% books$fr_id]

book_lib_en<-gutenberg_download(gutenberg_id=books$en_id, mirror = "http://aleph.gutenberg.org")  
book_lib_en %<>% left_join(books[,c('title','en_id')], by=c('gutenberg_id'='en_id')) %>%
  mutate(en_id = NULL)

book_lib_fr<-gutenberg_download(gutenberg_id=books$fr_id, mirror = "http://aleph.gutenberg.org")  
book_lib_fr %<>% left_join(books[,c('title','fr_id')], by=c('gutenberg_id'='fr_id')) %>%
  mutate(en_id = NULL)
#names(book_lib_fr)[3] <- "title"

obtain_freqs<-function(book_lib)
{
  words <- book_lib %>%
    unnest_tokens(word, text) %>%
    count(title, word, sort = TRUE)
  
  total_words <- words %>%
    group_by(title) %>%
    summarise(total = sum(n))
  
  words %<>% left_join(total_words)%>%  mutate(en_id = NULL)
  
  words_freq <- words %>%
    group_by(title) %>%
    mutate(rank = row_number(), freq = n / total)
  return(words_freq)
}

words_freq_en<-book_lib_en%>%obtain_freqs
words_freq_fr<-book_lib_fr%>%obtain_freqs
words_freq_en$language<-'en'
words_freq_fr$language<-'fr'
words_freq<-rbind(words_freq_en,words_freq_fr)
library(ggplot2)
ggplot(words_freq) + 
  geom_line(aes(x = rank, y = freq, color = title, linetype=language)) + 
  scale_x_log10() + scale_y_log10() +
  theme(legend.position = "bottom")

library(fields)

split_to_list<-function(tit)
{
  data<-words_freq %>% 
    ungroup() %>% 
    filter(title == tit) %>%  
    select(rank, freq)
  return(data)
}

book_names<-levels(as.factor(words_freq$title))
list_of_freqs <- lapply(book_names,split_to_list)

log.scale <- function(x, n) exp(seq(log(x[1]), log(x[length(x)]), length.out = n))

calculate_zipf_alpha<-function(data)
{
  ranks.scale <- with(data, seq(min(rank), max(rank), length.out = 20))
  ranks.scale <- log.scale(ranks.scale, 20)
  freq.sb <- stats.bin(data$rank, data$freq, breaks = ranks.scale)
  zipf1.lm <- lm(log(freq.sb$stats[2,]) ~ log(freq.sb$centers))
  A <- zipf1.lm$coefficients[1]
  alfa <- zipf1.lm$coefficients[2]
  
  plot(freq.sb$centers, freq.sb$stats[2,], log="xy", xlab = "ranga r", 
       ylab = "czestoc f", pch = 19, cex = 1.2, 
       main = substitute(paste("Rangowe prawo Zipfa ", alpha,"=",a),list(a = round(alfa, 2))))
  
  lines(ranks.scale, exp(A) * ranks.scale ** alfa, col = "red", lty = 2, lwd = 2)
}

lapply(list_of_freqs,calculate_zipf_alpha)






calculate_heaps_beta<-function(book_lib, shuffle)
{
  words_heaps <- book_lib %>% 
    unnest_tokens(word, text) %>% 
    group_by(title) %>%
    mutate( M = row_number(), V = cumsum(!duplicated(word)))
  if(shuffle)
  {
    words_heaps<-words_heaps[sample(nrow(words_heaps)),]
  }
  
  words_sum <- words_heaps %>% 
    summarise(a = lm(log10(V) ~ log10(M))$coefficients[1], b = lm(log10(V) ~ log10(M))$coefficients[2], stdr= summary(lm(log10(V) ~ log10(M)))$coefficients[2,2], pval= summary(lm(log10(V) ~ log10(M)))$coefficients[2,3])
  
  return(words_sum)
}  

betas_en<-calculate_heaps_beta(book_lib_en,F)
betas_fr<-calculate_heaps_beta(book_lib_fr,F)
betas<-data.frame(title_eng=betas_en$title,title_fr=betas_fr$title,beta_en=betas_en$b,std_en=betas_en$stdr,beta_fr=betas_fr$b,std_fr=betas_fr$stdr)

ggplot(data=betas,aes(x = beta_en, y = beta_fr)) + 
  geom_point() +
  geom_errorbar(aes(ymin=(beta_fr-std_fr), ymax=(beta_fr+std_fr))) +
  geom_errorbarh(aes(xmin = (beta_en-std_en),xmax = (beta_en+std_en)))+
  theme(legend.position = "bottom")

cor.test(betas$beta_en,betas$beta_fr)


betas_en<-calculate_heaps_beta(book_lib_en,T)
betas_fr<-calculate_heaps_beta(book_lib_fr,T)
betas<-data.frame(beta_en=betas_en$b,std_en=betas_en$stdr,beta_fr=betas_fr$b,std_fr=betas_fr$stdr)

ggplot(data=betas,aes(x = beta_en, y = beta_fr)) + 
  geom_point() +
  geom_errorbar(aes(ymin=(beta_fr-std_fr), ymax=(beta_fr+std_fr))) +
  geom_errorbarh(aes(xmin = (beta_en-std_en),xmax = (beta_en+std_en)))+
  theme(legend.position = "bottom")


cor.test(betas$beta_en,betas$beta_fr)
