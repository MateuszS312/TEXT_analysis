rm(list = ls())
library(tidytext)
library(tm)
library(dplyr)
library(topicmodels)

removeSpecialChars <- function(x) gsub("[^0-9A-Za-z///' ]"," ",x)
create.tdm<-function(data)
{
  tibble<- tibble(doc_id = data$doc_id, text = data$text )
  
  corpus<-VCorpus(DataframeSource(as.data.frame(tibble)))%>%
              tm_map(removeWords, stopwords("en")) %>% 
              tm_map(removePunctuation) %>%
              tm_map(content_transformer(tolower)) %>%
              tm_map(content_transformer(removeSpecialChars)) %>%
              tm_map(stripWhitespace) %>%
              tm_map(stemDocument)
  tdm<-DocumentTermMatrix(corpus)
  tdm<-as.matrix(tdm)
  tdm <- tdm[apply(tdm, 1, sum) > 1,]
  return(tdm)
}




fit.lda<-function(doc,k)
{
  doc.lda <- LDA(doc, k)
  return(doc.lda)
}

visualize.by.topics<-function(doc.lda)
{
  ap_topics <- tidy(doc.lda, matrix = "beta")
  ap_top_terms <- ap_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  ap_top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()
}
visualize.differences<-function(doc.lda)
{
  ap_topics <- tidy(doc.lda, matrix = "beta")
  beta_spread <- ap_topics %>%
    mutate(topic = paste0("topic", topic)) %>%
    spread(topic, beta) %>%
    filter(topic1 > .001 | topic2 > .001) %>%
    mutate(log_ratio = log2(topic2 / topic1))
  beta_spread %>% 
    arrange(log_ratio) %>% 
    mutate(term = reorder(term, log_ratio)) %>% 
    filter(abs(log_ratio) >4) %>% 
    ggplot() + 
    geom_col(aes(term, log_ratio, fill = as.factor(sign(log_ratio)))) + 
    coord_flip()
}

g <- gutenberg_works()
filter(g,author == 'Verne, Jules')
books.verne.id<-c(83, 103)
verne <- gutenberg_download(books.verne.id, mirror = "http://aleph.gutenberg.org")
colnames(verne)<-c('doc_id','text')
filter(g,author == 'Austen, Jane')
books.austen.id<-c(105, 121)
austen <- gutenberg_download(books.austen.id, mirror = "http://aleph.gutenberg.org")
colnames(austen)<-c('doc_id','text')

austen.lda<-austen%>%create.tdm()%>%
  fit.lda(2)

visualize.differences(austen.lda)

visualize.by.topics(austen.lda)

verne.lda<-verne%>%create.tdm()%>%
  fit.lda(2)

visualize.differences(verne.lda)

visualize.by.topics(verne.lda)

verne.austen<-rbind(verne,austen)
verne.austen.doc<-verne.austen%>%create.tdm()

verne.austen.lda<-verne.austen.doc%>%
  fit.lda(2)
visualize.differences(verne.austen.lda)

visualize.by.topics(verne.austen.lda)

verne.austen.lda3<-verne.austen.doc%>%fit.lda(3)
visualize.by.topics(verne.austen.lda3)

