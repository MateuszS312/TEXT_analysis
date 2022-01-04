rm(list=ls())

library(ggplot2)
library(gridExtra)
library(dplyr)
library(tm)
library(gsubfn) 
calculate_distance<-function(x) sqrt(sum(x**2))

tdm_scalar_prod<- function(tdm)
{
  tdm_counts_s_prod<-(sapply(1:length(acq), function(x) tdm[,x] %*% tdm))
  
  tdm_counts_s_prod[lower.tri(tdm_counts_s_prod, diag=T)]<-NA
  
  return(tdm_counts_s_prod)
}

tdm_cosine_distance<- function(tdm)
{
  tdm_counts_cos_dist<-(sapply(1:length(acq), function(x) (tdm[,x] %*% tdm)/(calculate_distance(tdm[,x])* apply(tdm, 2, calculate_distance))))
  tdm_counts_cos_dist[lower.tri(tdm_counts_cos_dist, diag=T)]<-NA
  
  
  return(tdm_counts_cos_dist)
}

data("acq")

print(acq[[1]]$content)

tdm_counts<-TermDocumentMatrix(acq, control=list(removePunctuation=T, stopwords=T,stemming=T))
tdm_counts<-as.matrix(tdm_counts)

tdm_counts_prod<- tdm_scalar_prod(tdm_counts)
df1<-data.frame(scalar_prod=tdm_counts_prod[upper.tri(tdm_counts_prod)])
g1<-ggplot(data=df1)
g1<-g1+geom_histogram(aes(x=scalar_prod),colour='black', fill='red')+
  ggtitle('Scalar product - counts')+ 
  ylab('#obs')+ 
  xlab('Scalar_product')



tdm_counts_cos<- tdm_cosine_distance(tdm_counts)
df2<-data.frame(cos_dist=tdm_counts_cos[upper.tri(tdm_counts_cos)])
g2<-ggplot(data=df2)
g2<-g2+geom_histogram(aes(x=cos_dist),colour='black', fill='green')+
  ggtitle('Cosine distance - counts')+ 
  ylab('#obs')+ 
  xlab('Cosine_distance')

print('Most simmilar when calculating scalar product are:')
print(which(tdm_counts_prod==max(tdm_counts_prod, na.rm=T), arr.ind = TRUE))
print('Most simmilar when calculating cosine distance are:')
print(which(tdm_counts_cos==max(tdm_counts_cos, na.rm=T), arr.ind = TRUE))


tdm_idf<-TermDocumentMatrix(acq, control=list(removePunctuation=T, stopwords=T,stemming=T,weighting = weightTfIdf))
tdm_idf<-as.matrix(tdm_idf)

tdm_idf_prod<- tdm_scalar_prod(tdm_idf)
df3<-data.frame(scalar_prod=tdm_idf_prod[upper.tri(tdm_idf_prod)])
g3<-ggplot(data=df3)
g3<-g3+geom_histogram(aes(x=scalar_prod),colour='black', fill='red')+
  ggtitle('Scalar product - tf-idf')+ 
  ylab('#obs')+ 
  xlab('Scalar_product')

tdm_idf_cos<- tdm_cosine_distance(tdm_idf)
df4<-data.frame(cos_dist=tdm_idf_cos[upper.tri(tdm_idf_cos)])
g4<-ggplot(data=df4)
g4<-g4+geom_histogram(aes(x=cos_dist),colour='black', fill='green')+
  ggtitle('Cosine distance - tf-idf')+ 
  ylab('#obs')+ 
  xlab('Cosine_distance')

print('Most simmilar when calculating scalar product after tf_idf are:')
print(which(tdm_idf_prod==max(tdm_idf_prod, na.rm=T), arr.ind = TRUE))
print('Most simmilar when calculating cosine distance after idf are:')
print(which(tdm_idf_cos==max(tdm_idf_cos, na.rm=T), arr.ind = TRUE))

grid.arrange(g1,g2,g3,g4, ncol=2)
