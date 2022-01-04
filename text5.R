library(dplyr)
library(tidytext)
library(magrittr)
library(tm)
library(caret)
library(e1071)
set.seed(5)
rm(list=ls())
#BASIC MODEL 
calculate.ACC<-function(data.class,data.predicted)
{
  CM<-table(data.class,data.predicted)
  ACC<-sum(diag(CM))/sum(CM)
}

data.bbc <- as_tibble(read.table("https://jsienkiewicz.pl/TEXT/lab/data_bbc.csv", stringsAsFactors = F))
data.bbc %>% 
  group_by(emo) %>% 
  summarise(n=n())



data.bbc[data.bbc$emo==-1,"emo"]=1
data.bbc$emo <- as.factor(data.bbc$emo)
data.bbc %>% 
  group_by(emo) %>% 
  summarise(n=n())

data.bbc %<>% 
  group_by(emo) %>% 
  slice_sample(n=500)

data.bbc %>% 
  group_by(emo) %>% 
  summarise(n=n())

data.bbc$doc_id <- 1:nrow(data.bbc)

src <- DataframeSource(as.data.frame(data.bbc[,c('text','doc_id')]))

corpus <- VCorpus(src)

tdm<-DocumentTermMatrix(corpus, control=list(stripWhitespace=T,removeNumbers=T,tolower=T,removePunctuation=T, stopwords=T))

#leave only words that appear at least 5 times
tdm <- tdm[, apply(tdm, 2, sum) > 4]
tdm
#drop documents which were left without any word after above procedure
tdm <- as.matrix(tdm)

ind<-apply(tdm, 1, sum) > 1
tdm <- tdm[ind, ]
class <- data.bbc$emo[ind]

dim(tdm); 

length(class);



#split data into train and test sample
trainIndex <- createDataPartition(class, p = .75, 
                                  list = FALSE, 
                                  times = 1)
tdm_test<-tdm[-trainIndex,]
target_test<-class[-trainIndex]
target_train<-class[trainIndex]
tdm_train<-tdm[trainIndex,]

bbc.svml <- svm(tdm_train, target_train, type = "C-classification", kernel = "linear")
bbc.svml.pred.train <- predict(bbc.svml, tdm_train)
bbc.svml.pred.test <- predict(bbc.svml, tdm_test)
table(target_train, bbc.svml.pred.train)
table(target_test, bbc.svml.pred.test)
print("Basic model")
print(calculate.ACC(target_train,bbc.svml.pred.train))
print(calculate.ACC(target_test,bbc.svml.pred.test))

#model is overfitting train data
data <- cbind(as.data.frame(tdm), class.out = class)


fit <- trainControl(method = "cv", number = 10)
model <- train(class.out ~ ., data = data, method = "svmLinear", trControl = fit)

print(confusionMatrix(model))


###########################################################################################################
#MODEL WITH STEMMING

data.bbc <- as_tibble(read.table("https://jsienkiewicz.pl/TEXT/lab/data_bbc.csv", stringsAsFactors = F))
data.bbc %>% 
  group_by(emo) %>% 
  summarise(n=n())



data.bbc[data.bbc$emo==-1,"emo"]=1
data.bbc$emo <- as.factor(data.bbc$emo)
data.bbc %>% 
  group_by(emo) %>% 
  summarise(n=n())

data.bbc %<>% 
  group_by(emo) %>% 
  slice_sample(n=500)

data.bbc %>% 
  group_by(emo) %>% 
  summarise(n=n())

data.bbc$doc_id <- 1:nrow(data.bbc)

src <- DataframeSource(as.data.frame(data.bbc))

corpus <- VCorpus(src)

tdm<-DocumentTermMatrix(corpus, control=list(stripWhitespace=T,removeNumbers=T,tolower=T,removePunctuation=T, stopwords=T,stemming=T))

#leave only words that appear at least 5 times
tdm <- tdm[, apply(tdm, 2, sum) > 4]
tdm
#drop documents which were left without any word after above procedure
tdm <- as.matrix(tdm)

ind<-apply(tdm, 1, sum) > 1
tdm <- tdm[ind, ]
class <- data.bbc$emo[ind]

dim(tdm); 

length(class);


#split data into train and test sample
trainIndex <- createDataPartition(class, p = .75, 
                                  list = FALSE, 
                                  times = 1)
tdm_test<-tdm[-trainIndex,]
target_test<-class[-trainIndex]
target_train<-class[trainIndex]
tdm_train<-tdm[trainIndex,]

bbc.svml <- svm(tdm_train, target_train, type = "C-classification", kernel = "linear")
bbc.svml.pred.train <- predict(bbc.svml, tdm_train)
bbc.svml.pred.test <- predict(bbc.svml, tdm_test)
table(target_train, bbc.svml.pred.train)
table(target_test, bbc.svml.pred.test)
print("Model with stemming")
print(calculate.ACC(target_train,bbc.svml.pred.train))
print(calculate.ACC(target_test,bbc.svml.pred.test))

#model is overfitting train data
data <- cbind(as.data.frame(tdm), class.out = class)


fit <- trainControl(method = "cv", number = 10)
model <- train(class.out ~ ., data = data, method = "svmLinear", trControl = fit)

print(confusionMatrix(model))

#########################################################################################################################

#model with TF IDF

data.bbc <- as_tibble(read.table("https://jsienkiewicz.pl/TEXT/lab/data_bbc.csv", stringsAsFactors = F))
data.bbc %>% 
  group_by(emo) %>% 
  summarise(n=n())



data.bbc[data.bbc$emo==-1,"emo"]=1
data.bbc$emo <- as.factor(data.bbc$emo)
data.bbc %>% 
  group_by(emo) %>% 
  summarise(n=n())

data.bbc %<>% 
  group_by(emo) %>% 
  slice_sample(n=500)

data.bbc %>% 
  group_by(emo) %>% 
  summarise(n=n())

data.bbc$doc_id <- 1:nrow(data.bbc)

src <- DataframeSource(as.data.frame(data.bbc[,c('text','doc_id')]))

corpus <- VCorpus(src)

tdm<-DocumentTermMatrix(corpus, control=list(stripWhitespace=T,removeNumbers=T,tolower=T,removePunctuation=T, stopwords=T,stemming=T,weighting=weightTfIdf))


tdm
#drop empty documents
tdm <- as.matrix(tdm)

ind<-apply(tdm, 1, sum) > 1
tdm <- tdm[ind, ]
class <- data.bbc$emo[ind]

dim(tdm); 

length(class);


#split data into train and test sample
trainIndex <- createDataPartition(class, p = .75, 
                                  list = FALSE, 
                                  times = 1)
tdm_test<-tdm[-trainIndex,]
target_test<-class[-trainIndex]
target_train<-class[trainIndex]
tdm_train<-tdm[trainIndex,]

bbc.svml <- svm(tdm_train, target_train, type = "C-classification", kernel = "linear")
bbc.svml.pred.train <- predict(bbc.svml, tdm_train)
bbc.svml.pred.test <- predict(bbc.svml, tdm_test)
table(target_train, bbc.svml.pred.train)
table(target_test, bbc.svml.pred.test)
print("Model with tfidf")
print(calculate.ACC(target_train,bbc.svml.pred.train))
print(calculate.ACC(target_test,bbc.svml.pred.test))

#model is overfitting train data
data <- cbind(as.data.frame(tdm), class.out = class)


fit <- trainControl(method = "cv", number = 10)
model <- train(class.out ~ ., data = data, method = "svmLinear", trControl = fit)

print(confusionMatrix(model))


##############################################################################################################

#MODEL WITH PCA

data.bbc <- as_tibble(read.table("https://jsienkiewicz.pl/TEXT/lab/data_bbc.csv", stringsAsFactors = F))
data.bbc %>% 
  group_by(emo) %>% 
  summarise(n=n())



data.bbc[data.bbc$emo==-1,"emo"]=1
data.bbc$emo <- as.factor(data.bbc$emo)
data.bbc %>% 
  group_by(emo) %>% 
  summarise(n=n())

data.bbc %<>% 
  group_by(emo) %>% 
  slice_sample(n=500)

data.bbc %>% 
  group_by(emo) %>% 
  summarise(n=n())

data.bbc$doc_id <- 1:nrow(data.bbc)

src <- DataframeSource(as.data.frame(data.bbc[,c('text','doc_id')]))

corpus <- VCorpus(src)

tdm<-DocumentTermMatrix(corpus, control=list(stripWhitespace=T,removeNumbers=T,tolower=T,removePunctuation=T, stopwords=T, stemming=T))

tdm<-data.frame(as.matrix(tdm))

Sc <- cor(tdm)
#calculate eigen vectors and values
eSc <- eigen(Sc)
#define gamma matrix
gamma<-eSc$vectors
#calculate mean value in each column
m<-colMeans(tdm)
#center data
tdm.prime<-t(apply(tdm,1,function(x) x-m))
#calculate transformed variables
tdm.Y<-as.matrix(tdm.prime)%*%gamma
#get rid of princiapal vectors connected to low eigen vals
tdm.Y<-tdm.Y[,abs(eSc$values)>10**(-3)]
# Plot cumulative variance connected to principal variables
cum.var<-cumsum(eSc$values**2)
barplot(cum.var/max(cum.var),ylim=c(0,1),names.arg=1:length(cum.var),las=2,ylab="Cumulative variance",xlab="Number of cumulated variables")
title("Cumalative variance", cex.main=1.4, font=2)

#use transformed data to fit the model
tdm.Y <- as.matrix(tdm.Y)
class <- data.bbc$emo


#split data into train and test sample
trainIndex <- createDataPartition(class, p = .75, 
                                  list = FALSE, 
                                  times = 1)
tdm_test<-tdm.Y[-trainIndex,]
target_test<-class[-trainIndex]
target_train<-class[trainIndex]
tdm_train<-tdm.Y[trainIndex,]

bbc.svml <- svm(tdm_train, target_train, type = "C-classification", kernel = "linear")
bbc.svml.pred.train <- predict(bbc.svml, tdm_train)
bbc.svml.pred.test <- predict(bbc.svml, tdm_test)
table(target_train, bbc.svml.pred.train)
table(target_test, bbc.svml.pred.test)
print("Model with PCA")
print(calculate.ACC(target_train,bbc.svml.pred.train))
print(calculate.ACC(target_test,bbc.svml.pred.test))

#model is overfitting train data
data <- cbind(as.data.frame(tdm), class.out = class)

#crossvalidation should help with this 
fit <- trainControl(method = "cv", number = 10)
model <- train(class.out ~ ., data = data, method = "svmLinear", trControl = fit)

print(confusionMatrix(model))


######################################################################################################
#MODEL WITH PCA AND TFIDF


data.bbc <- as_tibble(read.table("https://jsienkiewicz.pl/TEXT/lab/data_bbc.csv", stringsAsFactors = F))
data.bbc %>% 
  group_by(emo) %>% 
  summarise(n=n())



data.bbc[data.bbc$emo==-1,"emo"]=1
data.bbc$emo <- as.factor(data.bbc$emo)
data.bbc %>% 
  group_by(emo) %>% 
  summarise(n=n())

data.bbc %<>% 
  group_by(emo) %>% 
  slice_sample(n=500)

data.bbc %>% 
  group_by(emo) %>% 
  summarise(n=n())

data.bbc$doc_id <- 1:nrow(data.bbc)

src <- DataframeSource(as.data.frame(data.bbc[,c('text','doc_id')]))

corpus <- VCorpus(src)

tdm<-DocumentTermMatrix(corpus, control=list(stripWhitespace=T,removeNumbers=T,tolower=T,removePunctuation=T, stopwords=T, stemming=T,weighting=weightTfIdf))

tdm<-data.frame(as.matrix(tdm))

Sc <- cor(tdm)
#calculate eigen vectors and values
eSc <- eigen(Sc)
#define gamma matrix
gamma<-eSc$vectors
#calculate mean value in each column
m<-colMeans(tdm)
#center data
tdm.prime<-t(apply(tdm,1,function(x) x-m))
#calculate transformed variables
tdm.Y<-as.matrix(tdm.prime)%*%gamma
#get rid of princiapal vectors connected to low eigen vals
tdm.Y<-tdm.Y[,abs(eSc$values)>10**(-2)]
# Plot cumulative variance connected to principal variables
cum.var<-cumsum(eSc$values**2)
barplot(cum.var/max(cum.var),ylim=c(0,1),names.arg=1:length(cum.var),las=2,ylab="Cumulative variance",xlab="Number of cumulated variables")
title("Cumalative variance", cex.main=1.4, font=2)

#use transformed data to fit the model
tdm.Y <- as.matrix(tdm.Y)
class <- data.bbc$emo


#split data into train and test sample
trainIndex <- createDataPartition(class, p = .75, 
                                  list = FALSE, 
                                  times = 1)
tdm_test<-tdm.Y[-trainIndex,]
target_test<-class[-trainIndex]
target_train<-class[trainIndex]
tdm_train<-tdm.Y[trainIndex,]

bbc.svml <- svm(tdm_train, target_train, type = "C-classification", kernel = "linear")
bbc.svml.pred.train <- predict(bbc.svml, tdm_train)
bbc.svml.pred.test <- predict(bbc.svml, tdm_test)
table(target_train, bbc.svml.pred.train)
table(target_test, bbc.svml.pred.test)
print("Model with PCA and TFIDF")
print(calculate.ACC(target_train,bbc.svml.pred.train))
print(calculate.ACC(target_test,bbc.svml.pred.test))

#model is overfitting train data
data <- cbind(as.data.frame(tdm), class.out = class)

#crossvalidation should help with this 
fit <- trainControl(method = "cv", number = 10)
model <- train(class.out ~ ., data = data, method = "svmLinear", trControl = fit)

print(confusionMatrix(model))
