# loading the data set
sms <- read.csv(choose.files(),stringsAsFactors = FALSE)

#EDA
summary(sms)
sms$type <- as.factor(sms$type)
str(sms)

str(sms$type)
table(sms$type)

# build the corpus using the text mining pacakage
library(tm)

library(NLP)
sms_corpus <- VCorpus(VectorSource(sms$text))

#clean up the corpus using tm_map()
corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
corpus_clean <- tm_map(corpus_clean, content_transformer(removeNumbers))
corpus_clean <- tm_map(corpus_clean,removeWords,stopwords())
corpus_clean <- tm_map(corpus_clean,removePunctuation)
corpus_clean <- tm_map(corpus_clean,stripWhitespace)
corpus_clean <- tm_map(corpus_clean,PlainTextDocument)
corpus_clean <- tm_map(corpus_clean, stemDocument)

library(SnowballC)

as.character(sms_corpus[1:3])   # sms before cleaning 
as.character(corpus_clean[1:3]) # sms after cleaning


# create a document term spars matrix
sms_dtm <-DocumentTermMatrix(corpus_clean)
sms_dtm

# creating training and testing data
sms_raw_train <- sms[1:4169,]
sms_raw_test <- sms[4170:5559,]

sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms[4170:5559,]

sms_corpus_train <- sms_corpus[1:4169]
sms_corpus_test <- sms_corpus[4170:5559]

# checking the proportion of the spam is similar
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))


# indicator term for frequent words
sms_dict <- findFreqTerms(sms_dtm_train,5)

sms_train <- DocumentTermMatrix(sms_corpus_train,list(dictionary=sms_dict))
sms_test <- DocumentTermMatrix(sms_corpus_test,list(dictionary=sms_dict))


# convert counts to factor
convert_counts <- function(x){
  x <- ifelse(x>0,1,0)
  x <- factor(x,levels=c(0,1),labels = c('no','yes'))
}

sms_train <- apply(sms_train,MARGIN = 2,convert_counts)
sms_test <- apply(sms_test,MARGIN = 2,convert_counts)


# model creation
library(e1071)
model <- naiveBayes(sms_train,sms_raw_train$type)
model

sms_test_pred <- predict(model,sms_test)
library(gmodels)

CrossTable(sms_test_pred,sms_raw_test$type,prop.chisq = FALSE,prop.t = FALSE,prop.r = FALSE,
           dnn = c('predicted','actual'))


model1 <- naiveBayes(sms_train,sms_raw_train$type,laplace = 1)
pred1 <- predict(model1,sms_test)

CrossTable(pred1,sms_raw_test$type,prop.chisq = FALSE,prop.t = FALSE,prop.r = FALSE,
           dnn = c('predicted','actual'))
