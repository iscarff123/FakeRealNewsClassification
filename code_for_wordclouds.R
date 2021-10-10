library(tm)
library(SnowballC)
library(wordcloud2)

data <- read.csv("NewsInfo.csv")

real <- data[data$Type == "real",2]
rm(data)

################################################

memory.limit(size = 100000000000)
docs <- Corpus(VectorSource(real))
rm(real)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument)
dtm <- TermDocumentMatrix(docs)
rm(docs)
m <- as.matrix(dtm)
rm(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
rm(m)
d <- data.frame(word = names(v),freq=v)
rm(v)


wordcloud2(data = d, 
           main = "Most Common Words in Real News Data")
### Save this graph and put it on the google drive 


#############################################
rm(list = ls())

fake <- read.csv("Fake.csv", encoding = "UTF-8", as.is = TRUE)
fake$text <- tolower(fake$text)

fake <- data[data$Type == "fake",2]
rm(data)



docs <- Corpus(VectorSource(fake$text))
rm(fake)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs)
rm(docs)
m <- as.matrix(dtm)
rm(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
rm(m)
d <- data.frame(word = names(v),freq=v)
rm(v)


wordcloud2(data = d, 
           main = "Most Common Words in Fake News Data")
### Save this graph and put it on the google drive 











