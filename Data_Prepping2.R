

### Read in files
real <- read.csv("True.csv", encoding = "UTF-8", as.is = TRUE)
fake <- read.csv("Fake.csv", encoding = "UTF-8", as.is = TRUE)

### Convert date from factor to date
library(lubridate)
table(real$date)
real$date <- mdy(real$date)
class(real$date)

### There is one observation in Fake where the date is not a date.
### It looks to be the title of the articlke
options(max.print = 2000)
table(fake$date)

fake[which(fake$date == "MSNBC HOST Rudely Assumes Steel Worker Would Never Let His Son Follow in His Footsteps…He Couldn’t Be More Wrong [Video]"),]

### It appears this observations just doesn't have a date.
### Remove this observation
fake <- fake[-18934,]

### There are some dates where they are not of the same format.
### Drop these.
any(grepl("-",fake$date))
which(grepl("-",fake$date))

fake <- fake[-c(which(grepl("-",fake$date))),]

### Convert to date
fake$date <- mdy(fake$date)
class(fake$date)

### Add month and year varaibles
real$month <- factor(substring(real$date,6,7))
fake$month <- factor(substring(fake$date,6,7))
real$year <- factor(substring(real$date,1,4))
fake$year <- factor(substring(fake$date,1,4))


### Add the class of news article, real or fake
real$Type <- factor("real")
fake$Type <- factor("fake")


### There are some articles where the text is blank.
### Remove these
library(glue)
any(trim(real$title) == "")
any(trim(real$text) == "")
any(trim(fake$title) == "")
any(trim(fake$text) == "")

real <- real[trim(real$text) != "",]
fake <- fake[trim(fake$text) != "",]
real <- real[trim(real$text) != " ",]
fake <- fake[trim(fake$text) != " ",]
real <- real[trim(real$text) != "   ",]
fake <- fake[trim(fake$text) != "   ",]
real <- real[trim(real$text) != "    ",]
fake <- fake[trim(fake$text) != "    ",]



### Combine data
data <- rbind(real, fake)
levels(data$Type)

### Remove duplicates
data <- data[!duplicated(data$text),]

### Sort by time
data <- data[order(data$date),]
class(data$date)

dev.off()

rm(fake)
rm(real)
data <- data[,c(2,7,5)]
data$text <- tolower(data$text)
############# Text Mining #############

### Find 100 most popular words in all data
library(tm)
library(SnowballC)

memory.limit(size = 150000)

fakeText <- data[data$Type == "fake", 1]

### Text mine the first text
doc <- Corpus(VectorSource(fakeText[1]))
doc <- tm_map(doc, removeNumbers)
doc <- tm_map(doc, removePunctuation)
doc <- tm_map(doc, stripWhitespace)
doc <- tm_map(doc, removeWords, stopwords("english"))
dtm <- TermDocumentMatrix(doc)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
topwords <- data.frame(word = names(v),freq=v)
head(topwords)

### Create a dataframe to hold words and frequencies
popularWords <- data.frame(word = topwords$word, freq = topwords$freq)

### Now loop over all others and add them to popularWords
for(i in 2:length(fakeText)){
  doc <- Corpus(VectorSource(fakeText[i]))
  doc <- tm_map(doc, removeNumbers)
  doc <- tm_map(doc, removePunctuation)
  doc <- tm_map(doc, stripWhitespace)
  doc <- tm_map(doc, removeWords, stopwords("english"))
  dtm <- TermDocumentMatrix(doc)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  topwords <- data.frame(word = names(v),freq=v)
  
  ### Add to popularWords
  popularWords <- rbind(popularWords, topwords)
  
  rm(doc)
  rm(dtm)
  rm(m)
  rm(v)
  rm(topwords)
  
  print(paste(i, Sys.time(), nrow(popularWords), sep = "   "))
}


popularWords2 <- aggregate(popularWords$freq,
                           by = list(words = popularWords$word),
                           FUN = sum)
popularWords2Sorted <- popularWords2[order(popularWords2$x, decreasing = TRUE),]
head(popularWords2Sorted, 100)

Top100Words <- as.character(popularWords2Sorted$words[1:100])


Top100WordCounts <- data.frame(matrix(0, nrow = nrow(data), ncol = 100))
colnames(Top100WordCounts) <- Top100Words

library(stringr)

for (i in 1:nrow(data)){
  
  for (j in 1:100){
    
    Top100WordCounts[i,j] <- length(str_extract_all(data$text[i], Top100Words[j])[[1]])
    
    print(paste(i, j, Sys.time(), sep = "   "))
  }
}

############ Finalize data ##########

dataNew <- cbind(data, Top100WordCounts)
dataNew <- dataNew[,-1]

write.csv(dataNew, "NewsInfo.csv", row.names = FALSE)





