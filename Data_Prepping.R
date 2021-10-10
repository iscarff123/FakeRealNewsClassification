
### This code will create the dataframe needed for modeling


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
any(trim(real$title) == "")
any(trim(real$text) == "")
any(trim(fake$title) == "")
any(trim(fake$text) == "")

real <- real[trim(real$text) != "",]
fake <- fake[trim(fake$text) != "",]


### Combine data
data <- rbind(real, fake)
levels(data$Type)


barplot(table(data$Type), xlab = "News Type",
        main = "News Type")
table(data$Type)

### Sort by time
data <- data[order(data$date),]
class(data$date)

dev.off()

plot(as.Date(names(table(fake$date))), unname(table(fake$date)),
     type = "l", col = "red", ylim = c(0,max(unname(table(real$date)))+5),
     xlab = "Date", ylab = "Frequency", main = "Real vs. Fake News")
lines(as.Date(names(table(real$date))), unname(table(real$date)),
      col = "green")
rect(xleft = as.Date("2016-1-01"), ybottom = 0,
     xright = as.Date("2016-11-06"), ytop = 200,
     col = "blue", density = 3)
text(x = as.Date("2016-1-01"), y = 100,
     labels = "Election Year", cex = 2,
     adj = -0.2)
legend("topleft", legend = c("Real","Fake"),
       lty = 1,
       col = c("green","red"))

### Notice that for our data, during the election year, there was more
### fake news than real



############# Text Mining ################
library(tm)
library(SnowballC)

### Create a data frame to capture the top 50 words in the text of an article
### Each column corresponds to the top words in the text
### X1 = 1st most popular, X2, 2nd most popular, etc.
top50words <- data.frame(matrix(NA, 
                                nrow = nrow(data), ncol = 50))

### Make sure that the columns are coded as characters
for(k in 1:50){
  top50words[,k] <- as.character(top50words[,k])
}

### Go through each observation in the data
for(i in 1:nrow(data)){
  
  ### Converts string into a Corpus
  doc <- Corpus(VectorSource(data$text[1]))
  
  ### Makes string lower case
  doc <- tm_map(doc, content_transformer(tolower))
  
  ### Removes any extra white space
  doc <- tm_map(doc, stripWhitespace)
  
  ### Removes english stop words such as i, me, we, our, etc.
  ### Basically words than don't carry information
  doc <- tm_map(doc, removeWords, stopwords("english"))
  
  ### Removes any numbers
  doc <- tm_map(doc, removeNumbers)
  
  ### Removes punctuation
  doc <- tm_map(doc, removePunctuation)
  
  ### Stems the string
  ### to stem means to reduce words to their root.
  ### For example, “moving”, “moved” and “movement” are changed to “move”.
  doc <- tm_map(doc, stemDocument)
  
  ### The following 4 lines convert the Corpus into a data frame.
  ### It contains words sorted by fequency of occurance.
  dtm <- TermDocumentMatrix(doc)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  topwords <- data.frame(word = names(v),freq=v)
  
  
  ### Some observations have very little text.
  ### The removal of the english stopwords sometimes lead to no
  ### words remaining. Therefore, the first most common word is NA.
  ### These observations will be removed later in the code.
  ### Setting X1 as NA will keep track of that.
  
  if(nrow(topwords) == 0){
    top50words[i,1] <- NA
  }
  
  ### There are words remaining
  else{
    
    ### Grab the 50 most popular words
    for(j in 1:50){
      
      ### if there aren't 50, everything from the last word to 50 will be NA
      if(is.na(as.character(topwords$word[j]))){
        top50words[i,j] <- NA
      }
      
      ### If there are 50 words
      else{
        top50words[i,j] <- as.character(topwords$word[j])
        
      }
    }
  }
}


### Look for observations where X1 is NA
which(is.na(top50words$X1))

### Only 1. Remove it from the Top 50 Words and from the original data
data <- data[-1025,]
top50words <- top50words[-1025,]


### Now combine the information into one dataframe

dataFinal <- cbind(data,top50words)

### Output the final data
write.csv(dataFinal, "NewsInfo.csv", row.names = FALSE)



########## Word Clouds ############
rm(list = ls())

data <- read.csv("NewsInfo.csv")

real <- data[data$Type == "real",2]
fake <- data[data$Type == "fake", 2]

### Word cloud for real news
memory.limit(size = 100000000000000)
docs <- Corpus(VectorSource(real))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

library(wordcloud2)
wordcloud2(data = d)



### Word cloud for fake news
docs <- Corpus(VectorSource(fake))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

wordcloud2(data = d)

### Here's more info on making wordclouds
# https://cran.r-project.org/web/packages/wordcloud2/vignettes/wordcloud.html


