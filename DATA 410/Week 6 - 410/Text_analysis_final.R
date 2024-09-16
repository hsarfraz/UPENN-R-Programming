#-------------------------------
# Library
#-------------------------------
library(tm)
library(SnowballC)
library(wordcloud)
library(stringr)


obama <- read.csv("obama_redacted_final.csv")

names(obama)


reasons <- Corpus(VectorSource(obama$reasontovote))
reasons 
# now, we have 2322 documents instead of rows
inspect(reasons)

reasonsTDM <- TermDocumentMatrix(reasons) 
inspect(reasonsTDM)

reasonsMT <- as.matrix(reasonsTDM) 

reasonsMT <- sort(rowSums(reasonsMT), decreasing = TRUE) 
head(reasonsMT)

reasonsDF <- data.frame(word = names(reasonsMT), freq = reasonsMT)

head(reasonsDF)


# Cleaning up the data

reasons <- tm_map(reasons, removeWords, stopwords('english'))
stopwords('english')

reasons <- tm_map(reasons, removePunctuation)


reasonsTDM <- TermDocumentMatrix(reasons) 

reasonsMT <- as.matrix(reasonsTDM) 

reasonsMT <- sort(rowSums(reasonsMT), decreasing = TRUE) 

reasonsDF <- data.frame(word = names(reasonsMT), freq = reasonsMT)

head(reasonsDF)


# Making the wordcloud

pal <- brewer.pal(6,"Dark2")

wordcloud(reasonsDF$word, freq= reasonsDF$freq, max.words = 100, 
          random.order = FALSE, colors = pal)


# Your turn!

# Let's say we wanted to remove the word "change" from the cloud
# take a moment and see if you can edit our code to ensure that
# the word "CHANGE" does not appear


reasons <- tm_map(reasons, removeWords, c("change", stopwords('english')))

reasonsTDM <- TermDocumentMatrix(reasons) 

reasonsMT <- as.matrix(reasonsTDM) 

reasonsMT <- sort(rowSums(reasonsMT), decreasing = TRUE) 

reasonsDF <- data.frame(word = names(reasonsMT), freq = reasonsMT)

head(reasonsDF)


# Making the wordcloud

pal <- brewer.pal(6,"Dark2")

wordcloud(reasonsDF$word, freq= reasonsDF$freq, max.words = 100, 
          random.order = FALSE, colors = pal)

