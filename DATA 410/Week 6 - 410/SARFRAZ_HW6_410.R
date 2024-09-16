# install.packages(c('DT','shinythemes','shiny','rsconnect','mfx','ggplot2','survey',
#                    'foreign','stargazer','rgdal','plyr','leaflet','dplyr','htmltools',
#                    'tidyverse','ggmap','gridExtra','grid','lattice','tm','SnowballC',
#                    'wordcloud','stringr'))

rsconnect::setAccountInfo(name='hussainsarfraz', token='4AE36C11AB5A3AD92F46469A5B47EB15', secret='bmF6LSAPWCZS/f/Ao2p+kWKxUG+Z8aFunHJW0N3y')

library(rsconnect)
rsconnect::deployApp("C:/Users/hussainsarfraz/Desktop/DATA 410/SARFRAZ_FINAL_410")
deployApp(account = 'hussainsarfraz')

library(tm)
library(SnowballC)
library(wordcloud)
library(stringr)

setwd("C:/Users/hussainsarfraz/Desktop/DATA 410/Week 6 - 410/SARFRAZ_HW6_410")

clinton.likes <- read.csv("redacted_2016_clinton_likes.csv", header = TRUE)
clinton.dislikes <- read.csv("redacted_clinton_2016_dislikes.csv", header = TRUE)
trump.likes <- read.csv("redacted_trump_2016.csv", header = TRUE)
trump.dislikes <- read.csv("redacted_trump_2016_dislikes.csv", header = TRUE)

typeof(clinton.dislikes)

######1.Create a word cloud with the ANES 2016 open ended question about what respondents LIKE about Donald Trump. 
names(trump.likes)
names(trump.likes)[1] <- 'Respondent ID'
names(trump.likes)[2] <- 'What do you like about Trump?'

#install.packages('tidytext')
library(tidytext)
library(wordcloud2)

# what unnest does:
# splits elements of a vector into multiple rows,
# makes everything lower case (very helpful!)
trump.likes.words <-trump.likes %>% 
  unnest_tokens(word, `What do you like about Trump?`) 

#without removing stop words
# trump.likes.words %>% 
#   count(word, sort= TRUE) %>%
#   head()

trump.likes.wordsC <- trump.likes.words %>% 
  anti_join(stop_words) %>%
  count(word, sort= TRUE)

pal <- brewer.pal(6,"Dark2")
wordcloud(words = trump.likes.wordsC$word,
          freq  = trump.likes.wordsC$n,
          max.words = 100,
          random.order = FALSE,
          colors = pal)





trump.likes.wordsC[1, ]


trump.like.corpus <- Corpus(VectorSource(trump.likes$`What do you like about Trump?`))
#inspect(trump.like.corpus) #now, we have 4271 documents instead of rows

# Cleaning up the data
# trump.like.corpus <- tm_map(trump.like.corpus, removePunctuation)
# trump.like.corpus <- tm_map(trump.like.corpus, removeNumbers)
# trump.like.corpus <- tm_map(trump.like.corpus, content_transformer(tolower))
trump.like.corpus <- tm_map(trump.like.corpus, removeWords, stopwords("english"))
# trump.like.corpus <- tm_map(trump.like.corpus, stripWhitespace); #inspect(docs[1])
# trump.like.corpus <- tm_map(trump.like.corpus, stemDocument)

# table(stopwords('english') == 'his')
# stopwords('english')[stopwords('english') == 'his']
# stopwords('english')[stopwords('english') == 'get']
#mycorpus_clean <- tm_map(mycorpus1, content_transformer(tolower))#making everything lowercase


trump.like.corpus.term.doc <- TermDocumentMatrix(trump.like.corpus) 
#inspect(trump.like.corpus.term.doc)


trump.like.corpus.matrix <- as.matrix(trump.like.corpus.term.doc) 
trump.like.corpus.matrix <- sort(rowSums(trump.like.corpus.matrix), decreasing = TRUE) 

trump.like.corpus.DF <- data.frame(word = names(trump.like.corpus.matrix), freq = trump.like.corpus.matrix)

trump.like.corpus.DF[trump.like.corpus.DF=='his',]

# Making the wordcloud

pal <- brewer.pal(6,"Dark2")

wordcloud(words = trump.like.corpus.DF$word,
          freq  = trump.like.corpus.DF$freq,
          max.words = 100,
          random.order = FALSE,
          rot.per=0.35,
          colors = pal,
          # width=800, height=400,
          # scale=c(3.5,0.25)
          )


# install.packages('wordcloud2')
library(wordcloud2)
wordcloud2(data=trump.like.corpus.DF)

######2.Create a word cloud with the ANES 2016 open ended question about what respondents DISLIKE about Donald Trump. 

names(trump.dislikes)[1] <- 'Respondent ID'
names(trump.dislikes)[2] <- 'What do you dis-like about Trump?'

trump.dislike.corpus <- Corpus(VectorSource(trump.dislikes$`What do you dis-like about Trump?`))

# Cleaning up the data
# trump.dislike.corpus <- tm_map(trump.dislike.corpus, removePunctuation)
# trump.dislike.corpus <- tm_map(trump.dislike.corpus, removeNumbers)
# trump.dislike.corpus <- tm_map(trump.dislike.corpus, content_transformer(tolower))
trump.dislike.corpus <- tm_map(trump.dislike.corpus, removeWords, stopwords("english"))
# trump.dislike.corpus <- tm_map(trump.dislike.corpus, stripWhitespace); #inspect(docs[1])
# trump.dislike.corpus <- tm_map(trump.dislike.corpus, stemDocument)


trump.dislike.corpus.term.doc <- TermDocumentMatrix(trump.dislike.corpus) 

trump.dislike.corpus.matrix <- as.matrix(trump.dislike.corpus.term.doc) 
trump.dislike.corpus.matrix <- sort(rowSums(trump.dislike.corpus.matrix), decreasing = TRUE) 

trump.dislike.corpus.DF <- data.frame(word = names(trump.dislike.corpus.matrix), freq = trump.dislike.corpus.matrix)

# Making the wordcloud

pal <- brewer.pal(6,"Dark2")
wordcloud(trump.dislike.corpus.DF$word, freq= trump.dislike.corpus.DF$freq, max.words = 100, 
          random.order = FALSE, colors = pal)

######3.Create a word cloud with the ANES 2016 open ended question about what respondents LIKE about Hilary Clinton. 
 
names(clinton.likes)[1] <- 'Respondent ID'
names(clinton.likes)[2] <- 'What do you like about Clinton?'

clinton.like.corpus <- Corpus(VectorSource(clinton.likes$`What do you like about Clinton?`))

# Cleaning up the data
# clinton.like.corpus <- tm_map(clinton.like.corpus, removePunctuation)
# clinton.like.corpus <- tm_map(clinton.like.corpus, removeNumbers)
# clinton.like.corpus <- tm_map(clinton.like.corpus, content_transformer(tolower))
clinton.like.corpus <- tm_map(clinton.like.corpus, removeWords, stopwords("english"))
# clinton.like.corpus <- tm_map(clinton.like.corpus, stripWhitespace); #inspect(docs[1])
# clinton.like.corpus <- tm_map(clinton.like.corpus, stemDocument)

clinton.like.corpus.term.doc <- TermDocumentMatrix(clinton.like.corpus) 

clinton.like.corpus.matrix <- as.matrix(clinton.like.corpus.term.doc) 
clinton.like.corpus.matrix <- sort(rowSums(clinton.like.corpus.matrix), decreasing = TRUE) 

clinton.like.corpus.DF <- data.frame(word = names(clinton.like.corpus.matrix), freq = clinton.like.corpus.matrix)

# Making the wordcloud
pal <- brewer.pal(6,"Dark2")
wordcloud(clinton.like.corpus.DF$word, freq= clinton.like.corpus.DF$freq, max.words = 100, 
          random.order = FALSE, colors = pal)

######4.Create a word cloud with the ANES 2016 open ended question about what respondents DISLIKE about Hilary Clinton. 


names(clinton.dislikes)[1] <- 'Respondent ID'
names(clinton.dislikes)[2] <- 'What do you dis-like about Clinton?'

clinton.dislike.corpus <- Corpus(VectorSource(clinton.dislikes$`What do you dis-like about Clinton?`))

# Cleaning up the data
# clinton.dislike.corpus <- tm_map(clinton.dislike.corpus, removePunctuation)
# clinton.dislike.corpus <- tm_map(clinton.dislike.corpus, removeNumbers)
# clinton.dislike.corpus <- tm_map(clinton.dislike.corpus, content_transformer(tolower))
clinton.dislike.corpus <- tm_map(clinton.dislike.corpus, removeWords, stopwords("english"))
# clinton.dislike.corpus <- tm_map(clinton.dislike.corpus, stripWhitespace); #inspect(docs[1])
# clinton.dislike.corpus <- tm_map(clinton.dislike.corpus, stemDocument)


clinton.dislike.corpus.term.doc <- TermDocumentMatrix(clinton.dislike.corpus) 

clinton.dislike.corpus.matrix <- as.matrix(clinton.dislike.corpus.term.doc) 
clinton.dislike.corpus.matrix <- sort(rowSums(clinton.dislike.corpus.matrix), decreasing = TRUE) 

clinton.dislike.corpus.DF <- data.frame(word = names(clinton.dislike.corpus.matrix), freq = clinton.dislike.corpus.matrix)

# Making the wordcloud

pal <- brewer.pal(6,"Dark2")
wordcloud(clinton.dislike.corpus.DF$word, freq= clinton.dislike.corpus.DF$freq, max.words = 100, 
          random.order = FALSE, colors = pal)


######For your analysis:
######1.Compare the "like" word clouds for Trump and Obama (from lecture.) 
###Are there similarities/differences? 

###What conclusions do you draw?
  
######2.Compare the word clouds for Donald Trump and Hilary Clinton. 
###Comment on your findings. 
