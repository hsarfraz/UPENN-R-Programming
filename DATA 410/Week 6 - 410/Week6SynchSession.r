#---------------------------------------
# Text Mining & Sentiment Analysis
#---------------------------------------
library(dplyr)
library(tidyverse)
setwd("~/Desktop/Teaching - Penn/Courses/DATA401-101/Week6")
# Text Mining
# What is it?

# Text mining is a fancy way of saying - analyzing 
# qualitative data with traditionally quantitative methods
# word clouds are a type of text mining! 

# Why is it useful?
# Qualitative data is quite the beast. Using R can help make
# it faster, systematized, and less biased (maybe!)


# Sentiment Analysis

# What is it? A type of text mining!
# It is a process of assigning either a sentiment
# or an emotion to text data. 
# sentiment: positive or negative
# emotion: happy, angry, sad, etc


# Why is it useful?
# You can get the 'gist' without having to read
# hundreds or thousands of text documents


# For this lecture, we're going to use a package by
# Hadley Wickham & co

# The tidytext package uses the same 'tidy' principles
# as all of the 'tidy' packages:

# 1. each variable is a column
# 2. Each observation is a row
# 3. Each type of observational unit is a table (https://serialmentor.com/blog/2014/7/21/keep-your-data-tidy-part-ii)

# We're going to read in data from ANES on Chief Justice Roberts
roberts <- read.csv("roberts_responses.csv", stringsAsFactors = FALSE)

head(roberts)

# The column names aren't great, so we're going to clean those up
colnames(roberts)<-c("ID", "opinion")

#install.packages("tidytext")
library(tidytext)

# This data already fits the tidy text format, so we're almost ready
# to roll. But, to perform most types of text mining, we need to 
# turn our sentences into smaller pieces


# what unnest does:
# splits elements of a vector into multiple rows,
# makes everything lower case (very helpful!)
sentis <-roberts %>% 
  unnest_tokens(word, opinion)

head(sentis)


# Now we can get a simple frequency of words
# with the count function
sentis %>% 
  count(word, sort= TRUE)


# Let's get rid of stopwords when
# we run this again
sentis %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)




#-----------------------
# Sentiment Analysis

# Unigrams - based on a single word - They do NOT take 
# qualifiers into account

# 1. AFINN - scores words between -5 and + 5 (neg/pos)
# 2. Bing - classifies words as either positive or negative, 
# 3. nrc - classifies words as: positive, negative, 
# anger, anticipation, disgust, fear, joy, sadness, surprise
# and trust

# How they were created
# Licensing agreements

# install.packages("textdata")
library(textdata)

# Let's take a peak at them 
get_sentiments("afinn")

get_sentiments("bing")

get_sentiments("nrc")



# Let's start with the nrc

# I'm going to pull out just the negative associated words 
# from the nrc
nrc_negative <- filter(get_sentiments("nrc"), sentiment == "negative")

# then, we can join them to our data and pull out just the rows from the
# data that match our group of negative words
sentis %>%
  inner_join(nrc_negative) %>% 
  count(word, sort = TRUE)

# What is a limitation of this analysis? Any problems here?


# Now let's see how Bing's negative words may differ
bing_neg <- filter(get_sentiments("bing"), sentiment == "negative")

sentis %>%
  inner_join(bing_neg) %>% 
  count(word, sort = TRUE)

# Quite a bit of variation!


# Now let's try AFINN in its entirety - how do people feel about 
# roberts?
afindf <- sentis %>%
  inner_join(get_sentiments("afinn"))

# What does the average tell us?
mean(afindf$value)

# But is it fair?

aftable <- afindf %>%
  group_by(word) %>%
  summarise(sum(value))


# REMEMBER: now that you know how sentiment analysis works, you can
# start making decisions about how the tool can best be used to
# answer your question. Suppose we're trying to answer "how does the
# us population feel about justice roberts" and seeing our
# word frequency table, what might we want to do?

# Since sentiment analysis relies on words to come up with
# a numerical description, we may want to remove certain words
# that the function will read as influential when we know that they are
# probably not so

# for example, the words 'justice' and 'supreme' will probably return
# as highly positive - however, no one is using these terms because they
# are saying roberts is 'supreme' they are calling him a 'supreme court 
# justice' which is just his title, and shouldn't be used to analyze
# how that person feels about roberts. 
# And what about how the question was phrased? Should we include the
# word "no"?


#---------------------------------------------
# n-grams

# So far, we've been working with 'unigrams' or single words
# but language is complex - to really understand sentiment, 
# context and sentence construction should matter. 


# we'll make bigrams by adding two arguments to our
# unnest function
sentbi <-roberts %>% 
  unnest_tokens(word, opinion, token = "ngrams", n=2)


# now its looking like we are getting actual
# thoughts!
sentbi %>%
  count(word, sort = TRUE)


# The same logic of stopwords applies here as well
# however, in order to get rid of stop words, we
# have to take an extra step to separate our bigrams

# first, we'll need to separatet the two words
bigrams_separated <- sentbi %>%
  separate(word, c("word1", "word2"), sep = " ")

# then we can filter based on words not being in stopwords list
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# and now we can use our new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

# bigrams still dont' inherently take care of the
# qualifier issue. but they do allow us to manually
# get a little closer. let's look into bigrams that
# begin with "not"
bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)


# And now let's use AFINN to get closer
# to analyzing these qualified statements
AFINN <- get_sentiments("afinn")
AFINN

# we need to join based only on the second
# word so that AFINN does not take into account 
# the 'not' ALSO - AFINN was created for unigrams
# so it is not set up to interpret bigrams. By 
# joining on just the 2nd word, we're defacto
# analysing unigrams again
not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)

# words that were preceded by 'not'
not_words

# What should we do here? 

#-------------------------------------------------------
# let's see what people said they liked about
# Donald Trump

trump <- read.csv("redacted_trump_2016.csv")

colnames(trump)<- c("ID", "likes")


# let's unnest the words into bigrams
trumpbi <-trump %>% 
  unnest_tokens(word, likes, token = "ngrams", n=2)

# i have a hunch that if we look at the 
# bigrams, we'll see something interesting
bigrams_separated <- trumpbi %>%
  separate(word, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

# not words
bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)


#----------
# trigrams!
trumptri <-trump %>% 
  unnest_tokens(word, likes, token = "ngrams", n=3)


trigrams_separated <- trumptri %>%
  separate(word, c("word1", "word2", "word3"), sep = " ")


trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word)

# new trigram counts:
trigram_counts <- trigrams_filtered %>% 
  count(word1, word2,word3, sort = TRUE)

# let's find the not words
trigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2,word3, sort = TRUE)



#------------------------------------------------------

# Applying sentiment analysis
AFINN <- get_sentiments("afinn")

AFINN

not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)

# words that were preceded by 'not'
not_words


#--------------
# Let's check out what people liked about Hilary Clinton

hilary <- read.csv("redacted_2016_clinton_likes.csv")

colnames(hilary) <- c("ID", "likes")

# let's unnest the words into bigrams
hilarybi <-hilary %>% 
  unnest_tokens(word, likes, token = "ngrams", n=2)

# separate them
bigrams_separatedhc <- hilarybi %>%
  separate(word, c("word1", "word2"), sep = " ")

# remove the stopwords
bigrams_filteredhc <- bigrams_separatedhc %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_countshc <- bigrams_filteredhc %>% 
  count(word1, word2, sort = TRUE)

# does this surprise you?
bigram_countshc

# let's look at percentages

# trump percentage
bigram_counts$percent <- (bigram_counts$n/sum(bigram_counts$n))*100
bigram_counts

# clinton percentage
bigram_countshc$percent <- (bigram_countshc$n/sum(bigram_countshc$n))*100
bigram_countshc

# Very interesting!


#-----------------------------------------------------------
# Now let's compare the sentiments of their unigrams!
# We'll use bigrams so that we can filter out the 'nots'

# Let's go back and pull our trump bigrams without the
# word 'not'
trumpbi_wn <- bigrams_filtered %>%
  filter(word1!= "not") %>%
  count(word1, word2, sort = TRUE)
trumpbi_wn

# And let's get the same for HC
hilarybi_wn <- bigrams_filteredhc %>%
  filter(word1!= "not") %>%
  count(word1, word2, sort = TRUE)
hilarybi_wn

# Did they have anything in common? Where were the differences?
# let's prep some percentages
trumpbi_wn$percent <- (trumpbi_wn$n/sum(trumpbi_wn$n))*100
hilarybi_wn$percent <- (hilarybi_wn$n/sum(hilarybi_wn$n))*100

trumpbi_wn$words <- paste(trumpbi_wn$word1, trumpbi_wn$word2)
hilarybi_wn$words <- paste(hilarybi_wn$word1, hilarybi_wn$word2)

# merge together, but just keep the common words
together <- trumpbi_wn %>% 
  inner_join(hilarybi_wn, by = c(words = "words"))
# let's take a look
together

# now let's visualize!
ggplot(together, aes(x = percent.x, y = percent.y)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = words), check_overlap = TRUE, vjust = 1.5)+
  xlab("Donald Trump")+
  ylab("Hilary Clinton")



#-------------------------------------------------------------------------------


# We can re-do the analysis above for the 2020 election!


#-------------------------------------------------------
# let's see what people said they liked about
# Donald Trump in 2020!

trump<- read_csv("rep-candidate-2020-likes.csv")

colnames(trump)<- c("ID", "likes")


# let's unnest the words into bigrams
trumpbi <-trump %>% 
  unnest_tokens(word, likes, token = "ngrams", n=2)

# i have a hunch that if we look at the 
# bigrams, we'll see something interesting
bigrams_separated <- trumpbi %>%
  separate(word, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

# not words
bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)


#----------
# trigrams!
trumptri <-trump %>% 
  unnest_tokens(word, likes, token = "ngrams", n=3)


trigrams_separated <- trumptri %>%
  separate(word, c("word1", "word2", "word3"), sep = " ")

trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word)

# new bigram counts:
trigram_counts <- trigrams_filtered %>% 
  count(word1, word2,word3, sort = TRUE)

# let's find the not words
trigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2,word3, sort = TRUE)



#------------------------------------------------------

# Applying sentiment analysis
AFINN <- get_sentiments("afinn")

AFINN

not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)

# words that were preceded by 'not'
not_words


#--------------
# Let's check out what people liked about Joe Biden!

biden <- read_csv("dem-candidate-2020-likes.csv")

colnames(biden) <- c("ID", "likes")

# let's unnest the words into bigrams
bidenbi <-biden %>% 
  unnest_tokens(word, likes, token = "ngrams", n=2)

# separate them
bigrams_separatedb <- bidenbi %>%
  separate(word, c("word1", "word2"), sep = " ")

# remove the stopwords
bigrams_filteredb <- bigrams_separatedb %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_countsb <- bigrams_filteredb %>% 
  count(word1, word2, sort = TRUE)

# does this surprise you?
bigram_countsb

# let's look at percentages

# trump percentage
bigram_counts$percent <- (bigram_counts$n/sum(bigram_counts$n))*100
bigram_counts

# clinton percentage
bigram_countsb$percent <- (bigram_countsb$n/sum(bigram_countsb$n))*100
bigram_countsb

# Very interesting!


#-----------------------------------------------------------
# Now let's compare the sentiments of their unigrams!
# We'll use bigrams so that we can filter out the 'nots'

# Let's go back and pull our trump bigrams without the
# word 'not'
trumpbi_wn <- bigrams_filtered %>%
  filter(word1!= "not") %>%
  count(word1, word2, sort = TRUE)
trumpbi_wn

# And let's get the same for HC
bidenbi_wn <- bigrams_filteredb %>%
  filter(word1!= "not") %>%
  count(word1, word2, sort = TRUE)
bidenbi_wn

# Did they have anything in common? Where were the differences?
# let's prep some percentages
trumpbi_wn$percent <- (trumpbi_wn$n/sum(trumpbi_wn$n))*100
bidenbi_wn$percent <- (bidenbi_wn$n/sum(bidenbi_wn$n))*100

trumpbi_wn$words <- paste(trumpbi_wn$word1, trumpbi_wn$word2)
bidenbi_wn$words <- paste(bidenbi_wn$word1, bidenbi_wn$word2)

# merge together, but just keep the common words
together <- trumpbi_wn %>% 
  inner_join(bidenbi_wn, by = c(words = "words"))
# let's take a look
together

# now let's visualize!
ggplot(together, aes(x = percent.x, y = percent.y)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = words), check_overlap = TRUE, vjust = 1.5)+
  xlab("Donald Trump")+
  ylab("Joe Biden")



ggplot(together, aes(x = percent.x, y = percent.y)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = words), check_overlap = TRUE, vjust = 1.5)+
  xlab("Donald Trump")+
  ylab("Joe Biden")+
  ylim(0,6)+
  xlim(0,6)

