#Suppose you are finding the mean of random variables that have an expected 
#value of 76 and variance of a variance of 35. Applying the Central Limit 
#Theorem, use R to answer the following questions
# (a) What is the probability that the average is between 73 and 78 in a random
#     sample of 50 observations?

#another way to do the problem
# standrd.error <- sqrt(35)/sqrt(50)
# pnorm(78,76,standrd.error,lower.tail = T)-pnorm(73,76,standrd.error,lower.tail = T)

pnorm(78,76,sqrt(35)/sqrt(50),lower.tail = T)-pnorm(73,76,sqrt(35)/sqrt(50),lower.tail = T)

# (b) Find a symmetric 99 percent confidence interval on the mean in a random
#     sample of 75 observations?

#lb
qnorm(.005,76,sqrt(35)/sqrt(75),lower.tail=T)
#ub
qnorm(.995,76,sqrt(35)/sqrt(75),lower.tail=T)

# (c) How large must my sample be in order for there to be a 95 percent probability
#     that my sample average is between 75.5 and 76.5? (Note, you will likely have
#     to round the answer you find to the nearest whole number. We can't sample a
#     fraction of person. YET.)

#getting the upper interval for the standard normal variable
#qnorm(0.975,0,1,lower.tail = T) #1.959964

#getting the interval ratio for both graphs
#((qnorm(0.975,0,1,lower.tail = T))/0.5) #3.919928
#1.96/0.5 #3.92

#converting the sqrt(35) to how it will look in the standard normal graph
(sqrt(35)*((qnorm(0.975,0,1,lower.tail = T))/0.5)) #23.19061
#sqrt(35)*3.92 #23.19

#squaring the values to find the sampling size
(sqrt(35)*((qnorm(0.975,0,1,lower.tail = T))/0.5))**2 #538
# 23.19**2 #538

#checking answer
#qnorm(.975,76,sqrt(35)/sqrt("Sample_Size"),lower.tail = T) # =76.5
qnorm(.975,76,sqrt(35)/sqrt((sqrt(35)*((qnorm(0.975,0,1,lower.tail = T))/0.5))**2),lower.tail = T) #76.5

(sqrt(35)/(0.5/1.96))**2
(sqrt(35)/(1.96/0.5))**2

library(readxl)
library(tidyverse)
# 2. One thing that we do when we make election projections is collect data from a sample of
# precincts and project something about what vote outcomes are likely to be in the entire
# state based on what we observe in the sample. We are going to do a simplified example
# of this using data from Ohio's 2016 presidential election. You will find on Canvas a
# dataset entitled "Ohio2016.xlsx." This dataset contains the number of ballots cast for
# Trump, Clinton, and all other candidates in this election by precinct).

# . (a) Load this dataset into R (I used the read excel() function from the readxl
#                                 library, another option is to use the import() function from the 'rio' package).

setwd('C:/Users/hussainsarfraz/Desktop/DATA 310')
ohio2016.election <- read_xlsx('Ohio2016.xlsx')

ohio2016.election <- ohio2016.election %>%
  mutate(trump.vote = Trump/Ballots)

ohio2016.election %>%
  ggplot() +
  geom_histogram(aes(x = trump.vote)) +
  xlab("Trump Vote Percentages") +
  ylab('Count of Percentages') +
  theme_bw() +
  ggtitle('Percentage Distribution of Trump Votes') +
  theme(plot.title = element_text(hjust = 0.5)) 



#(b)Given that this is all of the precints in Ohio, we can treat this like a 'Census'.
# Calculate the population mean and variance for Trump's vote share in a precinct.
mean(ohio2016.election$trump.vote)
var(ohio2016.election$trump.vote)

#(c) Suppose we sampled Trump's vote share from 40 randomly selected precincts
# on Election Night and averaged them together. Combine your knowledge of the
# population mean and variance and the Central Limit Theorem to make a prediction about what the 99 percent confidence interval would be on this statistic.

#lb
qnorm(0.005,mean(ohio2016.election$trump.vote),sqrt(var(ohio2016.election$trump.vote))/sqrt(40),lower.tail = T)

#ub
qnorm(0.995,mean(ohio2016.election$trump.vote),sqrt(var(ohio2016.election$trump.vote))/sqrt(40),lower.tail = T)

#(d) Calculate the bounds of this 99 percent confidence interval if 80 or 120
# precincts were sampled instead.

#80 sample size
#lb
qnorm(0.005,mean(ohio2016.election$trump.vote),sqrt(var(ohio2016.election$trump.vote))/sqrt(80),lower.tail = T)
#ub
qnorm(0.995,mean(ohio2016.election$trump.vote),sqrt(var(ohio2016.election$trump.vote))/sqrt(80),lower.tail = T)


#120 sample size
#lb
qnorm(0.005,mean(ohio2016.election$trump.vote),sqrt(var(ohio2016.election$trump.vote))/sqrt(120),lower.tail = T)
#ub
qnorm(0.995,mean(ohio2016.election$trump.vote),sqrt(var(ohio2016.election$trump.vote))/sqrt(120),lower.tail = T)



#(e) To confirm that the bounds calculated by the Central Limit Theorem are
# correct, run the following simulation
# - Draw a random sample of 120 precincts from the data (I use the 'sample()'
#                                                        function to do this).
trump.sample <- sample(ohio2016.election$trump.vote, 120, replace = FALSE, prob = NULL)


# - Calculate the average Trump vote share in these precincts.
# - Store this average in a vector
trump.average <- mean(trump.sample)

# - Repeat those three steps 10,000 times using a for loop

repeats <- 10000
final.vector <- rep(NA, length(repeats))

for(i in 1:repeats){
  
  samp <- sample(ohio2016.election$trump.vote, 120, replace = F, prob = NULL)
  
  final.vector[i] <- mean(samp)
}

# - After running the 10,000 simulations, use the quantile() function to find 0.5
# and 99.5 percentiles of the average Trump vote share over the 10,000 simulations. 
# Compares these values to the bounds on the symmetric 99 percent
# confidence interval you estimated using the Central Limit Theorem.

quantile(final.vector, probs = c(0.005,0.995))

plot(density(final.vector))
mean(final.vector)

