library(tidyverse)
# How many dice
numdice = 5
# How many simulations
numsims = 1000000
# Initializes a vector that contains the outcome of each simulation
maxnumber <- matrix(NA, nrow = numsims, ncol = 1)  
# Sets the seed
set.seed(12221979)

for (s in 1:numsims) {
  
  # Draws uniform r.v.
  uniform <- runif(n = numdice, min = 0, max = 1)
  # Initializes a vector
  dice <- matrix(-9, nrow = numdice, ncol = 1)  
  # Transforms uniform r.v. into dice rolls
  # Applies inverse sampling 
  dice[uniform >= 0 & uniform <= (1/6)] <- 1
  dice[uniform > (1/6) & uniform <= (2/6)] <- 2
  dice[uniform > (2/6) & uniform <= (3/6)] <- 3
  dice[uniform > (3/6) & uniform <= (4/6)] <- 4
  dice[uniform > (4/6) & uniform <= (5/6)] <- 5
  dice[uniform > (5/6) & uniform <= (6/6)] <- 6
  
  # Finds the mode of dice and stores in maxnum
  uniqv <- unique(dice)
  maxnum <- uniqv[which.max(tabulate(match(dice, uniqv)))]
  
  # Stores how many dice equal the mode
  maxnumber[s] <- sum(dice == maxnum)
}

# Plots histogram of the max number from each simulation
maxnumberdf <- as.data.frame(maxnumber)
p <- ggplot(data=maxnumberdf, aes(x=V1)) +         
  geom_bar( color="black") + 
  scale_x_discrete(limits=c(1:numdice)) + 
  labs(title="1,000,000 sims.of 5 dice",  
       x="Max. of Same Number", 
       y = "Number of Realizations")

p