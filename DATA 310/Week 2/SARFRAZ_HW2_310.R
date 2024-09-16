library(tidyverse)
# 1. Solve the following questions about the specified normal random variables
# (a) X ??? N(26, 54) (i.e., mean of 26 and variance of 54)
#    i. p(24 < X < 30) = ?
pnorm(30,26,sqrt(54),lower.tail = T)-pnorm(24,26,sqrt(54),lower.tail = T)

#    ii. Solve for c such that p(X < 45) = p(Z < c), where Z ??? N(0, 1)
#first find area under the normal random variable curve
pnorm(45,26,sqrt(54),lower.tail = T)

#second, solve for c
qnorm(0.9951391,0,sqrt(1),lower.tail = T)
qnorm(pnorm(45,26,sqrt(54),lower.tail = T),0,sqrt(1),lower.tail = T)

#    iii. Find lb and ub on the symmetric CI such p(lb < X < ub) = 0.90 
#ub
qnorm(0.95,26,sqrt(54),lower.tail = T)
#lb
qnorm(0.05,26,sqrt(54),lower.tail = T)

# (b) Y ??? N(43, 23) (i.e., mean of 43 and variance of 23)
#    i. p(40 < Y < 50) = ?
pnorm(50,43,sqrt(23),lower.tail = T)-pnorm(40,43,sqrt(23),lower.tail = T)

#    ii. Solve for c such that p(Y < 35) = p(Z < c), where Z ??? N(0, 1)
#first find area under the normal random variable curve
pnorm(35,43,sqrt(23),lower.tail = T)

#second, solve for c
qnorm(0.04764642,0,sqrt(1),lower.tail = T)
qnorm(pnorm(35,43,sqrt(23),lower.tail = T),0,sqrt(1),lower.tail = T)

#     iii. Find the lb and ub on the symmetric CI such p(lb < Y < ub) = 0.99
#ub
qnorm(0.995,43,sqrt(23),lower.tail = T)
#lb
qnorm(0.005,43,sqrt(23),lower.tail = T)

#2.Modify the R code YahtzeeFirstRoll.R to answer the following questions:
###(a) Approximate probability of the "maxnumber" random variable equaling one
###    when rolling four dice
# How many dice
numdice = 4
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
p <- ggplot(data=maxnumberdf, aes(x=V1)) +         
  geom_bar( color="black") + 
  scale_x_discrete(limits=c(1:numdice)) + 
  labs(title="1,000,000 sims.of 5 dice",  
       x="Max. of Same Number", 
       y = "Number of Realizations")

p

maxnumberdf <- as.data.frame(maxnumber)

table(maxnumberdf)
prop.table(table(maxnumberdf))
#probability of one occuring with 4 dice
table(maxnumberdf)[0:1]/sum(table(maxnumberdf))
 
###(b) Approximate probability of the "maxnumber" random variable equaling three
###    or more when we roll six dice
# How many dice
numdice = 6
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

sum(table(maxnumberdf)[3:6])
sum(prop.table(table(maxnumberdf))[3:6])
#probability of 3 or more occuring with 6 dice
sum(table(maxnumberdf)[3:6])/sum(table(maxnumberdf))

###(c) In Yahtzee, there are two ways to roll what is called a "large straight"
###You either need to a) roll a one, a two, a three, a four, and a five on 
###your five dice,b) roll a two, a three, a four, a five, and a six on your 
###five dice. Approximate the probability of rolling a large straight with 5 dice.
# How many dice
numdice = 5
# How many simulations
numsims = 1000000
# Initializes a vector that contains the outcome of each simulation
maxnumber <- matrix(NA, nrow = numsims, ncol = 1)  
rollsum <- matrix(NA, nrow = numsims, ncol = 1)

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
  rollsum[s] <- sum(dice)
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
mean((rollsum == 15 | rollsum == 20) & maxnumber == 1)

#3.We are going to write a Monte Carlo simulation in R to simulate the flipping of a series
#of coins.
###(a) Before you start working on this problem, write down a series of 25 
###    made-up coin flips without using R or any other randomizing device
###    (e.g., {HTTHTHTHHTHTTTTTHTHTHTHTT}).
{THTTTHTTHHTTHHTTHHHTTTHHH}


###(b) Modify the R code FlippingCoinsPartial.R to simulate the approximate 
###    cdf of the random variable equal to the longest streak in a series of
###    25 fair coin flips when
# you run 100,000 simulations. Every spot where you observe "XXX" in the code
# indicates something that you will need to fill in for your simulation to work. I am
# pasting a copy of the cdf that my simulation created below for your reference.

# Sets the seed
set.seed(12221979)

# Defines the number of coins being flipped
numcoins = 25
# Defines the number of simulations
numsims = 100000
# Defines the probability that a coin is heads
probheads = 0.5

# Initializes a vector that contains the longest streak in every simulation
longstreak <- matrix(-9, nrow = numsims, ncol = 1) 

# Uses a for loop to loop over simulations
for (s in 1:numsims) {
  
  # Uses rbinom() function to draw a series Bernoulli r.v.s 
  # The number r.v.s in the series is defined by the parameter numcoins
  coins <- rbinom(numcoins, 1, probheads)
  # Initializes a vector that contains the streak after every coin
  streak <- matrix(-9, nrow = numcoins, ncol = 1) 
  # Initilaizes the streak to 1 after the first coing
  streak[1] <- 1
  # Uses a for loop to calculate streak after every coin
  for (c in 2:numcoins) {
    
    streak[c] <- 1 + streak[c-1]*(coins[c] == coins[c-1])
    
  }
  
  # Identifies the longest streak and stores in longest streak vector
  longstreak[s] <- max(streak)
  
}

# Uses table command to create a pdf
pdf <- table(longstreak) / numsims
# Creates the cdf by looping over the pdf
cdf <- pdf
for (c in 2:dim(cdf)) {
  cdf[c] <- cdf[c-1] + pdf[c]
}

# Makes cdf into a data frame for plotting purposes
cdfdf <- as.data.frame(cdf)
# Plots the cdf
p <- ggplot(data=cdfdf, aes(x=longstreak, y=Freq, group=1)) +
  geom_line()+
  geom_point()+
  labs(title = "CDF in 100,000 Sims.", x = "Length of Streak of Same Coin", y = "Pr(Longest Streak <= Length)")
p
# (c) Acording to the cdf, what is the probability of having a streak of more than 5
# coins show up?
1 - cdfdf$Freq[cdfdf$longstreak == 5]

# (d) Using the series of coin flips you constructed before beginning this
#     problem, what is the probability of having a streak longer than your
#     longest streak?

#my longest streak is 3 
1 - cdfdf$Freq[cdfdf$longstreak == 3]
