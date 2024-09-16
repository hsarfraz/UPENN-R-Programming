# Week 2 Homework key

# Question 1 
# Where X ~ N(26,54)
#Note that 54 is the *variance*, while p/qnorm asks for the standard deviation
#So we'll have to take the square root of 54

# 1.(ai) Find the prob that x lies between the two points
#pnorm(ub, mean, sd) - pnorm(lb, mean, sd)

pnorm(30, 26, sqrt(54), lower.tail = TRUE) - pnorm(24, 26, sqrt(54), lower.tail = TRUE)
# answer is .3141


#1.(aii)
#Solve for c such that p(X<45) =p(Z<c), where Z is standard normal.

pnorm(45,26,sqrt(54))

#The probability in the left tail of normal distribution X is .995

qnorm(.995, 0,1)

#The equivalent value that gives the same probability in the left tail
#under the standard normal is 2.57


# 1(aiii) find the 90 % symmetrical confidence interval 
#To have a 90%confidence interval we want 5% in each tail

qnorm(.95, 26, sqrt(54))
qnorm(.05, 26, sqrt(54))



#The 90% CI is [13.9  to  38.09]


# Question 2
#2(ai) What is p(40< Y <50)?
pnorm(50, 43, sqrt(23)) - pnorm(40, 43, sqrt(23))
#.661

#2(aii)
#Solve for c such that p(Y<35) =p(Z<c), where Z is standard normal.

pnorm(35,43,sqrt(23))

#The probability in the left tail of normal distribution X is .048

qnorm(.048, 0,1)

#The equivalent value that gives the same probability in the left tail
#under the standard normal is -1.66

#2b(iii)
#find the 99 % symmetrical confidence interval 
#To have a 99%confidence interval we want .05% in each tail
qnorm(.995, 43, sqrt(23), lower.tail = TRUE)
qnorm(.005, 43, sqrt(23), lower.tail = TRUE)


#The 99% CI is [30.64  to  55.35]

####################
# Question 2  Yahtzee

# 2a

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
maxnumberdf <- as.data.frame(maxnumber)
p <- ggplot(data=maxnumberdf, aes(x=V1)) +         
  geom_bar( color="black") + 
  scale_x_discrete(limits=c(1:numdice)) + 
  labs(title="1,000,000 sims.of 4 dice",  
       x="Max. of Same Number", 
       y = "Number of Realizations")
p

# A table will give us the distribution of maxnumber
table(maxnumber)

#Because this is out of 1 million simulations we see that around 27% of the time 
#we get a roll that has no repeating numbers



# 2b



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
  labs(title="1,000,000 sims.of 6 dice",  
       x="Max. of Same Number", 
       y = "Number of Realizations")
p

# Again, we can use a table to determine
table(maxnumber)
table(maxnumber)[3:6]
sum(table(maxnumber)[3:6])

#Around 37% of the time with 6 dice we get groupings of 3 and above.


# 2c

#Note that for the large straight two things have to be true
#(1) No repeating dice
#(2) The sum of the dice will equal 15 or 20
1+2+3+4+5
2+3+4+5+6

#It can't be just (1) because then you could get 1,3,4,5,6 etc.

#So the modification here is to also calculate the sum of the dice 
#that get rolled

# How many dice
numdice = 5
# How many simulations
numsims = 1000000
# Initializes a vector that contains the outcome of each simulation
maxnumber <- matrix(NA, nrow = numsims, ncol = 1)  
dice.sum <- matrix(NA, nrow = numsims, ncol = 1)  

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
  #Stores the summation of all the dice values
  dice.sum[s] <- sum(dice)
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

#The times when we have a large straight are when there are no repeating dice
#and the sum of the dice equal 15 or 20

mean(maxnumber==1 & (dice.sum==15| dice.sum==20))

#Around 3% of the time

#########################
# Question 3: Monte Carlo simulation

# 3b. 

# Defines the number of coins being flipped
numcoins = 25
# Defines the number of simulations
numsims = 100
# Defines the probability that a coin is heads
probheads = .5

# Initializes a vector that contains the longest streak in every simulation
longstreak <- matrix(NA, nrow = numsims, ncol = 1) 

# Uses a for loop to loop over simulations
for (s in 1:numsims) {
  
  # Uses rbinom() function to draw a series Bernoulli r.v.s 
  # The number r.v.s in the series is defined by the parameter numcoins
  coins <- rbinom(numcoins, size = 1, probheads)
  # Initializes a vector that contains the streak after every coin
  streak <- matrix(NA, nrow = numcoins, ncol = 1) 
  # Initilaizes the streak to 1 after the first coin
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


# 3c. 



# the probability of rolling a streak greater than 5 coins long is 30%
1 - cdfdf$Freq[cdfdf$longstreak==5]

#3d

#Depends on what your specific hand-made flips looked like, but
# for a max of 3...

# the probability of rolling a streak greater than 3 coins long is 85%
1 - cdfdf$Freq[cdfdf$longstreak==3]

#(Most of us severely underestimate how long of streaks are possible in 
#random processes. I bet your number in 3d is pretty high...)
