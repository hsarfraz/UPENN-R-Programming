# Reads in the necessary libraries
library(ggplot2)

# Defines the directory to export graphs
exportpath = "XXX"

# Sets the seed
set.seed(12221979)

# Defines the number of coins being flipped
numcoins = XXX
# Defines the number of simulations
numsims = XXX
# Defines the probability that a coin is heads
probheads = XXX

# Initializes a vector that contains the longest streak in every simulation
longstreak <- matrix(-9, nrow = XXX, ncol = 1) 

# Uses a for loop to loop over simulations
for (s in 1:numsims) {
  
  # Uses rbinom() function to draw a series Bernoulli r.v.s 
  # The number r.v.s in the series is defined by the parameter numcoins
  coins <- XXX
  # Initializes a vector that contains the streak after every coin
  streak <- matrix(-9, nrow = XXX, ncol = 1) 
  # Initilaizes the streak to 1 after the first coing
  streak[1] <- 1
  # Uses a for loop to calculate streak after every coin
  for (c in 2:numcoins) {

    streak[c] <- 1 + streak[c-1]*(coins[c] == coins[c-1])

  }
  
  # Identifies the longest streak and stores in longest streak vector
  longstreak[s] <- XXX

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
  labs(title = "CDF in XXX Sims.", x = "Length of Streak of Same Coin", y = "Pr(Longest Streak <= Length)")
# Exports a file containing the plot
ggsave(filename = "FlippingCoins.eps", plot = p, device = "eps", path = exportpath)