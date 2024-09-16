library(ggplot2)

# Defines the directory to export graphs
exportpath = "/Users/marcmeredith/Box Sync/Teaching/Data201/ROutput"
# Defines the values that a dice can take on
dicevalues = c(1, 2, 3, 4, 5, 6)
# Defines the probabilities of each of the values
diceprobabilities = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
# Defines how many dice to average over
numrolls <- 45
# Generates a CI at bottom that the statistic is realized between lb and ub
lb <- 3
ub <- 4

# Intializes the probability of the value of current count after the rollling the first dice
# A row contains a value of the current count (col. 1) and a probability of that count (col. 2)
currentcount <- cbind(dicevalues, diceprobabilities)

# Intializes the probability of the value of the second, third, etc dice
# A row contains a value of the dice (col. 1) and a probability of that value (col. 2)
nextdice <- cbind(dicevalues, diceprobabilities)
lengthnextdice = nrow(nextdice)

for (n in 2:numrolls) {
  
  # Puts the possible outcomes of the current count into a variable
  lengthcurrentcount = nrow(currentcount)
  
  # Initializes a new matrix that will contain the probability of each combination of the current count and next dice
  dicesum = matrix(-9, nrow = lengthcurrentcount * lengthnextdice, ncol = 2) 
  # Loops over every possible value of the current count
  for (i in 1:lengthcurrentcount) {
      # Loops over every possible value of next dice
      for (j in 1:lengthnextdice) {  
          # Calculates the total of the current count + next dice 
          dicesum[i + (j - 1) * lengthcurrentcount, 1] = currentcount[i, 1] + nextdice[j, 1] 
          # Calculates the joint probability of the current count and next dice
          dicesum[i + (j - 1) * lengthcurrentcount, 2] = currentcount[i, 2] * nextdice[j, 2] 
      }
  }
  # Converts the matrix dicesum into a data frame to use the aggregate function
  dicesumdf <- as.data.frame(dicesum)
  # Sums the probabilities associated with a common total
  dicesumdf <- aggregate(x = dicesumdf$V2, by = list(dicesumdf$V1), FUN = sum)
  # Returns the current count into a matrix
  currentcount <- as.matrix(dicesumdf)
}


# Graphs the PDF
colnames(dicesumdf)[1] <- "Avg"
colnames(dicesumdf)[2] <- "Prob"
# Turns the sum into an average
dicesumdf$Avg <- dicesumdf$Avg / numrolls
# Rounds the average into the nearest .01
dicesumdf$Avg <- round(dicesumdf$Avg, 2)
# Sums the probabilities associated with a common rounding
dicesumdf <- aggregate(x = dicesumdf$Prob, by = list(dicesumdf$Avg), FUN = sum)
colnames(dicesumdf)[1] <- "Avg"
colnames(dicesumdf)[2] <- "Prob"
# Concatenates a string to be used for the graph's title
titlestring <- paste("Avg. of", numrolls, "Dice Rolls")
# Concatenates a string to be used for the filename of the exported graph
filestring <- paste("Rolling", numrolls, "Dice.eps", sep = "")

p <- ggplot(data=dicesumdf, aes(x=Avg, y=Prob)) +
  geom_bar(stat="identity", width=0.01, color="black") + 
  scale_x_discrete(limits=c(1:6)) + 
  labs(title=titlestring,  x="Value", y = "Prob. Value is Realized")
ggsave(filename = filestring, plot = p, device = "eps", path = exportpath)

dicesumdf$CIRange <- (dicesumdf$Avg >= lb & dicesumdf$Avg <= ub)
dicesumdfCI <- aggregate(x = dicesumdf$Prob, by = list(dicesumdf$CIRange), FUN = sum)
print(dicesumdfCI)
