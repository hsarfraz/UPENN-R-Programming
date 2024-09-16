#--------------------------------------------------------------------------
# DATA 201 Week 1 Answer Key

#--------------------------------------------------------------------------


# Problem 1 ---------------------------------------------------------------

setwd("C:/Users/Marc/Dropbox/PORES/DATA Certificate/DATA 310 (201)/Week 1/12. Homework")

### Part A
alacourt <- read.csv("AlabamaCourt.csv")
head(alacourt)

### Part B
# creating variable black
alacourt$black <- FALSE
alacourt$black[alacourt$race == "B"] <- TRUE

# Creatd the variable amountremain
alacourt$amountremain <- alacourt$amountdue - alacourt$amountpaid

library(stargazer)

### Part C
# Subsetted the two groups
alacourt_lfos_black <- subset(alacourt, select = 
                                c(amountremain, amountdue, amountpaid), alacourt$black==TRUE)
alacourt_lfos_other <- subset(alacourt, select = 
                                c(amountremain, amountdue, amountpaid), alacourt$black==FALSE)

# Here I created the two tables
stargazer(alacourt_lfos_black, type="text",
          title = "African American", 
          summary.stat = c("n", "mean", "sd","median"),
          covariate.labels = c("Remaining Fines to Pay", "Total Fines" , "Total Fines Paid" ))
stargazer(alacourt_lfos_other, type="text",
          title = "Non African American", 
          summary.stat = c("n", "mean", "sd","median"),
                           covariate.labels = c("Remaining Fines to Pay", "Total Fines" , "Total Fines Paid" ))


### Part D

library(ggplot2)

#Adding 1 to each variables so that we can log
alacourt_lfos_black$amountremain <- alacourt_lfos_black$amountremain+1
alacourt_lfos_black$amountdue <- alacourt_lfos_black$amountdue+1
alacourt_lfos_black$amountpaid <- alacourt_lfos_black$amountpaid+1


alacourt_lfos_other$amountremain <- alacourt_lfos_other$amountremain+1
alacourt_lfos_other$amountdue <- alacourt_lfos_other$amountdue+1
alacourt_lfos_other$amountpaid <- alacourt_lfos_other$amountpaid+1

# Here I created the three density plots by arranging each of the data sets into paired
# separate data frames (due to the differing lengths)
p <- ggplot() + 
  geom_density(aes(x=x), colour="black", data = data.frame(x=alacourt_lfos_black$amountremain)) +
  geom_density(aes(x=x), colour="red", data = data.frame(x=alacourt_lfos_other$amountremain)) +
  scale_x_continuous(trans='log10',
                     breaks=c(1, 10, 100, 1000, 10000, 100000)) + 
  xlab("1+ Number of Cases") + 
  ylab("Amount Remaining")  + 
  theme(legend.position = "right")
p

p <- ggplot() + 
  geom_density(aes(x=x), colour="black", data = data.frame(x=alacourt_lfos_black$amountdue)) +
  geom_density(aes(x=x), colour="red", data = data.frame(x=alacourt_lfos_other$amountdue)) +
  scale_x_continuous(trans='log10',
                     breaks=c(1, 10, 100, 1000, 10000, 100000)) + 
  xlab("1 + Number of Cases") + 
  ylab("Amount Due") 
p

p <- ggplot() + 
  geom_density(aes(x=x), colour="black", data = data.frame(x=alacourt_lfos_black$amountpaid)) +
  geom_density(aes(x=x), colour="red", data = data.frame(x=alacourt_lfos_other$amountpaid)) +
  scale_x_continuous(trans='log10',
                     breaks=c(1, 10, 100, 1000, 10000, 100000)) + 
  xlab("1 + Number of Cases") + 
  ylab("Amount Paid") 
p




# Problem 2/3 ---------------------------------------------------------------

### Part A
library(ggplot2)
setwd("~/Dropbox/Data Science")
basketball_data<- read.csv("C:/Users/Marc/Dropbox/PORES/DATA Certificate/DATA 310 (201)/Week 1/12. Homework/CollegeBasketball.csv")

#
basketball_data$Change <- basketball_data$PredictedDifference - basketball_data$ActualDifference

p <- ggplot(basketball_data, aes(x=Change)) + 
  scale_x_continuous(breaks=seq(-30, 40, 5)) + 
  geom_histogram(bins = 70) + 
  xlab("Difference Between Actual and Predicted Margin for Favorite") + 
  ylab("Number of Games") 
p
mean(abs(basketball_data$Change))
### Part B

# Let W represent hte event that the favorite won a bb game
# by more points than expected
basketball_data$W <- basketball_data$Change < 0
mean(basketball_data$W)

# Let E represent the event that the favorite won a bb game by
# exactly the number of points that were expected
basketball_data$E<- basketball_data$Change == 0
mean(basketball_data$E)

# Let L be the event that the favorite won a basketball game by fewer
# points than expected or lost a game
basketball_data$L <- (basketball_data$Change > 0)
mean(basketball_data$L)

#Note that, if a favorite loses it necessarily means they lost the game.


### Part C

# First I created the Points Difference variable
basketball_data$PointsDifference <- basketball_data$PredictedPoints - basketball_data$ActualPoints

# I then reported the mean of the absolute value
mean(abs(basketball_data$PointsDifference))

q <- ggplot(basketball_data, aes(x=PointsDifference)) + 
  scale_x_continuous(breaks=seq(-30, 40, 5)) + 
  geom_histogram(bins = 70) + 
  xlab("Difference Between Actual and Predicted Margin for Favorite") + 
  ylab("Number of Games") 
q

### Part D
# Finally I calculated the probabilities of each of these
# events

basketball_data$M <- basketball_data$PointsDifference < 0
mean(basketball_data$M)

basketball_data$Tv <- basketball_data$PointsDifference == 0
mean(basketball_data$Tv)

basketball_data$Fv <- basketball_data$PointsDifference >0
mean(basketball_data$Fv)


### Part E

# We want to use our formula :
# P(A|B) = P(A ^ B)/P(B)

probWM <- mean(basketball_data$W & basketball_data$M)/mean(basketball_data$M)
probWM

probLM <- mean(basketball_data$L & basketball_data$M)/mean(basketball_data$M)
probLM

probWF <- mean(basketball_data$W & basketball_data$Fv)/mean(basketball_data$Fv)
probWF

probLF <- (mean(basketball_data$L & basketball_data$Fv))/mean(basketball_data$Fv)
probLF

