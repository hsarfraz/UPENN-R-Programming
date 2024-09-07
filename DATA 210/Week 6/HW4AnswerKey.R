#### Pset 4 Answer key - Week 5/6
#### DATA 210



require(purrr)
require(dplyr)
require(weights)



##########
## Question 1: birthday problem
##########

## A

## First we'll draw our sample of 500 birthdays.
## Instead of sampling from the actual days of the year
## (i.e. January 1, January 2, etc.), we'll sample from
## the numbers 1 through 365 (where each number represents
## a day of the year). We need to specify that we want
## to sample 'with replacement' so that each day of the
## year has the same probability of being the birthday
## of everybody in the group.


group.size <- 500
bdays <- sample(1:365, group.size, replace = T)
all(bdays != 1) ## checks if nobody in the group has a Jan 1 birthday


######################################################
## B

results <- c()
for(i in 1:1000){
    # first draw a new sample and check whether there was a jan 1 birthday
    jan1 <- all(sample(1:365, group.size, replace = T) != 1)
    
    # now store that TRUE or FALSE into the results vector
    results[i] <- jan1
}







######################################################
## C

mean(results) ## Your answer should return roughly 25.4% 
## Although, due to sampling, this answer will vary slighlty 
## each time you call this function 


######################################################
## D 

bday.function <- function(size){
    results <- c()
    for(ii in 1:1000){
        jan1 <- all(sample(1:365, size, replace = T) != 1)
        results[ii] <- jan1
    }
    mean(results)
}

bday.function(750) ## should be around 12-13%
## Although again this answer will vary slightly 



######################################################
## e

## There's faster ways to do this, but this is the easiest to understand:

all.group.results <- data.frame(size = 500:1500,
                                prob = NA)

for(ii in 1:nrow(all.group.results)){
    all.group.results$prob[ii] <- bday.function(all.group.results$size[ii])
    if(ii %% 50 == 0) print(ii)
}
## Note: this will take some time to run on most computers

plot(x = all.group.results$size,
     y = all.group.results$prob,
     xlab = "Group Size",
     ylab = "Probability",
     ylim = c(0,.26))






















##########
## Question 3: Weighting South Poll data
##########

poll <- rio::import("july-2019-sm-poll.sav")



## A

mean(poll$weight) ## 1
## The survey weights have an average of 1. This ensures that the sum of all the
## weights in the dataset is exactly equal to the actual number of people who
## took the survey. If they survey weights averaged 2, then using the weights
## would make it seem as if we had twice as many respondents in our dataset
## than we actually did. This artificially inflated sample size could mess up
## our calculations of variance and margin of error.

######################################################
## B

summary(poll$weight)

## The largest weight is exactly 7.0, which suggests that weight values that
## were greater than 7 were trimmed so that they equalled 7.



######################################################
## 3C

## The which.max() function is really helpful here. If you provide the function
## with a vector of numbers, it will tell you the index number of the
## largest number in the vector: 

which.max(poll$weight) ## row 7792 has the largest weight

poll[which.max(poll$weight), c("race","gender")] ## race and sex of this person are 3 and 2

attributes(poll$race)
attributes(poll$gender)

## Based on the attributes of these two variables, the person with the largest
## survey weight is a Hispanic woman 

## For a variety of reasons, Hispanic women tend to be a difficult
## group to recruit to take surveys. 

mean(poll$gender == 2 & poll$race == 3, na.rm = T) ## 3.1%




######################################################
## D

## We know from the last question that women are coded as '2' in the gender variable:

mean(poll$age, na.rm = T) ## 52.1 years old

weighted.mean(poll$age, w = poll$weight, na.rm = T) ## 45.0 years old

# In the unweighted data, the average age was 52 years old After weighting, this number decreases to 45.
## This suggests that there were fewer young people in the data that we'd have liked to have
## so young people tended to have larger weights than older people.


######################################################
## E

## Start by cleaning up the race variable so that it's easier to interpret:

poll$race <- factor(poll$race, labels = names(attr(poll$race, "labels")))

prop.table(table(poll$race)) ## could have also used wpct() here, just with no weights
wpct(poll$race, weight = poll$weight)

wpct(poll$race, weight = poll$weight) - prop.table(table(poll$race))

## After applying the weights: 
## the white and Asian groups drop in size by 12.0 and 0.4 percentage points (respectively). 
## The black and Hispanic groups increased in size by 3.9 and 9.5 percentage points (respectively).


######################################################
## F

## First set the responses of people who skipped the question to NA:

poll$taxes_improve_infrastructure[poll$taxes_improve_infrastructure == 5] <- NA

attributes(poll$taxes_improve_infrastructure)

weighted.mean(poll$taxes_improve_infrastructure < 3, poll$weight, na.rm = T)

## 59.0% of all people said that they would be willing to pay more taxes to pay for infrastructure improvement

attributes(poll$party)

## Republicans:
with(poll[poll$party == 1,],
     weighted.mean(taxes_improve_infrastructure < 3, weight, na.rm = T))

## Democrats:
with(poll[poll$party == 3,],
     weighted.mean(taxes_improve_infrastructure < 3, weight, na.rm = T))


## 49.4% of Republicans would be willing to pay higher taxes for infrastructure improvement
## 71.5% of Democrats would be willing




######################################################
## G


## There's lots of ways to answer this one. One way would be to see how many
## people trust the federal government as much or more as their state government:


## First, set the people who didn't answer the questions to missing

poll$trust_fed_gov[poll$trust_fed_gov == 6] <- NA
poll$trust_state_gov[poll$trust_state_gov == 6] <- NA


## Lower numbers indicate more trust, so we need to see how often the fed number is less than the state one

poll$trust.fed.less <- poll$trust_fed_gov < poll$trust_state_gov
poll$trust.them.equal <- poll$trust_fed_gov == poll$trust_state_gov


weighted.mean(poll$trust.fed.less, poll$weight, na.rm = T) 
weighted.mean(poll$trust.them.equal, poll$weight, na.rm = T)

## Only 15% of people trust the federal government more than the federal government
## 54% trust them equally

poll$party <- factor(poll$party, labels = names(attr(poll$party, "labels")))
with(poll[poll$trust.fed.less,], wpct(party, weight))

## 54% of Republicans trust the federal government more than their state government
## 31% of Democrats trust the federal government than their state government
## 15% of Independents ...

## This makes sense, given that at the time of the poll the federal government was
## controlled by Republicans






