##############################
## DATA 301
## Experiments and randomization
## Stephen Pettigrew
##############################

setwd("d:/Dropbox (PORES)/LPS-data301/Week 7/2 experiments and randomization/")

load("experiments-data.RData")



## First check out the data

head(pre.treatment)
summary(pre.treatment)


head(full.experiment)
summary(full.experiment)





###################
## Different ways to randomize treatment in an experiment


#####
## Simple randomization (i.e. flipping a coin for each person in the study)

## There's a bunch of ways to do this:

rbinom(1000, 1, prob = .5)

sample(c(T,F), 1000, replace = T)







## Anytime you're writing code in R that involves any sort of randomized
## process, you should set your seed. This ensures that when you rerun your
## code, you'll get exactly the same randomization each time. This is
## good because it makes your results replicable

set.seed(7423)

pre.treatment$treated <- rbinom(1000, 1, prob = .5)

head(pre.treatment)



## Notice that the prior randomization didn't end up with 500 treated and 500
## control:
table(pre.treatment$treated)




#####
## Complete randomization (i.e. ensure that you have a specific number in your
## treatment and control groups, rather than leaving it up to chance)


## With complete randomization we can ensure that we'll have equally sized groups:

sample(1:nrow(pre.treatment), # sample from the row numbers 
       500, ## the number of rows we want to be in the treated category
       replace = F)


pre.treatment$treated.complete <- F
pre.treatment$treated.complete[sample(1:nrow(pre.treatment), 500, replace = F)] <- T

table(pre.treatment$treated.complete)





#####
## Block randomized (i.e. either coin-flip randomization or complete randomization
## within groups)



## We'll block on race and education. So let's get the unique combinations of those
## variables:

blocks <- unique(pre.treatment[,c("race","bachelors")])
blocks



## Within each of these blocks, we'll do complete randomization

pre.treatment$treated.block <- F

ii <- 1

for(ii in 1:nrow(blocks)){
  
  this.block <- blocks[ii,]
  
  rows <- which(pre.treatment$race == this.block$race & 
                  pre.treatment$bachelors == this.block$bachelors)
  
  pre.treatment$treated.block[sample(rows, 
                                     size = round(length(rows)/2), 
                                     replace = F)] <- T
  
}

table(race = pre.treatment$race, 
      bachelors = pre.treatment$bachelors, 
      treated = pre.treatment$treated.block)









###########################
## Checking balance

## Before you estimate any treatment effects from your experiment, you need to 
## make sure that your randomization worked. In other words, you need to make
## sure that none of the pre-treatment variables can predict whether or not
## somebody is in the treatment or control group



## The easiest way to do this is to do a difference-of-means test:

mean(full.experiment$bachelors[full.experiment$treated == T]) ## 0.388
mean(full.experiment$bachelors[full.experiment$treated == F]) ## 0.39


mean(full.experiment$race[full.experiment$treated == T] == "white") ## 0.63
mean(full.experiment$race[full.experiment$treated == F] == "white") ## 0.628

mean(full.experiment$race[full.experiment$treated == T] == "black") ## 0.118
mean(full.experiment$race[full.experiment$treated == F] == "black") ## 0.118




## These are just the means. What we really care about is whether these
## differences are significantly different from zero. We can use a t-test to do this:

t.test(full.experiment$bachelors[full.experiment$treated == T],
       full.experiment$bachelors[full.experiment$treated == F]) ## p= 0.9483


t.test(full.experiment$race[full.experiment$treated == T] == "white",
       full.experiment$race[full.experiment$treated == F] == "white") ## p= 0.9479



## Another way you can check is by using linear regression, where the outcome
## variable is treatment. If no variables in the regression are significant
## predictors of the treatment, then you've got a good randomization:

summary(lm(treated ~ race + bachelors, data = full.experiment))









###########
## Estimating treatment effects:

## Typically what we're interested in is the average treatment effect (ATE)
## If we have a good randomization, then the ATE is equal to the average
## of the outcome variable for the treatment group, minus the average
## of the outcome variable for the control group


mean(full.experiment$voted[full.experiment$treated])
mean(full.experiment$voted[!full.experiment$treated])

mean(full.experiment$voted[full.experiment$treated]) - 
  mean(full.experiment$voted[!full.experiment$treated]) # ATE: about 7 percentage points




## You can also use a t-test to get standard errors and assess statistical significance:

results <- t.test(full.experiment$voted[!full.experiment$treated],
                  full.experiment$voted[full.experiment$treated])

results$estimate ## means for treated and control groups
diff(results$estimate) ## treatment effect

results$p.value ## when this value is less than 0.05, then we can say that the ATE
                 ## is statistically significantly different from zero





## You can also use regression to estimate treatment effects.
## When the only variable in your regression is the treatment, then linear
## regression is exact equal to a t-test:

summary(lm(voted ~ treated, data = full.experiment))


## The advantage of regression is that you can also control for 
## other variables. Because these variables aren't confounders (since
## you randomized the treatment), they shouldn't really change the estimate of
## the treatment effect. But controlling for other variables might make
## your standard errors smaller, which means you'll have a more precise estimate
## of the treatment effect:

summary(lm(voted ~ treated + bachelors + race, data = full.experiment))








## This experiment was a block-randomized study. Because of this, 
## we have multiple "mini-experiments" within each block that we randomized
## within. Because of this, we can look for "heterogeneous treatment effects"
## (i.e. different effects of the treatment for different types of people)

summary(lm(voted ~ treated + race, 
           data = full.experiment[full.experiment$bachelors == 1,])) ## ATE: not significant

summary(lm(voted ~ treated + race, 
           data = full.experiment[full.experiment$bachelors == 0,])) ## ATE: 0.09 percentage points

