##############################
##Data 301
##Weighting data
##Dr. Stephen Pettigrew
##pettigr@sas.upenn.edu
##############################

setwd("C:/Users/hussainsarfraz/Desktop/DATA 210/Week 6")

load("data-for-weighting.RData")

## You probably need to install these two packages:
#install.packages(c("weights","anesrake"))

require(weights)
require(anesrake)



## Let's look at how we can use weights
## to get weighted means or weighted proportions.

head(olympics)
str(olympics)










## Let's imagine we want to know the percentage of people who planned to watch the Rio Olympics (q0010).

attributes(olympics$q0010)


## If we don't use the weights, we can calculate this percentage a couple ways.

## One way would be to take a mean:

mean(olympics$q0010 == "Yes") ## 63.5%

## Or, if we want to leave off the people who didn't answer the question:

mean(olympics$q0010[olympics$q0010 != "No answer"] == "Yes") ## 64.2%









## From a statistical perspective, the calculations above tell us the percentage for people who
## took the survey who planned to watch the Olympics. They do not tell us the percentage of people
## in the broader population who plan to watch.

## In order to estimate that quantity, we have to use weights to slightly weight our sample
## so that the demographics of the people in our survey match the people in the broader population.


## If we already have weights in our dataset, then it's as easy as using the weighted.mean() function:

weighted.mean(olympics$q0010 == "Yes", w = olympics$weight) ## 58.2%


## Or, if we want to leave off the non-responses:

weighted.mean(olympics$q0010[olympics$q0010 != "No answer"] == "Yes", 
              w = olympics$weight[olympics$q0010 != "No answer"]) ## 58.9%

olympics$q0010[olympics$q0010 != "No answer"]

## Another R trick that'll make your code easier:

with(olympics[olympics$q0010 != "No answer",],
     weighted.mean(q0010 == "Yes",
                   w = weight)) ## 58.9%













## The unweighted estimates of the percent who plan to watch the Olympics were higher than
## the weighted estimates. What might this tell us about the relationship between taking the
## survey and being interested in the Olympics?


























## Groups of people who were over-represented in the survey (i.e. those with small weights)
## were more interested in the Olympics than those who were under-represented (i.e. those
## with big weights).






## What's the average age of people who took this survey?

mean(olympics$age)


## How does it differ if we use the weights

weighted.mean(olympics$age, w = olympics$weight)


## What's going on here mathematically?

sum(olympics$age) / length(olympics$age)  ## here's the math for the unweighted mean

sum(olympics$age * olympics$weight) / sum(olympics$weight) ## here's the math for the weighted mean













## The weighted mean function is useful for yes/no questions, or other instances where
## you only care about calculating one number.

## What if, instead, we wanted to know what percentage of people were excited about
## watching specific events (q0012)?

attributes(olympics$q0012)



## If we're ignoring the weights, an easy way to do this is by using a combination
## of the table() and prop.table() functions:

prop.table(table(olympics$q0012))


## Or, if we want to leave off the people who didn't answer the question:

prop.table(table(olympics$q0012[olympics$q0012 != "No answer"]))














## If we want to use the survey weights, there's lots of different ways we could calculate it.
## I personally like to use the wpct() function in the 'weights' package

wpct(olympics$q0012, weight = olympics$weight)


## Ignoring the 'no answer' responses:

with(olympics[olympics$q0012 != "No answer",],
     wpct(q0012, weight))


## The wpct() function can also be used even if you don't want to use weights.

prop.table(table(olympics$q0012))
wpct(olympics$q0012) # same as above, if you leave off the weight information













## What percentage of people strongly approved of the creation of a Refugee Olympic
## Team in 2016? (q0015)

## What was the breakdown of support for the 2024 Olympics being held in the US? (q0018)

## Use the survey weights and omit people who didn't answer these questions.



















with(olympics[olympics$q0015 != "No answer",],
     wpct(q0015, weight)) ## refugees


with(olympics[olympics$q0018 != "No answer",],
     wpct(q0018, weight)) ## olympics in US
















#############################
## Constructing survey weights


head(data)

## Rake weighting


## First we need to create a list of targets, and they have to be done in a specific format:


our.targets <- list(gender = c("f" = .5, "m" = .5),
                    
                    race = c("asian" = .05,
                             "black" = .12, 
                             "hisp" = .12, 
                             "other" = .07,
                             "white" = .64),
                    
                    educ = c("bachelors" = .2,
                             "grad" = .12,
                             "no bachelors" = .68))

our.targets















newly.constructed.weights <- anesrake(inputter = our.targets,
                                      dataframe = data, 
                                      caseid = data$id)
head(newly.constructed.weights)



## The actual weight values are stored inside the list newly.constructed.weights:

head(data.frame(newly.constructed.weights$weightvec))












## Let's merge those weights into our dataset:

data <- merge(data,
              data.frame(newly.constructed.weights$weightvec),
              by.x = "id",
              by.y = "row.names")

names(data)[ncol(data)] <- "weight"



head(data) ## now we have weights!








## Let's check that our weighted data matches our targets:

our.targets$gender # target value
wpct(data$gender)# unweighted data
wpct(data$gender, data$weight)# weighted


our.targets$race # target value
wpct(data$race)# unweighted data
wpct(data$race, data$weight)# weighted


our.targets$educ # target value
wpct(data$educ)# unweighted data
wpct(data$educ, data$weight)# weighted




