##############################
## DATA 301
## Reshaping data
## Stephen Pettigrew
##############################

# if you haven't installed these packages, you'll need to use install.packages() to do so
# install.packages(c("tidyr","stringr"))


library(tidyr)
library(dplyr)

setwd("d:/Dropbox (PORES)/LPS-data301/Week 3/")

load("data-cleaning-examples.RData")





## Today we'll be working with responses from a survey of millennials.

## In the video about long and wide data, we talked about three criteria
## that data should abide by in order to do analysis with it.

## Do the gen forward data meet those criteria? 

View(genfor)























## The data seem to be breaking two of our rules.

## First, there seem to be two rows for every observation. In this way
## the dataset is "long".

## Second, the contents of the Q10A variable (which asks about the most
## important political issue to each respondent) are split across 22 different
## columns. With respect to this variable, the dataset is "wide".


## There's also several column names that could be formatted better, and
## some of the variables could be cleaned up so they're easier to interpret.
## We'll look at fixing those in the next video.














####################################

## Converting from long to wide




## Before I start to reshape and reformat data, I try to remember to store 
## an untouched version of the data as its own object. That way, I can go back
## and check if the values of the reformatted version match those of the
## cleaned version

genfor.untouched <- genfor






## Our first step in cleaning the data will be to make sure that each survey
## respondent takes up only one row, instead of two.

## In order to do this, we'd first want to see whether the rows are pure duplicates,
## of it there's something different about them.

## Let's look at the first person's data (in rows 1 and 2)

genfor[1,]
genfor[2,]



## Do you notice anything different?




















genfor[1,] == genfor[2,]

## The approval.party and approval.value variables are different.
## The first row shows this respondent's approval of the Democrats,
## while the second row shows this respondent's approval of the
## Republicans.

## When we reshape, we'll want to get rid of those two columns
## and replace them with a dem.approval and rep.approval column.
## That way, we can have only one row per person.

## In other words, we'll need to turn the approval variables from
## being in a "long" format, to being in a "wide" format




## To do this, we'll use the spread() function from the tidyr package. We have to 
## tell the function which variable we'll want to use as our new column names
## (the "key") and which variable contains the values that will populate those 
## new columns (the "value")

genfor <- spread(genfor,
                 key = approval.party,
                 value = approval.value)

head(genfor)











######################################

## Converting from wide to long


## Now let's deal with the Q10A variable, which is spread across 22
## columns. Let's give it a quick look:

attributes(genfor.untouched$Q10A_1)
attributes(genfor.untouched$Q10A_2)



## So it's the same question, just split across multiple columns, where each
## column represents a different issue.




## So we're going to want to collapse these 22 columns into one. To do that
## we'll use the gather() function, which can be thought of as the opposite 
## of the spread() function.



## We have to tell it the key (which is the new name we want to give our
## key variable), the value (which is the new name we want to give our
## values variable), and a vector of all the columns that we're collapsing/gathering



## Rather than typing out all 22 column names, we're going to use the paste0() 
## function as a trick to give it all the variable names from Q10A_1 through Q10A_22

paste0("Q10A_", 1:22)

genfor <- gather(genfor,
                 key = "top.issue", ## we could have called this anything
                 value = "is.top", ## this could be anything also
                 paste0("Q10A_", 1:22))



## Now let's see what we have:

nrow(genfor)
View(genfor)

## we went from 1876 rows, to tends of thousands. We'll need 
## to get rid of all those extra rows. Why do you think this happened?





























## The is.top variable tells us if this particular issue was the top issue
## listed for this respondent. We only actually care about the issue which
## the respondent said it was the most important issue (and not the other
## 21 they didn't select.)



## We're not actually interested in when a person does not care about an issue.
## So we can drop all the rows where 'is.top' is zero:

genfor <- genfor[!(genfor$is.top == 0),]

nrow(genfor)



## That got us most of the way there, but we've still got more rows than we should

## Look at the data and see if you can figure out what's going on?

View(genfor)























## All the people who didn't answer the question were coded as 99 for every
## issue. So now there's 22 rows for each of those people.

## What we want is one row per person. We'll use the duplicated() function to do this.
## This function takes a vector and returns a FALSE if it's the first time this
## value has shown up in the vector, and a TRUE if it's appeared before.

duplicated(genfor$GENF_ID)




## If we use the ! to flip the TRUEs and FALSEs, then we'll have a vector we can use
## to ensure that we only have one row per person:

genfor <- genfor[!duplicated(genfor$GENF_ID),]

head(genfor)

## Now we have one last step. We don't need to keep both 'top.issue' and 'is.top'.
## 'top.issue' gives us all the information we need, except for the people who
## were 99's in 'val'. So let's set 'top.issue' to NA for those people, and then
## we can drop the 'val' variable.

genfor$top.issue[genfor$is.top == 99] <- NA

genfor$is.top <- NULL ## this is an easy trick you can use to drop variables in a dataframe




## Now we've got a dataset that abides by our rules of tidy data! We have one row per
## respondent, and each column represents a different piece of information about that
## respondent.

head(genfor)


## In the next video, we'll look at how we can further clean up this dataset to make it more
## useful.

## We'll store our results from this script so that we can pick them up with the next video:

save(genfor, genfor.untouched, file = "results-from-reshaping.RData")
