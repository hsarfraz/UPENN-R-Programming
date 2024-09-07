##############################
## DATA 301
## Recoding variables
## Stephen Pettigrew
##############################

# if you haven't installed these packages, you'll need to use install.packages() to do so
# install.packages(c("tidyr","stringr","lubridate","dplyr"))

require(tidyr)
require(lubridate)
require(stringr)
require(dplyr)

setwd("d:/Dropbox (PORES)/LPS-data301/Week 3/")


## We'll pick up where we left off from the video where we learned how to reshape
## data in R:

load("results-from-reshaping.RData")


##########################################

## Cleaning column names


## Typically, we want column names that are descriptive and useful for use to work
## with.

## What are some of the issues with our column names?

names(genfor)




## You may have noticed that some of the column names don't clearly indicate
## what is in them (Q0, Q1).

## There's also inconsistency between whether the column names are uppercase
## or lowercase. We'll start by fixing this



######################################


## Remember that the column names are simply stored as a character vector
## in one of the attributes of a dataframe.

## We manipulate this attribute by using the names() function.
## Let's use the tolower() function to convert all the column names to lowercase

tolower(c("AB C D", "dataScience")) ## here's a simple example of how the function works


## An easier way to change the column names in a dataframe is this:

names(genfor) <- tolower(names(genfor))


## Also equivalent to this:

colnames(genfor) <- tolower(colnames(genfor))



head(genfor)




######################################

## Now let's give some of the other variables more descriptive names.

names(genfor)

attributes(genfor.untouched$Q0) # this one is 2016 presidential vote
attributes(genfor.untouched$Q1) # this one is Trump approval



## Most of the variable names look pretty good, but lets change a few using the rename()
## function in tidyr. We have to specify the dataframe we're manipulating, then
## tell the function the old and new names of the columns in the format
## 'newname = oldname'. We don't need to use any quotation marks with this function.

genfor <- rename(genfor,
                 weight = weight1,
                 vote2016 = q0,
                 approve.trump = q1,
                 approve.dem = democratic,
                 approve.rep = republican)


head(genfor)


########

## I've got a personal pet-peeve about having underscores in column names.
## Google's R style guide recommends using periods instead of underscores. 
## Let's substitute the underscore in 'genf_id' for a period.

## We'll use gsub() to substitute the "_" for a "."

## gsub() takes three arguments. First is the character pattern or string that you
## want to replace. Second is what you want to replace it with. Third is
## the character vector you want to do the replacement in.

gsub("_",".",names(genfor))

names(genfor) <- gsub("_",".",names(genfor))










##########################################

## Recoding values in a vector or in the column of a dataframe.

## A lot of the stuff in this section will be review from prior classes, 
## although there's lots of different ways to accomplish the same task,
## so some of this might be new.


## The first thing we want to do is make sure our variables are the correct
## type of data (i.e. character, numeric, logical, etc)

summary(genfor) # gives us stats and information about each variable



## There's something wrong with the 'weight' variable. It should be numeric,
## but it doesn't have summary stats presented about it like the other
## numeric variables.

is.numeric(genfor$weight) # false...

is.factor(genfor$weight) # true



## So the weight variable, which should be numeric, is being stored as a factor.
## We need to change that so that we can properly use the weights.

## The obvious (but annoyingly wrong) way to do this would simply be to use
## the as.numeric() function to change the weights into numbers. But if you
## apply as.numeric() to a factor variable, it converts all the values to their
## arbitrary numeric values rather than their meaningful labels.

as.numeric(genfor$weight) # wrong


## We can get around this problem by first converting the weights into characters
## and then converting the character strings into numeric

as.numeric(as.character(genfor$weight))


## I can't tell you how frustratingly common this issue is, which is why I'm
## point it out now. You're most likely to encounter it when you read in a dataset
## from a spreadsheet, and R turns some of your columns into factors rather than
## numeric.


## We can confirm that this was the right choice by looking at a summary of the
## two attempts:

summary(as.numeric(genfor$weight)) # ranges from 1 to 1627
summary(as.numeric(as.character(genfor$weight))) # mean of 1. has a reasonable range


## So now let's store the corrected weights in the dataset:

genfor$weight <- as.numeric(as.character(genfor$weight))





######################

## Another common type of data is date data. R is smart enough to let
## you use some numeric operators with dates, but you first have to make
## sure the dates are in the right format.

## Right now, our dates are stored as a character string, so we can't do things
## like find the mean date that people took the survey.

summary(genfor$date)
median(genfor$date) # doesn't work


## But the package 'lubridate' makes it super easy to reformat dates

head(genfor$date) # the format is year, month, day

ymd(genfor$date) # still looks the same as before...

median(ymd(genfor$date)) # but this time we can take the median

genfor$date <- ymd(genfor$date) # store the results


genfor$date[1] - genfor$date[2] ## We can now do math with the dates!




## This is particularly useful if you're interested in subsetting
## the data based on the dates:

table(genfor$date) # let's imagine we only wanted those people who took the survey in Nov.

View(genfor[genfor$date >= "2017-11-01",])


## If you're working with data that has times and dates (i.e. 1:30pm 2-7-2019),
## then the strptime() function is your friend






#####################################

## Manipulating and cleaning string data




## A common thing you'll encounter with character data is extraneous
## white space at the beginning or end of a character string. 

## One possible way to check for this is to use the nchar() function, which
## returns the number of letters, numbers, spaces, punctuation marks, etc in
## a character string

nchar(genfor$state) ## they're mostly 2s, but there's a couple 12s
table(nchar(genfor$state)) ##1871 observations have 2 characters. 5 observations have 12


table(genfor$state) ## it looks like it's South Dakota that's the problem


## We could change the SD entry by hand. Or we can use a function that trims
## off leading or trailing white space automatically

trimws(genfor$state)
str_trim(genfor$state) ## this is from the 'stringr' package and is the same as trimws()

genfor$state <- trimws(genfor$state)

table(nchar(genfor$state)) # now everything is 2 characters!







######################################
## Recoding and replacing values
######################################


## The most common form of data cleaning is recoding variables.
## There are tons of different ways to do this in R.



## The most basic is recoding by taking advantage of the bracket notation.

## Let's work on recoding the vote2016 (formerly Q0) variable, 
## which is a question about 2016 presidential vote

table(genfor$vote2016)
attributes(genfor.untouched$Q0)


## So let's set the 98 and 99 responses to missing

genfor$vote2016[genfor$vote2016 %in% c(98,99)] <- NA

mary(genfor$vote2016) ## it worked!



## When you're replacing (for example) 56 values in a vector,
## the right side of the <- arrow needs to either be a vector with 56 things in it
## or a single value that will be substituted in for all 56 values in the vector.

## This is particularly important to keep in mind if you're manipulating a
## subset of a vector.



## As an example, let's imagine that you wanted to double the age of everybody
## who is under 20 years old.

## We could hard code it this way:

# genfor$age[genfor$age == 18] <- 36
# genfor$age[genfor$age == 19] <- 38

# But that's annoying if there's more than a couple values we're changing



## We could be a little more general with our code by doing it this way:

genfor$age[genfor$age < 20] <- genfor$age * 2

# why does the line of code above give us a warning?






















## Here's the right way to do it:

genfor$age[genfor$age < 20] <- genfor$age[genfor$age < 20] * 2


















## Clean data often has labels that are more descriptive than just arbitrary numbers.

## Write a line or two of code that changes the gender variable from 1/2 to M/F






























## Again there's lots of ways to do this:

genfor$gender[genfor$gender == 1] <- "M"
genfor$gender[genfor$gender == 2] <- "F"


## We could also use the recode() function to clean up the gender variable:

recode(genfor$gender, '1' = "M", '2' = "F") # this function is in the 'dplyr' package








## Each of those examples required us to do some degree of hard coding.
## What if, instead of only having to response categories we wanted to replace, we
## had 30? Or a thousand? We certainly wouldn't want to type out all those responses

## Let's see how to do that with the Trump approval variable (Q1 and approve.trump):

## here's the mapping of the numeric values to the labels:

attr(genfor.untouched$Q1, "labels") 
approval.lab <- attr(genfor.untouched$Q1, "labels")

## Notice there's two pieces of information in this object. There's the numeric
## values (1,2,3,4,5,98,99) and the text labels (strongly approve, somewhat approve...)

## The text labels are stored as names of the vector. We'll use these in a few
## lines of code, when we're ready to do the replacement






## To get meaningful labels assigned to these numeric values, we'll convert
## the numeric variable into a factor one using the factor() function. And
## we'll specify that the labels we want to assign to this new vector can
## be found in the attributes we just looked at:

factor(genfor$approve.trump, labels = names(approval.lab))

# store it as a new column so we can make sure everything went right:

genfor$approve.trump <- factor(genfor$approve.trump, 
                               labels = names(approval.lab))

table(genfor$approve.trump)



# And now we can do one last bit of cleaning by converting the non-answers to NAs:

genfor$approve.trump[genfor$approve.trump %in% c("refused","SKIPPED ON WEB")] <- NA

table(genfor$approve.trump) # it worked!






### We can do the same thing to vote2016/Q0 (2016 presidential vote)

genfor$vote2016 <- factor(genfor$vote2016,
                          labels = names(attr(genfor.untouched$Q0, "labels")))

## It gave us an error! It seems to be saying that we either gave it too many
## labels or too few

names(attr(genfor.untouched$Q0, "labels")) # we gave it 6 labels

## But because we replaced the 98s and 99s with NA, we only have 4 values
## that we need to replace:
unique(genfor$vote2016)


## How could we get around this issue?


















## Let's use the same line of code, but instead of giving it all 6 labels,
## let's leave off the last two:

names(attr(genfor.untouched$Q0, "labels"))[-c(5:6)]

#or:
names(attr(genfor.untouched$Q0, "labels"))[1:4]


genfor$vote2016 <- factor(genfor$vote2016,
                          labels = names(attr(genfor.untouched$Q0, "labels"))[1:4])


table(genfor$vote2016) 
## it worked! (remember, there's so few Trump votes because
## this is a survey of millenials, who strongly voted for Clinton)






#############################

## One last function... If a column contains information about more than one variable
## we might want to divide it into two columns using the separate() function.

## Let's imagine we wanted separate columns for year, month, and day that the
## respondent took the survey:

separate(genfor,
         col = "date",
         into = c("year","month","day"))

## The function will attempt to automatically detect and guess the character 
## that separates the columns. In this case it works just fine, but it's not a bad
## idea to specify exactly what you want, so that it doesn't make an unexpected
## mistake:

genfor <- separate(genfor,
                   col = "date",
                   into = c("year","month","day"),
                   sep = "-")

head(genfor)



## The opposite of the separate() function is the unite() function, which will
## take multiple columns and make them into one:

head(unite(genfor,
           col = "date",
           into = c("year","month","day"),
           sep = "-"))
