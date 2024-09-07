##############################
## DATA 301
## Conditional logic, bracket notation, and subsetting data
## Stephen Pettigrew
##############################


setwd("d:/Dropbox (PORES)/LPS-data301/Week 2/")


## Load the dataset that we used in the 'exploring data' video
load("genforward-oct17.RData")


library(dplyr)



######################################

## Assignment operators in R

# In R, there are a couple different ways to assign values to objects or
# arguments. In your work in R so far, you've encountered the 
# 'gets' arrow "<-" and the equals sign "="

# The gets arrow and equals sign are both examples of what are called
# "assignment operators".

# Generally, if we want to create a new object in our workspace, 
# we would want to use the gets arrow. When we're using a function
# and want to set a specific argument for that function to a value,
# we'd want to use the equals sign.

# Here's a simple example:

our.vector <- 1:5 # pronounced "our vector gets one through five"

mean(x = our.vector)




# In some cases, the gets arrow and equals sign can be used interchangably.
# These two lines of code accomplish the same thing. However, for a variety
# of reasons, it's stylistically better that you use the gets function
# in this case.

our.vector <- 1:5
our.vector = 1:5



# Similarly, you'll see that using the gets arrow inside a function will
# give you the same result as using the equals sign. But there's a slight
# nuance to how it works. Do you notice the difference in what happened?

mean(x = our.vector)
mean(x <- our.vector)






























# When we used the gets arrow inside of the function, we actually unintentionally
# created a new object in our workspace called 'x'. We almost never want to create
# objects in that way, so that's why we tend to use equals signs inside of functions
# and gets arrows to create objects outside of functions.











######################################

## Boolean operators

# In addition to assignment operators, there's a whole other set of operators
# in R called 'boolean operators'.

# A 'boolean' is a data type which takes one of two values, true or false.
# So far, all we've talked about is how you can manipulate a whole dataset
# or vector all at once. But what if we wanted to look at a subset of the
# data? To do this, we'd need to understand 'conditional logic,' which we
# use boolean operators to construct.







## The most basic boolean operator is a double equals sign. It will compare
## the values on the right side to the values on the left side and tell us
## whether they're equal

## In the most basic case, we can compare a single value on the left
## to a single value on the right:

2 == 1+1

1/2 == .5


2/3 == .6666 # Comes up false because of rounding







## The double equals can be a little more useful if we want to check
## multiple values at once. For example, we can compare multiple values
## on the left to a single value on the right:

c(1,2,3,4,5) == 5

5 == c(1,2,3,4,5)






## Alternatively, we can compare multiple values on the left to the 
## same number of values on the right.

c(1,2,3,4,5) == c(5,4,3,2,1)







## It's crucial to remember, however, that if you're using the double
## equals (as well as several other boolean operators) you must either
## have:
## 1. a single value on either the left or right side of the ==
##  or
## 2. an equal number of multiple values on both sides of the ==

## If you try to compare differently sized vectors, you'll end up getting
## this warning:

c(1,2,3) == c(1,2,3,4,5) # "longer object length is not a multiple of shorter object length"












#########################################

## Other common boolean operators include greater than and less than:


2 > 3

2 < 3


vec <- 1:5
vec > 3


## There's also a greater/less than or equal to operators:

vec >= 3

vec <= 3





#######################################################
## One extremely useful feature of logical data (i.e. TRUEs and FALSEs) 
## is that R will treat them like 1's and 0's without you having to
## explicitly turn them into numbers in your code.




## Use the sum() function to get the number of elements in "vec" that are greater
## than or equal to 3. Do this in one line of code























## Answer
sum(vec >= 3)











##############################################

## Sometimes we've created a bunch of TRUEs and FALSEs, but we instead
## realize that what we really need is to turn the TRUEs into FALSEs
## and FALSEs into TRUEs.

## We can do that by using another boolean operator, the exclamation point, !.


2 == 3 #FALSE

2 != 3 #TRUE

!(2 == 3) #TRUE. essentially the same as the one above

2 > 3 #FALSE

!(2 > 3) #TRUE

!c(T, F, T, T, T, F)








################################

## Boolean operators are particularly useful when dealing with variables
## in a dataframe.

## If we wanted to figure out which rows in the 'genfor' data were
## survey responses of people from Pennsylvania, we could do this:

genfor$state == "PA"


## You'll notice that this line of code returns a vector of TRUEs
## and FALSEs. Each TRUE represents somebody from Pennsylvania. Each
## FALSE represents somebody from another state.

## You can also see that the number of TRUEs and FALSEs in this vector
## is equal to the number of rows in the dataset. In other words,
## we now have a T/F vector that's equal length to the number of
## rows in our data:

length(genfor$state == "PA") # 1876

length(genfor$state == "PA") == nrow(genfor) # TRUE










##################################

## Now we're going to look at how we can leverage conditional logic statements
## in order to subset a dataset. In other words, how can we use logic to pull
## out a specific set of rows from a dataframe?

## Before we do that, let's talk about bracket notation in R. You've probably 
## seen this in your previous coursework in R, and it's something that you'll
## see more and more throughout this semester.



## Let's imagine that you have a vector of types of fruit:

fruits <- c("apples","oranges","bananas","pears")



## Now imagine (for some reason) that you wanted to pull out the third
## fruit in that vector. We could use bracket notation to do this. We simply
## put the number 3 inside of brackets, and put those brackets after the
## name of our object:

fruits[3]


## It works the same way, even if we hadn't created the fruit vector:

c("apples","oranges","bananas","pears")[3]









## We can do a similar thing with data that are not in vectors, but are instead
## in matrices or dataframes.

## The key thing to remember about using the bracket notation with matrices
## or dataframes is that those types of objects have both rows and columns.
## We need to be clear in our code as to whether we're trying to pull out
## a row or a column of data.

## So if we wanted to pull out the WEIGHT variable for the third person in 
## the genfor dataset:


names(genfor) ## WEIGHT1 is the 2nd column

genfor[3,2] ## 0.131849


## When we look at the full dataset, we can confirm that we pulled out the
## correct number:

head(genfor)



## The important thing to remember is that we need to be careful
## to put the row information first and the column information second.
## This is because when you describe the 'dimensionality' of a matrix or 
## dataframe, it's always rows-by-columns (never columns-by-rows).

## It's helpful to come up with a simple heuristic that helps you remember
## this. 
## Roller coaster. Racecar. RC cola. Ray Charles. Roman Catholic. Robocop.
## The possibilities are endless








## What if, instead of pulling one specific cell from our dataframe
## (like the 3rd row, 2nd columns), we wanted to pull out an entire
## 3rd row? Or the entire 2nd column? We can use the same notation:

genfor[3,] ## Just the third row

genfor[,2] ## Just the second column

## Notice the location of the comma inside the brackets. If the
## number comes before the comma, then it'll pull out that row.
## If it's after the comma, it'll pull out that column.

## So now let's bring all this together so that we can learn how to
## subset data...








############################################


## Let's say we wanted to look at all the survey respondents from Pennsylvania.
## How could we use a conditional logic statement to look at this subset?


# This code will produce either a TRUE or FALSE for every row in the dataset:

genfor$state == "PA"



# You've learned in earlier classes that you can use the filter() function
# to subset a dataframe. This line of code will take the genfor dataframe,
# and keep only the rows where the state variable equals PA

filter(genfor, genfor$state == "PA")





# But in R there's often multiple ways to accomplish the same task. We could have
# also used the subset() function:

subset(genfor, genfor$state == "PA")




# We can also use the bracket notation to do subsetting. Take couple minutes
# and see if you can figure out how you could use the bracket notation to
# keep only the Pennsylvania respondents in this data.





















## Our conditional logic statement created a vector with one TRUE or FALSE
## for each row in the data. We can include that before the comma inside
## the bracket, and that will tell R to keep only the rows where the
## logical statement evaluates to 'TRUE':

genfor[genfor$state == "PA",]



## If we had put the conditional logic statement after the comma, it would
## have told us that we were trying to select columns that don't exist
## in the dataframe. This is because we gave it 1876 TRUEs and FALSEs,
## but there's only 37 columns in the data

genfor[,genfor$state == "PA"]







# Now let's try another example. Write a line of code that pulls out the 'age' 
# of everybody in Pennsylvania. Then find the mean of those values.

# There's a few ways you could do this


















# There's a few ways we can do this:


genfor[genfor$state == "PA", "age"]



genfor$age[genfor$state == "PA"] # notice there's no commas in the bracket here.
# That's because in this case we're subsetting genfor$age, which is a 1 dimensional
# vector (i.e. it doesn't have length and width, it only has a length). So we don't
# need a comma inside the brackets



subset(genfor, genfor$state == "PA")$age
subset(genfor$age, genfor$state == "PA")
subset(genfor, genfor$state == "PA", select = "age")


filter(genfor$age, genfor$state == "PA") ## notice that this doesn't work, because filter()
## requires a dataframe as the first argument, and we've given it a vector




## Now we want to get the mean of those ages. We can do that by copying any of the
## above lines of code into the mean() function

mean(genfor$age[genfor$state == "PA"]) #25.34 years (if we ignore the survey weights)







###########################################


## What if we wanted to find the ages of all the women in Pennsylvania?

## First we'd need to figure out how gender is coded in the dataset:

## We can use the unique() function to get all the values of the 'gender' variable
unique(genfor$gender)

table(genfor$gender) # the table() function will also give us counts




## We need to figure out if women are coded as 1 or 2. We can do this by
## looking at the attributes of the variable:

attributes(genfor$gender) # women are coded as 2










## So now, how do we subset our data to be both women and Pennsylvanians?

## We can use the 'and' operator:

genfor$state == "PA" & genfor$gender == 2



# To pull out the ages of those PA women:

genfor$age[genfor$state == "PA" & genfor$gender == 2]




# To further illustrate the '&' operator:

(2 == 2) & (3 == 2) #FALSE

(2 == 2) & (3 == 3) #TRUE





# There's also an 'or' operator, denoted by the pipe:

(2 == 2) | (3 == 2) #TRUE

(2 == 8) | (3 == 2) #FALSE












## As practice, find the average age of survey respondents who are either
## Pennsylvania women, Oregon men, or anybody from Kentucky.
## Hint: Use parentheses to group your logical statements




























# Here's the logical statement:

(genfor$state == "PA" & genfor$gender == 2) | 
    (genfor$state == "OR" & genfor$gender == 1) | 
    genfor$state == "KY"


# Here's how we'd get the ages of the people that meet those conditions:

genfor$age[(genfor$state == "PA" & genfor$gender == 2) | 
               (genfor$state == "OR" & genfor$gender == 1) | 
               genfor$state == "KY"]


# or we could do it this way:


genfor[(genfor$state == "PA" & genfor$gender == 2) | 
           (genfor$state == "OR" & genfor$gender == 1) | 
           genfor$state == "KY", "age"]



# And then we can take the mean():

mean(genfor$age[(genfor$state == "PA" & genfor$gender == 2) | 
                    (genfor$state == "OR" & genfor$gender == 1) | 
                    genfor$state == "KY"]) # 24.82 years old














#######################################

## There's a few other useful operators and functions to know when you're
## doing boolean logic.


## Recall that the boolean operators we've seen so far all require that you
## compare one value to multiple values, or an equal number values on either
## side of the ==, >, <, etc.

## What if, instead, you want to compare uneven lengthed vectors?

## The %in% operator is useful in this case. Putting %in% between
## two vectors will check whether each value in the first vector appears
## at least once in the second

## Here's a simple example:

c(2,4,6,8) %in% c(1,2,3,4,5) ## 2 and 4 are in 1-through-5, but 6 and 8 are not



c("PA","RI","Michigan") %in% genfor$state







## Two other useful functions to use when generating booleans are any() and all().
## You can use them when a line of code generates several TRUEs and FALSEs.
## any() returns TRUE if there's a single TRUE value inside the parentheses.
## all() returns TRUE if every value inside the parentheses is TRUE

2 == c(1,2,3) ## FALSE TRUE FALSE

any(2 == c(1,2,3)) ## TRUE

all(2 == c(1,2,3)) ## FALSE




## Lastly, the which() function will take a TRUE/FALSE vector, and return in the
## index numbers of all the TRUEs


which(c(T,F,F,T)) ## 1 and 4 are TRUEs

which(genfor$state == "PA" & genfor$gender == 2) # these are the row numbers of PA women





