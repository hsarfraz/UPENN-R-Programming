##############################
## DATA 3
## Loops in R
## Dr. Stephen Pettigrew
##############################

setwd("d:/Dropbox (PORES)/LPS-data301/week 5/loops in r/")

pres <- read.csv("president-2016.csv", stringsAsFactors = F)

pres


###########################################################
## If/else statements
###########################################################


## Sometimes you want to perform an operation only if certain conditions are met.
## To accomplish this we use 'if' and 'else' statements

## An 'if' statement takes the form:
## if(logical boolean){function to perform if the boolean is TRUE}

x <- 100000000

if(x > 10000){
  print("x is a really big number")
}

if(x < 10000){
  print("x isn't that big")
} 
## Nothing happens here because x < 10000 evaluates to false










## We can use else{} statements to deal with instances where the boolean
## evaluates to FALSE

## Note that using multiple if/else statements requires that you wrap the whole thing
## in curly brackets, {}:

x <- 1

if(x > 10000){
  print("x is a really big number")
} else{
    print("x isn't that big")
}










## If the code that goes into the if{}/else{} statement is really basic, then you might consider
## using the ifelse() function instead:

ifelse(x > 10000,
       "x is a really big number",
       "x isn't that big")







## Typically though, it's better to avoid ifelse(), since often you want to 
## evaluate multiple 'if' statements:

y <- "Math!"

if(!is.numeric(y)) {
  "y isn't even a number"
} else if(y == 10000){
  "y equals 10000"
} else if(y > 10000){
  "y is a really big number"
}  else{
  "y isn't that big"
}







## To code that same thing using ifelse(), you have to write some nasty code:
## Don't ever write something like this. It's way too hard to follow.

ifelse(!is.numeric(y),
       "y isn't even a number",
       ifelse(y == 10000,
              "y equals 10000",
              ifelse(y > 10000,
                     "y is a really big number",
                     "y isn't that big")))











###########################################################
## For loops
###########################################################


## A "for-loop" is a way of repeating a process a large 
## number of times.  For each iteration of the loop
## R stores an index number which can be handy.


#######################################
## Indexing in loops
#######################################

## To understand loops in any programming language, you first
## need to understand indexing in a loop

## Take this very simple loop:
## In this case, the loop is indexed by 'i'.
## The loop starts with i = 1, goes through all the lines of the loop
## then returns to the top of the loop and sets i = 2. The loop knows to
## stop when it reaches i = 10

for(i in 1:10){
  
  print(i)

}





## That loop's indexing is identical to this one:

for(i in seq(1,10)){
  
  print(i)
  
}




## And this one:

index.vector <- 1:10

for(i in index.vector){
  
  print(i)
  
}





## We can put pauses into each iternation in the loop so that you can
## see that it's working one-at-a-time:


for(i in 1:10){
    
    print(i)
    Sys.sleep(1) ## pause R for 1 second
    
}











## Your indexes don't need to be sequential
# This one prints all even numbers between 2 and 50

for(i in seq(2,50,2)){
  
  print(i)
}


## Side note: "print()" statements are an incredibly useful
## thing to put into code. They'll help you debug your code by letting
## you know where your code hit snags. Or if you're looping over a huge number
## of things that take a really long time, they'll tell you where to restart
## your loop if you turn off your computer






## Your loop doesn't even need to be indexed by numbers:
a.bunch.of.words <- c("look", "how", "fancy", "we", "can", "be", "with", "loop", "indexes")

for(word in a.bunch.of.words){
  
  print(word)
  
}







## Another note about indexing and loops:
## If you put loops within loops, be careful that you never use the 
## the same letter to index two different loops.

## In other words, don't do this:

for(i in 1:10){
  
  for(i in 50:60){
    
    ## Some sort of code...
  }
  
}




## This won't necessarily prevent your code from working,
## but it could cause you a headache that can be frustrating to 
## try to diagnose. 

## Instead your loops should look something like this:

for(i in 1:10){
  
  for(j in 50:60){
    
    ## some sort of code...
  }
  
}









## Write a for-loop that takes the indexing 1:50, and
## prints each number in that range that is divisible by 3.

## Hint: you might want to use an if() statement inside your for loop.
## You may also want to use the modulus function, which gives you the
## remainder after you do division:

12 %% 2
12.5 %% 2
13 %% 2








for(i in 1:50){
  
  if(i %% 3 == 0){
    print(i)
  }
  
}








## Type your answer here















for(i in 1:50){
  if(i %% 3 == 0){
    print(i)
  }
}









## Now let's get a little fancier with our loops.
## Let's write a loop that calculates and stores the total number of votes that
## Trump received in each state

## First let's get the list of all the state names that appear in the data.

## How many states are there in the data? Hint: use the unique() function





















state.names <- unique(pres$state)

length(state.names)









## Now let's use our state.names vector to get the Trump vote in
## each of the states.


for(state in state.names){
  sum(pres$trump[pres$state == state])
}













## Since we want to store the results to look at after the loop is completed, we need
## to first define a placeholder for that data.

## You have to make a placeholder everytime you calculate something inside of a loop
## that you want to access after the loop is done.

## Let's make a dataframe to store the means in.

## First we'll store the state names in the first column of the dataframe,
## and the second column will contain the number of votes (which we'll start as all NAs)
trump.state <- data.frame(state = state.names,
                          votes = NA)


## Let's look at what we have:
head(trump.state)








## When writing loops I find it often is easier
## to narrow the scope of what I'm trying to do.
## Rather than thinking about all 50 states at once, let's start easy and write
## the code that will calculate the 2004/2008 mean for the first state

state.names[1] ## AL

pres$trump[pres$state == "AL"] ## This gives us the count of Trump votes in each county in Alabama

sum(pres$trump[pres$state == "AL"]) ## And there's the sum of Trump votes in all of Alabama

## And here's how we would store it in the dataframe:
trump.state$votes[trump.state$state == "AL"] <- 
  sum(pres$trump[pres$state == "AL"])






## Now how would we do this for every state?

## Well let's look at what our indexes look like

for(state in state.names){
  print(state)
}





for(state in state.names){
  
  internal.placeholder <- sum(pres$trump[pres$state == state])
  
  trump.state$votes[trump.state$state == state] <- internal.placeholder
  
}

trump.state








#####################################################
## When should we use loops, and when should we do something different?
#####################################################

## Answer: typically it's better to avoid using loops, if you can think
## of a way to write your code some other way.

## A common way to do this is called "vectorizing" your code.


## Think back to high school when you (might have) learned matrix algebra.

## There's an algorithm to do the multiplication that lends itself to using
## loops.

## Let's see how fast it is to do matrix multiplication with a loop, compared
## to using a vectorized function:


## We'll start by creating two matrices that we'll multiply together. We're
## using the rnorm() function to fill these matrices with numbers that are 
## randomly drawn from the standard normal distribution.


matrix.a <- matrix(rnorm(5000), ncol = 5)
matrix.b <- matrix(rnorm(5000), nrow = 5)




## How could we do the matrix multiplication using a loop?

result <- matrix(NA, 
                 nrow = 1000,
                 ncol = 1000)



## Look how long this takes!

starting.time <- Sys.time()

for(i in 1:nrow(result)){
  
  for(j in 1:nrow(result)){
    
    result[i,j] <- sum(matrix.a[i,] * matrix.b[,j])
    
  }
}

ending.time <- Sys.time()

for.loop.time <- ending.time - starting.time ## this is how long that for-loop took us
for.loop.time







## If we had remembered what we know about matrix algebra, we would have vectoried
## our code and just used the canned R function that does matrix algebra:

starting.time <- Sys.time()

result2 <- matrix.a %*% matrix.b ## Way faster!

ending.time <- Sys.time()

vectorized.time <- ending.time - starting.time ## this is hoe long that for-loop took us


as.numeric(for.loop.time) / as.numeric(vectorized.time) ## this is how many times slower the for loop was
