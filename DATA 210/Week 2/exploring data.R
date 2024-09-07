##############################
## DATA 301
## Exploring data in R
## Stephen Pettigrew
##############################

setwd("d:/Dropbox (PORES)/LPS-data301/Week 2/")





##########################################

## We're going to start today by talking about things you'll want to do the first
## time you're working with a new dataset.

## First we'll need to load the data
load("genforward-oct17.RData")



## The first thing I typically do is check how many rows and columns are in the dataset.
## For me, this is a way to double check that everything loaded the way I expected.

## If you were expecting a few dozen columns and a couple thousand rows, but your
## loaded data had 1 column with a million observations then you know something went wrong.

## You can check the number of rows and columns is by looking at the object's description
## in the environment on the right of the RStudio window.

## Even better, you can check it by using these functions:

dim(genfor) # checks the dimensionality of the dataframe (or matrix)

nrow(genfor) # checks how many rows/observations there are in the dataframe

ncol(genfor) # checks how many columns/variables/features there are in the dataframe



## Another thing to check is what 'object class' the loaded data are
## When you read in a spreadsheet from a CSV, Stata file, Excel file, etc
## it will store the data as a dataframe.
## But if you load an .RData file, it's possible that some of the objects
## won't be dataframes.

class(genfor) ## in this case, the RData file we loaded included one dataframe object







## The next thing I typically do is look at the names of the columns and rows

colnames(genfor)
names(genfor) # when you're working with dataframes, this is the same as colnames()

rownames(genfor) ## in this case, the row names are (arbitrarily) the row numbers
## In other words, the rownames() of this dataset don't provide meaningful information





## Next is to actually look at the dataset. There's lots of ways to do this

head(genfor) ## prints the first few rows of the dataset in your console

tail(genfor) ## prints the last few rows


## These two lines of code print the full dataset. Usually not the best idea,
## since datasets can be so big that it'll slow or crash your computer to print
## all of them. Luckily, R knows not to print more than a couple dozen lines of
## a dataset unless you specifically tell it otherwise
print(genfor)
genfor




## The downside of head(), tail(), print() is that it's a bit hard to understand
## what your data actually look like when it's printed in the console. The View()
## function solves that

View(genfor)

## Notice that the View() function shows us more information about the variables
## than we saw when we printed it in the console.





######################################################

## Objects in R can have 'attributes'. The column names or the number of rows/columns
## are examples of attributes of a dataframe.

attributes(genfor)


## But there can be other (less obvious, or even hidden) attributes embedded 
## within an object.

## In the genfor dataset, each column has its own set of attributes

attributes(genfor$GenF_ID)
attributes(genfor$WEIGHT1)
attributes(genfor$Q0)
attributes(genfor$gender)


## If we want to work with these attributes, we can extract them using attr()

attr(genfor$gender, "labels")



## Going another layer down, the object that results from the attr() function
## can have its own set of attributes

attributes(attr(genfor$gender, "labels"))


## So to recap, our dataframe has a set of attributes. One of those attributes
## is it's columns. Each column also has its own set of attributes, in this
## case descriptive labels for the numeric values. And those labels have their own
## attributes







## There are some special attributes that allow us to use the $ operator to 
## access them. In dataframes, you can pull out a specific column using its name:

genfor$age





## Lastly, we can use the str() function to get a detailed summary of everything
## in the dataset:

str(genfor)


