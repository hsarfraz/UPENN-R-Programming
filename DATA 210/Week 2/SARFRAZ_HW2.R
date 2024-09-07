############################
## Data 210 Week 2 Homework
## Hussain Sarfraz
## 11/6/2021
###########################

library(readr)
library(tidyverse)
library(dplyr)

#1. Load in the gapminder data from the gapminder package using the data() function.


#install.packages('dslabs') # Loading dslabs package
library(dslabs)
data(gapminder) 

gapminder %>%
  view()

#2. Use the class function to determine what type of object the data is.
class(gapminder)

#3. Check the dimensions of the gapminder data. Report the total number of rows and 
#   columns within the data frame.
nrow(gapminder)
ncol(gapminder)

#4. Print the column names to your console.
colnames(gapminder)

#5. Let's say we wanted to check for any attributes that might be hidden in the data. 
#   Use the attributes function to further explore the life expectancy and fertility
#   variables and report your findings.

attributes(gapminder)
attributes(gapminder$life_expectancy)
attributes(gapminder$fertility)

#6. Why do you think there aren't any hidden attributes in this dataframe?




#7. We want to find the population size of Norway in 1965. First, find this information
#   manually using the View() function, then use bracket notation to find the population
#   size by column and row.

gapminder[496,6]

#8. Suppose we want to filter our data to only observe countries with life expectancies
#   lower than 40. Use bracket notation to subset the data accordingly. Are there any
#   countries that recorded this low of a life expectancy in the 21st century?
  
gapminder[gapminder$life_expectancy < 40 ,] %>%
  view()

gapminder[gapminder$life_expectancy < 40 & gapminder$year > 2000,] %>%
  view()


#Problem 2: College Majors
#Part A
#1. Read in the data "recent-grads.csv" that can be found on Canvas. Name the data
#   recent_grads.

setwd("C:/Users/hussainsarfraz/Desktop/DATA 210")
recent_grads <-  read_csv('recent-grads.csv')

#2. Print out to your console the dimensions, class, and names of rows/columns of the
#   recent_grads object.

dim(recent_grads)
class(recent_grads)
rownames(recent_grads)
colnames(recent_grads)


#3. Check to see if there are attributes for the dataframe. Then, check to see if there 
#   are attributes for the columns. If there isn't, why is this the case?
attributes(recent_grads)

# no column attributes

#Part B
#1. Find the major category with highest and lowest median income.


which.max(recent_grads$Median)
recent_grads[which.max(recent_grads$Median), ]

which.min(recent_grads$Median)
recent_grads[which.min(recent_grads$Median), ]

#2. Find the average Median salary for both of those majory categories.

mean(recent_grads$Median[recent_grads$Major_category == "Engineering"])

mean(recent_grads$Median[recent_grads$Major_category == "Education"])


#3. Are there any professions that both (a) are one of the top 10 professions with the 
#   highest median income and (b) are in the top ten professions with the highest share
#   of women? Hint: start by making variables for top income and share of women.



top_income <- recent_grads[desc(recent_grads$Median), ] %>%
  head(10) %>%
  view()


top_income <- recent_grads[desc(recent_grads$ShareWomen), ] %>%
  head(10) %>%
  view()

share_of_women <- recent_grads %>%
  arrange(desc(ShareWomen)) %>%
  head(10) 

top_income
View(share_of_women)

#4. According to this dataset, are women underrepresented in STEM fields?

top_income$Major %in% share_of_women$Major