library(tidyverse)
library(readr)
library(dplyr)
library(scales)
require(weights)
require(anesrake)

# Part A

## Question 1

### Part A
setwd("C:/Users/hussainsarfraz/Desktop/DATA 210")
alabama.pop <-  read_csv('sub-est2016_1.csv')
alaska.pop <-  read_csv('sub-est2016_2.csv')

#checking what append function to use
ncol(alabama.pop) == ncol(alaska.pop) #checking column number
colnames(alabama.pop) == colnames(alaska.pop) #checking proper order
#using rbind() append function because number of columns and order is the same
alabama.alaska.pop <- rbind(alabama.pop,alaska.pop)

### Part B

state.pop <-  read_csv('sub-est2016_all.csv')

unique(state.pop$STNAME) #51 names under 'STNAME' column in dataset - District of Columbia not a state but district

### Part C

state.pop.tidy <- subset(state.pop, select = c(NAME, STNAME, POPESTIMATE2012))

### Part D

state.pop.tidy.51 <- subset(state.pop.tidy, NAME == STNAME) #filtering columns

sort(table(state.pop.tidy.51$NAME), decreasing = T) #seeing which state repeats

which(state.pop.tidy.51$NAME == "District of Columbia") #finding which row number contain the repeating values

state.pop.tidy.51[9, ] %in% state.pop.tidy.51[10, ] #checking if rows are same
#state.pop.tidy.51[9, ] == state.pop.tidy.51[10, ] #different way to check

state.pop.tidy.51 <- state.pop.tidy.51[-9, ] #making dataset have a total of 51 rows

state.pop.tidy.51$NAME %in% state.pop.tidy.51$STNAME #checking to see if columns have same values
#state.pop.tidy.51$NAME == state.pop.tidy.51$STNAME

state.pop.tidy.51$STNAME <- NULL

### Part E

state.areas <- read.csv('state-areas.csv')

#inner-merge
state.area.pop.merge <- merge(x = state.pop.tidy.51, 
      y = state.areas, 
      by.x = 'NAME',
      by.y = 'state') 

#Which observations can be matched?
unique(state.area.pop.merge$NAME)

#which observations are not a common match
anti_join(x = state.areas, 
          y = state.pop.tidy.51, 
          by = c('state' =  'NAME')) #	Puerto Rico is not the common match


### Part F

state.area.pop.merge <- state.area.pop.merge %>%
  mutate('2012 State Population Density' = POPESTIMATE2012/area..sq..mi.)

#other way to do it
#state.area.pop.merge$`2012 State Population Density` <- state.area.pop.merge$POPESTIMATE2012/state.area.pop.merge$area..sq..mi.

### Part G

state.economic.data <- read.csv('ECN_2012_US_52A1.csv')


state.economic.data.tidy <- state.economic.data[-1, ] #removing 1st row

### Part H

#checking if there are any values in the columns
unique(state.economic.data.tidy$GEO.annotation.id)
unique(state.economic.data.tidy$NAICS.annotation.id)

#removing two empty columns
state.economic.data.tidy$GEO.annotation.id <- NULL
state.economic.data.tidy$NAICS.annotation.id <- NULL

#making empty rows NA
state.economic.data.tidy$RCPTOT[state.economic.data.tidy$RCPTOT==''] <- NA

#removing the NA rows
state.economic.data.tidy <- state.economic.data.tidy[complete.cases(state.economic.data.tidy$RCPTOT),]

#checking to see the row commonalities 
state.economic.data.tidy[1, ] == state.economic.data.tidy[2, ]

#checking to see how many unique combinations can be made between the states and each sector
row.check <- nrow(unique(state.economic.data.tidy[ ,c("GEO.display.label", "NAICS.display.label")]))

#filtering the non-duplicate rows combos of state rows 
state.economic.data.tidy <- state.economic.data.tidy %>%
  filter(!duplicated(state.economic.data.tidy[ ,c("GEO.display.label", "NAICS.display.label")])) 

#checking to see if I correctly filtered out the repetitive rows
nrow(state.economic.data.tidy) == row.check

#checking to see if filter worked and there are no repeats in state and sector labels
# state.economic.data.tidy %>%
#   group_by(GEO.display.label,NAICS.display.label) %>%
#   summarize(row.count = n()) %>%
#   arrange(desc(row.count))

### Part I

#removing un-necessary columns
state.economic.data.tidy <- state.economic.data.tidy[ ,-c(1:2)]
state.economic.data.tidy <- state.economic.data.tidy[ ,-2]
state.economic.data.tidy <- state.economic.data.tidy[ ,-c(3:4)]
state.economic.data.tidy <- state.economic.data.tidy[ ,-c(4:8)]

#inner-merge
state.econ.pop.merge <- merge(x = state.economic.data.tidy, 
                              y = state.area.pop.merge, 
                              by.x = 'GEO.display.label',
                              by.y = 'NAME') 


#to check any unmatched values
# anti_join(x = state.economic.data.tidy, 
#       y = state.area.pop.merge, 
#       by = c('GEO.display.label'= 'NAME'))

#Another way to merge
# merge(x = state.area.pop.merge, 
#       y = state.economic.data.tidy , 
#       by.x = 'NAME',
#       by.y = 'GEO.display.label') %>% view()

#removing un-necessary columns in merged dataset 
state.econ.pop.merge <- state.econ.pop.merge[ ,-c(4:5)]

### Part J

state.econ.pop.merge %>%
  group_by(GEO.display.label) %>%
  summarise(total.state.revenue = sum(as.numeric(RCPTOT)),
            state.pop.density = `2012 State Population Density`) %>%
  distinct(state.pop.density, .keep_all= TRUE) %>%
  filter(state.pop.density < 400,
         total.state.revenue < 100000000) %>%
  ggplot(mapping = aes(x=state.pop.density , y=total.state.revenue)) +
  geom_point() +
  xlab("State Population Density") +
  ylab("State Total Revenue ($1000)") +
  ggtitle("State Population Density and Total State Revenue") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 10)) +
  scale_y_continuous(labels = comma)

state.econ.pop.merge %>%
  group_by(GEO.display.label) %>%
  summarise(total.state.revenue = sum(as.numeric(RCPTOT)),
            state.pop.density = `2012 State Population Density`) %>%
  distinct(state.pop.density, .keep_all= TRUE) %>%
  ggplot(mapping = aes(x=state.pop.density , y=total.state.revenue)) +
  geom_point() +
  xlab("State Population Density") +
  ylab("State Total Revenue ($1000)") +
  scale_x_continuous(breaks=c(0,1250,2500,3750,5000,6250,7500,8750)) +
  ggtitle("State Population Density and Total State Revenue") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 10)) +
   scale_y_continuous(labels = comma)

# Part B

load('nes.rda')
names(nes) <- gsub('_','.',names(nes))
nes.tidy <- nes

## Question 1

attributes(nes$interest.voted2008)
unique(nes$interest.voted2008)
unique(nes$interest.whovote2008)

nes.tidy$interest.voted2008[nes.tidy$interest.voted2008 %in% c(-8,-9)] <- NA
nes.tidy$interest.whovote2008[nes.tidy$interest.whovote2008 %in% c(-1,-8,-9)] <- NA

nrow(subset(nes.tidy, interest.whovote2008 == 1))/nrow(subset(nes.tidy, interest.voted2008 == 1))*100

#what percentage of survey respondents voted for Barack Obama in 2008?
#67.32312%

## Question 2

nes.tidy <- subset(nes.tidy, 0 <= ftgr.fedgov & ftgr.fedgov <= 100 )

#another way to filter values
#filter(nes.tidy, 0 <= ftgr.fedgov & ftgr.fedgov <= 100)

# #to check lowest values
# nes.tidy %>%
#   transmute(ftgr.fedgov=ftgr.fedgov) %>%
#   arrange(ftgr.fedgov) %>%
#   head(10)
# 
# #to check highest values
# nes.tidy %>%
#   transmute(ftgr.fedgov=ftgr.fedgov) %>%
#   arrange(desc(ftgr.fedgov)) %>%
#   head(10)

## Question 3

#pg. 32 
# 'weight_full' is intended for the analysis of the combined samples. If you want to 
# include all available cases, use this weight. -> pg.32

#'weight_full' -> pg. 84

#calculating weighted mean
weighted.mean(nes.tidy$ftgr.fedgov, w= nes.tidy$weight.full) #44.2899

#(nes.tidy$ftgr.fedgov * nes.tidy$weight.full)/sum(nes.tidy$weight.full) #calculting weighted mean to check if my value is correct

#calculating non-weighted mean
sum(nes.tidy$ftgr.fedgov)/nrow(nes.tidy)
#mean(nes.tidy$ftgr.fedgov) #another way to calculate mean

sum(nes.tidy$ftgr.fedgov)/nrow(nes.tidy) == mean(nes.tidy$ftgr.fedgov) #both means are equal

## Question 4

#unique(nes.tidy$prevote.regpty)

nes.tidy <- nes.tidy %>%
  mutate(democrat.or.republican = prevote.regpty)

#nes.tidy$democrat.or.republican <- nes.tidy$prevote.regpty

nes.tidy$democrat.or.republican[nes.tidy$democrat.or.republican %in% c(4,5,-1,-8,-9)] <- NA
nes.tidy$democrat.or.republican[nes.tidy$democrat.or.republican == 1] <- 'Democratic Party'
nes.tidy$democrat.or.republican[nes.tidy$democrat.or.republican == 2] <- 'Republican Party'

## Question 5

#checking to see if there are any "NA" values under the "prevote.regpty" column
#table(is.na(nes.tidy$prevote.regpty)) #all falses which means no "NA"s

#finding the Un-Weighted means for each value under the "prevote.regpty" column
unweighted.means <- prop.table(table(nes.tidy$prevote.regpty))*100
#finding the Un-Weighted mean difference 
unweighted.means[4] - unweighted.means[5] #17.3982 

#finding the Weighted means for each value under the "prevote.regpty" column
weighted.means <- wpct(nes.tidy$prevote.regpty, weight = nes.tidy$weight.full)*100
#finding the Weighted mean difference 
weighted.means[4] - weighted.means[5]

###### Anything after this line was to just DOUBLE CHECK MY CODE
###### Checking to see if my Un-Weighted Mean value for the Democratic Party was correct

nrow(subset(nes.tidy, democrat.or.republican=='Democratic Party')) #getting number of respondents that in favor for Democrats
#nrow(filter(nes.tidy, democrat.or.republican=='Democratic Party')) #another way to find democrat numbers

#calculating the un-weighted mean for democrat party
nrow(subset(nes.tidy, democrat.or.republican=='Democratic Party'))/nrow(nes.tidy)*100 #27.34003

#creating the "democrat.mean.check.dataset" to check my weighted mean calculations (can also be used to calculate the un-weighted means)
democrat.mean.check.dataset <- nes.tidy %>%
  mutate(demo.mean.count = ifelse((democrat.or.republican=='Democratic Party'), 1, 0),
         demo.mean.count = ifelse(is.na(demo.mean.count)==T, 0,demo.mean.count)
  )

#using "democrat.mean.check.dataset" to check my un-weighted mean calculation
# mean(democrat.mean.check.dataset$demo.mean.count)*100  #output: 27.34003

#checking if both un-weighted means are equal
# (mean(democrat.mean.check.dataset$demo.mean.count)*100) == (nrow(subset(nes.tidy, democrat.or.republican=='Democratic Party'))/nrow(nes.tidy)*100) #output: TRUE

###### Checking to see if my Un-Weighted Mean value for the Republican Party was correct

nrow(subset(nes.tidy, democrat.or.republican=='Republican Party')) #getting number of respondents that in favor for Republicans
#nrow(filter(nes.tidy, democrat.or.republican=='Republican Party')) #another way to find Republican numbers

#un-weighted mean for republican party
nrow(subset(nes.tidy, democrat.or.republican=='Republican Party'))/nrow(nes.tidy)*100 #9.94183

#creating the "republican.mean.check.dataset" to check my weighted mean calculations (can also be used to calculate the un-weighted means)
republican.mean.check.dataset <- nes.tidy %>%
  mutate(demo.mean.count = ifelse((democrat.or.republican=='Republican Party'), 1, 0),
         demo.mean.count = ifelse(is.na(demo.mean.count)==T, 0,demo.mean.count)
  )

#using "republican.mean.check.dataset" to check my un-weighted mean calculation
# mean(republican.mean.check.dataset$demo.mean.count)*100  #output: 9.94183

#checking if both un-weighted means are equal
# (mean(republican.mean.check.dataset$demo.mean.count)*100) == (nrow(subset(nes.tidy, democrat.or.republican=='Republican Party'))/nrow(nes.tidy)*100) #output: TRUE

###### Finding the difference between the Democratic and Republican Un-Weighted Means

#difference between democrat and rebublican un-weighted mean
(nrow(subset(nes.tidy, democrat.or.republican=='Democratic Party'))/nrow(nes.tidy)*100)-(nrow(subset(nes.tidy, democrat.or.republican=='Republican Party'))/nrow(nes.tidy)*100) 
#17.3982

# using the mean() functions only to check if I calculated the un-weighted mean correctly
# (mean(democrat.mean.check.dataset$demo.mean.count)*100)-(mean(republican.mean.check.dataset$demo.mean.count)*100)

# comparing the un-weighted mean values with what I previously got to see if the values are exactly the same
# (mean(democrat.mean.check.dataset$demo.mean.count)*100)-(mean(republican.mean.check.dataset$demo.mean.count)*100)==(nrow(subset(nes.tidy, democrat.or.republican=='Democratic Party'))/nrow(nes.tidy)*100)-(nrow(subset(nes.tidy, democrat.or.republican=='Republican Party'))/nrow(nes.tidy)*100) 

# Double-checking the number I got for the weighted mean of Democratic voters
(wpct(democrat.mean.check.dataset$demo.mean.count, weight = democrat.mean.check.dataset$weight.full)*100)[2]

# Double-checking the number I got for the weighted mean of Republicans voters
(wpct(republican.mean.check.dataset$demo.mean.count, weight = republican.mean.check.dataset$weight.full)*100)[2]

# I am using the "democrat.mean.check.dataset" and "republican.mean.check.dataset" datasets to check if my weighted mean calculations are correct
(wpct(democrat.mean.check.dataset$demo.mean.count, weight = democrat.mean.check.dataset$weight.full)*100)[2]-(wpct(republican.mean.check.dataset$demo.mean.count, weight =  republican.mean.check.dataset$weight.full)*100)[2]

# comparing the weighted mean values with what I previously got to see if the values are exactly the same
all.equal(((wpct(democrat.mean.check.dataset$demo.mean.count, weight = democrat.mean.check.dataset$weight.full)*100)[2]-(wpct(republican.mean.check.dataset$demo.mean.count, weight = republican.mean.check.dataset$weight.full)*100)[2]),(weighted.means[4] - weighted.means[5]))