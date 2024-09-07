

require(tidyr)
require(dplyr)
require(rio)



#########
## Question 3 - Genforward data
#########


genfor <- import("genforward sept 2017.sav")


## Let's start by creating a version of the exit data that we won't touch.
## That way we can look back at it later if we want.

genfor.untouched <- genfor


## Note: typically we'd want to do a bit of data cleaning before diving into
## our analysis, but I'm just going to dive right into the answers.

######
## A
######

## First, let's set missing values to NA so that they aren't included in these
## calculations:

genfor$Q1[genfor$Q1 %in% c(77,98,99)] <- NA

## Now calculate the percentage who strongly or somewhat approved

mean(genfor$Q1 %in% c(1,2), na.rm = T)

## 14.7 percent of millenials approve of the job Trump is doing.



######
## B
######

attributes(genfor.untouched$PartyID7)

## First, let's clean up the PartyID7 variable:

genfor$PartyID7[genfor$PartyID7 == -1] <- NA

## The Republicans are in categories 5, 6, and 7 of PartyID7

mean(genfor$Q1[genfor$PartyID7 %in% 5:7 & genfor$gender == 1] %in% c(1,2), 
     na.rm = T)

## 52.4% of Republican men approve of Trump's job.


mean(genfor$Q1[genfor$PartyID7 %in% 5:7 & genfor$gender == 2] %in% c(1,2), 
     na.rm = T)

## 45.5% of Republican women approve of his job.


mean(genfor$Q1[genfor$PartyID7 %in% 5:7 & genfor$gender == 1] %in% c(4,5), 
     na.rm = T)

## 31.1% of Republican men disapprove of the way he's doing his job.


mean(genfor$Q1[genfor$PartyID7 %in% 5:7 & genfor$gender == 2] %in% c(4,5), 
     na.rm = T)

## 38.0% of Republican women disapprove of the way he's doing his job



######
## C
######

## We want to create a single variable that includes the information that is spread
## across all the Q13_ columns:

genfor <- gather(genfor,
                 key = "issue",
                 value = "val",
                 names(genfor)[17:37])

## As we did in question 1B, we'll drop any values of zero and drop duplicate rows

genfor <- genfor[genfor$val != 0,]
genfor <- genfor[!duplicated(genfor$GenF_ID),]
genfor$val <- NULL

## Double check that the number of rows equals the number of unique respondents

nrow(genfor) == length(unique(genfor$GenF_ID))


## Now let's see which two issues were the most mentioned by Trump voters:

names(sort(table(genfor$issue[genfor$Q0 == 2]), decreasing = T)[1:2])

## It looks like the issues originally in Q13_21 and Q13_6 were the most prevalent.

attributes(genfor.untouched$Q13_21)
attributes(genfor.untouched$Q13_6)

## Terrorism/homeland security and healthcare were listed by Trump voters as the most
## important issues facing the country.


sort(table(genfor$issue[genfor$Q0 == 2]), decreasing = T) / sum(genfor$Q0 == 2)

## 18.2% of Trump voters listed terrorism/security as the top issue, 10.6% listed
## healthcare.



######
## D
######

## Take the top issue responses for Clinton voters, and see how many of them
## listed terrorism and healthcare as the top problem facing the country:

# First, terrorism:

mean(genfor$issue[genfor$Q0 == 1] == "Q13_21") # 4.4%


# Now, healthcare:

mean(genfor$issue[genfor$Q0 == 1] == "Q13_6") # 13.6%

# While only 4.4% of Clinton voters listed terrorism/security as their top issue,
# 13.6% listed healthcare.




######
## E
######

## Now let's see which three issues women over 30 listed:

names(sort(table(genfor$issue[genfor$gender == 2 & genfor$age > 30]), decreasing = T)[1:3])



attributes(genfor.untouched$Q13_6)
attributes(genfor.untouched$Q13_14)
attributes(genfor.untouched$Q13_21)


## Women over 30 listed health care, racism, and terrorism/homeland security (in a tie with environment/climate)
## as the three most important issues.

names(sort(table(genfor$issue[genfor$gender == 2 & genfor$age <= 30]), decreasing = T)[1:3])

attributes(genfor.untouched$Q13_3)

## Women under 30 listed health care, racism, and the environment/climate change





##########
## Question 5: NYC weather data
##########

##########
### A
##########

temps <- read.csv("nyc-central-park-temps.csv", stringsAsFactors = F)

temps <- separate(temps, DATE, into = c("year","month","day"), sep = "-")

sum(is.na(temps$TMAX))
sum(is.na(temps$TMIN))
## The dataset doesn't have any missing values for the min or max temperature


table(temps$year) %in% c(365, 366)
## every year except the first one (1869) has either 365 or 366 (in leap years) days of data

table(temps$month[temps$year == 1869])
## May 1869 is missing 7 days worth of data.



##########
### B
##########

temps$temp.diff <- temps$TMAX - temps$TMIN

mean(temps$temp.diff)
## On average, the high temperature is 14.7 degrees higher than the low temperature for that day

temps[temps$temp.diff == max(temps$temp.diff),]
## March 28, 1921 had a high temperature of 82 and a low of 34. A difference of 48 degrees.

aggregate(temp.diff ~ month, temps, mean)
# May tends to have the biggest swings in daily temperature, with a high/low difference of 17.1 degrees



##########
### C
##########

precip <- read.csv("nyc-central-park-precipitation.csv", stringsAsFactors = F)

precip <- separate(precip, DATE, into = c("year","month","day"), sep = "-")

## We'll perform a full/outer merge, so that we don't drop any rows of data. We'll merge on
## the year, month, and day variables

nyc <- merge(temps, 
            select(precip,-c(STATION, NAME)),  ## this prevents duplicates of the station and name variables from being created
             by = c("year","month","day"),
             all = T)

sum(nyc$SNOW > 1 & nyc$TMAX >= 50, na.rm = T)

## There have been 18 days with at least an inch of snowfall and a high temperature of 50 degrees.Many of you got 25. 
#We will discuss why (and you received credit for it) in synch session



##########
### D
##########

rain.by.month <- aggregate(PRCP ~ month, nyc, function(x) mean(x > 0))

rain.by.month[which.max(rain.by.month$PRCP),]
rain.by.month[which.min(rain.by.month$PRCP),]

## March tends to be the rainiest with 36.4% of days getting some precipitation
## October tends to be the driest with 27.2% of days getting some precipitation






##########
### E
##########

## Create several yearly temperature summary statistics.
## days.under.32 is the relevant one

year.summ <- summarize(group_by(nyc, year),
                       avg.high.temp = mean(TMAX, na.rm = T),
                       avg.low.temp = mean(TMIN, na.rm = T),
                       days.over.90 = sum(TMAX >= 90, na.rm = T),
                       days.over.95 = sum(TMAX >= 95, na.rm = T),
                       days.over.100 = sum(TMAX >= 100, na.rm = T),
                       days.under.32 = sum(TMIN <= 32, na.rm = T),
                       days.under.20 = sum(TMIN <= 20, na.rm = T),
                       days.under.10 = sum(TMIN <= 10, na.rm = T))


## Simple version of the graph:

plot(x = year.summ$year, 
     y = year.summ$days.under.32)


## Fancier version of the graph:

plot(x = year.summ$year, 
     y = year.summ$days.under.32,
     xlab = "", 
     ylab = "Number of days under 32 degrees F",
     main = "Weather in Central Park, NYC",
     bty = "l",
     pch = 18,
     xaxt = "n",
     col = "white")
abline(h = seq(0,300,20), col = "gray", lwd = .5)
abline(v = seq(1870,2020,10), col = "gray", lwd = .5)
points(x = year.summ$year, y = year.summ$days.under.32, pch = 18)
axis(1, at = seq(1870,2020,10))
abline(lm(days.under.32 ~ as.numeric(year), year.summ))


## There's a very clear pattern that NYC is getting fewer and fewer cold days.
## In fact, regression tells us that (on average) they get 0.22 fewer cold days
## with each passing year.
## In other words, every ~4 years they have one fewer day that goes below freezing.

summary(lm(days.under.32 ~ as.numeric(year), year.summ))


