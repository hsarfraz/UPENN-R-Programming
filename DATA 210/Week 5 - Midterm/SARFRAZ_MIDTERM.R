#3. Use the GenForward survey data (genforward sept 2017.sav) to answer the following 
#questions. If you need to do any data cleaning before performing these calculations,
#please include that code in your R script.

#You do not need to consider survey weights for any of the answers in this question. 
#Also, be sure that you are careful about how you handle people who did not answer 
#particular questions.

library(readr)
library(readxl)
library(tidyverse)
library(dplyr)
library(haven)

setwd("C:/Users/hussainsarfraz/Desktop/DATA 210")
genforward_2017 <-  read_sav('genforward sept 2017.sav')
genforward_2017_tidy <- genforward_2017

############################################## long data testing

#not long data since GenF_ID is not repeated
#genforward_2017[1,]==genforward_2017[2,]

#not long data since none of the rows are repeated
#sort(table(genforward_2017$GenF_ID),decreasing=T)

############################################## making all column names lowercase

names(genforward_2017_tidy) <- tolower(names(genforward_2017_tidy))
#view(genforward_2017_tidy)

############################################## giving variables more descriptive names
#attributes(genforward_2017$Q1)

genforward_2017_tidy <- rename(genforward_2017_tidy,
                               candidate.vote = q0)
#view(genforward_2017_tidy)

############################################## sub "_" in col names to "."
names(genforward_2017_tidy) <- gsub("_",".",names(genforward_2017_tidy))

#view(genforward_2017_tidy)

############################################## summary() to see each variable type
summary(genforward_2017_tidy)

#string variables/columns (rest are numeric)
#device, state, timezone, 

############################################## seeing if stings don't have unnecessary spaces
table(nchar(genforward_2017_tidy$state))


#a) What percent of the sample strongly approved or somewhat approved of the way that
#President Trump is handling his job as president (using question Q1)?
attributes(genforward_2017_tidy$q1)

respondent.percent <- prop.table(table(genforward_2017_tidy$q1))*100 
respondent.percent[1]+respondent.percent[2]

############################################## replacing some values to NA and then testing to see if it worked
#77=DON'T KNOW | 98=SKIPPED ON WEB | 99=refused
genforward_2017_tidy$q1[genforward_2017_tidy$q1 %in% c(77,98,99)] <- NA

table(genforward_2017_tidy$q1)

############################################## checking is rows total respondents are correct
nrow(genforward_2017_tidy)
nrow(genforward_2017)

############################################## getting answer
unique(genforward_2017_tidy$q1)
nrow(subset(genforward_2017_tidy, q1==1 | q1==2))/nrow(genforward_2017_tidy)*100


# genforward_2017_tidy %>%
#   group_by(q1) %>%
#   summarize(total.count = n()) %>%
#   mutate(overall.total = sum(total.count),
#          percent = total.count/overall.total*100) %>%
#   filter(q1==1|q1==2) %>%
#   transmute(sum= sum(percent))

# view(genforward_2017_tidy)

#b) What percentage of Republican men "strongly approve" or "somewhat approve" of the way
#Trump is handling his job as president? What is this percentage for Republican women? 
#What percentage of Republican men and Republican women (separately) "somewhat disapprove"
#or "strongly disapprove" of Trump? For this question, you should use the PartyID7 
#variable and consider anybody who is in the "Lean Republican", "Moderate Republican",
#or "Strong Republican" categories to be a Republican.

attributes(genforward_2017$Q1)
attributes(genforward_2017$gender)
attributes(genforward_2017$PartyID7)

#Strongly approve = 1 | Somewhat approve = 2
#Somewhat disapprove = 4 | Strongly disapprove = 5
# male = 1 | female = 2
# "Lean Republican", "Moderate Republican", or "Strong Republican" 5,6,7

############################################## getting answer for male trump approval
male.trump.approval <- genforward_2017_tidy %>%
  filter(q1==1|q1==2 &
         gender==1 &
         partyid7==5|partyid7==6|partyid7==7) %>%
  nrow()

male.trump.approval/nrow(genforward_2017_tidy)*100

############################################## getting answer for female trump approval
female.trump.approval <- genforward_2017_tidy %>%
  filter(q1==1|q1==2 &
           gender==2 &
           partyid7==5|partyid7==6|partyid7==7) %>%
  nrow()

female.trump.approval/nrow(genforward_2017_tidy)*100

############################################## getting answer for male trump dis-approval
male.trump.disapproval <- genforward_2017_tidy %>%
  filter(q1==4|q1==5 &
           gender==1 &
           partyid7==5|partyid7==6|partyid7==7) %>%
  nrow()

male.trump.disapproval/nrow(genforward_2017_tidy)*100

############################################## getting answer for female trump dis-approval
female.trump.disapproval <- genforward_2017_tidy %>%
  filter(q1==4|q1==5 &
           gender==2 &
           partyid7==5|partyid7==6|partyid7==7) %>%
  nrow()

female.trump.disapproval/nrow(genforward_2017_tidy)*100


# c) Which two issues did 2016 Trump voters indicate were the most important problems facing the
# country? What percentage of Trump voters listed each of these two issues as the top issue?
view(genforward_2017_tidy)
attributes(genforward_2017$Q13_1)
unique(genforward_2017$Q13_1)
attributes(genforward_2017_tidy$q13.1)
attributes(genforward_2017_tidy$q13.2)

############################################## going from wide->long
genforward_2017_tidy <- gather(genforward_2017_tidy,
       key = 'important.issue.topic',
       value = 'important.issue.value',
       paste0('q13.',1:22)) 

############################################## only zero and ones in column. no other value
unique(genforward_2017_tidy$important.issue.value)

############################################## removing zero in column
genforward_2017_tidy <- genforward_2017_tidy[!(genforward_2017_tidy$important.issue.value == 0) , ]

############################################## removing important.issue.value column
genforward_2017_tidy$important.issue.value <- NULL

############################################## replacing some values to NA and then testing to see if it worked
#77=DON'T KNOW | 98=SKIPPED ON WEB | 99=refused
genforward_2017_tidy$candidate.vote[genforward_2017_tidy$candidate.vote %in% c(77,98,99)] <- NA

############################################## seeing what number represents trump
unique(genforward_2017_tidy$candidate.vote)

############################################## creating trump only dataset
trump.vote.only <- subset(genforward_2017_tidy, candidate.vote == 2)

############################################## total no. of trump voters
nrow(trump.vote.only)

############################################## seeing two most important issue
sort(table(trump.vote.only$important.issue.topic),decreasing = T)

attributes(genforward_2017$Q13_21)
attributes(genforward_2017$Q13_6)

#[Terrorism and homeland security]       [Health care]
# q13.21                             and q13.6
#    43                                   25

############################################## answer
43/nrow(trump.vote.only)*100 # 18.22034
25/nrow(trump.vote.only)*100 # 10.59322
(43+25)/nrow(trump.vote.only)*100 # 28.81356


view(genforward_2017_tidy)

#   d) What percentage of 2016 Clinton voters listed these two issues are the most important problem
# facing the country?
############################################## creating clinton only dataset
clinton.vote.only <- subset(genforward_2017_tidy, candidate.vote == 1)

############################################## total no. of clinton voters
nrow(clinton.vote.only)

############################################## seeing two most important issue
nrow(subset(clinton.vote.only, important.issue.topic=='q13.21'))/nrow(clinton.vote.only)*100
nrow(subset(clinton.vote.only, important.issue.topic=='q13.6'))/nrow(clinton.vote.only)*100
#4.454865
#13.59906

(nrow(subset(clinton.vote.only, important.issue.topic=='q13.21'))+nrow(subset(clinton.vote.only, important.issue.topic=='q13.6')))/nrow(clinton.vote.only)*100

nrow(subset(clinton.vote.only, important.issue.topic=='q13.21'|important.issue.topic=='q13.6'))/nrow(clinton.vote.only)*100


#   e) What are the top three issues that women over 30 years old care about? Are these
#top issues the same for women aged 30 and under?
view(genforward_2017_tidy)
attributes(genforward_2017$gender)
############################################## seeing two most important issue
women.over.30 <- subset(genforward_2017_tidy, age > 30 & gender==2)

sort(table(women.over.30$important.issue.topic),decreasing = T)

#[Health care]            [Racism]          [Terrorism and homeland security]
# q13.6                    q13.14             q13.21
#    28                    26                 19

attributes(genforward_2017$Q13_6)
attributes(genforward_2017$Q13_14)
attributes(genforward_2017$Q13_21)
############################################## seeing two most important issue
women.under.30 <- subset(genforward_2017_tidy, age <= 30 & gender==2)

sort(table(women.under.30$important.issue.topic),decreasing = T)

#  [Racism]            [Health care]         [Environment and climate change]
#   q13.14                q13.6              q13.3
#    160                  98                 85

attributes(genforward_2017$Q13_14)
attributes(genforward_2017$Q13_6)
attributes(genforward_2017$Q13_3)

# For this exercise, we'll be working with daily weather data from a weather station in 
#New York City's Central Park. The station has been running continuously since January
#of 1869, so we'll be able to analyze 150 years of weather patterns. The data come from
#a dataset extraction tool provided by the National Oceanic and Atmospheric 
#Administration.3

# a. Begin by loading the temperature data.4 Use the separate() function to turn the 
#DATE variable into three separate variables for year, month, and date. Which years are
#missing at least one day of temperature data, and how many days are missing?
nyc.temperature <-  read_csv('nyc-central-park-temps.csv')
nyc.temperature.tidy <- nyc.temperature
names(nyc.temperature.tidy) <- tolower(names(nyc.temperature.tidy))

nyc.temperature.tidy <- separate(nyc.temperature.tidy,
                            col='date',
                            into = c('year','month','day'),
                            sep = '-') 

day.total.temp <- nyc.temperature.tidy %>%
  group_by(year) %>%
  summarize(total.days = n()) %>%
  arrange(total.days)

365 - day.total.temp[1,2]

#   b. Create a variable that tells us the difference between the highest and lowest 
#temperature for each day.Across the full dataset, what the average of this difference?

#Which day during this 150 year window had the biggest difference between the highest 
#and lowest temperature? 

#Averaging across years, which month tends to have the highest 
#average difference in daily high and low temperatures?
#------------------------------------------------------
#difference between the highest and lowest temperature for each day
nyc.temperature.tidy <- nyc.temperature.tidy %>%
  mutate(temp.diff = tmax - tmin)

#what the average of this difference?
sum(nyc.temperature.tidy$temp.diff)/nrow(nyc.temperature.tidy) #14.73218

#Which day during this 150 year window had the biggest difference between the highest 
#and lowest temperature?
nyc.temperature.tidy %>%
  group_by(day) %>%
  summarize(total.day.diff = sum(temp.diff)) %>%
  arrange(desc(total.day.diff))
  
  
nyc.temperature.tidy %>%
  arrange(desc(temp.diff)) #the 28th of March in 1921 had the biggest temperature difference of 48 degrees Fahrenheit


#Averaging across years, which month tends to have the highest 
#average difference in daily high and low temperatures?
nyc.temperature.tidy %>% 
  group_by(.,year, month) %>% 
  summarize(diff.total = sum(temp.diff),
            day.total = n()) %>%
  mutate(average.diff = diff.total/day.total) %>%
  arrange(desc(average.diff)) # month of may has the highest ave. temp diff

#c. Load and merge in the precipitation data. What type of merge does it mark sense
#to perform? Which variable(s) will you merge on? Perform the merge, then use the
#results to figure out how many days in the past 150 years had a high temperature of at
#least 50 degrees and received at least 1 inch of snowfall.
nyc.precipitation <-  read_csv('nyc-central-park-precipitation.csv')
nyc.precipitation.tidy <- nyc.precipitation
names(nyc.precipitation.tidy) <- tolower(names(nyc.precipitation.tidy))

nyc.precipitation.tidy <- separate(nyc.precipitation.tidy,
         col='date',
         into = c('year','month','day'),
         sep = '-') 

# anti_join(x = nyc.precipitation.tidy, 
#           y = nyc.temperature.tidy , 
#           by = c('year','month','day'))

nyc.temp.percip.merge <- merge(x = nyc.temperature.tidy, 
      y = nyc.precipitation.tidy, 
      by = c('year','month','day'),
      all = T)

#how many days in the past 150 years had a high temperature of at
#least 50 degrees and received at least 1 inch of snowfall.

  nyc.temp.percip.merge %>%
  filter(tmax>=50 & snow>=1) %>%
    nrow() #25 days

# d. Aggregate the data by month to figure out what percentage of days have had 
#preciptation since 1869. Your resulting dataset should have 12 rows (one per month).
#You should use the PRCP variable (and ignore the SNOW variable). Which month tends to
#have the most rainy days in New York City? What percentage of days does it usually
#rain in this month? And which month tends to be the dryest (i.e. fewest days with
#precipitation)? What percentage?
  
days.since.1869 <-  nyc.temp.percip.merge %>% 
    filter(year>=1869) %>%
  nrow()

percip.by.month <-  nyc.temp.percip.merge %>% 
    filter(prcp > 0 & year>=1869) %>%
    group_by(., month) %>% 
    summarize(percip.days = n()) 

#what percentage of days have had preciptation since 1869
sum(percip.by.month$percip.days)/days.since.1869*100#33.12708% days have precipitation since 1869

# Which month tends to have the most rainy days in New York City?
arrange(percip.by.month,desc(percip.days)) #march has the most rainy days
#What percentage of days does it usually rain in this month?
percip.by.month[3,2]/days.since.1869*100#3.092031% 

#And which month tends to be the dryest (i.e. fewest days with precipitation)? 
#What percentage?
arrange(percip.by.month,percip.days) #september has the least rainy days/is dry
percip.by.month[9,2]/days.since.1869*100#2.296207% 
  
##################### to check if my count worked
  # nyc.temp.percip.merge %>% 
  #   mutate(day.count = 1) %>% 
  #   filter(prcp > 0 & year>=1869) %>%
  #   group_by(., month) %>% 
  #   summarize(percip.days = sum(day.count)) 
  
# e. Use aggregation to figure out how many days in each year since 1869 had a low
#temperature of 32 degree or below. Use the plot() function or ggplot2 to make a simple
#graph of the relationship between the year (on the x-axis) and the number of cold days
#in Central Park (on the y-axis). What pattern do you notice in this graph?

nyc.temp.percip.merge %>% 
  filter(tmin <= 32 & year>=1869) %>%
  group_by(., year) %>% 
  summarize(low.32temp.days = n()) %>%
  ggplot(mapping = aes(x=year,y=low.32temp.days)) +
  geom_point(color='blue') +
  xlab("Year") + 
  ylab("Number of Cold Days in Central Park") +
  scale_x_discrete(breaks=c(1869,1884,1899,1914,1929,1944,1959,1974,1989,2004,2018)) +
  ggtitle("Cold Days (below 32 degrees Farenheight) in NYC Central Park") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 10)) 

#### DOES NOT WORK BECAUSE OF geom_smooth()
nyc.temp.percip.merge %>%
  filter(tmin <= 32 & year>=1869) %>%
  group_by(., year) %>%
  summarize(low.32temp.days = n()) %>%
  ggplot(mapping = aes(x=as.numeric(year),y=low.32temp.days)) +
  geom_point(color='blue') +
  geom_smooth(color='black') +
  xlab("Year") +
  ylab("Number of Cold Days in Central Park") +
  scale_x_continuous(breaks=c(1869,1884,1899,1914,1929,1944,1959,1974,1989,2004,2018)) +
  ggtitle("Cold Days (below 32 degrees Farenheight) in NYC Central Park") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 10))