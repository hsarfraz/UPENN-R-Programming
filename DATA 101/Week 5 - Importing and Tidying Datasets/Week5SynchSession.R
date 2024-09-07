#------------------------------------------------------
# Week 5: Tidying Data
#------------------------------------------------------
library(tidyverse)
setwd("~/Desktop/Teaching - Penn/Courses/Data101-620/Fall2021/Week5")
#------------------------------------------------------
# Notes: HWs look increasingly good!
# Please don't wait until the last hour to knit your
# rmd file, you should be knitting along the way to
# test that everything is working. These issues are 
# often challenging to diagnose, so you do want
# to treat knitting the file as an additional 
# homework question. 
geom_bar 
#------------------------------------------------------
# Long vs. Wide?


#Loading in wide and long data
load(file="ReshapingDataCreation.Rdata")

#Consider the first dataset, which is covid cases in counties on 4 dates
dat

#This is "wide" data because we have a variable (date) that is spread across multiple rows

#When we convert it to long, we will have multiple observations per county, one for each date.

#So we need two new rows, date, and covid.cases


#Historically, tidyverse used "gather" to go from wide to long data. Syntax looked like:


dat.l <- gather(dat,
                key= date,
                value = covid.cases,
                date_1_1_2021:date_1_4_2021)
dat.l

#Now the unit of analysis is county-day.


#But recently, "gather" was shifted (but not deprecated) to "pivot_longer"

dat.l2<-pivot_longer(dat,
                     date_1_1_2021:date_1_4_2021,
                     names_to="date",
                     values_to="covid.cases")




dat.l2

#Can we make those dates, dates?
#One helpful command with working with strings is gsub
 dat.l2%>%
          mutate(date = gsub("date_","", date))

library(lubridate)

dat.l2<- dat.l2 %>%
           mutate(date = gsub("date_","", date)) %>%
           mutate(date = mdy(date))
dat.l2 





##Long to wide
#Let's think about the second dataset

dat2

#Here this is long data, we have districts spanning across multiple rows, one 
#for each candidate.

#We might want this to be wide data if we want a single summation of the election
# for each district

#Converting to wide - traditional is "spread"

dat2.w <- spread(dat2,
                 key=candidate,
                 value=votes)

dat2.w

#Now, it's pivot_wider

dat2.w2 <- pivot_wider(dat2,
                 names_from=candidate,
                 values_from=votes)

dat2.w2

#We went form the unit of analysis being district-candidates, to the unit of analysis
#being districts.


#Then for example, we could calculate each candidates percent of the total vote

dat2.w2 %>%
  mutate(total_votes =Biden + Trump,
         perc_biden = Biden/total_votes,
         perc_trump = Trump/total_votes)



#You don't often want to delete data, but let's say we just wanted to make
#a nice table and only wanted district, perc.biden, perc.trump, and make
#the values rounded.

dat2.w2 %>%
  mutate(total_votes =Biden + Trump,
         percbiden = round(Biden/total_votes*100),
         perctrump = round(Trump/total_votes*100))%>%
  select(district, percbiden, perctrump)



#Of course, we can use the alternative commands to put each back in the original format

returnwide <- pivot_wider(dat.l2,
                names_from =date,
                values_from=covid.cases)
returnwide


returnlong <- pivot_longer(dat2.w2,
                           Biden:Trump,
                           names_to="candidate",
                           values_to="votes")

returnlong


#A real world example:
#Pull most recent data
url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
jh.w <- read_csv(url)
jh.w

#Current end date
names(jh.w)
#X9.29.21

#Convert to long, keep only county state, date, and cases, make date variable a
# date variable
jh.w %>%        
      pivot_longer(`1/22/20`:`9/29/21`,
                   names_to="date",
             values_to="cases") %>%
      select(FIPS, Province_State, date, cases) %>%
      mutate(date = mdy(date))


#ASSIGN THINGS TO OBJECTS!!!!
jh <- jh.w %>%        
  pivot_longer(`1/22/20`:`9/29/21`,
               names_to="date",
               values_to="cases") %>%
  select(FIPS, Province_State, date, cases) %>%
  mutate(date = mdy(date))


#Create a graph that has the total number of COVID cases over time for
#our local states
jh %>%
  filter(Province_State %in% c("Pennsylvania","New Jersey", "Delaware", "New York"))%>%
  group_by(date, Province_State)%>%
  summarise(med.cases = sum(cases))%>%
  ggplot() +
    geom_line(mapping=aes(x=date,y=med.cases,color=Province_State)) 



#######
#------------------------------------------------------
# Getting creative

#Working with the schools data

schools<-read.csv("Philly_schools.csv", stringsAsFactors = F)

# So we've compared zip codes, now lets try comparing other groups
# of schools. We could group schools by zipcodes, but there's actually 
# another sneaky way of breaking up the schools data
# School codes aren't actually arbitrarily assigned- the first number in
# the code actually is a grouping. The grouping is greographic in nature
# and spans several zip codes. I am not sure why the boudaries are set
# as they are. But when we start getting into mapping, perhaps we'll
# revisit these data and come up with some ideas. 

# So, its possible to treat the school codes variable as numeric, and create
# groupings based on the values. But a much simpler way is to simply pull out
# the first number of each code into its own column

schools %>% 
  separate(col=School_code,into=c("Grouping","ID"), sep=1)


# Your turn!

# Using the new "Grouping" column- create a facet plot
# that shows the difference in average total suspensions
# by grouping. Make the plot as complete and attractive
# as possible. EMAIL IT TO ME via Canvas. Consider this an attention check,
# but also a chance for everyone to get an extra point. 




