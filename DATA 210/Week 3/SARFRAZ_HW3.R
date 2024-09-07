library(readr)
library(tidyverse)
library(dplyr)

#Question 1
#a. Begin by reading the file "recent-grads.csv" into R. How many rows and columns are
#there in the data? What is the unit of analysis of the data?

setwd("C:/Users/hussainsarfraz/Desktop/DATA 210/Week 3")
recent_grads <-  read_csv('recent-grads.csv')

nrow(recent_grads)
ncol(recent_grads)

# the unit of analysis for the data is that each row represents a single/different college major

#b. How many different "major categories" is the data divided into? Which category has
#the most majors in it? How many majors are in this category?

unique(recent_grads$Major_category) #result: 30 different major categories

table(recent_grads$Major_category) #Engineering appears in 29 rows

table(recent_grads[recent_grads$Major_category == 'Engineering', ]$Major)

#c. In total, how many women are included in the dataset? What percentage of people in
#the dataset are women?

sum(recent_grads$Women,na.rm=T)

sum(recent_grads$Total,na.rm=T)

sum(recent_grads$Women,na.rm=T)/sum(recent_grads$Total,na.rm=T) *100

#d. Which major had the highest percentage of women graduates? Which one had the lowest
#percentage? What where those two percentages?

recent_grads %>%
  arrange(desc(ShareWomen)) # Early Childhood Education - 96.89537%

recent_grads %>%
  arrange(ShareWomen) # Industrial Arts & Consumer Services - 0%


#e. How many people in the data majored in a field in the "Health" category? What 
#percentage of those people have a full-time, year-round job?
    
recent_grads[recent_grads$Major_category == "Health", ] %>%
  view()

sum(recent_grads[recent_grads$Major_category == "Health", ]$Total)

sum(recent_grads[recent_grads$Major_category == "Health", ]$Full_time)/sum(recent_grads[recent_grads$Major_category == "Health", ]$Total) *100

sum(recent_grads[recent_grads$Major_category == "Health", ]$Full_time_year_round)/sum(recent_grads[recent_grads$Major_category == "Health", ]$Total) *100


#f. Create a variable that tells you the spread in (i.e. the difference between) the 25th
#and 75th percentiles of income earned by people who received each major. Among majors
#with an unemployment rate of less than 6%, which one had the largest spread in salaries?

recentgrads_w_percentilediff <- recent_grads %>%
  mutate(percentile_diff = P75th - P25th,
         Unemployment_rate = Unemployment_rate *100) 

unemployment_less6 <- recentgrads_w_percentilediff[recentgrads_w_percentilediff$Unemployment_rate < 6, ] 

unemployment_less6 %>%
  arrange(desc(percentile_diff)) # ASTRONOMY AND ASTROPHYSICS

#Question 2
load("C:/Users/hussainsarfraz/Desktop/DATA 210/Week 3/exit-poll-2016.RData")

#view(exit)

#a. What is our unit of observation in these survey data? Does the dataset contain
#exactly one row for every observation? 
#If not, write code to reorganize the data so that each observation shows up as one, 
#and only one, row. 
#After you've done that, write a line of code to check whether there is one row per 
#respondent (who are each assigned a unique number in the id column). 
#Lastly, recode the numeric values of any new
#variables into meaningful labels (such as 'favorable' and 'unfavorable').
---
# Our unit of observation is that each row represents a person who voted

# The dataset does not contain exactly one row for each voter. The dataset currently has
# 5,914 rows because each voter's row is repeated which doubles the row amount.

exit[1,] == exit[2,]

exit_tidy <- spread(exit,
                 key = favorable.cand,
                 value = favorable.rating)

nrow(exit_tidy)

#attr(exit$favorable.rating, "labels")
clinton.trump_rating <- attr(exit$favorable.rating, "labels")

#factor(exit_tidy$clinton , labels=names(clinton.trump_rating))
exit_tidy$clinton <- factor(exit_tidy$clinton,
                            labels=names(clinton.trump_rating))

#factor(exit_tidy$trump , labels=names(clinton.trump_rating))
exit_tidy$trump <- factor(exit_tidy$trump,
                            labels=names(clinton.trump_rating))
#view(exit_tidy)

#b. The information about each respondent's education has been split across four columns.
#Reformat the data so that there is a single variable called educ that contains four
#possible values ('hs', 'some college','bachelors','postgrad'). Set any missing values
#to NA.
  
#attributes(exit$educ.hs)
#attributes(exit$educ.somecoll)

exit_tidy <- gather(exit_tidy,
       key = 'educ.column',
       value = 'educ.value',
       c('educ.hs','educ.somecoll','educ.bach','educ.postgrad'))

#view(exit_tidy)

#removing rows that have zero                   
exit_tidy <- exit_tidy[!(exit_tidy$educ.value==0),]

#view(exit_tidy)

duplicated(exit_tidy$id)

#removing rows which are repeated
exit_tidy <- exit_tidy[!duplicated(exit_tidy$id), ]

#view(exit_tidy)

# making the educ,column values that are in a row where educ.value==99 equal NA
#unique(exit_tidy$educ.value)
exit_tidy$educ.column[exit_tidy$educ.value==99] <- NA

# removed the educ.value column because at this point we have removed all the rows that
# people did not answer in the education section and can now see what answer each 
# participant has given in terms of education
exit_tidy$educ.value <- NULL

#unique(exit_tidy$educ.column)
exit_tidy$educ.column[exit_tidy$educ.column=='educ.hs'] <- 'hs'
exit_tidy$educ.column[exit_tidy$educ.column=='educ.somecoll'] <- 'some college'
exit_tidy$educ.column[exit_tidy$educ.column=='educ.bach'] <- 'bachelors'
exit_tidy$educ.column[exit_tidy$educ.column=='educ.postgrad'] <- 'postgrad'
                                                                      
#c. Now that the dataset is in good order, we can move on to cleaning individual 
#variables/columns. Start by splitting the sex.age.race variable into three separate 
#columns. Clean those new variables so that missing or unknown values are coded as NA.

exit_tidy <- separate(exit_tidy,
         col='sex.age.race',
         into = c('sex','age','race'),
         sep = ' ') 

# view(exit_tidy)

unique(exit_tidy$sex)
unique(exit_tidy$age)
unique(exit_tidy$race)

exit_tidy$sex[exit_tidy$sex=='unknown'] <- NA
exit_tidy$age[exit_tidy$age=='-999'] <- NA
exit_tidy$race[exit_tidy$race=='NA'] <- NA


#d. Create a new variable called third.party . This variable should equal 0 if the
#respondent voted for Clinton or Trump and 1 if the respondent voted for another
#candidate. If the respondent did not vote, or we do not know whether or not they voted,
#this variable should be set to NA. After you're done, remove PRSPA16 from the dataset.

#attr(exit$PRSPA16,'labels')
participant.candidate.vote <- attr(exit$PRSPA16,'labels')

#factor(exit_tidy$PRSPA16 , labels=names(participant.candidate.vote))
exit_tidy$PRSPA16 <- factor(exit_tidy$PRSPA16 ,
                            labels=names(participant.candidate.vote))

# view(exit_tidy)
unique(exit_tidy$third.party)


exit_tidy <- exit_tidy %>%
  mutate(third.party = PRSPA16,
         third.party = ifelse((PRSPA16 == 'Donald Trump'| PRSPA16 == 'Hillary Clinton'),
                              '0','1'),
         third.party = ifelse((PRSPA16 == 'Did not vote'),
                              NA, third.party)) 

# removing PRSPA16 column
exit_tidy$PRSPA16 <- NULL


#e. Often data scientists or social science researchers will refer to a variable that is
#coded as True/False or 0/1 as a 'dummy' or 'indicator' variable. Convert the married
#variable into a dummy variable, where 1 (or TRUE) indicates that somebody is married
#and 0 (or FALSE) indicates that they are not.

# view(exit_tidy)

attributes(exit$married)
unique(exit_tidy$married)

exit_tidy$married[exit_tidy$married==1] <- 'married'
exit_tidy$married[exit_tidy$married==2] <- 'not married'

#f. Recode the PHIL3 and partyid variables so that their values have meaningful labels,
#rather than just arbitrary numbers.

# attributes(exit$PHIL3)
# attr(exit$PHIL3,'labels')
political.standing <- attr(exit$PHIL3,'labels')
exit_tidy$PHIL3 <- factor(exit_tidy$PHIL3 ,
                            labels=names(political.standing))

# attributes(exit$partyid)
# attr(exit$partyid,'labels')
political.party.standing <- attr(exit$partyid,'labels')
exit_tidy$partyid <- factor(exit_tidy$partyid ,
                          labels=names(political.party.standing))

unique(exit_tidy$PHIL3)
unique(exit_tidy$partyid)
# view(exit_tidy)

#g. Make sure that every column has a name that is adequately descriptive and uniformly
#formatted

exit_tidy <- rename(exit_tidy,
                    vote_participant_id = id,
                    political_party_standing = PHIL3,
                    political_standing = partyid,
                    education = educ.column,
                    voting_candidate_category = third.party)
