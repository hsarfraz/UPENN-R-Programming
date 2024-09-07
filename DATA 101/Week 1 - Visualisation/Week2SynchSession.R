#---------------------------------------------------------
# DATA 101 Week 2: Cleaning and Transforming Data
library(tidyverse)

# Usually in the synchronous sessions, I will be exposing you to
# new content, however for this week, since the lecture videos
# do cover so much content- and its such important content-
# i'm going to mostly stick with working with the functions
# directly from the videos. But, i am going to do it
# in a slightly different way

# The lecture videos introduce you to new content, and then 
# give examples of how that content can be used. 

# In the synchronous sessions, I'll try to flip that script- we'll 
# see how a particular situation might call for the variables you learned
# about earlier in the lectures. You'll get the most out of these sessions
# if you have viewed all of the lecture videos before the synchronous
# session starts. 


# A quick review of new concepts in from the lecture videos:

library(nycflights13)
flights

#Command that lets you quickly determine what's in a dataset:
names(flights)

#Note that running these commands in isolation doesn't really *do* anything
#Without saving the subsequent dataset, or stringing together code with the pipe
#these commands just execute what you said to do without saving the result

#### filter() :subset your data (remove unwanted data)
#Focus on only certain rows
filter(flights, arr_time>700)
#Again, R isn't saving this. Nothing has changed with flights.
#We could save a new dataset that is filtered like this:

flights.long <- filter(flights, arr_time>700)
summarise(flights.long, range(arr_time))

#But in this course we will mostly think about piping this
#filtered dataset into other commands.

filter(flights, arr_time>700)%>%
  summarise(range(arr_time))

#### select() : isolate a particular variable of interest
#Focus on only certain columns
select(flights, carrier, flight) #carrier and flight only
select(flights, -carrier) #everything but carrier
select(flights, carrier, everything()) #carrier and then everything else


#### group_by() : apply to categorical variables to define subgroups within the variable
group_by(flights, carrier)

#### summarise() : get a summary statistic
summarise(flights, mean(dep_time,na.rm=T))
summarise(flights, max(dep_time,na.rm=T))
summarise(flights, min(dep_time,na.rm=T))
summarise(flights, median(dep_time,na.rm=T))

#### arrange() : print out your data in a certain way
arrange(flights, sched_dep_time) #lowest to highest scheduled dep time
arrange(flights, desc(sched_dep_time)) #highest to lowest

#### mutate(): create a new variable using other information in the dataset (feature engineering)
mutate(flights, delay.hours = arr_delay/60)

#### ifelse() : make a conditional statement (if this, then do that, if not this, then do somethine else.)
mutate(flights, cancelled = ifelse(is.na(dep_time),1,0))
#(More on conditional statements below)


#### %>% : the pipe: shuttles data between functions
mutate(flights, delay.hours = arr_delay/60) %>%
  select(delay.hours,arr_delay)

mutate(flights, cancelled = ifelse(is.na(dep_time),1,0)) %>%
  summarise(mean(cancelled))



# What I want to focus on today is breaking down data science prompts into 
# an executable strategy. This is really a case of practice, practice, practice
# but I do have a few tips to help you get started, and I do think that watching
# me walk through the steps will help you internalize how to go about
# thinking through these problems 


# Functions tend to only do one element of what we need them to do: and this is
# by design. Think of functions as individual building blocks and in order
# to make any type of structure, they need to be put together and you can 
# rearrange those same blocks in many different ways. 
# I think I'll be able to illustrate why this is during tonight's lecture
# 

#---------------------------------------------------------
# Breaking down the question


# Word problems are tricky, full of possible misinterpretations. 
# How we speak is so different than how we need to build code, that there 
# really is a translation that needs to happen
# I see often that students
# will read a problem, miss a key sentence or word and end up answering a 
# different question entirely. In class, that's okay because we always give partial
# credit. But in our work, precision will matter greatly. How we'll go about 
# breaking down the question might feel a little silly at first, but I think
# we're all better off being over prescriptive and feeling a little foolish than
# accidentally putting effort into a question that wasn't asked.

#One of my favorite maxims is: Clear writing is clear thinking. Being able
#to think clearly about a problem and answer it is what is necessary to eventually
#learn how to write about data clearly. You would be surprised at how much you need
#to work to explain things clearly and simply, even when writing for very technical audiences.
#Need to work even harder to explain things to non-technical audiences.

#Its always a good
# idea as a data scientist anyway to rephrase someone's question and read it back to them
# to confirm they're on the same page
# We're really learning a new language here and how questions and asked and answered
# really are different. 

# How to break down the question for translation:
# 1. read over the question several times. Note any variables that
# are in the question -are any variables called out by name? If so, we'll
# need to make sure they're included in our answer code somehow. Are there any variables that
# aren't called out by name, but are effectually included in the question? 

# 2. How are those variables mentioned related to each other? How to do they "map"

# 3. Once i know which varialbes I need to find, and how they need to be investigated, 
# what am i being asked to produce? A simple number? A relationship- does that mean a graph?


# PRE-STEP: this will only work if you are familiar with the varialbes
# always check into those first.

#---------------------------------------------------------
#We're going to work with some data on Philadelphia schools
#I've saved this in the "Data/Raw" folder of my dropbox

#I can load via the read.csv command
#Note that you will have to change this to your name

setwd("~/Desktop/Teaching - Penn/Courses/Data101-620/Fall2021/Week2") #WHERE MY DATA IS, PROBABLY NOT YOURS

schools <- read.csv("Philly_schools.csv")


#Can also hard code this:
schools<-read.csv("~/Desktop/Teaching - Penn/Courses/Data101-620/Fall2021/Week2/Philly_schools.csv")

#Convert to tibble so we can use with tidyverse

schools <- as_tibble(schools)
schools
names(schools)
#---------------------------------------------------------
# Prompt 1
# Let's try breaking down the prompt below:

# I am moving to Philadelphia with my 16 year old daughter, Cindy, and 
# am looking for the best school for her. I haven't picked a zip code to
# live in yet, so we'll choose where to live based off of which school
# is right for her. I believe that the absolute best learning environment
# for Cindy is where teachers are the most invested in students. Where
# should Cindy go to school? Can you send me a list of recommendations?

# Step 1- break down the prompt below. I like to do this in pieces, starting with
# what variables i need, and then moving into what do i do with them
# Which variables in this dataset should
# we use to get our answer?






# School level, teacher attendance (could use teacher salary?)





# Step 2: what do we need to do with those variables?







# I'm looking for schools in the city of philly that are 
# high schools. then i need to see which hs has the highest teacher
# attendance. 

# Now you understand the question, and what you're really looking for. Now we 
# can actually begin to code. 

# How do I interact with JUST philly high schools?









# Lets test it to make sure it works, test the code piece by piece
filter(schools, SCHOOL_LEVEL_NAME == "HIGH SCHOOL")

# So that is giving my just high schools- great!
# what's the next step?








# I want to sort by teacher attendance and print out that list
filter(schools, SCHOOL_LEVEL_NAME == "HIGH SCHOOL") %>% 
  arrange(Teacher_attendance)








# Let's make sure the list puts the top contender first
filter(schools, SCHOOL_LEVEL_NAME == "HIGH SCHOOL") %>% 
  arrange(desc(Teacher_attendance))

# Workflow is important- adding one new line after ensuring the previous line works
# is best practice 

# What if we just wanted the name of the school, the address, and attendance?










# Not too bad!
filter(schools, SCHOOL_LEVEL_NAME == "HIGH SCHOOL") %>% 
  arrange(desc(Teacher_attendance)) %>% 
  select(SCHOOL_NAME_1, ADDRESS, SCHOOL_ZIP,Teacher_attendance)

#Now, when doing this in the real world there is no need to do this in piecemeal
#fashion, running one line of this at a time. All we need to do here is to run these last three
#lines of code to ge the answer we want. 
#We want our code to efficiently do the minimum in order to get the answer we want.





#--------------------------------------------------------- 
# Prompt 2: In the
# prompt above, we chose to use teacher attendance as an indicator for teacher
# investment. Our theory was that if an educator is invested, they'll show up to
# class more often. We can't really test that hypothesis given this dataset- but
# we can look for indicators that this might be a helpful metric. One idea is to
# look at the variability within the teacher attendance variable- is there a big
# difference in that variable among schools? 
# what about the difference in average average teacher attendance between elementary,
# middle, high, and career/tech schools?







# Part a- spread of teacher attendance variable
summary(schools)


# Part b- difference in average average teacher attendance
# by school type

# What does that mean? What variables are we using?








# School level, teacher attendance
# And how are we using them?






# Take the average teacher attendance by school level:
# Split up the School level column by school type, then
# take the average of the teacher attendance column by that school type

schools %>% 
  group_by(SCHOOL_LEVEL_NAME) %>%
summarise(attendance = mean(Teacher_attendance))



# How can I save this data?
Attendance <- schools %>% 
  group_by(SCHOOL_LEVEL_NAME) %>%
  summarise(attendance=mean(Teacher_attendance))


# Now it lives in my global environment and I can 
# interact with that data without repeating the code above
Attendance







#---------------------------------------------------------
# Prompt 3: 
# create a variable that makes a proportion of new students

# Prompt: A recent study suggests that there may be an association
# between the percent of new students enrolled at a given school 
# and the number of total suspensions in that schools per year. 
# Can we replicate these findings?


# What variables do we need?








# This one is tricky! Here, the question is specifically asking
# for the percentage of new students enrolled, which is different than our 
# new student enrollment column! That column is just the total number of students
# But we do have the number of newly enrolled students, and the total enrollment,
# so we have everything we need to create the variable in question! This is
# called feature engineering, and we can use the mutate function to create
# the variable. 


# So lets make the variable with mutate:
schools %>% 
  mutate(NEP= (New_student/Enrollment)*100)



# And now we need to see if we can show the relationship 
# between this new variable, NEP, and total suspensions
# Whenever i hear the words "relationship" or "association"
# I always think 'graph' - what type of graph will fit this data 
# well?

#Both of these variables are continuous, which cues me to think 
#of a scatterplot






# Here we can see there does appear to be some correlation
schools %>% 
  mutate(NEP= (New_student/Enrollment)*100) %>% 
  ggplot(mapping = aes(x=NEP, y=Total_suspensions))+
  geom_point() 



#Another possibility here would have been to consider the correlation
#between these two coefficients:
schools %>% 
  mutate(NEP= (New_student/Enrollment)*100) %>% 
  summarise(cor(NEP, Total_suspensions))

#There is a small positive correlation between these things.



#What would we need to do if we wanted to figure out if this relationship
#differed across school level?

#With tidyverse that's super easy to do:


schools %>% 
  mutate(NEP= (New_student/Enrollment)*100) %>% 
  group_by(SCHOOL_LEVEL_NAME)%>%
  summarise(cor(NEP, Total_suspensions))

#The correlation varies wildly!



#---------------------------------------------------------------------------
# Prompt 4

# What percentage of philly schools are Career & technical schools?


# In the lecture videos, Prof Levendusky mentioned a cool trick for percentages-
# if you use a dummy variable and take an average, you'll get the percentage of
# "ones" in the dataset

# So let's add a column that records whether or not
# a row is a Career/Tech school or not
# How do we add a column? How do we assign 1s and 0s?









# use mutate to add a column, and ifelse to add 1s and 0s
schools %>% 
  mutate(CTP=ifelse(SCHOOL_LEVEL_NAME=="CAREER AND TECHNICAL HIGH SCHL",1,0))

# The next step is to take the mean of the new column

schools %>% 
  mutate(CTP=ifelse(SCHOOL_LEVEL_NAME=="CAREER AND TECHNICAL HIGH SCHL",1,0)) %>% 
  summarise(mean(CTP)*100)
  



#IF TIME ALLOWS:

##Some more info on conditional logic

#If we give R some expressions that can be T or F, R will evaluate them:

2 == 1+1

1/2 == .5

2/3 == .6666 # False because of imperfect rounding


## We can also use the double equals sign (or any other boolean operator)
## on vectors

vec <- c(1,2,3,4,5)

vec == 3


## Other common boolean operators are greater than and less than:

2 > 3

2 < 3

vec > 3


## And to answer why we need to use double equals signs, it's
#because we can also do  greater/less than or equal to:

vec >= 3

vec <= 3



## R can also think about a boolean string as a vector of 1s(T) and 0s(F)

#So you can actually do some basic accounting of them:

sum(vec >= 3)
mean(vec >= 3)


#### Another key boolean operator is the exclamation point, which negates logical
#### statements.

2 == 3 #FALSE

2 != 3 #TRUE

!(2 == 3) #TRUE. essentially the same as the one above

2 > 3 #FALSE

!(2 > 3) #TRUE

!c(T, F, T,T,T, F)

#There are also some built in operators for datasets
#we saw, for example, the is.na() function above

summarise(flights, mean(is.na(dep_time)))












