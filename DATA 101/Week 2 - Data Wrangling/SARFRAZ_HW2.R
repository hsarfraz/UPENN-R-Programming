#----------------------------------------------
# DATA 101 - HOMEWORK 2
# Homework 2: Hussain Sarfraz
#----------------------------------------------

# Please save this script as "YOUR LAST NAME_HW2.R" and upload the script to Canvas. 
# You should also upload a word document containing your write up and graphs.
# Please type your code into the sections outlined below. 

# Load these libraries
library(nycflights13)
library(tidyverse)
?flights
view(flights)
#----------------------------------------------
# Question 1

# Here is the object 'not_cancelled' being created
not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay)) # filtering delayed flights because we want to see non-cancelled flights

# Showing average delays. avg_delay2 shows average for delayed flights only
not_cancelled %>% 
  group_by(.,year, month, day) %>% 
  summarize(.,
            avg_delay1 = mean(arr_delay),
            avg_delay2 = mean(arr_delay[arr_delay > 0]) # the ave positive delay
  )

#----------------------------------------------
# Question 2

#avg delays and proportion of flights cancelled

flights %>%
  group_by(.,year, month, day) %>% 
  mutate(cancelled = ifelse((is.na(dep_delay) & is.na(arr_delay)),1,0)) %>%
  summarize(.,
            avg_delay1 = mean(dep_delay,na.rm=TRUE),
            flights_proportion = 100*mean(cancelled)) %>%
  filter(flights_proportion < 10) %>%
  ggplot(mapping = aes(x = avg_delay1, y = flights_proportion)) + 
  geom_point(alpha=1/10)

##avg delays and proportion of flights cancelled without filter function

flights %>%
  group_by(.,year, month, day) %>% 
  mutate(cancelled = ifelse((is.na(dep_delay) & is.na(arr_delay)),1,0)) %>%
  summarize(.,
            avg_delay1 = mean(dep_delay,na.rm=TRUE),
            flights_proportion = 100*mean(cancelled)) %>%
  ggplot(mapping = aes(x = avg_delay1, y = flights_proportion)) + 
  geom_point(alpha=1/10)


#----------------------------------------------
# Question 3

# code for scatterplot of early flight proportion for each hour

not_cancelled %>%
  group_by(.,hour) %>%
  mutate(flight_early = ifelse((dep_delay <= 0),1,0)) %>%
  summarize(.,
            ontime_proportion = 100*mean(flight_early)) %>%
  ggplot(mapping = aes(x = hour, y = ontime_proportion)) + 
  geom_point()


#----------------------------------------------
# Question 4

# code for scatterplot of 30 min late flight proportion sorted by each carrier

not_cancelled %>%
  group_by(.,carrier) %>%
  mutate(flight_30minlate = ifelse((dep_delay >= 30),1,0)) %>%
  summarize(.,
            f_30minlate_proportion = 100*mean(flight_30minlate)) %>%
  ggplot(mapping = aes(x = carrier, y = f_30minlate_proportion)) + 
  geom_point()

# A bar chart of 30 min late flight proportion, sorted by each carrier 
not_cancelled %>%
  group_by(.,carrier) %>%
  mutate(flight_30minlate = ifelse((dep_delay >= 30),1,0)) %>%
  summarize(.,
            f_30minlate_proportion = 100*mean(flight_30minlate)) %>%
  ggplot(mapping = aes(x = carrier, y = f_30minlate_proportion)) + 
  geom_bar(stat='identity')

#----------------------------------------------
# Question 5

# The code I had first. I noticted the graph was too big so I added 
# the filter function to get the value of the smallest average
not_cancelled %>%
  group_by(.,dest) %>%
  summarize(.,
            arrivaldelay_average = mean(arr_delay)) %>%
  ggplot(mapping = aes(x = dest, y = arrivaldelay_average)) + 
  geom_point()

# The code that displays the smallest average of arrivals
# and the destination that this value came from

not_cancelled %>%
  group_by(.,dest) %>%
  summarize(.,
            arrivaldelay_average = mean(arr_delay)) %>%
  filter(arrivaldelay_average < -20) %>%
  ggplot(mapping = aes(x = dest, y = arrivaldelay_average)) + 
  geom_point()


#----------------------------------------------
# Bonus
install.packages("Lahman")
library(Lahman)
batting <- as_tibble(Lahman::Batting)
?Batting 
view(Batting)
# players with best and worst averages

Batting %>%
  group_by(.,playerID) %>%
  summarize(.,
            batting_average = sum(H)/sum(AB)) %>%
  View()
  ggplot(mapping = aes(x = playerID, y = batting_average)) + 
  geom_point()

# players with at least 500 at bats

Batting %>%
  group_by(.,playerID) %>%
  filter(AB >= 500) %>%
  summarize(.,
            batting_average = H/AB) %>%
  ggplot(mapping = aes(x = playerID, y = batting_average)) + 
  geom_point()

# players with at least 700 at bats (shows the graph more clearly)

Batting %>%
  group_by(.,playerID) %>%
  filter(AB >= 700) %>%
  summarize(.,
            batting_average = H/AB) %>%
  ggplot(mapping = aes(x = playerID, y = batting_average)) + 
  geom_point()