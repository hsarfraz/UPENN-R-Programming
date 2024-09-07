install.packages("nycflights13")
library(dplyr)
library(nycflights13)
library(tidyverse)
flights
View(flights)
?flights

# filter function
filter(flights,month==12,day==1)

filter(flights, month==11|month==12)

filter(flights, dep_delay>30)
filter(flights, carrier=="UA")

#arrange function
longestflight <- arrange(flights, desc(air_time))
shortestflight <- arrange(flights, air_time)

View(longestflight)
View(shortestflight)

#select function
select(flights, -dep_time | -sched_dep_time)
select(flights, -dep_time & -sched_dep_time)
select(flights, -c(dep_time,sched_dep_time))

#mutate and transmute function
flights_sml<-select(flights,
                    year:day,
                    ends_with("delay"),
                    distance,
                    air_time)

mutate(flights_sml,speed=distance/air_time*60)
transmute(flights_sml,speed=distance/air_time*60)
transmute(flights,
          dep_time,
          hour=dep_time%/%100,
          minute=dep_time%%100)

longest_delay <- mutate(flights_sml,
                        delay_rank = min_rank(arr_delay))
arrange(longest_delay, delay_rank)

hour_dataset <- transmute(flights,
                          hour = dep_time%/%100,
                          minute = dep_time%%100,
                          totalmin = (hour*60)+minute)
arrange(hour_dataset, desc(hour))

# Summarize function
summarize(flights, delay=mean(dep_delay,na.rm=TRUE))
by_day <- group_by(flights,year,month,day)
summarize(by_day,delay=mean(dep_delay,na.rm=TRUE))

by_month <- group_by(flights,month)
monthly_ave <- summarize(by_month, delay=mean(dep_delay,na.rm=TRUE))
arrange(monthly_ave, delay)

# Using pipe w/ summarize function
by_dest <- group_by(flights, dest)
delay <- summarize(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE) )

 # graphing everything
ggplot(data = delay,mapping = aes(x=dist,y=delay))+
  geom_point(aes(size=count), alpha=1/3)+
  geom_smooth(se=FALSE)

delay <- filter(delay, count > 20, dest != "HNL")
ggplot(data = delay, mapping=aes(x=dist,y=delay))+
  geom_point(aes(size=count),alpha=1/3)+
  geom_smooth(se=FALSE)

filter(delay, dist>4500)

# pipeing %>% is the pipe
delays <- flights %>%
  group_by(dest) %>%
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  filter(count > 20, dest != "HNL")

arrival_delay <- flights %>%
  group_by(carrier) %>%
  summarise(
    count = n(),
    arr_delay,
    delay = mean(arr_delay, na.rm = TRUE)
  ) 

# 8: Exploring Missing Data
flights %>% 
   group_by(year, month, day) %>% 
   summarise(mean = mean(dep_delay))

 # seeing the missing data in departure delay
filter(flights, is.na(dep_delay))

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(
    delay = mean(arr_delay))

ggplot(data = delays, mapping = aes(x = delay)) +
  geom_freqpoly(binwidth = 10)

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )
ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)

 #filtering out noise
delays %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) + 
    geom_point(alpha = 1/10)
# 9: Combining functions together

flights %>% 
  mutate(.,cancelled = ifelse((is.na(dep_delay) & is.na(arr_delay)),1,0)) %>%
  group_by(carrier) %>% 
  summarise(100*mean(cancelled))

# Step 1: Tell whether each flight was cancelled 

flights %>%
   transmute(cancelled = is.na(dep_time))

 # How many flights does each carrier fly?
flights %>% 
  group_by(.,carrier) %>% 
  summarise(
    count = n())
 # showing ave delays. avg_delay2 shows average for delayed flights only
not_cancelled %>% 
   group_by(.,year, month, day) %>% 
   summarize(.,
      avg_delay1 = mean(arr_delay),
      avg_delay2 = mean(arr_delay[arr_delay > 0]) # the ave positive delay
      )

 # First and Last departure for each day
not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(
    first_dep = first(dep_time), 
    last_dep = last(dep_time)
  )

 # How many carriers fly to each destination
not_cancelled %>% 
   group_by(dest) %>% 
   summarise(carriers = n_distinct(carrier)) %>% 
   arrange(desc(carriers))

 # In flights dataset how many destinations and planes are included?
flights %>% 
  summarise(n_distinct(dest),
            n_distinct(tailnum))

 # How many different airplanes does each carrier fly?
flights %>%
  group_by(carrier) %>%
  summarize(n_distinct(tailnum))

 # Using count to see the number of flights going to each destination
not_cancelled %>% 
   count(dest)

 # percentage of flights that are delayed by more than 1 hour
not_cancelled %>% 
   group_by(year, month, day) %>% 
   summarise(hour_perc = 100*mean(arr_delay > 60))

daily <- group_by(flights, year, month, day)
per_day <- summarise(daily, flights = n())
per_month <- summarise(per_day, flights = sum(flights))
per_year <- summarise(per_month, flights = sum(flights))
per_day 

daily %>% 
   ungroup() %>% # no longer grouped by date
   summarise(flights = n()) # all flights

dep_time