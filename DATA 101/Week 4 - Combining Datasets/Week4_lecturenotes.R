library(nycflights13)
library(tidyverse)
library(dplyr)

nycflights13::airlines
?nycflights13

?flights
view(flights)
view(planes)
?planes

# creating object that will include the combined datasets
flights2 <- flights %>% 
   select(year:day, hour, origin, dest, tailnum, carrier, dep_delay) 

# combining flights and airlines dataset
flights2 %>% 
   left_join(airlines, by="carrier")

# combining flights and planes dataset
flights2 %>% 
  left_join(planes, by="tailnum") %>%
  filter(!is.na(dep_delay)) %>% 
  group_by(seats) %>% 
  summarize(avg_delay = mean(dep_delay)) %>% 
  ggplot(mapping=aes(x=seats,y=avg_delay)) + 
  geom_point() + 
  geom_smooth()

arrange(planes,desc(seats))

flights2 %>% 
  left_join(planes, by="tailnum") %>%
  filter(!is.na(dep_delay) & seats < 401) %>% 
  group_by(seats) %>% 
  summarize(avg_delay = mean(dep_delay)) %>% 
  ggplot(mapping=aes(x=seats,y=avg_delay)) + 
  geom_point() + 
  geom_smooth()

flights2 %>% 
  left_join(planes, by="tailnum") %>%
  filter(!is.na(dep_delay), seats < 401) %>% 
  group_by(seats) %>% 
  summarize(avg_delay = mean(dep_delay)) %>% 
  ggplot(mapping=aes(x=seats,y=avg_delay)) + 
  geom_point() + 
  geom_smooth()


top5 <- flights %>% 
   count(dest, sort=T) %>% 
   head(5)

flights %>% 
   semi_join(top5)

flights %>% 
   anti_join(planes, by="tailnum") %>% 
   count(tailnum, sort=T)

flights %>% 
    anti_join(planes, by="tailnum") %>% 
   filter(., is.na(tailnum)) %>% 
   count(carrier)

flew50 <- flights %>% 
  group_by(tailnum) %>% 
  count() %>% 
  filter(n > 49) 
flights %>% 
  semi_join(flew50)

