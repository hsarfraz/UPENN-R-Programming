library(nycflights13)
library(tidyverse)
library(dplyr)
library(gridExtra)
# Question 1
anti_join(flights, airports, by = c("dest" = "faa"))
anti_join(airports, flights, by = c("faa" = "dest"))
# Question 2
not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  filter(year == 2013) %>%
  group_by(.,year, month, day) %>% 
  mutate(total_delay = dep_delay + arr_delay) %>%
  summarize(ave_delay = mean(total_delay, na.rm=T),) %>%
  arrange(desc(ave_delay)) %>%
  head(1) 

hour_temp <- weather %>%
  filter(year == 2013, month == 3, day == 8) %>%
  group_by(hour) %>%
  summarize(ave_temp = mean(temp), ave_humid = mean(humid)) %>%
  ggplot() +
  geom_line( mapping = aes(x=hour,y=ave_temp))

hour_humid <- weather %>%
  filter(year == 2013, month == 3, day == 8) %>%
  group_by(hour) %>%
  summarize(ave_temp = mean(temp), ave_humid = mean(humid)) %>%
  ggplot() +
  geom_line( mapping = aes(x=hour,y=ave_humid))

grid.arrange(hour_temp, hour_humid)

# Question 3
not_cancelled %>%
  left_join(planes, by = "tailnum") %>%
  mutate(avg_trip_speed = distance/(air_time/60)) %>%
  group_by(model, manufacturer) %>%
  summarize(ave_model_speed = mean(avg_trip_speed,na.rm=T)) %>%
  arrange(desc(ave_model_speed)) %>%
  head(10)


# Question 4

flights %>%
  left_join(planes, by= "tailnum") %>%
  mutate(plane_age = year.x - year.y) %>% 
  group_by(plane_age) %>%
  summarize(ave_dep_delay = mean(dep_delay, na.rm=T),
            ave_arr_delay = mean(arr_delay, na.rm=T)) %>%
  ggplot() +
  geom_point(mapping = aes(x=plane_age, y=ave_dep_delay))

flights %>%
  left_join(planes, by= "tailnum") %>%
  mutate(plane_age = year.x - year.y) %>% 
  group_by(plane_age) %>%
  summarize(ave_dep_delay = mean(dep_delay, na.rm=T),
            ave_arr_delay = mean(arr_delay, na.rm=T)) %>%
  ggplot() +
  geom_point(mapping = aes(x=plane_age, y=ave_arr_delay))