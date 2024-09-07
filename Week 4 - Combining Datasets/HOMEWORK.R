library(nycflights13)
library(tidyverse)
# Question 1

view(flights)
view(airports)

?flights
?airports

anti_join(flights, airports, by = c("dest" = "faa"))  %>%
  distinct(dest)

anti_join(airports, flights, by = c("faa" = "dest")) %>%
  distinct(faa)

# Question 2

?flights
?weather

view(weather)

flights %>% 
  distinct(origin)

weather %>% 
  distinct(origin)

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay)) # filtering delayed flights because we want to see non-cancelled flights

not_cancelled %>% 
  filter(year == 2013) %>%
  group_by(.,year, month, day) %>% 
  mutate(total_delay = dep_delay + arr_delay) %>%
  summarize(ave_delay = mean(total_delay,na.rm=T),) %>%
  arrange(desc(ave_delay)) %>%
  head(1) %>%
  view()

install.packages("gridExtra")
library(gridExtra)

# hour vs. ave_temp
hour_temp <- weather %>%
  filter(year == 2013, month == 3, day == 8) %>%
  group_by(hour) %>%
  summarize(ave_temp = mean(temp), ave_humid = mean(humid)) %>%
  ggplot() +
  geom_line( mapping = aes(x=hour,y=ave_temp))

# hour vs. ave_humid
hour_humid <- weather %>%
  filter(year == 2013, month == 3, day == 8) %>%
  group_by(hour) %>%
  summarize(ave_temp = mean(temp), ave_humid = mean(humid)) %>%
  ggplot() +
  geom_line( mapping = aes(x=hour,y=ave_humid))

  
grid.arrange(hour_temp, hour_humid)


# Question 3

view(planes)
view(flights)

not_cancelled %>%
  mutate(avg_speed = distance/air_time) %>%
  arrange(desc(avg_speed)) %>%
  view()

not_cancelled %>%
  left_join(planes, by = "tailnum") %>%
  mutate(avg_trip_speed = distance/(air_time/60)) %>%
  group_by(model, manufacturer) %>%
  summarize(ave_model_speed = mean(avg_trip_speed,na.rm=T)) %>%
  arrange(desc(ave_model_speed)) %>%
  view()




# Question 4

# plane vs. dep delay
flights %>%
  left_join(planes, by= "tailnum") %>%
  mutate(plane_age = year.x - year.y) %>% 
  group_by(plane_age) %>%
  summarize(ave_dep_delay = mean(dep_delay, na.rm=T),
            ave_arr_delay = mean(arr_delay, na.rm=T)) %>%
   ggplot() +
   geom_point(mapping = aes(x=plane_age, y=ave_dep_delay))

 # plane vs. arrival delay
flights %>%
   left_join(planes, by= "tailnum") %>%
   mutate(plane_age = year.x - year.y) %>% 
   group_by(plane_age) %>%
   summarize(ave_dep_delay = mean(dep_delay, na.rm=T),
             ave_arr_delay = mean(arr_delay, na.rm=T)) %>%
   ggplot() +
   geom_point(mapping = aes(x=plane_age, y=ave_arr_delay))
