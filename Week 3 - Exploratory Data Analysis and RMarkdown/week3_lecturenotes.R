library(tidyverse)

# Exploring the data
summary(diamonds)

ggplot(data=diamonds)+ 
  geom_histogram(aes(x=price))

ggplot(data=diamonds)+ 
  geom_histogram(aes(x=price), bins=250)

ggplot(data=diamonds)+ 
  geom_histogram(aes(x=carat), bins=250)

diamonds %>%
   filter(carat >= 0.89, carat <= 1.1) %>%
   count(carat) %>%
   print(n = 30)

diamonds %>%
  filter(carat >= 1.44, carat <= 1.55) %>%
  count(carat) %>%
  print(n = 30)

ggplot(diamonds) + 
  geom_point(mapping=aes(x=carat,y=price))

ggplot(diamonds) + 
  geom_boxplot(mapping=aes(x=color,y=price)) + coord_flip()

ggplot(data = diamonds) + 
  geom_point(aes(x = carat, y = price, color=color, alpha = 0.5))

ggplot(data=diamonds) + 
  geom_bar(aes(x=cut))

ggplot(diamonds) + 
  geom_boxplot(mapping=aes(x=cut,y=price)) + 
  coord_flip() 

ggplot(data = diamonds) + 
  geom_point(aes(x = carat, y = price, color=cut), alpha=0.5)

ggplot(data = diamonds) + 
  geom_count(mapping=aes(x=cut,y=color))

#flights

library(nycflights13)
data("flights")

flights %>% 
   group_by(month,day) %>% 
   summarise(avg_delay = mean(arr_delay, na.rm=T)) %>% 
   ungroup() %>% 
   arrange(-avg_delay) %>% 
   head(10)

flights %>% 
   filter(arr_delay >= 0) %>% 
   group_by(month,day) %>% 
   summarise(avg_delay = mean(arr_delay, na.rm=T)) %>% 
   ungroup() %>% 
   arrange(-avg_delay) %>%
  head(10)

install.packages("lubridate")
library(lubridate)
view(flights)
?flights
?airports

flights %>% 
  mutate(flight_day = make_datetime(year,month,day)) %>%
  group_by(flight_day, origin) %>% 
  summarise(avg_delay = mean(arr_delay, na.rm=T)) %>% 
  ggplot(., mapping=aes(x=flight_day, y=avg_delay, color=origin)) + 
  geom_point() + 
  geom_smooth()

flights %>% 
  mutate(flight_day = make_datetime(year,month,day)) %>%
  group_by(flight_day, origin) %>% 
  summarise(avg_delay = mean(arr_delay, na.rm=T)) %>% 
  ggplot(., mapping=aes(x=flight_day, y=avg_delay)) + 
  geom_point(mapping = aes(color=origin)) + 
  geom_smooth()

flight_graph <- flights %>% 
  mutate(flight_day = make_datetime(year,month,day)) %>% 
  group_by(flight_day,origin) %>% 
  summarise(avg_delay = mean(arr_delay, na.rm=T)) 

library(scales)

ggplot(flight_graph, mapping=aes(x=flight_day, y=avg_delay)) + 
  geom_point(mapping=aes(color=origin)) + 
  geom_smooth() + 
  ylab("Average Arrival Delay (Minutes)") + 
  xlab("") + 
  scale_x_datetime(date_breaks="3 months",
                   labels=date_format("%b")) + 
  scale_y_continuous(breaks=c(0,30,60,90,120)) + 
  ggtitle("Average Daily Flight Delay by NYC Airport, 2013") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = 'bottom') +
  scale_color_discrete(name="Originating Airport", 
                       labels=c("Newark","Kennedy","LaGuardia"))
