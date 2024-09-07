#-------------------------------------------------
# Topic 3 HW: Exploratory Data Analysis Homework
# Name: Hussain Sarfraz
#-------------------------------------------------
install.packages("latexpdf")
library(latexpdf)

install.packages("tinytex")
library(tinytex)

tinytex::install_tinytex()


library(tidyverse)
library(scales)
#-------------------------------------------------
# Question 1

#exploring the data
# summary shows mean, median, and mode
summary(diamonds)

# STEP 1: creating the barchart to count 
#         number of clarity types in dataset

ggplot(data=diamonds) + 
  geom_bar(aes(x=clarity)) + 
  ylab("Number of Diamonds") + 
  xlab("Diamond Clarity Scale") + 
  scale_x_discrete(labels = c(
    "I1" = 8, "SI2"= 6 , "SI1"= 5 , "VS2"= 4,
    "VS1"= 3, "VVS2"= 2, "VVS1"= 1, "IF" = 0 )) +
  scale_y_continuous(breaks=c(0,2500,5000,7500,10000,12500)) + 
  ggtitle("Number of Diamond Clarity Types") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
  
# STEP 2: creating a boxplot to compare prices
#         of diamond clarity types 

ggplot(diamonds) + 
  geom_boxplot(mapping=aes(x=clarity,y=price)) + 
  coord_flip() + 
  ylab("Price") + 
  xlab("Diamond Clarity Scale") + 
  scale_x_discrete(labels = c(
    "I1" = 8, "SI2"= 6 , "SI1"= 5 , "VS2"= 4,
    "VS1"= 3, "VVS2"= 2, "VVS1"= 1, "IF" = 0 )) +
  scale_y_continuous(breaks=c(0,2500,5000,10000,15000,20000)) + 
  ggtitle("Price of Diamond Clarity Types") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# STEP 3: creating a scatterplot of carat-price and adding a 
#         color aethetic that represents different clarity types

ggplot(data = diamonds) + 
  geom_point(aes(x = carat, y = price, color=clarity), alpha=0.5) + 
  ylab("Price") + 
  xlab("Diamond Weight (carat)") + 
  scale_y_continuous(breaks=c(0,5000,10000,15000,20000)) + 
  ggtitle("Price of Diamond by their Weight") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(name="Clarity Types", 
                       labels=c(
                         "I1" = 8, "SI2"= 6 , "SI1"= 5 , "VS2"= 4,
                         "VS1"= 3, "VVS2"= 2, "VVS1"= 1, "IF" = 0 ))

# STEP 4: creating a scatterplot of carat-price and adding a 
#         color aethetic that represents color and cut

#  carat-price scatterplot with a color aesthetic 
ggplot(data = diamonds) + 
  geom_point(aes(x = carat, y = price, color=color), alpha=0.5) + 
  ylab("Price") + 
  xlab("Diamond Weight (carat)") + 
  scale_y_continuous(breaks=c(0,5000,10000,15000,20000)) + 
  ggtitle("Price of Diamond by their Weight") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(name="Diamond Color")

#  carat-price scatterplot with a cut aesthetic 
ggplot(data = diamonds) + 
  geom_point(aes(x = carat, y = price, color=cut), alpha=0.5) + 
  ylab("Price") + 
  xlab("Diamond Weight (carat)") + 
  scale_y_continuous(breaks=c(0,5000,10000,15000,20000)) + 
  ggtitle("Price of Diamond by their Weight") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(name="Diamond Cut")

# STEP 5: geom_count for clarity-color and clarity-cut

# plot for clarity and color 
ggplot(data = diamonds) + 
  geom_count(mapping=aes(x=clarity,y=color)) + 
  ylab("Diamond Color") + 
  xlab("Diamond Clarity") +
  scale_x_discrete(labels = c(
    "I1" = 8, "SI2"= 6 , "SI1"= 5 , "VS2"= 4,
    "VS1"= 3, "VVS2"= 2, "VVS1"= 1, "IF" = 0 )) + 
  ggtitle("Diamond color and clarity count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(size='Diamond Count')

# plot for clarity and cut 

ggplot(data = diamonds) + 
  geom_count(mapping=aes(x=clarity,y=cut)) + 
  ylab("Diamond Cut") + 
  xlab("Diamond Clarity") +
  scale_x_discrete(labels = c(
    "I1" = 8, "SI2"= 6 , "SI1"= 5 , "VS2"= 4,
    "VS1"= 3, "VVS2"= 2, "VVS1"= 1, "IF" = 0 )) + 
  ggtitle("Diamond cut and clarity count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(size='Diamond Count')


#-------------------------------------------------
# Question 2


library(nycflights13)

# STEP 1: creating a object that has non-cancelled flights
not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

# STEP 2: scatterplot of flight # for each destination against
#         ave. arrival delay for each destination

not_cancelled %>% 
  group_by(dest) %>% 
  summarize(., count =n() ,
            avg_delay1 = mean(arr_delay)) %>%
  ggplot(aes(x = count, y = avg_delay1)) +   
  geom_point() + 
  geom_smooth() +
  ylab("Average Arrival Delay") + 
  xlab("Number of Flights for each Destination") + 
  scale_y_continuous(breaks=c(-20,-10,0,10,20,30,40,50)) +
  scale_x_continuous(breaks=c(0,2500,5000,7500,10000,12500,15000,17000))+
  ggtitle("Destination arrival delays and flight numbers") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



#-------------------------------------------------
# Question 3A

# scatterplot of average airplane speed and distance airplane travelled 
not_cancelled %>% 
  mutate(avg_speed = distance/air_time) %>%
  ggplot(aes(x = avg_speed, y = distance)) +   
    geom_point() +   
  ylab("Distance Airplane Travelled") + 
  xlab("Airplane Average Speed") + 
  scale_y_continuous(breaks=c(0,1000,2000,3000,4000,5000)) +
  scale_x_continuous(breaks=c(0,2,4,6,8,10,12))+
  ggtitle("Distance Travelled by Average Airplane Speed") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


#-------------------------------------------------
# Question 3B

# calculation to get the proportion on arrived flights that are on time 
 # (even though the flight was delayed)
not_cancelled %>% 
  mutate(on_time = ifelse((arr_delay <= 0),1,0),
         delay_flights = ifelse((dep_delay > 0),1,0)) %>%
  filter(delay_flights == 1) %>%
  summarize(., on_time_proportion = 100*mean(on_time))


#-------------------------------------------------
# Question 3C

#calculation to get the estimated delay time of when a flights arrival 
 # would be on time

not_cancelled %>% 
  mutate(on_time = ifelse((arr_delay <= 0),1,0),
         delay_flights = ifelse((dep_delay > 0),1,0)) %>%
  filter(delay_flights == 1, on_time == 1) %>%
  summarize(.,mean_of_dep_delay = mean(dep_delay))
