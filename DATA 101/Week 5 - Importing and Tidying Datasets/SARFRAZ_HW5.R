library(gridExtra)

# Question 1

library(tidyverse)


# Part 1: Loading data in R
install.packages("readxl")
library(readxl)
setwd("C:/Users/hussainsarfraz/Desktop/DATA 101/Week 5 - Importing and Tidying Datasets")
tuition_dataset <-  read_xlsx('us_avg_tuition.xlsx')

# Part 2: Tidying the data

view(tuition_dataset)

tuition_tidy <- tuition_dataset %>%
  pivot_longer(c('2004':'2015'),
               names_to = "Year",
               values_to = "Average_Tuition")

view(tuition_tidy)

# Question 2

tuition_bystate <- tuition_tidy %>%
  group_by(State) %>%
  summarize(state_ave_tuition = mean(Average_Tuition)) %>%
  arrange(desc(state_ave_tuition)) 
  
  
# Question 3


tuition_bystate %>%
  ggplot(mapping = aes(reorder(State,
                               state_ave_tuition), state_ave_tuition)) + 
  geom_bar(stat='identity') + 
  coord_flip() +
  xlab("States") + 
  ylab("Tuition") +
  scale_y_continuous(breaks=c(0,2000,4000,6000,8000,10000,12000)) + 
  ggtitle("Average Tuition for each State") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 4),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 10))
  
# Question 4

view(tuition_tidy)

percent_avetuition <- tuition_tidy %>%
  filter(Year == 2004 | Year == 2015) %>%
  group_by(State) %>%
  summarise(percent_avedecrease=(last(Average_Tuition)-first(Average_Tuition))/first(Average_Tuition)) %>%
  ggplot(mapping = aes(reorder(State,
                               percent_avedecrease), percent_avedecrease)) + 
  geom_bar(stat='identity') + 
  coord_flip() +
  xlab("States") + 
  ylab("Percentage") +
  ggtitle("Percent decrease of Average Tuition Cost") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 4),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 10))
 
  
### Question 4: Part A

absolute_tuition_change <- tuition_tidy %>%
  filter(Year == 2004 | Year == 2015) %>%
  group_by(State) %>%
  summarise(percent_avedecrease=(last(Average_Tuition)-first(Average_Tuition))) %>%
  ggplot(mapping = aes(reorder(State,
                               percent_avedecrease), percent_avedecrease)) + 
  geom_bar(stat='identity') + 
  coord_flip() +
  xlab("States") + 
  ylab("Percentage") +
  ggtitle("Absolute Value change in Tutition") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 4),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 10))

grid.arrange(percent_avetuition, absolute_tuition_change, ncol=2)


# Question 5

view(tuition_tidy)

tuition_tidy %>%
  filter(State == "Pennsylvania" | State == "New York" | State == "Ohio" |
         State == "Virginia" | State == "Kentucky") %>%
  ggplot(mapping = aes(x = Year, y = Average_Tuition, color=State, group=State)) + 
  geom_line() +
  geom_point()
  

# Bonus Question



tuition_tidy %>%
  ggplot() + 
  geom_line(mapping = aes(x = Year, y = Average_Tuition, group = State,
                          color = ifelse(State == 'New York','#073763','##5e1e42'))) +
  facet_wrap( ~State,nrow=5,) +
  xlab("Years (from 2004-2015)") + 
  ylab("Average Tuition") +
  ggtitle("Tuition Trends for 50 States") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 6),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 5),
        strip.text.x = element_text(size = 5)) +
  theme(legend.position = "none")
  

 # scale_color_discrete(name='',
 #                      labels=c('', '')) +


# Ti re adjust the graph size
 # theme(axis.text.x = element_text(angle = 40, hjust = 1))

# To save graph images in laptop
# ggsave("Walk Distance Graph", dpi=300, dev='jpg', height=4.5, width=6.5, units="in")

